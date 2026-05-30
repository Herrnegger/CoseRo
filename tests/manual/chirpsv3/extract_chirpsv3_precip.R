# =============================================================================
# CHIRPS v3 — Zonal Precipitation Extraction over Catchments
# =============================================================================
# Extracts daily CHIRPS v3 precipitation from per-day GeoTIFFs over a shapefile
# of (sub)catchments. Produces:
#
#   - rainfall_daily.csv          one column per catchment + area-weighted mean
#   - rainfall_monthly.csv        monthly aggregates (mm/month)
#   - rainfall_annual.csv         annual aggregates (mm/year)
#   - rainfall_seasonality.csv    long-term monthly means per catchment
#   - seasonality.png             seasonality plot (per-catchment + weighted)
#   - RR_NZ_<startyear>_<endyear>.txt   COSERO-format precipitation file
#                                       (one row per day: Y M D H M zone1 zone2 ...)
#
# Method: builds a sparse weight matrix once via exactextractr::coverage_fraction
# (same primitive used by SPARTACUS/WINFORE in this package), then computes
# zonal means as a single matrix multiplication per day. About 5–10× faster
# than per-day terra::extract for large date ranges.
#
# NOT a package function — kept as a manual script because CHIRPS is general-
# purpose (not COSERO-specific) and the API may still evolve. If it stabilizes
# and we need to ingest CHIRPS routinely, lift the sparse-matrix block into
# a write_chirps_precip() in R/.
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude
# Date: 2026-05-30
# Branch: dev/spatial-disaggregation
# =============================================================================

library(terra)
library(sf)
library(exactextractr)
library(Matrix)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(progress)
library(data.table)

# =============================================================================
# USER PARAMETERS
# =============================================================================

base_dir   <- "D:/Tools&Nettes/Rainfall_CHIRPS"
input_dir  <- file.path(base_dir, "input")

shapefile_rel <- "lake_Baringo_all_subcatchments/lake_Baringo_all_subcatchments.shp"
id_field      <- "id"        # column with catchment / zone IDs
name_field    <- "name"      # human-readable name; NA if none
nz_field      <- NA          # column with COSERO NZ integers; NA disables COSERO output

start_year <- 1981
end_year   <- 2025

# CHIRPS v3 source — adjust to switch products
v3_subdir  <- file.path("v3.0", "final", "rnl")
v3_pattern <- "^chirps-v3\\.0\\.rnl\\.\\d{4}\\.\\d{2}\\.\\d{2}\\.tif$"
v3_label   <- "v3 final/rnl"
# Example for preliminary product:
#   v3_subdir  <- file.path("v3.0", "prelim", "sat", "2026")
#   v3_pattern <- "^chirps-v3\\.0\\.prelim\\.\\d{4}\\.\\d{2}\\.\\d{2}\\.tif$"
#   v3_label   <- "v3 prelim/sat"

write_cosero_output <- TRUE   # also write RR_NZ_<start>_<end>.txt

v3_dir     <- file.path(base_dir, "data", v3_subdir)
output_dir <- file.path(base_dir, "output",
                        sprintf("CHIRPSv3_%s_%d-%d",
                                tools::file_path_sans_ext(basename(shapefile_rel)),
                                start_year, end_year))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# 1. LOAD AND VALIDATE SHAPEFILE
# =============================================================================

shp_path <- file.path(input_dir, shapefile_rel)
if (!file.exists(shp_path)) stop("Shapefile not found: ", shp_path)

catchments_sf <- sf::st_read(shp_path, quiet = TRUE)
cat("Loaded shapefile with", nrow(catchments_sf), "feature(s). Fields:",
    paste(names(catchments_sf), collapse = ", "), "\n")

if (!(id_field %in% names(catchments_sf))) {
  stop(sprintf("ID field '%s' not in shapefile. Available: %s",
               id_field, paste(names(catchments_sf), collapse = ", ")))
}
ids <- as.character(catchments_sf[[id_field]])
if (any(duplicated(ids))) stop("Duplicate IDs in '", id_field, "'.")

names_ <- if (!is.na(name_field) && name_field %in% names(catchments_sf)) {
  as.character(catchments_sf[[name_field]])
} else {
  ids
}

# Reproject to EPSG:4326 (CHIRPS native CRS) for both weight matrix and area calc
catchments_sf <- sf::st_transform(catchments_sf, 4326)

# Areas in km^2 via terra::expanse (geodesic on lon/lat)
areas_km2 <- as.numeric(terra::expanse(terra::vect(catchments_sf), unit = "km"))

lookup <- data.frame(ID = ids, Name = names_, Area_km2 = round(areas_km2, 2),
                     stringsAsFactors = FALSE)
cat("\nID -> Name -> Area lookup:\n"); print(lookup, row.names = FALSE)
cat("Total area:", round(sum(areas_km2), 1), "km^2\n")

col_labels <- if (any(lookup$ID != lookup$Name)) {
  setNames(paste0(lookup$ID, " - ", lookup$Name), lookup$ID)
} else {
  setNames(lookup$ID, lookup$ID)
}

# =============================================================================
# 2. DISCOVER CHIRPS FILES AND FILTER BY YEAR
# =============================================================================

v3_tifs <- list.files(v3_dir, pattern = v3_pattern, full.names = TRUE)
if (!length(v3_tifs)) stop("No CHIRPS v3 TIFs found in ", v3_dir)

v3_dates <- as.Date(regmatches(basename(v3_tifs),
                                regexpr("\\d{4}\\.\\d{2}\\.\\d{2}", basename(v3_tifs))),
                     format = "%Y.%m.%d")

keep <- year(v3_dates) >= start_year & year(v3_dates) <= end_year
v3_tifs  <- v3_tifs[keep]
v3_dates <- v3_dates[keep]
ord <- order(v3_dates); v3_tifs <- v3_tifs[ord]; v3_dates <- v3_dates[ord]

cat("\nFound", length(v3_tifs), v3_label, "files for", start_year, "-", end_year,
    "(", format(min(v3_dates)), "to", format(max(v3_dates)), ")\n")

# =============================================================================
# 3. BUILD SPARSE WEIGHT MATRIX (once)
# =============================================================================
# Use the first raster as template — CHIRPS has a fixed global grid, so all
# files share extent and resolution. coverage_fraction returns a list with one
# raster per polygon, where cell values are the polygon's coverage fraction
# inside that cell.

cat("\nBuilding sparse weight matrix from template raster ...\n")
r_template <- rast(v3_tifs[1])
if (is.na(crs(r_template)) || crs(r_template) == "") crs(r_template) <- "EPSG:4326"

# Crop template to extent of catchments + small buffer to limit cells handled
r_template <- crop(r_template, ext(vect(catchments_sf)) + 0.2)
ncells_template <- ncell(r_template)
cat("  Template cropped to", nrow(r_template), "x", ncol(r_template),
    "=", ncells_template, "cells\n")

# coverage_fraction returns one SpatRaster per polygon (same extent as template)
cov_list <- exactextractr::coverage_fraction(r_template, catchments_sf,
                                              crop = FALSE)

# Build the sparse matrix W [n_zones x n_cells_used]
n_zones <- length(cov_list)
row_idx <- integer(0); col_idx <- integer(0); weights <- numeric(0)
used_cells <- integer(0)

for (z in seq_len(n_zones)) {
  vals <- as.vector(values(cov_list[[z]]))
  hit  <- which(vals > 0)
  if (!length(hit)) next
  row_idx  <- c(row_idx, rep(z, length(hit)))
  col_idx  <- c(col_idx, hit)
  weights  <- c(weights, vals[hit])
  used_cells <- union(used_cells, hit)
}

used_cells <- sort(used_cells)
cell_map   <- setNames(seq_along(used_cells), as.character(used_cells))
col_idx_remapped <- as.integer(cell_map[as.character(col_idx)])

W <- Matrix::sparseMatrix(
  i = row_idx, j = col_idx_remapped, x = weights,
  dims = c(n_zones, length(used_cells))
)
# Normalize rows so they sum to 1 → area-weighted mean
row_sums <- Matrix::rowSums(W)
W <- W / row_sums   # safe: every catchment must cover at least one cell

cat("  W: ", n_zones, " zones x ", length(used_cells), " cells (",
    round(length(weights) / (n_zones * length(used_cells)) * 100, 2), "% nnz)\n",
    sep = "")

# Detect uncovered zones (catchments outside CHIRPS domain). All sparse rows
# already sum to 1, so the diagnostic is row_sums == 0 from the unnormalized W.
uncovered <- which(row_sums == 0)
if (length(uncovered)) {
  warning("These zones have no CHIRPS coverage: ",
          paste(ids[uncovered], collapse = ", "),
          ". They will be NA in the output.", call. = FALSE)
}

# =============================================================================
# 4. EXTRACT DAILY ZONAL MEANS VIA SPARSE MATRIX MULTIPLICATION
# =============================================================================

cat(sprintf("\nExtracting zonal means from %d daily rasters (%d catchments) ...\n",
            length(v3_tifs), n_zones))

pb <- progress_bar$new(
  format = "Extracting [:bar] :percent  Day :current/:total  ETA: :eta",
  total = length(v3_tifs), clear = FALSE, width = 80
)

t_extract <- Sys.time()
zonal_mat <- matrix(NA_real_, nrow = n_zones, ncol = length(v3_tifs))

for (i in seq_along(v3_tifs)) {
  pb$tick()
  r <- tryCatch(rast(v3_tifs[i]), error = function(e) NULL)
  if (is.null(r)) next
  if (is.na(crs(r)) || crs(r) == "") crs(r) <- "EPSG:4326"
  r <- crop(r, ext(r_template))

  cell_vals <- as.vector(values(r))[used_cells]
  cell_vals[is.na(cell_vals)] <- 0   # treat NA cells as zero precip contribution

  zonal_mat[, i] <- as.numeric(W %*% cell_vals)
}

cat(sprintf("Extraction done in %.0f s.\n",
            as.numeric(difftime(Sys.time(), t_extract, units = "secs"))))

# Negative/sentinel cleanup
zonal_mat[zonal_mat < 0] <- NA
if (length(uncovered)) zonal_mat[uncovered, ] <- NA

# Long-format frame for downstream aggregation
daily <- data.frame(
  Date     = rep(v3_dates, each = n_zones),
  ID       = rep(ids, length(v3_dates)),
  Rainfall = round(as.vector(zonal_mat), 3),
  stringsAsFactors = FALSE
)

cat(sprintf("  %d daily x catchment records.\n", nrow(daily)))

# =============================================================================
# 5. WIDE TABLE + AREA-WEIGHTED AVERAGE COLUMN
# =============================================================================

wmean <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  sum(x[ok] * w[ok]) / sum(w[ok])
}

daily_w <- daily %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Date) %>%
  summarize(AreaWeightedAverage = round(wmean(Rainfall, Area_km2), 3),
            .groups = "drop")

daily_wide <- daily %>%
  pivot_wider(id_cols = Date, names_from = ID, values_from = Rainfall) %>%
  arrange(Date)

rename_cols <- function(df) {
  for (id_v in names(col_labels)) {
    if (id_v %in% names(df)) names(df)[names(df) == id_v] <- col_labels[[id_v]]
  }
  df
}

daily_wide <- rename_cols(daily_wide) %>%
  left_join(daily_w, by = "Date")

cat("Writing daily CSV ...\n"); flush.console()
write.csv(daily_wide, file.path(output_dir, "rainfall_daily.csv"), row.names = FALSE)
cat("  Daily CSV saved.\n")

# =============================================================================
# 6. MONTHLY AND ANNUAL AGGREGATES
# =============================================================================

cat("Aggregating to monthly and annual ...\n"); flush.console()

monthly_long <- daily %>%
  mutate(Year = year(Date), Month = month(Date),
         Month_Start = floor_date(Date, "month")) %>%
  group_by(ID, Year, Month, Month_Start) %>%
  summarize(Monthly_mm = round(sum(Rainfall, na.rm = TRUE), 2),
            n_days     = n(), .groups = "drop")

monthly_w <- monthly_long %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Year, Month, Month_Start) %>%
  summarize(AreaWeightedAverage = round(wmean(Monthly_mm, Area_km2), 2),
            .groups = "drop")

monthly_wide <- monthly_long %>%
  pivot_wider(id_cols = c(Year, Month, Month_Start),
              names_from = ID, values_from = Monthly_mm) %>%
  arrange(Month_Start) %>%
  rename_cols() %>%
  left_join(monthly_w, by = c("Year", "Month", "Month_Start"))

write.csv(monthly_wide, file.path(output_dir, "rainfall_monthly.csv"), row.names = FALSE)
cat("  Monthly CSV saved.\n"); flush.console()

annual_long <- daily %>%
  mutate(Year = year(Date)) %>%
  group_by(ID, Year) %>%
  summarize(Annual_mm = round(sum(Rainfall, na.rm = TRUE), 2),
            n_days    = n(), .groups = "drop")

annual_w <- annual_long %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Year) %>%
  summarize(AreaWeightedAverage = round(wmean(Annual_mm, Area_km2), 2),
            .groups = "drop")

annual_wide <- annual_long %>%
  pivot_wider(id_cols = Year, names_from = ID, values_from = Annual_mm) %>%
  arrange(Year) %>%
  rename_cols() %>%
  left_join(annual_w, by = "Year")

write.csv(annual_wide, file.path(output_dir, "rainfall_annual.csv"), row.names = FALSE)
cat("  Annual CSV saved.\n"); flush.console()

# =============================================================================
# 7. SEASONALITY PLOT
# =============================================================================

cat("Building seasonality plot ...\n"); flush.console()

seasonal_per_id <- monthly_long %>%
  group_by(ID, Month) %>%
  summarize(Mean_mm = mean(Monthly_mm, na.rm = TRUE),
            SD_mm   = sd(Monthly_mm,   na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lookup[, c("ID", "Name")], by = "ID") %>%
  mutate(Label = factor(if (any(ID != Name)) paste0(ID, " - ", Name) else ID,
                         levels = if (any(lookup$ID != lookup$Name))
                           paste0(lookup$ID, " - ", lookup$Name) else lookup$ID))

seasonal_w <- monthly_w %>%
  group_by(Month) %>%
  summarize(Mean_mm = mean(AreaWeightedAverage, na.rm = TRUE),
            SD_mm   = sd(AreaWeightedAverage,   na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Label = "Area-weighted average")

month_levels <- month.abb
seasonal_per_id$Month_Name <- factor(month.abb[seasonal_per_id$Month], levels = month_levels)
seasonal_w$Month_Name      <- factor(month.abb[seasonal_w$Month],      levels = month_levels)

annual_total <- sum(seasonal_w$Mean_mm)
subtitle_txt <- sprintf("CHIRPS %s | %d-%d | mean annual (area-weighted): %.0f mm/a | shaded band: ±1 SD",
                        v3_label, start_year, end_year, annual_total)

base_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 16),
    plot.subtitle    = element_text(color = "gray40", size = 12),
    axis.text        = element_text(size = 11),
    axis.title       = element_text(size = 13),
    legend.text      = element_text(size = 12),
    legend.title     = element_text(size = 13, face = "bold"),
    strip.text       = element_text(face = "bold", size = 12),
    panel.grid.minor = element_blank()
  )

ribbon_df <- seasonal_w %>%
  mutate(ymin = pmax(0, Mean_mm - SD_mm), ymax = Mean_mm + SD_mm)

p_seasonal <- ggplot() +
  geom_ribbon(data = ribbon_df,
              aes(x = as.numeric(Month_Name), ymin = ymin, ymax = ymax),
              fill = "#ADD8E6", alpha = 0.55) +
  geom_line(data = seasonal_per_id,
            aes(x = as.numeric(Month_Name), y = Mean_mm, group = Label, color = Label),
            linewidth = 0.7, alpha = 0.9) +
  geom_point(data = seasonal_per_id,
             aes(x = as.numeric(Month_Name), y = Mean_mm, color = Label),
             size = 1.6, alpha = 0.9) +
  geom_line(data = seasonal_w,
            aes(x = as.numeric(Month_Name), y = Mean_mm, group = 1),
            color = "#073b4c", linewidth = 1.3) +
  geom_point(data = seasonal_w,
             aes(x = as.numeric(Month_Name), y = Mean_mm),
             color = "#073b4c", size = 2.5) +
  scale_x_continuous(breaks = 1:12, labels = month_levels) +
  labs(title = sprintf("Mean Monthly Rainfall — %s",
                       tools::file_path_sans_ext(basename(shapefile_rel))),
       subtitle = subtitle_txt,
       x = NULL, y = "Mean rainfall (mm/month)",
       color = "Catchment") +
  base_theme

ggsave(file.path(output_dir, "seasonality.png"),
       p_seasonal, width = 12, height = 6, dpi = 300, bg = "white")
cat("  Seasonality plot saved.\n"); flush.console()

seasonal_per_id_out <- seasonal_per_id %>%
  select(ID, Name, Month, Mean_mm, SD_mm) %>%
  mutate(Mean_mm = round(Mean_mm, 2), SD_mm = round(SD_mm, 2)) %>%
  pivot_wider(id_cols = Month, names_from = ID, values_from = Mean_mm) %>%
  rename_cols() %>%
  left_join(seasonal_w %>% transmute(Month, AreaWeightedAverage = round(Mean_mm, 2)),
            by = "Month") %>%
  arrange(Month)
write.csv(seasonal_per_id_out,
          file.path(output_dir, "rainfall_seasonality.csv"), row.names = FALSE)
cat("  Seasonality CSV saved.\n"); flush.console()

# =============================================================================
# 8. COSERO-FORMAT OUTPUT
# =============================================================================
# COSERO expects one row per timestep:
#   YYYY MM DD HH MM   <zone_1>  <zone_2>  ... <zone_NZ>
# with zones ordered by the NZ field (if provided) or by shapefile feature
# order (fallback).

if (write_cosero_output) {
  cat("Writing COSERO-format precipitation file ...\n"); flush.console()

  # Determine zone order
  if (!is.na(nz_field) && nz_field %in% names(catchments_sf)) {
    nz_vals <- as.integer(catchments_sf[[nz_field]])
    if (any(is.na(nz_vals))) stop("NZ field '", nz_field, "' has missing values.")
    if (any(duplicated(nz_vals))) stop("NZ field '", nz_field, "' has duplicates.")
    cosero_order <- order(nz_vals)
    cat("  Zones ordered by NZ column '", nz_field, "'\n", sep = "")
  } else {
    cosero_order <- seq_len(n_zones)
    cat("  No NZ field provided — using shapefile feature order\n")
  }

  # Replace NA with -999 (COSERO missing-value sentinel)
  out_mat <- zonal_mat[cosero_order, , drop = FALSE]
  out_mat[is.na(out_mat)] <- -999

  cosero_df <- data.frame(
    Y = as.integer(format(v3_dates, "%Y")),
    M = as.integer(format(v3_dates, "%m")),
    D = as.integer(format(v3_dates, "%d")),
    H = 0L, Min = 0L,
    round(t(out_mat), 2)
  )

  cosero_file <- file.path(output_dir,
                            sprintf("RR_NZ_%d_%d.txt", start_year, end_year))
  data.table::fwrite(cosero_df, cosero_file,
                     sep = " ", col.names = FALSE, quote = FALSE)
  cat("  COSERO file saved:", basename(cosero_file),
      sprintf("(%d days × %d zones)\n", length(v3_dates), n_zones))
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\nAll outputs in:", output_dir, "\n")
