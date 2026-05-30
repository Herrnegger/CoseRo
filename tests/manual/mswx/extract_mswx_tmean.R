# =============================================================================
# MSWX Tmean — Zonal Temperature Extraction over Catchments
# =============================================================================
# Extracts daily MSWX mean temperature from per-day NetCDF files over a
# shapefile of (sub)catchments. Produces:
#
#   - tmean_daily.csv              one column per catchment + area-weighted mean
#   - tmean_monthly.csv            monthly means (°C)
#   - tmean_annual.csv             annual means (°C)
#   - tmean_seasonality.csv        long-term monthly means per catchment
#   - seasonality.png              seasonality plot (per-catchment + weighted)
#   - annual_timeseries.png        annual area-weighted Tmean with trend line
#   - Tm_NZ_<startyear>_<endyear>.txt   COSERO-format temperature file
#                                       (one row per day: Y M D H M zone1 zone2 ...)
#
# Method: sparse weight matrix built once via exactextractr::coverage_fraction
# (same primitive used by SPARTACUS/WINFORE in this package), then zonal means
# computed as a single matrix multiplication per day. Mirrors the CHIRPS precip
# extraction script in tests/manual/chirpsv3/.
#
# Uses MSWX's bundled Tmean product directly (no Tmax/Tmin computation).
# For Alpine catchments with snow, prefer Dall'Amico-weighted Tmean from
# Tmax+Tmin (see R/spartacus_preprocessing.R) — MSWX Tmean is fine for
# temperate / tropical settings where snow is not a concern.
#
# NOT a package function — kept as a manual script because MSWX is general-
# purpose (not COSERO-specific) and the API may still evolve.
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

base_dir   <- "D:/Tools&Nettes/MSWX_Temperature"
input_dir  <- file.path(base_dir, "input/Kigzei_Major_Catchments")

shapefile_rel <- "Kigezi_major_catchments.shp"
id_field      <- "Catch_ID"  # column with catchment / zone IDs
name_field    <- NA          # human-readable name; NA if none
nz_field      <- NA          # column with COSERO NZ integers; NA disables COSERO output

start_year <- 1981
end_year   <- 2025

mswx_subdir  <- "Tmean"
mswx_pattern <- "^\\d{7}\\.nc$"   # YYYYDOY.nc (e.g. 2020001.nc)
mswx_label   <- "MSWX Tmean"

write_cosero_output <- TRUE   # also write Tm_NZ_<start>_<end>.txt

mswx_dir   <- file.path(base_dir, mswx_subdir)
output_dir <- file.path(base_dir, "output",
                        sprintf("MSWX_Tmean_%s_%d-%d",
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

# Reproject to EPSG:4326 (MSWX native CRS)
catchments_sf <- sf::st_transform(catchments_sf, 4326)
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
# 2. DISCOVER MSWX FILES AND FILTER BY YEAR
# =============================================================================

mswx_files <- list.files(mswx_dir, pattern = mswx_pattern, full.names = TRUE)
if (!length(mswx_files)) stop("No MSWX NetCDF files found in ", mswx_dir)

# Parse YYYYDOY from filename
extract_date <- function(f) {
  base <- tools::file_path_sans_ext(basename(f))
  as.Date(paste(substr(base, 1, 4), substr(base, 5, 7)), format = "%Y %j")
}
mswx_dates <- extract_date(mswx_files)

keep <- year(mswx_dates) >= start_year & year(mswx_dates) <= end_year
mswx_files <- mswx_files[keep]
mswx_dates <- mswx_dates[keep]
ord <- order(mswx_dates); mswx_files <- mswx_files[ord]; mswx_dates <- mswx_dates[ord]

cat("\nFound", length(mswx_files), mswx_label, "files for", start_year, "-", end_year,
    "(", format(min(mswx_dates)), "to", format(max(mswx_dates)), ")\n")

# =============================================================================
# 3. BUILD SPARSE WEIGHT MATRIX (once)
# =============================================================================

cat("\nBuilding sparse weight matrix from template raster ...\n")
r_template <- rast(mswx_files[1])
if (is.na(crs(r_template)) || crs(r_template) == "") crs(r_template) <- "EPSG:4326"

# Crop template to extent of catchments + small buffer
r_template <- crop(r_template, ext(vect(catchments_sf)) + 0.2)
cat("  Template cropped to", nrow(r_template), "x", ncol(r_template),
    "=", ncell(r_template), "cells\n")

cov_list <- exactextractr::coverage_fraction(r_template, catchments_sf,
                                              crop = FALSE)

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
row_sums <- Matrix::rowSums(W)
W <- W / row_sums

cat("  W: ", n_zones, " zones x ", length(used_cells), " cells (",
    round(length(weights) / (n_zones * length(used_cells)) * 100, 2), "% nnz)\n",
    sep = "")

uncovered <- which(row_sums == 0)
if (length(uncovered)) {
  warning("These zones have no MSWX coverage: ",
          paste(ids[uncovered], collapse = ", "),
          ". They will be NA in the output.", call. = FALSE)
}

# =============================================================================
# 4. EXTRACT DAILY ZONAL MEANS VIA SPARSE MATRIX MULTIPLICATION
# =============================================================================

cat(sprintf("\nExtracting zonal means from %d daily rasters (%d catchments) ...\n",
            length(mswx_files), n_zones))

pb <- progress_bar$new(
  format = "Extracting [:bar] :percent  Day :current/:total  ETA: :eta",
  total = length(mswx_files), clear = FALSE, width = 80
)

t_extract <- Sys.time()
zonal_mat <- matrix(NA_real_, nrow = n_zones, ncol = length(mswx_files))
last_valid_vals <- NULL   # carry forward for corrupt/unreadable files

for (i in seq_along(mswx_files)) {
  pb$tick()
  r <- tryCatch(rast(mswx_files[i]), error = function(e) NULL)

  if (is.null(r)) {
    if (!is.null(last_valid_vals)) {
      zonal_mat[, i] <- as.numeric(W %*% last_valid_vals)
    }
    next
  }

  if (is.na(crs(r)) || crs(r) == "") crs(r) <- "EPSG:4326"
  r <- crop(r, ext(r_template))

  cell_vals <- as.vector(values(r))[used_cells]
  if (anyNA(cell_vals) && !is.null(last_valid_vals)) {
    cell_vals[is.na(cell_vals)] <- last_valid_vals[is.na(cell_vals)]
  } else {
    cell_vals[is.na(cell_vals)] <- 0
  }

  zonal_mat[, i] <- as.numeric(W %*% cell_vals)
  last_valid_vals <- cell_vals
}

cat(sprintf("Extraction done in %.0f s.\n",
            as.numeric(difftime(Sys.time(), t_extract, units = "secs"))))

# Mark uncovered zones as NA throughout
if (length(uncovered)) zonal_mat[uncovered, ] <- NA

# Long-format frame
daily <- data.frame(
  Date  = rep(mswx_dates, each = n_zones),
  ID    = rep(ids, length(mswx_dates)),
  Tmean = round(as.vector(zonal_mat), 2),
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
  summarize(AreaWeightedAverage = round(wmean(Tmean, Area_km2), 2),
            .groups = "drop")

daily_wide <- daily %>%
  pivot_wider(id_cols = Date, names_from = ID, values_from = Tmean) %>%
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
write.csv(daily_wide, file.path(output_dir, "tmean_daily.csv"), row.names = FALSE)
cat("  Daily CSV saved.\n")

# =============================================================================
# 6. MONTHLY AND ANNUAL AGGREGATES
# =============================================================================

cat("Aggregating to monthly and annual ...\n"); flush.console()

monthly_long <- daily %>%
  mutate(Year = year(Date), Month = month(Date),
         Month_Start = floor_date(Date, "month")) %>%
  group_by(ID, Year, Month, Month_Start) %>%
  summarize(Monthly_C = round(mean(Tmean, na.rm = TRUE), 2),
            n_days    = n(), .groups = "drop")

monthly_w <- monthly_long %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Year, Month, Month_Start) %>%
  summarize(AreaWeightedAverage = round(wmean(Monthly_C, Area_km2), 2),
            .groups = "drop")

monthly_wide <- monthly_long %>%
  pivot_wider(id_cols = c(Year, Month, Month_Start),
              names_from = ID, values_from = Monthly_C) %>%
  arrange(Month_Start) %>%
  rename_cols() %>%
  left_join(monthly_w, by = c("Year", "Month", "Month_Start"))

write.csv(monthly_wide, file.path(output_dir, "tmean_monthly.csv"), row.names = FALSE)
cat("  Monthly CSV saved.\n"); flush.console()

annual_long <- daily %>%
  mutate(Year = year(Date)) %>%
  group_by(ID, Year) %>%
  summarize(Annual_C = round(mean(Tmean, na.rm = TRUE), 2),
            n_days   = n(), .groups = "drop")

annual_w <- annual_long %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Year) %>%
  summarize(AreaWeightedAverage = round(wmean(Annual_C, Area_km2), 2),
            .groups = "drop")

annual_wide <- annual_long %>%
  pivot_wider(id_cols = Year, names_from = ID, values_from = Annual_C) %>%
  arrange(Year) %>%
  rename_cols() %>%
  left_join(annual_w, by = "Year")

write.csv(annual_wide, file.path(output_dir, "tmean_annual.csv"), row.names = FALSE)
cat("  Annual CSV saved.\n"); flush.console()

# =============================================================================
# 7. SEASONALITY PLOT
# =============================================================================

cat("Building seasonality plot ...\n"); flush.console()

seasonal_per_id <- monthly_long %>%
  group_by(ID, Month) %>%
  summarize(Mean_C = mean(Monthly_C, na.rm = TRUE),
            SD_C   = sd(Monthly_C,   na.rm = TRUE),
            .groups = "drop") %>%
  left_join(lookup[, c("ID", "Name")], by = "ID") %>%
  mutate(Label = factor(if (any(ID != Name)) paste0(ID, " - ", Name) else ID,
                         levels = if (any(lookup$ID != lookup$Name))
                           paste0(lookup$ID, " - ", lookup$Name) else lookup$ID))

seasonal_w <- monthly_w %>%
  group_by(Month) %>%
  summarize(Mean_C = mean(AreaWeightedAverage, na.rm = TRUE),
            SD_C   = sd(AreaWeightedAverage,   na.rm = TRUE),
            .groups = "drop") %>%
  mutate(Label = "Area-weighted average")

month_levels <- month.abb
seasonal_per_id$Month_Name <- factor(month.abb[seasonal_per_id$Month], levels = month_levels)
seasonal_w$Month_Name      <- factor(month.abb[seasonal_w$Month],      levels = month_levels)

annual_mean_C <- mean(seasonal_w$Mean_C, na.rm = TRUE)
subtitle_txt <- sprintf("%s | %d-%d | area-weighted annual mean: %.1f°C | shaded band: ±1 SD",
                        mswx_label, start_year, end_year, annual_mean_C)

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
  mutate(ymin = Mean_C - SD_C, ymax = Mean_C + SD_C)

p_seasonal <- ggplot() +
  geom_ribbon(data = ribbon_df,
              aes(x = as.numeric(Month_Name), ymin = ymin, ymax = ymax),
              fill = "#90EE90", alpha = 0.55) +
  geom_line(data = seasonal_per_id,
            aes(x = as.numeric(Month_Name), y = Mean_C, group = Label, color = Label),
            linewidth = 0.7, alpha = 0.9) +
  geom_point(data = seasonal_per_id,
             aes(x = as.numeric(Month_Name), y = Mean_C, color = Label),
             size = 1.6, alpha = 0.9) +
  geom_line(data = seasonal_w,
            aes(x = as.numeric(Month_Name), y = Mean_C, group = 1),
            color = "#2c5f2d", linewidth = 1.3) +
  geom_point(data = seasonal_w,
             aes(x = as.numeric(Month_Name), y = Mean_C),
             color = "#2c5f2d", size = 2.5) +
  scale_x_continuous(breaks = 1:12, labels = month_levels) +
  labs(title = sprintf("Mean Monthly Temperature — %s",
                       tools::file_path_sans_ext(basename(shapefile_rel))),
       subtitle = subtitle_txt,
       x = NULL, y = "Mean temperature (°C)",
       color = "Catchment") +
  base_theme

ggsave(file.path(output_dir, "seasonality.png"),
       p_seasonal, width = 12, height = 6, dpi = 300, bg = "white")
cat("  Seasonality plot saved.\n"); flush.console()

seasonal_per_id_out <- seasonal_per_id %>%
  select(ID, Name, Month, Mean_C, SD_C) %>%
  mutate(Mean_C = round(Mean_C, 2), SD_C = round(SD_C, 2)) %>%
  pivot_wider(id_cols = Month, names_from = ID, values_from = Mean_C) %>%
  rename_cols() %>%
  left_join(seasonal_w %>% transmute(Month, AreaWeightedAverage = round(Mean_C, 2)),
            by = "Month") %>%
  arrange(Month)
write.csv(seasonal_per_id_out,
          file.path(output_dir, "tmean_seasonality.csv"), row.names = FALSE)
cat("  Seasonality CSV saved.\n"); flush.console()

# =============================================================================
# 8. ANNUAL TIMESERIES PLOT (with trend line)
# =============================================================================

cat("Building annual timeseries plot ...\n"); flush.console()

p_annual <- ggplot(annual_w, aes(x = Year, y = AreaWeightedAverage)) +
  geom_line(color = "#2c5f2d", linewidth = 1) +
  geom_point(color = "#2c5f2d", size = 2.5) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40",
              linetype = "dashed", linewidth = 0.8) +
  labs(title    = sprintf("Annual Mean Temperature — %s",
                          tools::file_path_sans_ext(basename(shapefile_rel))),
       subtitle = sprintf("Area-weighted across all catchments | overall mean: %.2f°C",
                          mean(annual_w$AreaWeightedAverage, na.rm = TRUE)),
       x = "Year", y = "Mean temperature (°C)") +
  base_theme

ggsave(file.path(output_dir, "annual_timeseries.png"),
       p_annual, width = 10, height = 6, dpi = 300, bg = "white")
cat("  Annual timeseries plot saved.\n"); flush.console()

# =============================================================================
# 9. COSERO-FORMAT OUTPUT
# =============================================================================

if (write_cosero_output) {
  cat("Writing COSERO-format temperature file ...\n"); flush.console()

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

  out_mat <- zonal_mat[cosero_order, , drop = FALSE]
  out_mat[is.na(out_mat)] <- -999

  cosero_df <- data.frame(
    Y = as.integer(format(mswx_dates, "%Y")),
    M = as.integer(format(mswx_dates, "%m")),
    D = as.integer(format(mswx_dates, "%d")),
    H = 0L, Min = 0L,
    round(t(out_mat), 2)
  )

  cosero_file <- file.path(output_dir,
                            sprintf("Tm_NZ_%d_%d.txt", start_year, end_year))
  data.table::fwrite(cosero_df, cosero_file,
                     sep = " ", col.names = FALSE, quote = FALSE)
  cat("  COSERO file saved:", basename(cosero_file),
      sprintf("(%d days × %d zones)\n", length(mswx_dates), n_zones))
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\nAll outputs in:", output_dir, "\n")
