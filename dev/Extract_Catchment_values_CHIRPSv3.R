# Extract_Catchment_values_CHIRPSv3.R
# Generic extraction of CHIRPS v3 daily GeoTIFFs over a shapefile of (sub)catchments.
# Produces daily / monthly / annual CSVs with one column per catchment plus an
# area-weighted average column, and a seasonality plot per catchment.

# 1. User parameters ####
shapefile_rel  <- "lake_Baringo_all_subcatchments/lake_Baringo_all_subcatchments.shp"
id_field       <- "id"           # column used to identify catchments in outputs
name_field     <- "name"         # optional human-readable name; set to NA if none
start_year     <- 1981
end_year       <- 2025

# CHIRPS v3 source: change to switch dataset
v3_subdir      <- file.path("v3.0", "final", "rnl")
v3_pattern     <- "^chirps-v3\\.0\\.rnl\\.\\d{4}\\.\\d{2}\\.\\d{2}\\.tif$"
v3_label       <- "v3 final/rnl"
# For prelim/sat 2026 use e.g.
#   v3_subdir  <- file.path("v3.0", "prelim", "sat", "2026")
#   v3_pattern <- "^chirps-v3\\.0\\.prelim\\.\\d{4}\\.\\d{2}\\.\\d{2}\\.tif$"
#   v3_label   <- "v3 prelim/sat"

base_dir   <- "D:/Tools&Nettes/Rainfall_CHIRPS"
input_dir  <- file.path(base_dir, "input")
v3_dir     <- file.path(base_dir, "data", v3_subdir)
output_dir <- file.path(base_dir, "output",
                        sprintf("CHIRPSv3_%s_%d-%d",
                                tools::file_path_sans_ext(basename(shapefile_rel)),
                                start_year, end_year))
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# 2. Packages ####
pkgs <- c("terra", "lubridate", "dplyr", "tidyr", "ggplot2", "scales", "progress")
for (p in pkgs) {
  if (!require(p, character.only = TRUE)) { install.packages(p); library(p, character.only = TRUE) }
}

# 3. Shapefile prep ####
shp_path <- file.path(input_dir, shapefile_rel)
if (!file.exists(shp_path)) stop("Shapefile not found: ", shp_path)
catchments <- vect(shp_path)
cat("Loaded shapefile with", nrow(catchments), "feature(s). Fields:",
    paste(names(catchments), collapse = ", "), "\n")

if (!(id_field %in% names(catchments))) {
  stop(sprintf("ID field '%s' not in shapefile. Available: %s",
               id_field, paste(names(catchments), collapse = ", ")))
}
ids <- as.character(values(catchments)[[id_field]])
if (any(duplicated(ids))) stop("Duplicate IDs in '", id_field, "'.")

if (!is.na(name_field) && name_field %in% names(catchments)) {
  names_ <- as.character(values(catchments)[[name_field]])
} else {
  names_ <- ids
}

catchments_4326 <- project(catchments, "EPSG:4326")
areas_km2 <- as.numeric(expanse(catchments_4326, unit = "km"))
lookup <- data.frame(ID = ids, Name = names_, Area_km2 = round(areas_km2, 2),
                     stringsAsFactors = FALSE)
cat("\nID -> Name -> Area lookup:\n"); print(lookup, row.names = FALSE)
cat("Total area:", round(sum(areas_km2), 1), "km^2\n")

# Sortable column labels: "ID — Name" if Names differ; otherwise just ID
col_labels <- if (any(lookup$ID != lookup$Name)) {
  setNames(paste0(lookup$ID, " - ", lookup$Name), lookup$ID)
} else {
  setNames(lookup$ID, lookup$ID)
}

# 4. Discover v3 TIFs and filter by year ####
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

# 5. Extract daily zonal means ####
extract_day <- function(tif_path, the_date) {
  r <- tryCatch(rast(tif_path), error = function(e) NULL)
  if (is.null(r)) return(NULL)
  if (is.na(crs(r)) || crs(r) == "") crs(r) <- "EPSG:4326"
  r_crop <- crop(r, ext(catchments_4326) + 0.2)
  vals <- terra::extract(r_crop, catchments_4326,
                         fun = "mean", exact = FALSE, na.rm = TRUE)
  data.frame(Date = the_date, ID = ids,
             Rainfall = round(as.numeric(vals[, 2]), 3),
             stringsAsFactors = FALSE)
}

cat(sprintf("\nExtracting zonal means from %d daily rasters over %d catchment(s) ...\n",
            length(v3_tifs), nrow(catchments_4326)))
t_extract <- Sys.time()

pb <- progress_bar$new(
  format = "Extracting [:bar] :percent  Day :current/:total  ETA: :eta",
  total = length(v3_tifs), clear = FALSE, width = 80
)

daily_list <- vector("list", length(v3_tifs))
for (i in seq_along(v3_tifs)) {
  pb$tick()
  daily_list[[i]] <- extract_day(v3_tifs[i], v3_dates[i])
}

cat(sprintf("Extraction done in %.0f s.\n",
            as.numeric(difftime(Sys.time(), t_extract, units = "secs"))))

cat("Combining daily records ...\n"); flush.console()
daily <- bind_rows(daily_list)
daily$Rainfall[daily$Rainfall < 0] <- NA
cat(sprintf("  %d daily x catchment records.\n", nrow(daily)))

# 6. Long -> wide with area-weighted average column ####
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

# Rename catchment columns to "ID - Name" if Names differ
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

# 7. Monthly and annual aggregations ####
cat("Aggregating to monthly and annual ...\n"); flush.console()
monthly_long <- daily %>%
  mutate(Year = year(Date), Month = month(Date),
         Month_Start = floor_date(Date, "month")) %>%
  group_by(ID, Year, Month, Month_Start) %>%
  summarize(Monthly_mm = round(sum(Rainfall, na.rm = TRUE), 2),
            n_days = n(), .groups = "drop")

monthly_w <- monthly_long %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Year, Month, Month_Start) %>%
  summarize(AreaWeightedAverage = round(wmean(Monthly_mm, Area_km2), 2),
            .groups = "drop")

monthly_wide <- monthly_long %>%
  pivot_wider(id_cols = c(Year, Month, Month_Start),
              names_from = ID, values_from = Monthly_mm) %>%
  arrange(Month_Start)
monthly_wide <- rename_cols(monthly_wide) %>%
  left_join(monthly_w, by = c("Year", "Month", "Month_Start"))

write.csv(monthly_wide, file.path(output_dir, "rainfall_monthly.csv"), row.names = FALSE)
cat("  Monthly CSV saved.\n"); flush.console()

annual_long <- daily %>%
  mutate(Year = year(Date)) %>%
  group_by(ID, Year) %>%
  summarize(Annual_mm = round(sum(Rainfall, na.rm = TRUE), 2),
            n_days = n(), .groups = "drop")

annual_w <- annual_long %>%
  left_join(lookup[, c("ID", "Area_km2")], by = "ID") %>%
  group_by(Year) %>%
  summarize(AreaWeightedAverage = round(wmean(Annual_mm, Area_km2), 2),
            .groups = "drop")

annual_wide <- annual_long %>%
  pivot_wider(id_cols = Year, names_from = ID, values_from = Annual_mm) %>%
  arrange(Year)
annual_wide <- rename_cols(annual_wide) %>%
  left_join(annual_w, by = "Year")

write.csv(annual_wide, file.path(output_dir, "rainfall_annual.csv"), row.names = FALSE)
cat("  Annual CSV saved.\n"); flush.console()

# 8. Seasonality plot — mean monthly rainfall per catchment + weighted ####
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

# Combined seasonality plot: per-catchment thin lines + weighted thick line + SD band
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

# Save the per-catchment seasonality table as CSV too
seasonal_per_id_out <- seasonal_per_id %>%
  select(ID, Name, Month, Mean_mm, SD_mm) %>%
  mutate(Mean_mm = round(Mean_mm, 2), SD_mm = round(SD_mm, 2)) %>%
  pivot_wider(id_cols = Month, names_from = ID, values_from = Mean_mm)
seasonal_per_id_out <- rename_cols(seasonal_per_id_out) %>%
  left_join(seasonal_w %>% transmute(Month, AreaWeightedAverage = round(Mean_mm, 2)),
            by = "Month") %>%
  arrange(Month)
write.csv(seasonal_per_id_out,
          file.path(output_dir, "rainfall_seasonality.csv"), row.names = FALSE)
cat("  Seasonality CSV saved.\n"); flush.console()

cat("\nAll outputs in:", output_dir, "\n")
