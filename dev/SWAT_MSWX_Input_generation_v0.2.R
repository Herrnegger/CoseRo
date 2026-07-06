# ====================================================================================================
# Script: MSWX Temperature to SWAT Input Data Generator
# Purpose: Process MSWX NetCDF temperature data to generate SWAT model input files
# Author: Gemini AI Refactor
# ====================================================================================================

# 1. SETUP ####
# ====================================================================================================

## 1.1 Load Packages ----
# Installs packages if they are not already available, then loads them.
required_packages <- c("terra", "sf", "lubridate", "dplyr", "tidyr", "ggplot2", "scales")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
sapply(required_packages, require, character.only = TRUE)

## 1.2 User Parameters ----
# Define base paths
base_dir <- "D:/Tools&Nettes/MSWX_Temperature"
output_dir <- file.path(base_dir, "Mpanga/SWAT_Tmin_Tmax_output_until_2025")

# Define input file paths
nc_tmax_dir <- file.path(base_dir, "Tmax")
nc_tmin_dir <- file.path(base_dir, "Tmin")
dem_file <- file.path(base_dir, "Mpanga/Data/other/DEM_Mpanga_MERIT.tif")
shapefile <- file.path(base_dir, "Mpanga/Data/other/Mpanga_Watershed_SWAT_1.shp")

# SWAT & QC parameters
# start_date <- as.Date("2001-01-01") # NOTE: This is now determined automatically from the data.
min_valid_temp <- -50 # Min valid temperature (°C)
max_valid_temp <- 60  # Max valid temperature (°C)

## 1.3 Validate Inputs & Create Output Directory ----
# Stop with an error if any required file or directory does not exist.
input_paths <- c("Tmax Folder" = nc_tmax_dir, "Tmin Folder" = nc_tmin_dir, "DEM File" = dem_file, "Shapefile" = shapefile)
for(name in names(input_paths)){
  path <- input_paths[name]
  if (!file.exists(path)) stop(paste(name, "not found at:", path))
}

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
cat("✓ Setup complete. Inputs validated and output directory is ready.\n")

# ====================================================================================================
# 2. LOAD & PREPARE SPATIAL DATA ####
# ====================================================================================================
cat("-> Loading spatial data and processing grid information...\n")

## 2.1 Load Vectors and Rasters ----
watershed_sf <- st_read(shapefile, quiet = TRUE) |> st_transform(4326)
dem <- rast(dem_file)
first_nc <- rast(list.files(nc_tmax_dir, pattern = "\\.nc$", full.names = TRUE)[1])

## 2.2 Define Analysis Grid ----
# Project watershed to NetCDF CRS and find all grid cells within it
watershed_nc_crs <- project(vect(watershed_sf), crs(first_nc))
buffer_size <- max(res(first_nc)) * 2
watershed_ext_buffered <- ext(watershed_nc_crs) + buffer_size

# Get cell numbers and coordinates for the extraction
grid_subset <- crop(first_nc, watershed_ext_buffered)
cell_numbers <- cells(grid_subset, watershed_nc_crs)[, 2]
coords_latlon <- xyFromCell(grid_subset, cell_numbers) |>
  as.data.frame() |>
  st_as_sf(coords = c("x", "y"), crs = crs(first_nc)) |>
  st_transform(4326) |>
  st_coordinates()

n_cells <- nrow(coords_latlon)
cat(sprintf("✓ Found %d grid cells within the watershed.\n", n_cells))

# ====================================================================================================
# 3. PROCESS TEMPERATURE DATA ####
# ====================================================================================================
cat("-> Preparing and validating temperature file lists...\n")

## 3.1 Check & Synchronize Temperature Files ----
extract_date <- function(f) as.Date(paste0(substr(basename(f), 1, 4), "-", substr(basename(f), 5, 7)), format="%Y-%j")
nc_tmax_files <- list.files(nc_tmax_dir, pattern = "\\.nc$", full.names = TRUE)
nc_tmin_files <- list.files(nc_tmin_dir, pattern = "\\.nc$", full.names = TRUE)

tmax_dates <- extract_date(nc_tmax_files)
tmin_dates <- extract_date(nc_tmin_files)

# Find any date mismatches and provide a detailed error
missing_tmin <- setdiff(tmax_dates, tmin_dates)
missing_tmax <- setdiff(tmin_dates, tmax_dates)
if (length(missing_tmin) > 0 || length(missing_tmax) > 0) {
  msg <- "Error: Temp file mismatch."
  if (length(missing_tmin) > 0) msg <- paste(msg, sprintf("\n - Missing Tmin files for: %s", paste(head(missing_tmin, 5), collapse=", ")))
  if (length(missing_tmax) > 0) msg <- paste(msg, sprintf("\n - Missing Tmax files for: %s", paste(head(missing_tmax, 5), collapse=", ")))
  stop(msg)
}
# Use the validated & sorted date sequence for the rest of the script
dates <- sort(tmax_dates)
start_date <- dates[1] # Automatically set start_date to the first day in the dataset
n_days <- length(dates)
nc_tmax_files <- nc_tmax_files[order(tmax_dates)]
nc_tmin_files <- nc_tmin_files[order(tmin_dates)]
cat(sprintf("✓ Found %d matching Tmax/Tmin files from %s to %s.\n", n_days, min(dates), max(dates)))


## 3.2 Build and Extract Data from Validated Stacks ----
cat("-> Verifying and loading all daily files. This is the most robust method...\n")

# We will build a list of raster objects, substituting bad files as we go.
raster_list_tmax <- vector("list", n_days)
raster_list_tmin <- vector("list", n_days)

# Load the first day's files to have a valid starting point.
# A check in section 3.1 ensures these are readable.
last_good_tmax <- rast(nc_tmax_files[1])
last_good_tmin <- rast(nc_tmin_files[1])
raster_list_tmax[[1]] <- last_good_tmax
raster_list_tmin[[1]] <- last_good_tmin

# Loop through the rest of the files
pb <- txtProgressBar(min = 2, max = n_days, style = 3)
for (i in 2:n_days) {
  # Attempt to load the Tmax file for the current day
  current_tmax <- try(rast(nc_tmax_files[i]), silent = TRUE)
  if (!inherits(current_tmax, "try-error") && nlyr(current_tmax) > 0) {
    raster_list_tmax[[i]] <- current_tmax
    last_good_tmax <- current_tmax # Update the last known good raster
  } else {
    warning(sprintf("Tmax file for %s failed. Using previous day's data.", dates[i]))
    raster_list_tmax[[i]] <- last_good_tmax # Substitute with the last good one
  }
  
  # Attempt to load the Tmin file for the current day
  current_tmin <- try(rast(nc_tmin_files[i]), silent = TRUE)
  if (!inherits(current_tmin, "try-error") && nlyr(current_tmin) > 0) {
    raster_list_tmin[[i]] <- current_tmin
    last_good_tmin <- current_tmin
  } else {
    warning(sprintf("Tmin file for %s failed. Using previous day's data.", dates[i]))
    raster_list_tmin[[i]] <- last_good_tmin
  }
  setTxtProgressBar(pb, i)
}
close(pb)

cat("\n-> Creating final stacks and extracting data...\n")

# Create the final stacks from the validated list of raster objects. This is fast.
tmax_stack <- rast(raster_list_tmax)
tmin_stack <- rast(raster_list_tmin)

# Crop the stacks and extract the time series data
tmax_stack_cropped <- crop(tmax_stack, watershed_ext_buffered)
tmin_stack_cropped <- crop(tmin_stack, watershed_ext_buffered)

# Extract data and transpose to the (days x cells) matrix format
tmax_matrix <- t(as.matrix(terra::extract(tmax_stack_cropped, cell_numbers)))
tmin_matrix <- t(as.matrix(terra::extract(tmin_stack_cropped, cell_numbers)))


## 3.3 Quality Control ----
# Apply validity range checks
tmax_matrix[tmax_matrix < min_valid_temp | tmax_matrix > max_valid_temp] <- NA
tmin_matrix[tmin_matrix < min_valid_temp | tmin_matrix > max_valid_temp] <- NA

# Swap values where Tmin > Tmax
inconsistent_idx <- !is.na(tmax_matrix) & !is.na(tmin_matrix) & tmax_matrix < tmin_matrix
if (any(inconsistent_idx)) {
  temp_swap <- tmax_matrix[inconsistent_idx]
  tmax_matrix[inconsistent_idx] <- tmin_matrix[inconsistent_idx]
  tmin_matrix[inconsistent_idx] <- temp_swap
}
cat("✓ Temperature data extracted and quality checked.\n")

# ====================================================================================================
# 4. GENERATE SWAT INPUT FILES ####
# ====================================================================================================
cat("-> Generating SWAT input files...\n")

## 4.1 Extract Elevation & Create Station File ----
elevation_values <- terra::extract(dem, vect(st_transform(st_as_sf(as.data.frame(coords_latlon), coords = c("X", "Y"), crs = 4326), crs(dem))))[,2]
if(anyNA(elevation_values)) elevation_values[is.na(elevation_values)] <- mean(elevation_values, na.rm = TRUE)

station_info <- data.frame(
  ID = 1:n_cells,
  NAME = paste0("tmp_mpanga", sprintf("%03d", 1:n_cells)),
  LAT = round(coords_latlon[, 2], 2),
  LONG = round(coords_latlon[, 1], 2),
  ELEVATION = round(elevation_values, 0)
)
writeLines("ID,NAME,LAT,LONG,ELEVATION", file.path(output_dir, "St_tmp_Mpanga.txt"))
write.table(station_info, file.path(output_dir, "St_tmp_Mpanga.txt"), sep = ",", row.names = FALSE, quote = FALSE, col.names = FALSE, append = TRUE)

## 4.2 Create Individual Station Time Series Files ----
pb <- txtProgressBar(min = 0, max = n_cells, style = 3)
for (i in 1:n_cells) {
  station_tmax <- tmax_matrix[, i]
  station_tmin <- tmin_matrix[, i]
  
  # Fill any remaining NA values with the spatial average for that day
  if (anyNA(station_tmax)) station_tmax[is.na(station_tmax)] <- rowMeans(tmax_matrix, na.rm = TRUE)[is.na(station_tmax)]
  if (anyNA(station_tmin)) station_tmin[is.na(station_tmin)] <- rowMeans(tmin_matrix, na.rm = TRUE)[is.na(station_tmin)]
  
  # Write file in "YYYYMMDD,TMAX,TMIN" format
  swat_data <- c(format(start_date, "%Y%m%d"), paste(round(station_tmax, 1), round(station_tmin, 1), sep = ","))
  writeLines(swat_data, file.path(output_dir, paste0(station_info$NAME[i], ".txt")))
  setTxtProgressBar(pb, i)
}
close(pb)
cat(sprintf("\n✓ %d station files created.\n", n_cells))

# ====================================================================================================
# 5. CALCULATE STATISTICS & VISUALIZE ####
# ====================================================================================================
cat("-> Calculating statistics and generating plots...\n")

## 5.1 Calculate Aggregate Statistics ----
# Create a daily summary dataframe for the entire watershed
temp_df <- data.frame(Date = dates, Tmean = rowMeans(tmax_matrix + tmin_matrix, na.rm = TRUE) / 2)

# Calculate long-term monthly and annual stats
monthly_stats <- temp_df %>%
  group_by(Month = month(Date)) %>%
  summarize(Tmean = mean(Tmean, na.rm = TRUE), .groups = 'drop')

annual_stats <- temp_df %>%
  group_by(Year = year(Date)) %>%
  summarize(Tmean = mean(Tmean, na.rm = TRUE), .groups = 'drop')

## 5.2 Create Plots ----
# Monthly Climatology Plot
p_seasonal <- ggplot(monthly_stats, aes(x = Month, y = Tmean)) +
  geom_line(color = "red", linewidth = 1) +
  geom_point(color = "red", size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  labs(
    title = "Monthly Mean Temperature Climatology",
    subtitle = sprintf("Data period: %s - %s", min(dates), max(dates)),
    x = "Month", y = "Mean Temperature (°C)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "MSWX_seasonal_temperature.png"), p_seasonal, 
       width = 10, height = 6, dpi = 300, bg = "white")

# Annual Time Series Plot
p_annual <- ggplot(annual_stats, aes(x = Year, y = Tmean)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "grey40", linetype = "dashed") +
  labs(
    title = "Annual Mean Temperature Time Series",
    subtitle = sprintf("Overall Mean: %.1f°C", mean(annual_stats$Tmean, na.rm = TRUE)),
    x = "Year", y = "Mean Temperature (°C)"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(output_dir, "MSWX_annual_temperature.png"), p_annual, 
       width = 10, height = 6, dpi = 300, bg = "white")

# ====================================================================================================
# 6. FINAL SUMMARY REPORT ####
# ====================================================================================================
tmax_completeness <- (1 - sum(is.na(tmax_matrix)) / length(tmax_matrix)) * 100
tmin_completeness <- (1 - sum(is.na(tmin_matrix)) / length(tmin_matrix)) * 100
warmest_month <- month.abb[monthly_stats$Month[which.max(monthly_stats$Tmean)]]
coldest_month <- month.abb[monthly_stats$Month[which.min(monthly_stats$Tmean)]]

cat("\n========================================================\n")
cat("✓ MSWX to SWAT Processing Complete!\n")
cat("========================================================\n")
cat(sprintf("Processed %d days of data for %d grid cells.\n", n_days, n_cells))
cat(sprintf("Date Range: %s to %s\n", min(dates), max(dates)))
cat(sprintf("Data Completeness: %.1f%% (Tmax), %.1f%% (Tmin)\n", tmax_completeness, tmin_completeness))
cat(sprintf("Annual Mean Temp: %.1f°C\n", mean(annual_stats$Tmean, na.rm = TRUE)))
cat(sprintf("Warmest/Coldest Months: %s / %s\n", warmest_month, coldest_month))
cat(sprintf("Output files saved to: %s\n", output_dir))
cat("========================================================\n")