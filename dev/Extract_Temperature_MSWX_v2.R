# ====================================================================================================
# Script: Extract Temperature Data from MSWX NetCDF Files
# Purpose: Extract Tmax, Tmin, and Tmean from NetCDF files for stations
# Features: Smart ID handling, Point/Polygon support, Metadata preservation
# ====================================================================================================

# 1. Setup ####
## 1.1 User Parameters ####
shapefile <- "Kigezi_major_catchments.shp"
station_id_field <- "Catch_ID" 
output_filename_prefix <- "temperature_MSWX_Kigezi_major_catchments"

## 1.2 Load Required Packages ####
required_packages <- c("terra", "lubridate", "dplyr", "tidyr", "ggplot2", "scales")
for(pkg in required_packages) {
  if(!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

## 1.3 Set Paths ####
base_dir <- "D:/Tools&Nettes/MSWX_Temperature"
nc_tmax_dir <- file.path(base_dir, "Tmax")
nc_tmin_dir <- file.path(base_dir, "Tmin")
nc_tmean_dir <- file.path(base_dir, "Tmean")
input_dir <- file.path(base_dir, "input/Kigzei_Major_Catchments")
output_dir <- file.path(base_dir, "output/Kigzei_Major_Catchments")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 2. Load Spatial Data ####
shapefile_path <- file.path(input_dir, shapefile)
stations <- suppressWarnings(vect(shapefile_path))

# Verify ID field exists
if (!(station_id_field %in% names(stations))) {
  stop(paste("Error: Field", station_id_field, "not found. Available:",
             paste(names(stations), collapse=", ")))
}

# Rename ID field
original_id_field <- station_id_field
names(stations)[names(stations) == original_id_field] <- "ID_standardized"
station_id_field <- "ID_standardized"

# Get CRS and reproject
nc_tmax_files <- list.files(nc_tmax_dir, pattern = "\\.nc$", full.names = TRUE)
first_nc <- rast(nc_tmax_files[1])
nc_crs <- crs(first_nc)
stations_repr <- project(stations, nc_crs)

# Get resolution
spatial_resolution <- res(first_nc)
cat("Spatial resolution:", spatial_resolution[1], "x", spatial_resolution[2], "degrees\n")

# 3. Get File Lists ####
nc_tmin_files <- list.files(nc_tmin_dir, pattern = "\\.nc$", full.names = TRUE)
nc_tmean_files <- list.files(nc_tmean_dir, pattern = "\\.nc$", full.names = TRUE)

# Extract dates
extract_date <- function(f) {
  date_str <- basename(f)
  year <- substr(date_str, 1, 4)
  doy <- substr(date_str, 5, 7)
  as.Date(paste(year, doy), format = "%Y %j")
}

tmax_dates <- extract_date(nc_tmax_files)
tmin_dates <- extract_date(nc_tmin_files)
tmean_dates <- extract_date(nc_tmean_files)

# Sort files by date
nc_tmax_files <- nc_tmax_files[order(tmax_dates)]
nc_tmin_files <- nc_tmin_files[order(tmin_dates)]
nc_tmean_files <- nc_tmean_files[order(tmean_dates)]

tmax_dates <- sort(tmax_dates)
tmin_dates <- sort(tmin_dates)
tmean_dates <- sort(tmean_dates)

cat(sprintf("Found %d Tmax files: %s to %s\n", length(nc_tmax_files), min(tmax_dates), max(tmax_dates)))
cat(sprintf("Found %d Tmin files: %s to %s\n", length(nc_tmin_files), min(tmin_dates), max(tmin_dates)))
cat(sprintf("Found %d Tmean files: %s to %s\n", length(nc_tmean_files), min(tmean_dates), max(tmean_dates)))

# Find common dates
common_dates <- Reduce(intersect, list(tmax_dates, tmin_dates, tmean_dates))
cat(sprintf("\nCommon dates: %d days from %s to %s\n",
            length(common_dates), min(common_dates), max(common_dates)))

# 4. Processing Function ####
process_netcdf <- function(nc_file, stations_repr, id_field, var_name, last_valid_raster = NULL) {
  # Extract date
  date_str <- basename(nc_file)
  year <- substr(date_str, 1, 4)
  doy <- substr(date_str, 5, 7)
  date <- as.Date(paste(year, doy), format = "%Y %j")

  # Try to read the netCDF file
  nc_rast <- tryCatch({
    rast(nc_file)
  }, error = function(e) {
    warning(sprintf("Failed to read %s. Using last valid data. Error: %s",
                   basename(nc_file), e$message))
    return(NULL)
  })

  # Use last valid raster if current file failed to read
  use_fallback <- FALSE
  if (is.null(nc_rast)) {
    if (is.null(last_valid_raster)) {
      stop(sprintf("First NetCDF file %s is corrupted and no fallback available.", basename(nc_file)))
    }
    nc_rast <- last_valid_raster
    use_fallback <- TRUE
  }

  # Crop raster - with error handling for extent issues
  nc_crop <- tryCatch({
    crop(nc_rast, ext(stations_repr) + 0.1)
  }, error = function(e) {
    warning(sprintf("Failed to crop %s (extent issue). Using last valid raster. Error: %s",
                   basename(nc_file), e$message))
    # If cropping fails, use the last valid raster
    if (is.null(last_valid_raster)) {
      stop(sprintf("Cannot crop first file %s and no fallback available", basename(nc_file)))
    }
    use_fallback <<- TRUE
    return(crop(last_valid_raster, ext(stations_repr) + 0.1))
  })

  # Extract values
  temp_values <- terra::extract(nc_crop, stations_repr,
                                fun = "mean",
                                exact = FALSE,
                                na.rm = TRUE)

  # Create result dataframe - one row per station
  n_stations <- nrow(temp_values)

  # Extract station IDs properly from SpatVector using values() to get a proper dataframe
  station_ids <- values(stations_repr)[[id_field]]

  result <- data.frame(
    Date = rep(date, n_stations),
    stringsAsFactors = FALSE
  )
  result[[var_name]] <- temp_values[,2]
  result[[id_field]] <- station_ids

  # Return raster: if we used fallback, return last_valid_raster unchanged
  # Otherwise return the newly read raster for future use
  raster_to_return <- if(use_fallback) last_valid_raster else nc_rast

  return(list(data = result, raster = raster_to_return))
}

# 5. Process All Files ####
cat("\n=== Processing Tmax ===\n")
tmax_results <- list()
last_valid_tmax <- NULL
for (i in seq_along(nc_tmax_files)) {
  if (i %% 100 == 0 || i == length(nc_tmax_files)) {
    cat(sprintf("Processing file %d of %d\n", i, length(nc_tmax_files)))
  }
  result <- process_netcdf(nc_tmax_files[i], stations_repr, station_id_field, "Tmax", last_valid_tmax)
  tmax_results[[i]] <- result$data
  last_valid_tmax <- result$raster
}

cat("\n=== Processing Tmin ===\n")
tmin_results <- list()
last_valid_tmin <- NULL
for (i in seq_along(nc_tmin_files)) {
  if (i %% 100 == 0 || i == length(nc_tmin_files)) {
    cat(sprintf("Processing file %d of %d\n", i, length(nc_tmin_files)))
  }
  result <- process_netcdf(nc_tmin_files[i], stations_repr, station_id_field, "Tmin", last_valid_tmin)
  tmin_results[[i]] <- result$data
  last_valid_tmin <- result$raster
}

cat("\n=== Processing Tmean ===\n")
tmean_results <- list()
last_valid_tmean <- NULL
for (i in seq_along(nc_tmean_files)) {
  if (i %% 100 == 0 || i == length(nc_tmean_files)) {
    cat(sprintf("Processing file %d of %d\n", i, length(nc_tmean_files)))
  }
  result <- process_netcdf(nc_tmean_files[i], stations_repr, station_id_field, "Tmean", last_valid_tmean)
  tmean_results[[i]] <- result$data
  last_valid_tmean <- result$raster
}

# 6. Combine Results ####
cat("\n=== Combining results ===\n")
tmax_data <- do.call(rbind, tmax_results)
tmin_data <- do.call(rbind, tmin_results)
tmean_data <- do.call(rbind, tmean_results)

# Convert IDs - ensure they're simple vectors
cat("\nOriginal ID values (first 10):\n")
print(head(tmax_data$ID_standardized, 10))

# Test if IDs are numeric
numeric_test <- suppressWarnings(as.numeric(tmax_data$ID_standardized[1]))
if (!is.na(numeric_test)) {
  cat("Station IDs are numeric.\n")
  tmax_data$ID_standardized <- as.numeric(tmax_data$ID_standardized)
  tmin_data$ID_standardized <- as.numeric(tmin_data$ID_standardized)
  tmean_data$ID_standardized <- as.numeric(tmean_data$ID_standardized)
  use_numeric_ids <- TRUE
} else {
  cat("Station IDs are character.\n")
  tmax_data$ID_standardized <- as.character(tmax_data$ID_standardized)
  tmin_data$ID_standardized <- as.character(tmin_data$ID_standardized)
  tmean_data$ID_standardized <- as.character(tmean_data$ID_standardized)
  use_numeric_ids <- FALSE
}

# Round values
tmax_data$Tmax <- round(tmax_data$Tmax, 2)
tmin_data$Tmin <- round(tmin_data$Tmin, 2)
tmean_data$Tmean <- round(tmean_data$Tmean, 2)

# Check for duplicates
cat("\n=== Checking duplicates ===\n")
check_and_fix_duplicates <- function(data, var_name) {
  dups <- data %>%
    group_by(Date, ID_standardized) %>%
    summarize(n = n(), .groups = 'drop') %>%
    filter(n > 1)

  if (nrow(dups) > 0) {
    cat("WARNING:", nrow(dups), "duplicates in", var_name, ". Taking mean...\n")
    data <- data %>%
      group_by(Date, ID_standardized) %>%
      summarize(across(starts_with(substr(var_name,1,1)), mean, na.rm = TRUE), .groups = 'drop')
  } else {
    cat("No duplicates in", var_name, "\n")
  }
  return(as.data.frame(data))
}

tmax_data <- check_and_fix_duplicates(tmax_data, "Tmax")
tmin_data <- check_and_fix_duplicates(tmin_data, "Tmin")
tmean_data <- check_and_fix_duplicates(tmean_data, "Tmean")

# 7. Create Wide Format ####
cat("\n=== Creating wide format ===\n")
tmax_wide <- pivot_wider(tmax_data, id_cols = Date, names_from = ID_standardized,
                         values_from = Tmax, names_prefix = "Tmax_")
tmin_wide <- pivot_wider(tmin_data, id_cols = Date, names_from = ID_standardized,
                         values_from = Tmin, names_prefix = "Tmin_")
tmean_wide <- pivot_wider(tmean_data, id_cols = Date, names_from = ID_standardized,
                          values_from = Tmean, names_prefix = "Tmean_")

# Sort by date
tmax_wide <- tmax_wide[order(tmax_wide$Date), ]
tmin_wide <- tmin_wide[order(tmin_wide$Date), ]
tmean_wide <- tmean_wide[order(tmean_wide$Date), ]

# Sort columns
sort_temp_columns <- function(df, prefix) {
  temp_cols <- grep(paste0(prefix, "_"), names(df), value = TRUE)
  if (use_numeric_ids) {
    col_numbers <- as.numeric(gsub(paste0(prefix, "_"), "", temp_cols))
    col_order <- c("Date", temp_cols[order(col_numbers)])
  } else {
    col_order <- c("Date", sort(temp_cols))
  }
  df[, col_order]
}

tmax_wide <- sort_temp_columns(tmax_wide, "Tmax")
tmin_wide <- sort_temp_columns(tmin_wide, "Tmin")
tmean_wide <- sort_temp_columns(tmean_wide, "Tmean")

# 8. Create Metadata ####
cat("\n=== Creating metadata ===\n")
station_metadata <- as.data.frame(stations_repr)
if (use_numeric_ids) {
  station_metadata$ID_standardized <- as.numeric(as.character(station_metadata$ID_standardized))
} else {
  station_metadata$ID_standardized <- as.character(station_metadata$ID_standardized)
}

# Calculate areas
geom_type <- geomtype(stations_repr)[1]
cat("Geometry type:", geom_type, "\n")
if (geom_type == "points") {
  areas <- rep(1, nrow(stations_repr))
} else {
  areas <- expanse(stations_repr, unit = "km")
}

station_metadata$Area_km2 <- areas

# Save station metadata file
metadata_file <- file.path(output_dir, paste0(output_filename_prefix, "_station_metadata.csv"))
metadata_output <- station_metadata[, !names(station_metadata) %in% c("geom", "geometry")]
write.csv(metadata_output, metadata_file, row.names = FALSE)
cat("Station metadata saved to:", metadata_file, "\n")

# 9. Save Temperature Data with Metadata Headers ####
cat("\n=== Saving temperature data ===\n")

save_with_inline_metadata <- function(data_wide, prefix, var_name) {
  # Get column names
  temp_cols <- grep(paste0(prefix, "_"), names(data_wide), value = TRUE)
  station_ids <- gsub(paste0(prefix, "_"), "", temp_cols)
  if (use_numeric_ids) station_ids <- as.numeric(station_ids)

  # Create metadata rows
  metadata_rows <- list()
  for (attr_name in names(station_metadata)) {
    if (!(attr_name %in% c("geom", "geometry"))) {
      meta_row <- data.frame(Date = as.character(attr_name), stringsAsFactors = FALSE)

      for (col_name in temp_cols) {
        station_id <- gsub(paste0(prefix, "_"), "", col_name)
        if (use_numeric_ids) station_id <- as.numeric(station_id)

        match_idx <- which(station_metadata$ID_standardized == station_id)
        if (length(match_idx) > 0) {
          value <- station_metadata[[attr_name]][match_idx[1]]
          meta_row[[col_name]] <- as.character(value)
        } else {
          meta_row[[col_name]] <- NA
        }
      }
      metadata_rows[[attr_name]] <- meta_row
    }
  }

  # Combine metadata and data
  metadata_df <- do.call(rbind, metadata_rows)
  data_wide$Date <- as.character(data_wide$Date)
  for (col in temp_cols) {
    data_wide[[col]] <- as.character(data_wide[[col]])
  }

  final_output <- rbind(metadata_df, data_wide)

  # Save
  output_file <- file.path(output_dir, paste0(output_filename_prefix, "_", var_name, ".csv"))
  write.csv(final_output, output_file, row.names = FALSE)
  cat(var_name, "data saved to:", output_file, "\n")
}

save_with_inline_metadata(tmax_wide, "Tmax", "Tmax")
save_with_inline_metadata(tmin_wide, "Tmin", "Tmin")
save_with_inline_metadata(tmean_wide, "Tmean", "Tmean")

# 10. Calculate Tmean from Tmax+Tmin ####
cat("\n=== Calculating Tmean from Tmax+Tmin ===\n")
tmean_calc <- tmax_data %>%
  select(Date, ID_standardized, Tmax) %>%
  left_join(tmin_data %>% select(Date, ID_standardized, Tmin), by = c("Date", "ID_standardized")) %>%
  mutate(Tmean_calc = round((Tmax + Tmin) / 2, 2)) %>%
  select(Date, ID_standardized, Tmean_calc)

tmean_calc_wide <- pivot_wider(tmean_calc, id_cols = Date, names_from = ID_standardized,
                                values_from = Tmean_calc, names_prefix = "Tmean_")
tmean_calc_wide <- tmean_calc_wide[order(tmean_calc_wide$Date), ]
tmean_calc_wide <- sort_temp_columns(tmean_calc_wide, "Tmean")

save_with_inline_metadata(tmean_calc_wide, "Tmean", "Tmean_calculated")

# 11. Seasonal Analysis ####
cat("\n=== Generating seasonal analysis ===\n")

# Calculate monthly statistics
calc_seasonal_stats <- function(temp_data, var_name) {
  monthly_data <- temp_data %>%
    mutate(Month = month(Date), Year = year(Date)) %>%
    group_by(ID_standardized, Year, Month) %>%
    summarize(Monthly_Temp = mean(.data[[var_name]], na.rm = TRUE), .groups = 'drop')

  # Join with area data
  station_areas_df <- data.frame(
    ID_standardized = station_metadata$ID_standardized,
    Area_km2 = station_metadata$Area_km2
  )

  monthly_with_area <- left_join(monthly_data, station_areas_df, by = "ID_standardized")

  # Weighted means by year and month
  weighted_by_year <- monthly_with_area %>%
    group_by(Year, Month) %>%
    summarize(Weighted_Temp = weighted.mean(Monthly_Temp, Area_km2, na.rm = TRUE), .groups = 'drop')

  # Overall monthly statistics
  overall_seasonal <- weighted_by_year %>%
    group_by(Month) %>%
    summarize(
      Mean_Temp = mean(Weighted_Temp, na.rm = TRUE),
      Std_Temp = sd(Weighted_Temp, na.rm = TRUE),
      Std_Temp_Adj = ifelse(is.na(Std_Temp) | Std_Temp < 0.5, 0.5, Std_Temp),
      Upper_CI = Mean_Temp + Std_Temp_Adj,
      Lower_CI = Mean_Temp - Std_Temp_Adj,
      .groups = 'drop'
    )

  # Individual station monthly means
  station_monthly <- monthly_data %>%
    group_by(ID_standardized, Month) %>%
    summarize(Mean_Temp = mean(Monthly_Temp, na.rm = TRUE), .groups = 'drop')

  # Annual means per station
  annual_means <- monthly_data %>%
    group_by(ID_standardized) %>%
    summarize(Annual_Temp = mean(Monthly_Temp, na.rm = TRUE), .groups = 'drop')

  station_monthly <- left_join(station_monthly, annual_means, by = "ID_standardized")

  return(list(
    overall = overall_seasonal,
    by_station = station_monthly,
    weighted_annual = mean(overall_seasonal$Mean_Temp, na.rm = TRUE)
  ))
}

tmax_seasonal <- calc_seasonal_stats(tmax_data, "Tmax")
tmin_seasonal <- calc_seasonal_stats(tmin_data, "Tmin")
tmean_seasonal <- calc_seasonal_stats(tmean_data, "Tmean")

cat(sprintf("Area-weighted annual Tmax: %.1f°C\n", tmax_seasonal$weighted_annual))
cat(sprintf("Area-weighted annual Tmin: %.1f°C\n", tmin_seasonal$weighted_annual))
cat(sprintf("Area-weighted annual Tmean: %.1f°C\n", tmean_seasonal$weighted_annual))

# 12. Annual Analysis ####
cat("\n=== Calculating annual statistics ===\n")

calc_annual_stats <- function(temp_data, var_name) {
  station_areas_df <- data.frame(
    ID_standardized = station_metadata$ID_standardized,
    Area_km2 = station_metadata$Area_km2
  )

  annual_data <- temp_data %>%
    mutate(Year = year(Date)) %>%
    group_by(ID_standardized, Year) %>%
    summarize(Annual_Temp = mean(.data[[var_name]], na.rm = TRUE), .groups = 'drop')

  annual_with_area <- left_join(annual_data, station_areas_df, by = "ID_standardized")

  weighted_annual <- annual_with_area %>%
    group_by(Year) %>%
    summarize(Weighted_Temp = weighted.mean(Annual_Temp, Area_km2, na.rm = TRUE), .groups = 'drop')

  return(weighted_annual)
}

tmax_annual <- calc_annual_stats(tmax_data, "Tmax")
tmin_annual <- calc_annual_stats(tmin_data, "Tmin")
tmean_annual <- calc_annual_stats(tmean_data, "Tmean")

# 13. Create Seasonal Plots ####
cat("\n=== Creating seasonal plots ===\n")

create_ribbon <- function(seasonal_stats) {
  upper_ci <- seasonal_stats %>%
    select(Month, Upper_CI) %>%
    rename(y = Upper_CI) %>%
    arrange(Month)

  lower_ci <- seasonal_stats %>%
    select(Month, Lower_CI) %>%
    rename(y = Lower_CI) %>%
    arrange(desc(Month))

  ribbon_data <- bind_rows(upper_ci, lower_ci)
  ribbon_data$x <- c(upper_ci$Month, lower_ci$Month)
  return(ribbon_data)
}

ribbon_tmax <- create_ribbon(tmax_seasonal$overall)
ribbon_tmin <- create_ribbon(tmin_seasonal$overall)
ribbon_tmean <- create_ribbon(tmean_seasonal$overall)

prepare_labels <- function(station_data) {
  dec_data <- station_data %>%
    filter(Month == 12) %>%
    arrange(Mean_Temp)

  if (nrow(dec_data) > 1) {
    y_range <- max(dec_data$Mean_Temp) - min(dec_data$Mean_Temp)
    total_label_height <- max(y_range * 2, y_range + 3)
    spacing <- total_label_height / (nrow(dec_data) - 1)
    dec_data$Adjusted_Y <- min(dec_data$Mean_Temp) + ((seq_len(nrow(dec_data)) - 1) * spacing)
  } else {
    dec_data$Adjusted_Y <- dec_data$Mean_Temp
  }
  return(dec_data)
}

dec_tmax <- prepare_labels(tmax_seasonal$by_station)
dec_tmin <- prepare_labels(tmin_seasonal$by_station)
dec_tmean <- prepare_labels(tmean_seasonal$by_station)

create_seasonal_plot <- function(overall_data, station_data, ribbon_data, dec_labels,
                                 var_name, color_main, color_ribbon, annual_mean) {
  p <- ggplot() +
    geom_polygon(data = ribbon_data, aes(x = x, y = y),
                 fill = color_ribbon, alpha = 0.3) +
    geom_line(data = station_data,
              aes(x = Month, y = Mean_Temp, group = ID_standardized),
              color = "gray60", alpha = 0.5, linewidth = 0.5) +
    geom_line(data = overall_data,
              aes(x = Month, y = Mean_Temp, group = 1),
              color = color_main, linewidth = 1.5) +
    geom_point(data = overall_data,
               aes(x = Month, y = Mean_Temp),
               color = color_main, size = 3) +
    geom_segment(data = dec_labels,
                 aes(x = 12, xend = 12.2, y = Mean_Temp, yend = Adjusted_Y),
                 color = "gray60", linetype = "dashed", linewidth = 0.5) +
    geom_text(data = dec_labels,
              aes(x = 12.3, y = Adjusted_Y,
                  label = sprintf("%s (%.1f°C)", ID_standardized, round(Annual_Temp, 1))),
              hjust = 0, vjust = 0.5, size = 3, color = "gray40") +
    labs(
      title = paste("Monthly Mean", var_name, "from MSWX Data"),
      subtitle = sprintf("Thin lines: Individual stations | Thick line: Area-weighted average (%.1f°C) | Shaded area: ±1 SD",
                        annual_mean),
      x = "Month",
      y = paste(var_name, "(°C)")
    ) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 10, color = "gray40"),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = 1:12, labels = month.abb,
                       limits = c(1, 13.5), expand = expansion(mult = c(0.01, 0))) +
    coord_cartesian(clip = "off")

  return(p)
}

p_seasonal_tmax <- create_seasonal_plot(
  tmax_seasonal$overall, tmax_seasonal$by_station, ribbon_tmax, dec_tmax,
  "Tmax", "red", "#FFB6C1", tmax_seasonal$weighted_annual
)

p_seasonal_tmin <- create_seasonal_plot(
  tmin_seasonal$overall, tmin_seasonal$by_station, ribbon_tmin, dec_tmin,
  "Tmin", "blue", "#ADD8E6", tmin_seasonal$weighted_annual
)

p_seasonal_tmean <- create_seasonal_plot(
  tmean_seasonal$overall, tmean_seasonal$by_station, ribbon_tmean, dec_tmean,
  "Tmean", "darkgreen", "#90EE90", tmean_seasonal$weighted_annual
)

ggsave(file.path(output_dir, paste0(output_filename_prefix, "_seasonal_Tmax.png")),
       p_seasonal_tmax, width = 10, height = 7, dpi = 300)
ggsave(file.path(output_dir, paste0(output_filename_prefix, "_seasonal_Tmin.png")),
       p_seasonal_tmin, width = 10, height = 7, dpi = 300)
ggsave(file.path(output_dir, paste0(output_filename_prefix, "_seasonal_Tmean.png")),
       p_seasonal_tmean, width = 10, height = 7, dpi = 300)

cat("Seasonal plots saved.\n")

# 14. Create Annual Temperature Plot ####
cat("\n=== Creating annual temperature plot ===\n")

annual_combined <- tmax_annual %>%
  rename(Tmax = Weighted_Temp) %>%
  left_join(tmin_annual %>% rename(Tmin = Weighted_Temp), by = "Year") %>%
  left_join(tmean_annual %>% rename(Tmean = Weighted_Temp), by = "Year")

annual_long <- annual_combined %>%
  pivot_longer(cols = c(Tmax, Tmin, Tmean),
               names_to = "Variable",
               values_to = "Temperature")

p_annual <- ggplot(annual_long, aes(x = Year, y = Temperature, color = Variable)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.8) +
  scale_color_manual(values = c("Tmax" = "red", "Tmin" = "blue", "Tmean" = "darkgreen"),
                     labels = c("Tmax" = "Maximum Temperature",
                               "Tmin" = "Minimum Temperature",
                               "Tmean" = "Mean Temperature")) +
  labs(
    title = "Annual Temperature Time Series from MSWX Data",
    subtitle = sprintf("Area-weighted averages | Period: %d-%d",
                      min(annual_combined$Year), max(annual_combined$Year)),
    x = "Year",
    y = "Temperature (°C)",
    color = "Variable"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold")
  )

ggsave(file.path(output_dir, paste0(output_filename_prefix, "_annual_timeseries.png")),
       p_annual, width = 10, height = 7, dpi = 300)

cat("Annual plot saved.\n")

# 15. Save Statistical Summaries ####
cat("\n=== Saving statistical summaries ===\n")

seasonal_summary <- data.frame(
  Month = 1:12,
  Month_Name = month.abb,
  Tmax_Mean = tmax_seasonal$overall$Mean_Temp,
  Tmax_SD = tmax_seasonal$overall$Std_Temp_Adj,
  Tmin_Mean = tmin_seasonal$overall$Mean_Temp,
  Tmin_SD = tmin_seasonal$overall$Std_Temp_Adj,
  Tmean_Mean = tmean_seasonal$overall$Mean_Temp,
  Tmean_SD = tmean_seasonal$overall$Std_Temp_Adj
)

write.csv(seasonal_summary,
          file.path(output_dir, paste0(output_filename_prefix, "_seasonal_summary.csv")),
          row.names = FALSE)

write.csv(annual_combined,
          file.path(output_dir, paste0(output_filename_prefix, "_annual_summary.csv")),
          row.names = FALSE)

cat("Statistical summaries saved.\n")

cat("\n========================================================\n")
cat("Temperature Extraction Complete!\n")
cat("========================================================\n")
cat(sprintf("Processed %d days of data\n", nrow(tmax_wide)))
cat(sprintf("Date Range: %s to %s\n", min(common_dates), max(common_dates)))
cat(sprintf("Number of stations: %d\n", nrow(stations)))
cat(sprintf("\nArea-weighted annual temperatures:\n"))
cat(sprintf("  Tmax:  %.2f°C\n", tmax_seasonal$weighted_annual))
cat(sprintf("  Tmin:  %.2f°C\n", tmin_seasonal$weighted_annual))
cat(sprintf("  Tmean: %.2f°C\n", tmean_seasonal$weighted_annual))
cat(sprintf("\nOutput files saved to: %s\n", output_dir))
cat("========================================================\n")
