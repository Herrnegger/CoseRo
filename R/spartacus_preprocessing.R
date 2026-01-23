#' SPARTACUS Data Preprocessing for COSERO
#'
#' Functions to extract SPARTACUS gridded climate data (precipitation and temperature)
#' and write them in COSERO meteorological input format.
#'
#' @name spartacus_preprocessing
#' @keywords internal
NULL

#' Extract SPARTACUS Precipitation for COSERO Input
#'
#' Processes yearly SPARTACUS precipitation NetCDF files and writes a single
#' text file in COSERO meteorological input format. The function performs
#' spatial aggregation by zone using area-weighted means and handles large
#' datasets efficiently through parallel processing and batched I/O.
#'
#' @param nc_dir Directory containing yearly SPARTACUS precipitation NetCDF files
#'   (pattern: SPARTACUS2-DAILY_RR_YYYY.nc)
#' @param output_dir Directory where the output .txt file will be saved
#' @param model_zones sf object containing modeling zones, must include a zone ID column
#' @param nz_col Name of the column with zone IDs (default: "NZ"). Output will be
#'   sorted by this column to ensure consistent ordering.
#' @param n_cores Number of cores for parallel processing. If NULL (default),
#'   uses all available cores minus 1.
#'
#' @return Invisibly returns the path to the output file
#'
#' @details
#' **Output Format:**
#' The output file follows COSERO's meteorological input format:
#' - Rows: One per day (chronologically ordered)
#' - Columns: YYYY MM DD HH mm NZ1 NZ2 ... NZn
#' - Values: Area-weighted mean precipitation (mm) per zone
#' - Filename: `P_NZ_<start_year>_<end_year>.txt`
#'
#' **Performance:**
#' - Uses exactextractr for fast area-weighted spatial aggregation
#' - Parallel processing with batched execution to optimize CPU and memory usage
#' - High-speed disk I/O using data.table::fwrite
#' - Processes 10+ years of daily 1km data in minutes
#'
#' **SPARTACUS Data:**
#' - Source: GeoSphere Austria (https://doi.org/10.60669/m6w8-s545)
#' - Spatial Resolution: 1 km
#' - Temporal Resolution: Daily
#' - Projection: ETRS89 (EPSG:4258)
#' - Coverage: Austria (1961-present)
#'
#' @examples
#' \dontrun{
#' library(sf)
#' zones <- st_read("model_zones.shp")
#'
#' # Process precipitation data
#' write_spartacus_precip(
#'   nc_dir = "data/SPARTACUS_Daily/RR",
#'   output_dir = "output/cosero_input",
#'   model_zones = zones,
#'   nz_col = "NZ",
#'   n_cores = 4
#' )
#' }
#'
#' @seealso \code{\link{write_spartacus_temp}} for temperature preprocessing
#'
#' @export
write_spartacus_precip <- function(nc_dir, output_dir, model_zones,
                                    nz_col = "NZ", n_cores = NULL) {

  # ── Validation ──
  if (!dir.exists(nc_dir)) {
    stop("nc_dir does not exist: ", nc_dir)
  }
  if (!inherits(model_zones, "sf")) {
    stop("model_zones must be an sf object")
  }
  if (!nz_col %in% names(model_zones)) {
    stop("Column '", nz_col, "' not found in model_zones")
  }

  if (is.null(n_cores)) n_cores <- parallel::detectCores() - 1

  # Use SPARTACUS-specific pattern for precipitation
  files <- list.files(nc_dir, pattern = "SPARTACUS2-DAILY_RR_\\d{4}\\.nc$",
                      full.names = TRUE)
  files <- sort(files)  # Ensure chronological order

  if (length(files) == 0) {
    stop("No SPARTACUS precipitation files found in ", nc_dir,
         "\nExpected pattern: SPARTACUS2-DAILY_RR_YYYY.nc")
  }

  # ── Setup spatial objects (do once) ──
  # Ensure strict ordering by NZ column for the model input
  model_zones <- model_zones[order(model_zones[[nz_col]]), ]
  n_zones <- nrow(model_zones)

  ref_rast <- terra::rast(files[1])
  if (sf::st_crs(model_zones) != sf::st_crs(ref_rast)) {
    message("Reprojecting model_zones to match raster CRS (EPSG:4258)...")
    model_zones <- sf::st_transform(model_zones, sf::st_crs(ref_rast))
  }

  # Pre-calculate extent to minimize I/O inside the loop
  target_ext <- terra::ext(model_zones)

  # ── Prepare output file ──
  years_range <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files))
  fname <- sprintf("P_NZ_%s_%s.txt", min(years_range), max(years_range))
  outfile <- file.path(output_dir, fname)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (file.exists(outfile)) {
    message("Removing existing output file: ", basename(outfile))
    file.remove(outfile)
  }

  # ── Process in Batches ──
  # We process in batches equal to n_cores to maximize CPU usage
  # while preventing memory issues
  batch_size <- n_cores
  n_batches <- ceiling(length(files) / batch_size)

  message(sprintf("Processing %d files across %d zones using %d cores...",
                  length(files), n_zones, n_cores))
  message(sprintf("Output: %s", basename(outfile)))

  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  pb <- txtProgressBar(max = length(files), style = 3)
  files_processed <- 0

  for (batch_idx in seq_len(n_batches)) {

    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(files))
    batch_files <- files[start_idx:end_idx]

    # Parallel extraction within batch
    batch_results <- furrr::future_map(seq_along(batch_files), function(i) {
      f <- batch_files[i]

      tryCatch({
        # Load raster and crop to area of interest
        r_year <- terra::rast(f)
        r_year <- terra::crop(r_year, target_ext)

        # 1. Spatial Extraction (Rows = NZs, Cols = Days)
        # exact_extract is fastest when processing the whole stack at once
        vals_df <- exactextractr::exact_extract(r_year, model_zones, 'mean',
                                                 progress = FALSE)

        # 2. Transpose for COSERO format (Rows = Days, Cols = NZs)
        vals_mat <- t(as.matrix(vals_df))

        # 3. Vectorized Date Matrix (YYYY MM DD HH mm)
        dates <- terra::time(r_year)
        n_days <- length(dates)

        date_mat <- cbind(
          as.integer(format(dates, "%Y")),
          as.integer(format(dates, "%m")),
          as.integer(format(dates, "%d")),
          matrix(0, nrow = n_days, ncol = 2) # HH and mm (always 0 for daily data)
        )

        # Return with index to maintain chronological order
        list(index = i, data = cbind(date_mat, vals_mat))

      }, error = function(e) {
        warning("\nFailed to process ", basename(f), ": ", e$message)
        list(index = i, data = NULL)
      })

    }, .options = furrr::furrr_options(seed = NULL))

    # Sort batch results by index to ensure chronological order within batch
    batch_results <- batch_results[order(sapply(batch_results, `[[`, "index"))]

    # ── High-speed Append to Disk ──
    for (result in batch_results) {
      if (!is.null(result$data)) {
        # fwrite is much faster than write.table for large datasets
        data.table::fwrite(
          as.data.frame(result$data),
          file = outfile,
          append = TRUE,
          sep = " ",
          col.names = FALSE,
          row.names = FALSE,
          quote = FALSE,
          showProgress = FALSE
        )
      }
      files_processed <- files_processed + 1
      setTxtProgressBar(pb, files_processed)
    }

    # Clean up batch results to free memory
    rm(batch_results)
    gc(verbose = FALSE)
  }

  close(pb)
  message("\nProcessing complete.")
  message("Output saved to: ", outfile)

  return(invisible(outfile))
}


#' Extract SPARTACUS Temperature (Tmean) for COSERO Input
#'
#' Processes yearly SPARTACUS min/max temperature NetCDF files, calculates
#' pixel-wise mean temperature, and writes a single text file in COSERO
#' meteorological input format. Handles large datasets efficiently through
#' parallel processing and batched I/O.
#'
#' @param tmin_dir Directory containing yearly SPARTACUS Tmin NetCDF files
#'   (pattern: SPARTACUS2-DAILY_TN_YYYY.nc)
#' @param tmax_dir Directory containing yearly SPARTACUS Tmax NetCDF files
#'   (pattern: SPARTACUS2-DAILY_TX_YYYY.nc)
#' @param output_dir Directory where the output .txt file will be saved
#' @param model_zones sf object containing modeling zones, must include a zone ID column
#' @param nz_col Name of the column with zone IDs (default: "NZ"). Output will be
#'   sorted by this column to ensure consistent ordering.
#' @param n_cores Number of cores for parallel processing. If NULL (default),
#'   uses all available cores minus 1.
#'
#' @return Invisibly returns the path to the output file
#'
#' @details
#' **Output Format:**
#' The output file follows COSERO's meteorological input format:
#' - Rows: One per day (chronologically ordered)
#' - Columns: YYYY MM DD HH mm NZ1 NZ2 ... NZn
#' - Values: Area-weighted mean temperature (°C) per zone
#' - Filename: `T_NZ_<start_year>_<end_year>.txt`
#'
#' **Temperature Calculation:**
#' Tmean is calculated at the pixel level before spatial aggregation:
#' `Tmean = (Tmin + Tmax) / 2`
#'
#' This approach is more accurate than averaging zone-aggregated min/max values,
#' especially in areas with high topographic variability.
#'
#' **Performance:**
#' - Pixel-wise calculation using terra's vectorized C++ operations
#' - exactextractr for fast area-weighted spatial aggregation
#' - Parallel processing with batched execution
#' - High-speed disk I/O using data.table::fwrite
#'
#' **SPARTACUS Data:**
#' - Source: GeoSphere Austria (https://doi.org/10.60669/m6w8-s545)
#' - Spatial Resolution: 1 km
#' - Temporal Resolution: Daily
#' - Projection: ETRS89 (EPSG:4258)
#' - Coverage: Austria (1961-present)
#' - Parameters: TN (minimum), TX (maximum)
#'
#' @examples
#' \dontrun{
#' library(sf)
#' zones <- st_read("model_zones.shp")
#'
#' # Process temperature data
#' write_spartacus_temp(
#'   tmin_dir = "data/SPARTACUS_Daily/TN",
#'   tmax_dir = "data/SPARTACUS_Daily/TX",
#'   output_dir = "output/cosero_input",
#'   model_zones = zones,
#'   nz_col = "NZ",
#'   n_cores = 4
#' )
#' }
#'
#' @seealso \code{\link{write_spartacus_precip}} for precipitation preprocessing
#'
#' @export
write_spartacus_temp <- function(tmin_dir, tmax_dir, output_dir, model_zones,
                                  nz_col = "NZ", n_cores = NULL) {

  # ── Validation ──
  if (!dir.exists(tmin_dir)) {
    stop("tmin_dir does not exist: ", tmin_dir)
  }
  if (!dir.exists(tmax_dir)) {
    stop("tmax_dir does not exist: ", tmax_dir)
  }
  if (!inherits(model_zones, "sf")) {
    stop("model_zones must be an sf object")
  }
  if (!nz_col %in% names(model_zones)) {
    stop("Column '", nz_col, "' not found in model_zones")
  }

  if (is.null(n_cores)) n_cores <- parallel::detectCores() - 1

  # Get files with SPARTACUS-specific patterns
  tmin_files <- list.files(tmin_dir, pattern = "SPARTACUS2-DAILY_TN_\\d{4}\\.nc$",
                           full.names = TRUE)
  tmax_files <- list.files(tmax_dir, pattern = "SPARTACUS2-DAILY_TX_\\d{4}\\.nc$",
                           full.names = TRUE)

  # Extract years and find common ground
  tmin_years <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(tmin_files))
  tmax_years <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(tmax_files))

  common_years <- sort(intersect(tmin_years, tmax_years))

  if (length(common_years) == 0) {
    stop("No matching years found between Tmin and Tmax directories.\n",
         "  Tmin files: ", length(tmin_files), "\n",
         "  Tmax files: ", length(tmax_files))
  }

  # Warn about missing years
  missing_tmin <- setdiff(tmax_years, tmin_years)
  missing_tmax <- setdiff(tmin_years, tmax_years)
  if (length(missing_tmin) > 0) {
    message("Warning: Tmin missing for years: ", paste(missing_tmin, collapse = ", "))
  }
  if (length(missing_tmax) > 0) {
    message("Warning: Tmax missing for years: ", paste(missing_tmax, collapse = ", "))
  }

  # ── Setup spatial objects ──
  model_zones <- model_zones[order(model_zones[[nz_col]]), ]
  n_zones <- nrow(model_zones)

  ref_rast <- terra::rast(tmin_files[1])

  if (sf::st_crs(model_zones) != sf::st_crs(ref_rast)) {
    message("Reprojecting model_zones to match raster CRS (EPSG:4258)...")
    model_zones <- sf::st_transform(model_zones, sf::st_crs(ref_rast))
  }

  target_ext <- terra::ext(model_zones)

  # ── Prepare output file ──
  fname <- sprintf("T_NZ_%s_%s.txt", min(common_years), max(common_years))
  outfile <- file.path(output_dir, fname)

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (file.exists(outfile)) {
    message("Removing existing output file: ", basename(outfile))
    file.remove(outfile)
  }

  # ── Process in Batches ──
  batch_size <- n_cores
  n_batches <- ceiling(length(common_years) / batch_size)

  message(sprintf("Processing %d years across %d zones using %d cores...",
                  length(common_years), n_zones, n_cores))
  message(sprintf("Output: %s", basename(outfile)))

  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  pb <- txtProgressBar(max = length(common_years), style = 3)
  files_processed <- 0

  for (batch_idx in seq_len(n_batches)) {

    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(common_years))
    batch_years <- common_years[start_idx:end_idx]

    batch_results <- furrr::future_map(seq_along(batch_years), function(i) {
      yr <- batch_years[i]

      tryCatch({
        # Find specific files for this year
        f_min <- tmin_files[which(tmin_years == yr)]
        f_max <- tmax_files[which(tmax_years == yr)]

        # Load and crop immediately to reduce memory footprint
        r_min <- terra::crop(terra::rast(f_min), target_ext)
        r_max <- terra::crop(terra::rast(f_max), target_ext)

        # Pixel-level Tmean calculation (Vectorized C++ in terra)
        r_mean <- (r_min + r_max) / 2

        # 1. Spatial Extraction (Rows = NZs, Cols = Days)
        vals_df <- exactextractr::exact_extract(r_mean, model_zones, 'mean',
                                                 progress = FALSE)

        # 2. Transpose for COSERO format (Rows = Days, Cols = NZs)
        vals_mat <- t(as.matrix(vals_df))

        # 3. Vectorized Date Matrix (YYYY MM DD HH mm)
        dates <- terra::time(r_mean)
        n_days <- length(dates)

        date_mat <- cbind(
          as.integer(format(dates, "%Y")),
          as.integer(format(dates, "%m")),
          as.integer(format(dates, "%d")),
          matrix(0, nrow = n_days, ncol = 2) # HH and mm (always 0 for daily data)
        )

        list(index = i, data = cbind(date_mat, vals_mat))

      }, error = function(e) {
        warning("\nFailed to process year ", yr, ": ", e$message)
        list(index = i, data = NULL)
      })

    }, .options = furrr::furrr_options(seed = NULL))

    # Sort batch results to maintain chronological order
    batch_results <- batch_results[order(sapply(batch_results, `[[`, "index"))]

    # ── High-speed Append to Disk ──
    for (result in batch_results) {
      if (!is.null(result$data)) {
        data.table::fwrite(
          as.data.frame(result$data),
          file = outfile,
          append = TRUE,
          sep = " ",
          col.names = FALSE,
          row.names = FALSE,
          quote = FALSE,
          showProgress = FALSE
        )
      }
      files_processed <- files_processed + 1
      setTxtProgressBar(pb, files_processed)
    }

    # Clean up batch results to free memory
    rm(batch_results)
    gc(verbose = FALSE)
  }

  close(pb)
  message("\nProcessing complete.")
  message("Output saved to: ", outfile)

  return(invisible(outfile))
}
