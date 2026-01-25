#' SPARTACUS Data Preprocessing for COSERO
#'
#' @description
#' High-performance extraction of SPARTACUS gridded climate data for Austria.
#' Uses a sparse weight matrix approach that maps modeling zones directly to
#' raw NetCDF memory layout for extremely fast, RAM-efficient processing.
#'
#' @details
#' **IMPORTANT: These functions are specifically designed for SPARTACUS NetCDF files
#' from GeoSphere Austria. They will NOT work with other gridded datasets due to
#' the hardcoded filename patterns and the raw cell index mapping logic.**
#'
#' ## The Sparse Weight Matrix Approach
#'
#' The key innovation is building a sparse weight matrix that maps directly to
#' the raw (unflipped) NetCDF storage layout. This avoids expensive raster
#' transformations during processing:
#'
#' 1. SPARTACUS NetCDF files store data top-to-bottom (row 0 = north),
#'    but `exactextractr` works with standard GIS orientation (row 0 = south).
#'    Flipping large rasters for each timestep is slow and memory-intensive.
#'
#' 2. We flip the raster ONCE to get correct cell-polygon overlaps,
#'    then mathematically transform those cell indices back to raw NetCDF positions.
#'    The formula reverses the row order: `raw_row = (nrows - 1) - flipped_row`
#'
#' 3. A sparse matrix W where `W[zone, cell] = coverage_fraction`.
#'    Computing zonal means becomes a single matrix multiplication:
#'    `zonal_means = W %*% cell_values` - highly optimized by the Matrix package.
#'
#' This approach is ~10x faster than traditional methods because:
#' - We extract only the cells we need (not the entire grid)
#' - Sparse matrix algebra replaces nested loops
#' - No per-timestep raster transformations
#'
#' The primary functions are:
#' \itemize{
#'   \item \code{\link{write_spartacus_precip}}: For precipitation (RR).
#'   \item \code{\link{write_spartacus_temp}}: For mean temperature (from TN and TX).
#' }
#'
#' @name spartacus_preprocessing
#' @keywords internal
NULL

# ============================================================================
# Helper: Format elapsed time as seconds or minutes
# ============================================================================
format_elapsed <- function(start_time) {
  elapsed_secs <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (elapsed_secs >= 60) {
    sprintf("%.1f min", elapsed_secs / 60)
  } else {
    sprintf("%.1f sec", elapsed_secs)
  }
}

#' Extract SPARTACUS Precipitation for COSERO Input
#'
#' Processes yearly SPARTACUS precipitation NetCDF files and writes a single
#' text file in COSERO meteorological input format, and optionally a binary file.
#'
#' @param nc_dir Directory containing yearly SPARTACUS precipitation NetCDF files
#'   (pattern: `SPARTACUS2-DAILY_RR_YYYY.nc`)
#' @param output_dir Directory where the output files will be saved
#' @param model_zones sf object containing modeling zones (polygons)
#' @param nz_col Name of the column with zone IDs (default: "NZ")
#' @param years Optional integer vector of years to process (default: all available)
#' @param n_cores Number of cores for parallel processing (default: 1)
#' @param write_binary If TRUE, also writes a Fortran-compatible binary file
#'
#' @details
#' **Note**: This function only works with SPARTACUS NetCDF files from GeoSphere
#' Austria. The filename pattern `SPARTACUS2-DAILY_RR_YYYY.nc` is required.
#'
#' The sparse weight matrix approach (see \code{\link{spartacus_preprocessing}})
#' enables processing of 60+ years of daily data.
#'
#' @return Invisibly returns a list with paths to the generated files:
#'   \item{txt}{Path to the text output file}
#'   \item{bin}{Path to the binary file (or NULL if write_binary=FALSE)}
#'
#' @importFrom terra rast flip time ncol nrow
#' @importFrom exactextractr exact_extract
#' @importFrom Matrix sparseMatrix rowSums
#' @importFrom data.table fwrite
#' @importFrom sf st_crs st_transform
#' @importFrom future plan multisession sequential future value
#' @importFrom furrr future_map furrr_options
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' zones <- st_read("path/to/your/zones.shp")
#'
#' # Process precipitation data using 4 cores
#' write_spartacus_precip(
#'   nc_dir = "path/to/precip_ncs",
#'   output_dir = "output/data",
#'   model_zones = zones,
#'   years = 2020:2021,
#'   n_cores = 4,
#'   write_binary = TRUE
#' )
#' }
write_spartacus_precip <- function(nc_dir, output_dir, model_zones,
                                   nz_col = "NZ", years = NULL,
                                   n_cores = 1, write_binary = FALSE) {

  t_total_start <- Sys.time()

  # ── Validation & File Discovery ──
  if (!dir.exists(nc_dir)) stop("nc_dir does not exist: ", nc_dir)
  if (!inherits(model_zones, "sf")) stop("model_zones must be an sf object")
  if (!nz_col %in% names(model_zones)) stop("Column '", nz_col, "' not found")

  # Find SPARTACUS precipitation files (strict naming pattern)
  files <- sort(list.files(nc_dir, pattern = "SPARTACUS2-DAILY_RR_\\d{4}\\.nc$",
                           full.names = TRUE))
  if (length(files) == 0) stop("No SPARTACUS precipitation files found")

  # Filter by requested years if specified
  if (!is.null(years)) {
    file_years <- as.integer(gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files)))
    files <- files[file_years %in% years]
    if (length(files) == 0) stop("No files for specified years")
  }

  # ── Spatial Setup ──
  # Sort zones by ID for consistent output column order
  model_zones <- model_zones[order(model_zones[[nz_col]]), ]
  n_zones <- nrow(model_zones)

  # Load reference raster for grid dimensions
  r_raw <- terra::rast(files[1])[[1]]
  r_flipped <- terra::flip(r_raw, "vertical")  # Standard GIS orientation

  # Ensure CRS match

  if (sf::st_crs(model_zones) != sf::st_crs(r_flipped)) {
    message("Reprojecting model_zones to match raster CRS...")
    model_zones <- sf::st_transform(model_zones, sf::st_crs(r_flipped))
  }

  # ── Build Sparse Weight Matrix with Raw Cell Indices ──
  # This is the core optimization: we compute polygon-cell overlaps on the

  # flipped raster (correct geometry), then map indices back to raw storage.
  message("Building sparse weight matrix for ", n_zones, " zones...")

  # Get cell indices and coverage fractions from flipped raster
  template <- exactextractr::exact_extract(r_flipped, model_zones,
                                           include_cell = TRUE, progress = FALSE)

  # Grid dimensions for index transformation
  ncols <- terra::ncol(r_raw)
  nrows <- terra::nrow(r_raw)

  # Transform flipped cell indices to raw NetCDF indices
  # The flip reverses row order: raw_row = (nrows - 1) - flipped_row
  map_to_raw <- function(flipped_cell) {
    row_idx <- (flipped_cell - 1) %/% ncols      # Row in flipped raster
    col_idx <- (flipped_cell - 1) %% ncols       # Column (unchanged)
    raw_row_idx <- (nrows - 1) - row_idx         # Row in raw storage
    raw_row_idx * ncols + col_idx + 1L           # Cell index in raw storage
  }

  # Apply transformation to all zone templates
  for (z in seq_along(template)) {
    template[[z]]$raw_cell <- map_to_raw(template[[z]]$cell)
  }

  # Build sparse matrix: rows=zones, cols=unique cells, values=coverage
  all_raw_cells <- unique(unlist(lapply(template, `[[`, "raw_cell")))
  cell_map <- setNames(seq_along(all_raw_cells), all_raw_cells)

  row_idx <- rep(seq_along(template), vapply(template, nrow, integer(1)))
  col_idx <- cell_map[as.character(unlist(lapply(template, `[[`, "raw_cell")))]
  weights <- unlist(lapply(template, `[[`, "coverage_fraction"))

  W <- Matrix::sparseMatrix(i = row_idx, j = col_idx, x = weights,
                            dims = c(n_zones, length(all_raw_cells)))

  # Normalize rows so weights sum to 1 (area-weighted mean)
  W <- W / Matrix::rowSums(W)

  # Free memory
  rm(template, row_idx, col_idx, weights, r_raw, r_flipped); gc()

  # ── Output File Setup ──
  yrs <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files))
  base_name <- sprintf("P_NZ_%s_%s", min(yrs), max(yrs))
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  bin_file <- file.path(output_dir, paste0(base_name, ".bin"))

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (file.exists(txt_file)) file.remove(txt_file)
  if (write_binary && file.exists(bin_file)) file.remove(bin_file)

  # ── Process Files (Parallel or Sequential) ──
  message(sprintf("Processing %d files using %d core(s)...", length(files), n_cores))

  # Worker function: extract values and compute zonal means via matrix multiply
  process_file <- function(f, cells, weight_matrix) {
    r <- terra::rast(f)
    dates <- terra::time(r)

    # Extract only the cells we need from raw storage
    cell_vals <- r[cells]
    if (is.data.frame(cell_vals)) cell_vals <- as.matrix(cell_vals)
    storage.mode(cell_vals) <- "double"

    # Zonal means via sparse matrix multiplication [n_zones x n_days]
    result <- as.matrix(weight_matrix %*% cell_vals)

    # Format dates for COSERO (YYYY MM DD HH MM)
    date_mat <- cbind(
      as.integer(format(dates, "%Y")),
      as.integer(format(dates, "%m")),
      as.integer(format(dates, "%d")),
      0L, 0L
    )

    list(
      txt_data = cbind(date_mat, round(t(result), 2)),
      raw_vals = result,      # For binary [n_zones x n_days]
      raw_dates = date_mat    # For binary header
    )
  }

  # Execute processing
  if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(future::sequential), add = TRUE)

    n_files <- length(files)
    results <- vector("list", n_files)
    t_start <- Sys.time()

    # Launch futures and collect results with progress
    futures <- lapply(files, function(f) {
      future::future(process_file(f, all_raw_cells, W),
                     seed = NULL, packages = c("terra", "Matrix"))
    })
    for (i in seq_along(futures)) {
      results[[i]] <- future::value(futures[[i]])
      cat(sprintf("\r  %d/%d files processed", i, n_files))
      flush.console()
    }
    cat(sprintf(" (%s)\n", format_elapsed(t_start)))
  } else {
    # Sequential with progress bar
    pb <- txtProgressBar(max = length(files), style = 3)
    results <- vector("list", length(files))
    for (i in seq_along(files)) {
      results[[i]] <- process_file(files[i], all_raw_cells, W)
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }

  # ── Write Output Files ──
  message("Writing output files...")

  if (write_binary) {
    bin_con <- file(bin_file, "wb")
    on.exit(close(bin_con), add = TRUE)
  }

  for (res in results) {
    # Text file: space-separated, COSERO format
    data.table::fwrite(as.data.frame(res$txt_data), txt_file, append = TRUE,
                       sep = " ", col.names = FALSE, quote = FALSE)

    # Binary file: Fortran-compatible (header + values per day)
    if (write_binary) {
      n_days <- ncol(res$raw_vals)
      for (d in seq_len(n_days)) {
        # Header: Y, M, D, H, M (5 x 4-byte integers)
        writeBin(as.integer(res$raw_dates[d, ]), bin_con, size = 4)
        # Values: NZ values (NZ x 4-byte reals)
        writeBin(as.numeric(res$raw_vals[, d]), bin_con, size = 4)
      }
    }
  }

  message("Files saved to ", output_dir)
  message("Total time: ", format_elapsed(t_total_start))
  invisible(list(txt = txt_file, bin = if(write_binary) bin_file else NULL))
}


#' Extract SPARTACUS Temperature (Tmean) for COSERO Input
#'
#' Processes yearly SPARTACUS min/max temperature NetCDF files, calculates
#' pixel-wise mean temperature (Tmean = (Tmin + Tmax) / 2), and writes output
#' in COSERO meteorological input format.
#'
#' @param tmin_dir Directory containing yearly SPARTACUS Tmin NetCDF files
#'   (pattern: `SPARTACUS2-DAILY_TN_YYYY.nc`)
#' @param tmax_dir Directory containing yearly SPARTACUS Tmax NetCDF files
#'   (pattern: `SPARTACUS2-DAILY_TX_YYYY.nc`)
#' @param output_dir Directory where the output files will be saved
#' @param model_zones sf object containing modeling zones (polygons)
#' @param nz_col Name of the column with zone IDs (default: "NZ")
#' @param years Optional integer vector of years to process (default: all available)
#' @param n_cores Number of cores for parallel processing (default: 1)
#' @param write_binary If TRUE, also writes a Fortran-compatible binary file
#'
#' @details
#' **Note**: This function only works with SPARTACUS NetCDF files from GeoSphere
#' Austria. The filename patterns `SPARTACUS2-DAILY_TN_YYYY.nc` (Tmin) and
#' `SPARTACUS2-DAILY_TX_YYYY.nc` (Tmax) are required.
#'
#' Mean temperature is calculated at the pixel level BEFORE spatial aggregation:
#' `Tmean[pixel] = (Tmin[pixel] + Tmax[pixel]) / 2`, then aggregated to zones.
#'
#' The sparse weight matrix approach (see \code{\link{spartacus_preprocessing}})
#' enables processing of 60+ years of daily data.
#'
#' @return Invisibly returns a list with paths to the generated files:
#'   \item{txt}{Path to the text output file}
#'   \item{bin}{Path to the binary file (or NULL if write_binary=FALSE)}
#'
#' @importFrom terra rast flip time ncol nrow
#' @importFrom exactextractr exact_extract
#' @importFrom Matrix sparseMatrix rowSums
#' @importFrom data.table fwrite
#' @importFrom sf st_crs st_transform
#' @importFrom future plan multisession sequential future value
#' @importFrom furrr future_map furrr_options
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' zones <- st_read("path/to/your/zones.shp")
#'
#' # Process temperature with binary output
#' write_spartacus_temp(
#'   tmin_dir = "path/to/tmin",
#'   tmax_dir = "path/to/tmax",
#'   output_dir = "output/data",
#'   model_zones = zones,
#'   n_cores = 4,
#'   write_binary = TRUE
#' )
#' }
write_spartacus_temp <- function(tmin_dir, tmax_dir, output_dir, model_zones,
                                 nz_col = "NZ", years = NULL,
                                 n_cores = 1, write_binary = FALSE) {

  t_total_start <- Sys.time()

  # ── Validation & File Discovery ──
  if (!dir.exists(tmin_dir)) stop("tmin_dir does not exist")
  if (!dir.exists(tmax_dir)) stop("tmax_dir does not exist")
  if (!inherits(model_zones, "sf")) stop("model_zones must be an sf object")
  if (!nz_col %in% names(model_zones)) stop("Column '", nz_col, "' not found")

  # Find SPARTACUS temperature files (strict naming patterns)
  tmin_files <- list.files(tmin_dir, pattern = "SPARTACUS2-DAILY_TN_\\d{4}\\.nc$", full.names = TRUE)
  tmax_files <- list.files(tmax_dir, pattern = "SPARTACUS2-DAILY_TX_\\d{4}\\.nc$", full.names = TRUE)

  if (length(tmin_files) == 0) stop("No SPARTACUS Tmin files found in: ", tmin_dir)
  if (length(tmax_files) == 0) stop("No SPARTACUS Tmax files found in: ", tmax_dir)

  # Find years present in both Tmin and Tmax directories
  tmin_yrs <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(tmin_files))
  tmax_yrs <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(tmax_files))
  common_years <- sort(intersect(tmin_yrs, tmax_yrs))

  if (!is.null(years)) common_years <- sort(intersect(common_years, as.character(years)))
  if (length(common_years) == 0) stop("No matching years found between Tmin and Tmax")

  # ── Spatial Setup with Raw Cell Index Mapping ──
  # Sort zones by ID for consistent output column order
  model_zones <- model_zones[order(model_zones[[nz_col]]), ]
  n_zones <- nrow(model_zones)

  # Use first Tmin file as reference for grid dimensions
  r_raw <- terra::rast(tmin_files[grep(common_years[1], tmin_files)])[[1]]
  r_flipped <- terra::flip(r_raw, "vertical")  # Standard GIS orientation

  # Ensure CRS match
  if (sf::st_crs(model_zones) != sf::st_crs(r_flipped)) {
    message("Reprojecting model_zones to match raster CRS...")
    model_zones <- sf::st_transform(model_zones, sf::st_crs(r_flipped))
  }

  # ── Build Sparse Weight Matrix ──
  # See spartacus_preprocessing documentation for detailed explanation
  message("Building sparse weight matrix for ", n_zones, " zones...")
  template <- exactextractr::exact_extract(r_flipped, model_zones, include_cell = TRUE, progress = FALSE)

  # Grid dimensions for index transformation
  ncols <- terra::ncol(r_raw)
  nrows <- terra::nrow(r_raw)

  # Transform flipped cell indices to raw NetCDF indices
  map_to_raw <- function(flipped_cell) {
    row_idx <- (flipped_cell - 1) %/% ncols      # Row in flipped raster
    col_idx <- (flipped_cell - 1) %% ncols       # Column (unchanged)
    raw_row_idx <- (nrows - 1) - row_idx         # Row in raw storage
    raw_row_idx * ncols + col_idx + 1L           # Cell index in raw storage
  }

  for (z in seq_along(template)) template[[z]]$raw_cell <- map_to_raw(template[[z]]$cell)

  # Build sparse matrix: rows=zones, cols=unique cells, values=coverage
  all_raw_cells <- unique(unlist(lapply(template, `[[`, "raw_cell")))
  cell_map <- setNames(seq_along(all_raw_cells), all_raw_cells)
  row_idx <- rep(seq_along(template), vapply(template, nrow, integer(1)))
  col_idx <- cell_map[as.character(unlist(lapply(template, `[[`, "raw_cell")))]
  weights <- unlist(lapply(template, `[[`, "coverage_fraction"))

  W <- Matrix::sparseMatrix(i = row_idx, j = col_idx, x = weights, dims = c(n_zones, length(all_raw_cells)))
  W <- W / Matrix::rowSums(W)  # Normalize for area-weighted mean

  # Free memory
  rm(template, row_idx, col_idx, weights, r_raw, r_flipped); gc()

  # ── Output File Setup ──
  base_name <- sprintf("T_NZ_%s_%s", min(common_years), max(common_years))
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  bin_file <- file.path(output_dir, paste0(base_name, ".bin"))
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (file.exists(txt_file)) file.remove(txt_file)
  if (write_binary && file.exists(bin_file)) file.remove(bin_file)

  # ── Process Year Pairs (Parallel or Sequential) ──
  message(sprintf("Processing %d years using %d core(s)...", length(common_years), n_cores))

  # Worker function: load Tmin/Tmax, compute Tmean, aggregate to zones
  process_file_pair <- function(yr, tmin_files, tmax_files, cells, weight_matrix) {
    f_min <- tmin_files[grep(yr, tmin_files)]
    f_max <- tmax_files[grep(yr, tmax_files)]

    r_min <- terra::rast(f_min)
    r_max <- terra::rast(f_max)
    dates <- terra::time(r_min)

    # Extract only needed cells from raw storage (no flipping)
    v_min <- as.matrix(r_min[cells])
    v_max <- as.matrix(r_max[cells])
    storage.mode(v_min) <- "double"
    storage.mode(v_max) <- "double"

    # Tmean at pixel level before aggregation
    v_mean <- (v_min + v_max) / 2

    # Zonal means via sparse matrix multiplication
    result <- as.matrix(weight_matrix %*% v_mean)

    # Format dates for COSERO
    date_mat <- cbind(as.integer(format(dates, "%Y")), as.integer(format(dates, "%m")),
                      as.integer(format(dates, "%d")), 0L, 0L)

    list(txt_data = cbind(date_mat, round(t(result), 2)), raw_vals = result, raw_dates = date_mat)
  }

  # Execute processing
  t_start <- Sys.time()
  if (n_cores > 1) {
    future::plan(future::multisession, workers = n_cores)
    on.exit(future::plan(future::sequential), add = TRUE)
    results <- furrr::future_map(common_years, process_file_pair, tmin_files = tmin_files,
                                 tmax_files = tmax_files, cells = all_raw_cells, weight_matrix = W,
                                 .options = furrr::furrr_options(packages = c("terra", "Matrix")))
  } else {
    results <- pbapply::pblapply(common_years, process_file_pair, tmin_files = tmin_files,
                                 tmax_files = tmax_files, cells = all_raw_cells, weight_matrix = W)
  }

  # ── Write Output Files ──
  message("Writing output files...")
  if (write_binary) {
    bin_con <- file(bin_file, "wb")
    on.exit(close(bin_con), add = TRUE)
  }

  for (res in results) {
    # Text file: space-separated, COSERO format
    data.table::fwrite(as.data.frame(res$txt_data), txt_file, append = TRUE, sep = " ", col.names = FALSE, quote = FALSE)

    # Binary file: Fortran-compatible (header + values per day)
    if (write_binary) {
      for (d in seq_len(ncol(res$raw_vals))) {
        writeBin(as.integer(res$raw_dates[d, ]), bin_con, size = 4)
        writeBin(as.numeric(res$raw_vals[, d]), bin_con, size = 4)
      }
    }
  }

  message("Files saved to: ", output_dir)
  message("Total time: ", format_elapsed(t_total_start))
  invisible(list(txt = txt_file, bin = if(write_binary) bin_file else NULL))
}
