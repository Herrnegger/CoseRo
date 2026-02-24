#' WINFORE ET0 Data Preprocessing for COSERO
#'
#' @description
#' High-performance extraction of WINFORE gridded potential evapotranspiration
#' (ET0) data for Austrian hydrological zones. Uses the same sparse weight
#' matrix approach as the SPARTACUS preprocessing functions
#' (\code{\link{write_spartacus_precip}}, \code{\link{write_spartacus_temp}}).
#'
#' WINFORE v2.1 provides daily reference (potential) evapotranspiration on the
#' same 1 km Lambert Conformal Conic grid as SPARTACUS. The NetCDF files store
#' data with vertically flipped rows (south at top), so a vertical flip is
#' applied during processing.
#'
#' @details
#' The weight matrix is built once from the model zones and then reused for all
#' files. Each yearly file is read with \pkg{ncdf4}, the relevant cells are
#' extracted, and zonal means are computed via a single sparse matrix
#' multiplication \code{W \%*\% cell_values}. This is ~10x faster than
#' traditional polygon-based extraction.
#'
#' Unlike SPARTACUS precipitation, **no temporal shift** is applied because ET0
#' is a standard daily total (00:00-00:00 CET).
#'
#' **Missing Data:** If the NetCDF contains days with no valid data (all NA),
#' these are filled by carrying forward the last valid day's values. A warning
#' is issued listing the affected dates.
#'
#' @references
#' GeoSphere Austria (2023). WINFORE v2.1 — Water Balance Information and
#' Forecasting for Austria. Daily potential evapotranspiration and standardized
#' indices.
#' \href{https://data.hub.geosphere.at/dataset/winfore_v2-1d}{GeoSphere Data Hub}
#'
#' @name winfore_preprocessing
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils flush.console setTxtProgressBar txtProgressBar
NULL


#' Extract WINFORE ET0 for COSERO Input
#'
#' Processes yearly WINFORE ET0 NetCDF files and writes output in COSERO
#' meteorological input format. Supports parallel processing and optional
#' binary output.
#'
#' @param nc_dir Directory containing yearly WINFORE ET0 NetCDF files
#'   (pattern: \code{WINFORE2_ET0_YYYY.nc})
#' @param output_dir Directory where the output files will be saved
#' @param model_zones sf object containing modeling zones (polygons)
#' @param nz_col Name of the column with zone IDs (default: \code{"NZ"})
#' @param years Optional integer vector of years to process. If \code{NULL},
#'   all available files are processed.
#' @param n_cores Number of cores for parallel processing (default: 1).
#'   Uses \pkg{future} and \pkg{furrr} for parallelization.
#' @param write_binary If \code{TRUE}, also writes a Fortran-compatible binary
#'   file (default: \code{FALSE}).
#'
#' @return Invisibly returns a list with paths to the generated files:
#'   \item{txt}{Path to the text output file}
#'   \item{bin}{Path to the binary file (or NULL if write_binary=FALSE)}
#'
#' @section Output Format:
#' The text output file contains one row per day with columns:
#' \code{YYYY MM DD HH MM val_1 val_2 ... val_NZ}, where each \code{val_i} is
#' the area-weighted mean ET0 (mm/day) for model zone \code{i}.
#'
#' @examples
#' \dontrun{
#' library(sf)
#'
#' # Load model zones
#' zones <- st_read("path/to/model_zones.shp")
#'
#' # Process all available years (sequential)
#' write_winfore_et0(
#'   nc_dir     = "D:/Data/GeoSphere/WINFORE_ET0",
#'   output_dir = "D:/COSERO_project/input",
#'   model_zones = zones,
#'   nz_col     = "NZ"
#' )
#'
#' # Process specific years with parallel processing and binary output
#' write_winfore_et0(
#'   nc_dir      = "D:/Data/GeoSphere/WINFORE_ET0",
#'   output_dir  = "D:/COSERO_project/input",
#'   model_zones = zones,
#'   nz_col      = "NZ",
#'   years       = 2000:2024,
#'   n_cores     = 4,
#'   write_binary = TRUE
#' )
#' }
#'
#' @seealso \code{\link{download_geosphere_data}} for downloading WINFORE files,
#'   \code{\link{write_spartacus_precip}} and \code{\link{write_spartacus_temp}}
#'   for SPARTACUS preprocessing.
#'
#' @importFrom data.table fwrite
#' @export
write_winfore_et0 <- function(nc_dir, output_dir, model_zones,
                               nz_col = "NZ", years = NULL,
                               n_cores = 1, write_binary = FALSE) {

  # Check for required suggested packages
  required_pkgs <- c("terra", "sf", "exactextractr", "Matrix", "future", "furrr")
  missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("The following packages are required for WINFORE preprocessing but are not installed: ",
         paste(missing_pkgs, collapse = ", "),
         "\nInstall them with: install.packages(c(",
         paste0('"', missing_pkgs, '"', collapse = ", "), "))",
         call. = FALSE)
  }

  t_total_start <- Sys.time()

  # -- Validation & File Discovery --
  if (!dir.exists(nc_dir)) stop("nc_dir does not exist: ", nc_dir, call. = FALSE)
  if (!inherits(model_zones, "sf")) stop("model_zones must be an sf object", call. = FALSE)
  if (!nz_col %in% names(model_zones)) stop("Column '", nz_col, "' not found", call. = FALSE)
  n_cores <- min(n_cores, max(1L, parallel::detectCores(logical = FALSE) - 1L))

  # Find WINFORE ET0 files (strict naming pattern)
  files <- sort(list.files(nc_dir, pattern = "WINFORE2_ET0_\\d{4}\\.nc$",
                           full.names = TRUE))
  if (length(files) == 0) stop("No WINFORE ET0 files found in: ", nc_dir, call. = FALSE)

  # Filter by requested years if specified
  if (!is.null(years)) {
    file_years <- as.integer(gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files)))
    files <- files[file_years %in% years]
    if (length(files) == 0) stop("No WINFORE files for specified years", call. = FALSE)
  }

  # -- Spatial Setup --
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

  # -- Build Sparse Weight Matrix with Raw Cell Indices --
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

  # -- Output File Setup --
  yrs <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files))
  base_name <- sprintf("ET0_NZ_%s_%s", min(yrs), max(yrs))
  txt_file <- file.path(output_dir, paste0(base_name, ".txt"))
  bin_file <- file.path(output_dir, paste0(base_name, ".bin"))

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (file.exists(txt_file)) file.remove(txt_file)
  if (write_binary && file.exists(bin_file)) file.remove(bin_file)

  # -- Process Files (Parallel or Sequential) --
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

    # Handle missing days: fill with last valid day (carry forward)
    na_days <- which(colSums(is.na(result)) == nrow(result))
    if (length(na_days) > 0) {
      na_dates <- format(dates[na_days], "%Y-%m-%d")
      warning("Missing data for ", length(na_days), " day(s) in ",
              basename(f), ": ", paste(na_dates, collapse = ", "),
              ". Filling with last valid day.", call. = FALSE)
      for (d in na_days) {
        # Find last valid day before this one
        prev <- d - 1L
        while (prev >= 1L && all(is.na(result[, prev]))) prev <- prev - 1L
        if (prev >= 1L) {
          result[, d] <- result[, prev]
        } else {
          # No valid day before — try next valid day
          nxt <- d + 1L
          while (nxt <= ncol(result) && all(is.na(result[, nxt]))) nxt <- nxt + 1L
          if (nxt <= ncol(result)) {
            result[, d] <- result[, nxt]
          } else {
            result[, d] <- 0  # Fallback: fill with 0
          }
        }
      }
    }

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
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    cat(sprintf(" (%.1f sec)\n", elapsed))
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

  # -- Write Output Files --
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

  total_elapsed <- as.numeric(difftime(Sys.time(), t_total_start, units = "secs"))
  message("Files saved to ", output_dir)
  if (total_elapsed < 60) {
    message(sprintf("Total time: %.1f seconds", total_elapsed))
  } else {
    message(sprintf("Total time: %.1f minutes", total_elapsed / 60))
  }
  invisible(list(txt = txt_file, bin = if (write_binary) bin_file else NULL))
}
