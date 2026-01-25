#' SPARTACUS Precipitation Extraction (Parallel Version)
#'
#' Alternative implementation using future/furrr for parallel processing.
#' Kept as backup - the main function in spartacus_preprocessing.R uses
#' a more efficient template-based approach.
#'
#' @inheritParams write_spartacus_precip
#' @param n_cores Number of cores for parallel processing (default: 4)
#' @keywords internal
write_spartacus_precip_parallel <- function(nc_dir, output_dir, model_zones,
                                            nz_col = "NZ", years = NULL,
                                            n_cores = 4) {


  if (!dir.exists(nc_dir)) stop("nc_dir does not exist: ", nc_dir)
  if (!inherits(model_zones, "sf")) stop("model_zones must be an sf object")
  if (!nz_col %in% names(model_zones)) stop("Column '", nz_col, "' not found")

  files <- sort(list.files(nc_dir, pattern = "SPARTACUS2-DAILY_RR_\\d{4}\\.nc$",
                           full.names = TRUE))
  if (length(files) == 0) stop("No SPARTACUS precipitation files found")

  if (!is.null(years)) {
    file_years <- as.integer(gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files)))
    files <- files[file_years %in% years]
    if (length(files) == 0) stop("No files for specified years")
    message("Processing years: ", paste(sort(years[years %in% file_years]), collapse = ", "))
  }

  model_zones <- model_zones[order(model_zones[[nz_col]]), ]
  n_zones <- nrow(model_zones)

  ref_rast <- terra::flip(terra::rast(files[1])[[1]], "vertical")

  if (sf::st_crs(model_zones) != sf::st_crs(ref_rast)) {
    message("Reprojecting model_zones to match raster CRS...")
    model_zones <- sf::st_transform(model_zones, sf::st_crs(ref_rast))
  }

  rast_ext <- terra::ext(ref_rast)
  zones_ext <- terra::ext(model_zones)

  overlap <- !(zones_ext$xmax < rast_ext$xmin ||
               zones_ext$xmin > rast_ext$xmax ||
               zones_ext$ymax < rast_ext$ymin ||
               zones_ext$ymin > rast_ext$ymax)

  if (!overlap) {
    stop("No spatial overlap between model_zones and raster data.")
  }

  target_ext_vec <- as.vector(terra::ext(model_zones))

  years_range <- gsub(".*_(\\d{4})\\.nc$", "\\1", basename(files))
  outfile <- file.path(output_dir, sprintf("P_NZ_%s_%s.txt",
                                           min(years_range), max(years_range)))

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  if (file.exists(outfile)) file.remove(outfile)

  message(sprintf("Processing %d files across %d zones using %d core(s)...",
                  length(files), n_zones, n_cores))

  model_zones_sf <- model_zones[, nz_col]

  future::plan(future::multisession, workers = n_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  results <- furrr::future_map(files, function(f) {
    tryCatch({
      requireNamespace("terra", quietly = TRUE)
      requireNamespace("exactextractr", quietly = TRUE)

      crop_ext <- terra::ext(target_ext_vec)
      r <- terra::crop(terra::flip(terra::rast(f), "vertical"), crop_ext)

      vals_df <- exactextractr::exact_extract(r, model_zones_sf, 'mean',
                                              progress = FALSE)
      vals_mat <- round(t(as.matrix(vals_df)), 2)

      dates <- terra::time(r)
      date_mat <- cbind(
        as.integer(format(dates, "%Y")),
        as.integer(format(dates, "%m")),
        as.integer(format(dates, "%d")),
        0L, 0L
      )

      rm(r); gc()
      return(cbind(date_mat, vals_mat))

    }, error = function(e) {
      warning("Error in ", basename(f), ": ", e$message)
      return(NULL)
    })
  }, .options = furrr::furrr_options(seed = NULL), .progress = TRUE)

  message("Writing data to disk...")
  for (result in results) {
    if (!is.null(result)) {
      data.table::fwrite(as.data.frame(result), file = outfile, append = TRUE,
                         sep = " ", col.names = FALSE, quote = FALSE)
    }
  }

  message("Saved: ", outfile)
  invisible(outfile)
}
