#' GeoSphere Austria Data Download
#'
#' @description
#' Download daily gridded climate data from the GeoSphere Austria data hub.
#' Supports SPARTACUS v2.1 (precipitation, min/max temperature) and
#' WINFORE v2.1 (potential evapotranspiration).
#'
#' @details
#' Files are downloaded as annual NetCDF files into product-specific
#' subdirectories under `output_dir`. Existing files are skipped by default.
#'
#' **Available products:**
#' \itemize{
#'   \item `"SPARTACUS_RR"` — Daily precipitation (mm), 1 km grid over Austria
#'   \item `"SPARTACUS_TN"` — Daily minimum temperature (°C), 1 km grid
#'   \item `"SPARTACUS_TX"` — Daily maximum temperature (°C), 1 km grid
#'   \item `"WINFORE_ET0"` — Daily potential evapotranspiration (mm), 1 km grid
#' }
#'
#' **File naming patterns:**
#' \itemize{
#'   \item SPARTACUS: `SPARTACUS2-DAILY_{param}_{year}.nc`
#'   \item WINFORE: `WINFORE2_{param}_{year}.nc`
#' }
#'
#' **years = "all":** Queries the GeoSphere data hub filelisting page to
#' determine the available year range for each product automatically.
#'
#' Requires the `httr` package. Downloaded files can be used directly with
#' \code{\link{write_spartacus_precip}} and \code{\link{write_spartacus_temp}}.
#'
#' @param product Character vector of one or more products to download.
#'   Valid values: `"SPARTACUS_RR"`, `"SPARTACUS_TN"`, `"SPARTACUS_TX"`,
#'   `"WINFORE_ET0"`.
#' @param output_dir Path to the output directory. Product-specific
#'   subdirectories are created automatically (e.g. `output_dir/SPARTACUS_RR/`).
#' @param years Integer vector of years to download, or `"all"` to
#'   automatically query the server for all available years.
#'   Default: `"all"`.
#' @param timeout Timeout in seconds for each file download. Default: 300.
#' @param overwrite Logical. If `TRUE`, re-download files that already exist.
#'   Default: `FALSE`.
#' @param verbose Logical. Print progress messages. Default: `TRUE`.
#'
#' @return Invisibly returns a named list (one element per product) of
#'   character vectors with paths to successfully downloaded files.
#'
#' @references
#' Hiebl, J. and Frei, C. (2016). Daily temperature grids for Austria since
#' 1961 — concept, creation and applicability. \emph{Theor. Appl. Climatol.}
#' \strong{124}(1):161-178.
#' \href{https://doi.org/10.1007/s00704-015-1411-4}{doi:10.1007/s00704-015-1411-4}
#'
#' Hiebl, J. and Frei, C. (2018). Daily precipitation grids for Austria since
#' 1961 — development and evaluation of a spatial dataset for hydroclimatic
#' monitoring and modelling. \emph{Theor. Appl. Climatol.} \strong{132}(1):327-345.
#' \href{https://doi.org/10.1007/s00704-017-2093-x}{doi:10.1007/s00704-017-2093-x}
#'
#' GeoSphere Austria (2023). SPARTACUS v2 — Spatial Reanalysis of Temperature
#' and Precipitation for Austria.
#' \href{https://doi.org/10.60669/m6w8-s545}{doi:10.60669/m6w8-s545}
#'
#' GeoSphere Austria (2023). WINFORE v2.1 — Daily Potential Evapotranspiration
#' and Standardized Indices for Austria.
#' \href{https://doi.org/10.60669/f6ed-2p24}{doi:10.60669/f6ed-2p24}
#'
#' Haslinger, K. and Bartsch, A. (2016). Creating long-term gridded fields of
#' reference evapotranspiration in Alpine terrain based on a recalibrated
#' Hargreaves method. \emph{Hydrol. Earth Syst. Sci.} \strong{20}:1211-1223.
#' \href{https://doi.org/10.5194/hess-20-1211-2016}{doi:10.5194/hess-20-1211-2016}
#'
#' @seealso
#' \code{\link{write_spartacus_precip}} to extract SPARTACUS precipitation
#' for COSERO model zones,
#' \code{\link{write_spartacus_temp}} to extract SPARTACUS temperature
#' for COSERO model zones,
#' \code{\link{write_winfore_et0}} to extract WINFORE ET0
#' for COSERO model zones.
#'
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#'
#' @examples
#' \dontrun{
#' # Download precipitation for 2020-2023
#' download_geosphere_data(
#'   product    = "SPARTACUS_RR",
#'   output_dir = "D:/Data/GeoSphere",
#'   years      = 2020:2023
#' )
#'
#' # Download all SPARTACUS products for all available years
#' download_geosphere_data(
#'   product    = c("SPARTACUS_RR", "SPARTACUS_TN", "SPARTACUS_TX"),
#'   output_dir = "D:/Data/GeoSphere",
#'   years      = "all"
#' )
#'
#' # Download SPARTACUS + WINFORE, re-downloading existing files
#' download_geosphere_data(
#'   product    = c("SPARTACUS_RR", "WINFORE_ET0"),
#'   output_dir = "D:/Data/GeoSphere",
#'   years      = 2015:2025,
#'   overwrite  = TRUE
#' )
#' }
download_geosphere_data <- function(product,
                                    output_dir,
                                    years     = "all",
                                    timeout   = 300,
                                    overwrite = FALSE,
                                    verbose   = TRUE) {

  # --- Input validation -------------------------------------------------------

  valid_products <- c("SPARTACUS_RR", "SPARTACUS_TN", "SPARTACUS_TX",
                      "WINFORE_ET0")
  unknown <- setdiff(product, valid_products)
  if (length(unknown) > 0) {
    stop("Unknown product(s): ", paste(unknown, collapse = ", "),
         ". Valid: ", paste(valid_products, collapse = ", "),
         call. = FALSE)
  }

  if (!is.character(years) || !identical(years, "all")) {
    years <- suppressWarnings(as.integer(years))
    if (any(is.na(years)) || length(years) == 0) {
      stop("'years' must be an integer vector or \"all\"", call. = FALSE)
    }
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required for downloading. ",
         "Install with: install.packages('httr')", call. = FALSE)
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # --- Main loop --------------------------------------------------------------

  downloaded_paths <- stats::setNames(
    vector("list", length(product)), product
  )

  total_dl   <- 0L
  total_skip <- 0L
  total_fail <- 0L

  for (prod in product) {
    cfg <- .geosphere_product_config(prod)

    # Resolve years
    if (identical(years, "all")) {
      if (verbose) cat("Querying available years for", prod, "...\n")
      yr_range <- .geosphere_query_years(cfg$base_url, cfg$param)
      if (is.null(yr_range)) {
        warning("Could not determine available years for ", prod,
                ". Falling back to 1961-2025.", call. = FALSE)
        yr_range <- 1961:2025
      }
    } else {
      yr_range <- years
    }

    n_files <- length(yr_range)
    if (verbose) {
      cat(sprintf("\nDownloading %s (%d-%d, %d files)\n",
                  prod, min(yr_range), max(yr_range), n_files))
    }

    prod_dir <- file.path(output_dir, prod)
    dir.create(prod_dir, recursive = TRUE, showWarnings = FALSE)

    # Progress bar
    if (verbose) {
      pb <- utils::txtProgressBar(min = 0, max = n_files,
                                  style = 3, width = 50)
    }

    prod_dl   <- 0L
    prod_skip <- 0L
    prod_fail <- 0L
    paths_ok  <- character(0)
    t_start   <- proc.time()[["elapsed"]]

    for (k in seq_along(yr_range)) {
      yr       <- yr_range[k]
      filename <- cfg$filename_fn(yr)
      file_url <- paste0(cfg$base_url, cfg$param, "/", filename)
      dest     <- file.path(prod_dir, filename)

      status <- .geosphere_download_file(file_url, dest,
                                         timeout   = timeout,
                                         overwrite = overwrite)

      if (status == "downloaded") {
        prod_dl  <- prod_dl + 1L
        paths_ok <- c(paths_ok, dest)
        if (verbose) {
          sz <- file.info(dest)$size / 1024^2
          message(sprintf("  Downloaded: %s (%.1f MB)", filename, sz))
        }
      } else if (status == "skipped") {
        prod_skip <- prod_skip + 1L
        paths_ok  <- c(paths_ok, dest)
      } else {
        prod_fail <- prod_fail + 1L
      }

      if (verbose) {
        utils::setTxtProgressBar(pb, k)

        # ETA estimate (after at least 1 file has been attempted)
        elapsed <- proc.time()[["elapsed"]] - t_start
        if (k > 0 && elapsed > 0) {
          eta_sec <- elapsed / k * (n_files - k)
          if (eta_sec >= 60) {
            eta_str <- sprintf("%.1f min", eta_sec / 60)
          } else {
            eta_str <- sprintf("%.0f sec", eta_sec)
          }
          # Update title of progress bar with ETA
          cat(sprintf("\r  ETA: ~%s", eta_str),
              strrep(" ", 10), sep = "")
          utils::flush.console()
        }
      }

      Sys.sleep(0.1)  # Polite pause between requests
    }

    if (verbose) {
      close(pb)
      cat(sprintf(
        "\n  Summary: %d downloaded | %d existing | %d failed\n",
        prod_dl, prod_skip, prod_fail))
    }

    downloaded_paths[[prod]] <- paths_ok
    total_dl   <- total_dl   + prod_dl
    total_skip <- total_skip + prod_skip
    total_fail <- total_fail + prod_fail
  }

  # --- Grand total ------------------------------------------------------------
  if (verbose && length(product) > 1) {
    cat(sprintf(
      "\nTotal: %d downloaded | %d existing | %d failed\n",
      total_dl, total_skip, total_fail))
    if (total_fail > 0) {
      cat("WARNING: Some files failed to download.\n")
    }
  }

  invisible(downloaded_paths)
}


# ============================================================================
# Internal helpers
# ============================================================================

#' @keywords internal
.geosphere_product_config <- function(product) {
  spartacus_base <- paste0(
    "https://public.hub.geosphere.at/datahub/resources/",
    "spartacus-v2-1d-1km/filelisting/"
  )
  winfore_base <- paste0(
    "https://public.hub.geosphere.at/datahub/resources/",
    "winfore-v2-1d-1km/filelisting/"
  )

  configs <- list(
    SPARTACUS_RR = list(
      base_url    = spartacus_base,
      param       = "RR",
      filename_fn = function(yr) sprintf("SPARTACUS2-DAILY_RR_%d.nc", yr)
    ),
    SPARTACUS_TN = list(
      base_url    = spartacus_base,
      param       = "TN",
      filename_fn = function(yr) sprintf("SPARTACUS2-DAILY_TN_%d.nc", yr)
    ),
    SPARTACUS_TX = list(
      base_url    = spartacus_base,
      param       = "TX",
      filename_fn = function(yr) sprintf("SPARTACUS2-DAILY_TX_%d.nc", yr)
    ),
    WINFORE_ET0 = list(
      base_url    = winfore_base,
      param       = "ET0",
      filename_fn = function(yr) sprintf("WINFORE2_ET0_%d.nc", yr)
    )
  )

  configs[[product]]
}

#' @keywords internal
.geosphere_query_years <- function(base_url, param) {
  url <- paste0(base_url, param, "/")
  tryCatch({
    resp <- httr::GET(url, httr::timeout(30))
    if (httr::status_code(resp) != 200) return(NULL)
    html_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    # Extract 4-digit years from filenames like _1961.nc or _2025.nc
    matches <- regmatches(html_text,
                          gregexpr("_(\\d{4})\\.nc", html_text))[[1]]
    years_found <- as.integer(regmatches(matches,
                              regexpr("\\d{4}", matches)))
    if (length(years_found) == 0) return(NULL)
    sort(unique(years_found))
  }, error = function(e) NULL)
}

#' @keywords internal
.geosphere_download_file <- function(url, dest, timeout = 300,
                                     overwrite = FALSE) {
  if (file.exists(dest) && !overwrite) return("skipped")

  tryCatch({
    resp <- httr::GET(
      url,
      httr::write_disk(dest, overwrite = TRUE),
      httr::timeout(timeout)
    )
    if (httr::status_code(resp) == 200) {
      "downloaded"
    } else {
      if (file.exists(dest)) file.remove(dest)
      "failed"
    }
  }, error = function(e) {
    if (file.exists(dest)) file.remove(dest)
    "failed"
  })
}
