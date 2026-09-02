# Interactive Map + Linked Time-Series Viewer ####
# Shows COSERO subbasins / gauging stations on a leaflet map; clicking a
# station opens a near-fullscreen modal with four linked dygraphs panels
# (.runoff, .prec, .plus, .plus1) plus NSE/KGE metrics.
#
# Data backend: on first launch the four COSERO output files are converted to
# fst sidecar files in output/.cache/. When COSERO's binary twin files
# (COSERO.runoffB, .precB, .plusB, .plus1B) are present, a fast readBin()
# path is used for the conversion; otherwise the existing ASCII readers are
# reused. Per click, only the columns of the selected subbasin are read from
# the fst cache (random column access, single-digit ms).
#
# COSERO binary output format (little-endian, decoded from COSERO.exe output;
# verified value-by-value against the ASCII twins):
#   COSERO.runoffB: 1 x int32 header (NB = number of subbasins), then per
#                   timestep: 5 x int32 (Y M D H Min) + 2*NB x float32
#                   (QOBS_i, QSIM_i per subbasin -- Qloc is NOT written)
#   COSERO.precB:   1 x int32 header (NB), then per timestep 2*NB x float32
#                   (PRAINGEB_i, PSNOWGEB_i) -- no date fields
#   COSERO.plusB:   2 x int32 header (NB, nvar), then per timestep
#                   nvar*NB x float32 in ASCII column order -- no date fields
#   COSERO.plus1B:  2 x int32 header (NB, nvar), same layout, values are the
#                   same cumulative sums as the ASCII file (timestep
#                   conversion happens at cache-build time)
# The date-less files take their time axis from COSERO.runoff(B); record
# counts are cross-checked and any mismatch falls back to the ASCII reader.
# Column names are not stored in the binaries -- they are read from the
# ASCII header line ("yyyy mm dd hh mm ..."), which is why the binary fast
# path still requires the ASCII twin to be present.
#
# IMPORTANT -- stale tails: the Fortran writers (dev/datalog_ZRVIEW_RunoffB.f,
# dev/datalog_ZRVIEW_Output.f) open the *B files with access='direct',
# Recl=4 and never truncate them. A shorter run overwrites the first records
# in place and leaves the tail of an earlier, longer run in the file, so the
# file size is NOT a reliable record count. The reader determines the
# effective record count from the ASCII twin's first and last data rows
# (record 1 must match the first row; the effective end is the first record
# matching the last row by date AND values) and ignores anything beyond.
# If no valid region can be identified, it falls back to the ASCII reader.

# 1 Launcher #####

#' Interactive Map of COSERO Subbasins and Gauging Stations
#'
#' Launches a Shiny app showing gauging stations and/or the
#' intermediate-catchment polygons on an interactive map (selectable Light /
#' OpenStreetMap / Topographic / Hillshade / Satellite base maps). Stations
#' and catchment
#' polygons can be coloured by performance metric (NSE, KGE, r or BETA) via a
#' "Colour by metric" selector, a "Find subbasin" box zooms straight to a
#' subbasin by ID (type-ahead; useful when there are thousands of features),
#' and a "Full extent" button re-zooms to all features. Hovering a feature shows a popup with its long-term monthly
#' discharge regime (observed vs simulated, with annual-mean reference lines)
#' and an inverted rain/snow hyetograph (mm/month). Clicking a station or a
#' catchment polygon opens a near-full-screen viewer with four linked
#' time-series panels -- runoff (observed/simulated), precipitation, fluxes
#' (COSERO.plus) and system states (COSERO.plus1) -- together with NSE, KGE
#' and the KGE components (r, alpha, beta) for the selected subbasin. All
#' panels share a common, zoomable time axis (drag to zoom, double-click to
#' reset). Closing the window (or clicking next to it) returns to the map.
#'
#' @param stations_shp Optional path to a point shapefile with the gauging
#'   stations, or an \code{sf} object. Must contain a subbasin ID column
#'   (see \code{subbasin_id_field}). Large station sets (10,000+) are
#'   supported. If NULL, the station layer is skipped and catchment
#'   polygons become the clickable layer. At least one of
#'   \code{stations_shp} / \code{catchments_shp} is required.
#' @param catchments_shp Optional path to a polygon shapefile with the
#'   intermediate catchments (or an \code{sf} object). If NULL (default),
#'   the catchment layer is skipped. When present the polygons are also
#'   clickable (and the only clickable layer when \code{stations_shp} is
#'   NULL); they must carry a subbasin ID column too.
#' @param cosero_path COSERO project path. Time series are read from
#'   \code{output/COSERO.runoff}, \code{.prec}, \code{.plus}, \code{.plus1}.
#'   On first launch these files are converted to a fast binary cache
#'   (fst format) in \code{output/.cache/}; later launches and every click
#'   read from the cache (milliseconds instead of seconds). If COSERO's
#'   binary output twins (\code{COSERO.runoffB} etc.) are present, they are
#'   used for the conversion (no ASCII parsing of the bulk data). Binaries
#'   are validated against the ASCII twin's first and last data rows; stale
#'   binaries from an earlier run are detected and skipped automatically.
#' @param subbasin_id_field Name of the attribute column holding the COSERO
#'   subbasin ID in the \strong{stations} layer (and the catchments layer
#'   too, unless \code{catchment_id_field} is given). If NULL (default), the
#'   columns NB, ID, SB, basin and subbasin are tried (case-insensitive, with
#'   an optional trailing underscore, so \code{ID_} / \code{NB_} match).
#' @param catchment_id_field Name of the subbasin ID column in the
#'   \strong{catchments} layer, when it differs from the stations layer
#'   (e.g. gauges link via \code{ID_} but catchments via \code{NB_}). If
#'   NULL (default), \code{subbasin_id_field} is used, falling back to
#'   auto-detection.
#' @param catchment_simplify_pct Percentage of polygon vertices retained for
#'   map display (default 5). Uses topology-preserving simplification
#'   (rmapshaper), so shared boundaries between intermediate catchments stay
#'   seamless. Affects display only; the shapefile on disk is never
#'   modified. Use 100 to disable simplification.
#' @param spinup Integer number of leading model timesteps (e.g. days or
#'   hours, depending on the model timestep) excluded when computing the
#'   objective functions (NSE, KGE, r, BETA), both for the map station
#'   colours and the per-station header strip (default 365). The
#'   time-series plots always show the full period including the spin-up;
#'   only the metrics ignore it. Use 0 to disable.
#' @param clean_cache Logical (default FALSE). If TRUE, the fst cache in
#'   \code{output/.cache/} is deleted and rebuilt from the source files
#'   (binary twins when present, otherwise ASCII) before the app launches.
#'   Use this to force a re-read after the COSERO outputs changed in a way
#'   the mtime/size check would not catch.
#'
#' @details
#' \strong{Layers and shapefiles.} At least one of \code{stations_shp} /
#' \code{catchments_shp} must be supplied; either may be a file path or an
#' \code{sf} object, and both are transformed to WGS84 for display. Each
#' layer must carry a subbasin ID column whose values match the COSERO
#' subbasin numbers used in the output column suffixes (e.g. \code{QSIM_0001}).
#' The two layers may use \emph{different} ID columns
#' (\code{subbasin_id_field} for stations, \code{catchment_id_field} for
#' catchments); column names are matched case-insensitively and with an
#' optional trailing underscore, so \code{"ID"} matches \code{ID_} and
#' \code{"NB"} matches \code{NB_}. Whichever layer(s) you provide is
#' clickable (points drawn on top of polygons when both are present); if a
#' clicked feature's ID does not match any output column, the viewer reports
#' "no data".
#'
#' \strong{Feature colouring.} The "Colour by metric" selector recolours both
#' the station points and the catchment polygons (so it works in
#' catchments-only projects) by NSE, KGE, r or BETA, with a matching legend.
#' Polygons are drawn semi-transparent so the basemap stays readable.
#' NSE/KGE/r use a
#' sequential blue (good) to red (worse) scale; BETA uses a diverging scale
#' around 1 (under- vs over-estimation, balanced within 5 percent). Metrics
#' are computed once at launch with the spin-up excluded; features without
#' observations are shown in grey.
#'
#' \strong{Hover and click.} Resting on a feature (~250 ms) opens a popup
#' with the monthly discharge regime and rain/snow hyetograph; the popup
#' stays until another feature is hovered (or is dismissed with its close
#' button). Clicking opens the four-panel viewer; the flux and state panels
#' have variable selectors with removable chips, and the modal layout fills
#' the window automatically.
#'
#' \strong{Required packages} (all in Suggests, checked at launch):
#' \code{sf}, \code{leaflet}, \code{dygraphs}, \code{xts}, \code{fst} and
#' \code{base64enc} (for the hover popup image); \code{rmapshaper} only when
#' \code{catchments_shp} is given with \code{catchment_simplify_pct < 100}.
#' If \code{leafgl} is installed, stations are drawn with WebGL (handles
#' 10,000+ points trivially); otherwise canvas circle markers are used.
#'
#' \strong{Cache.} The fst cache is invalidated automatically when a source
#' file's modification time or size changes; pass \code{clean_cache = TRUE}
#' to force a rebuild. The per-subbasin metrics are cached next to it in
#' \code{station_metrics.rds}, keyed on the runoff fst identity and
#' \code{spinup}, so they are recomputed only when the cache is rebuilt or a
#' different spin-up is requested. Subbasins without observations (Qobs = -999) show the
#' simulated series only and "no observations" instead of metrics. Metric
#' handling follows the package convention: values <= -999 and negative
#' observations are treated as NA and dropped pairwise.
#'
#' \strong{Metrics CSV.} At each launch the per-subbasin objective functions
#' (computed with the spin-up excluded) are written to
#' \code{output/.cache/station_metrics.csv}: columns \code{subbasin, lon, lat,
#' coord_source, NSE, KGE, r, alpha, beta, n, spinup}. The coordinate is the
#' gauging-station point when available, otherwise the catchment representative
#' point (WGS84). Handy as a stand-alone table, e.g. for spatial clustering of
#' model performance.
#'
#' @return A \code{shiny.appobj}; called for its side effect of launching
#'   the app.
#'
#' @seealso \code{\link{launch_cosero_app}} for the full COSERO workbench,
#'   \code{\link{read_cosero_output}} for scripted output reading.
#'
#' @export
#' @examples
#' \dontrun{
#' # Minimal call: stations shapefile + project path
#' launch_cosero_map(
#'   stations_shp = "D:/gis/gauges.shp",
#'   cosero_path  = "D:/COSERO_project"
#' )
#'
#' # Stations + catchments using different ID columns (gauges ID_, basins NB_)
#' launch_cosero_map(
#'   stations_shp       = "D:/gis/gauges.shp",
#'   catchments_shp     = "D:/gis/catchments.shp",
#'   cosero_path        = "D:/COSERO_project",
#'   subbasin_id_field  = "ID",
#'   catchment_id_field = "NB",
#'   catchment_simplify_pct = 10
#' )
#'
#' # Catchments only (no gauges) -- polygons become the clickable layer
#' launch_cosero_map(
#'   catchments_shp     = "D:/gis/catchments.shp",
#'   cosero_path        = "D:/COSERO_project",
#'   catchment_id_field = "NB"
#' )
#'
#' # Force a cache rebuild and use a 1-year metric spin-up
#' launch_cosero_map(
#'   stations_shp = "D:/gis/gauges.shp",
#'   cosero_path  = "D:/COSERO_project",
#'   spinup       = 365,
#'   clean_cache  = TRUE
#' )
#' }
launch_cosero_map <- function(stations_shp = NULL,
                              catchments_shp = NULL,
                              cosero_path,
                              subbasin_id_field = NULL,
                              catchment_id_field = NULL,
                              catchment_simplify_pct = 5,
                              spinup = 365,
                              clean_cache = FALSE) {

  if (is.null(stations_shp) && is.null(catchments_shp)) {
    stop("Provide at least one of 'stations_shp' or 'catchments_shp'", call. = FALSE)
  }
  if (!is.numeric(catchment_simplify_pct) || length(catchment_simplify_pct) != 1 ||
      catchment_simplify_pct <= 0 || catchment_simplify_pct > 100) {
    stop("'catchment_simplify_pct' must be a single number in (0, 100]", call. = FALSE)
  }
  if (!is.numeric(spinup) || length(spinup) != 1 || is.na(spinup) || spinup < 0) {
    stop("'spinup' must be a single non-negative number", call. = FALSE)
  }
  spinup <- as.integer(spinup)
  if (!is.logical(clean_cache) || length(clean_cache) != 1 || is.na(clean_cache)) {
    stop("'clean_cache' must be a single TRUE or FALSE", call. = FALSE)
  }

  need_simplify <- !is.null(catchments_shp) && catchment_simplify_pct < 100
  check_map_viewer_packages(need_simplify = need_simplify)

  if (missing(cosero_path) || !dir.exists(cosero_path)) {
    stop("'cosero_path' must be an existing COSERO project directory", call. = FALSE)
  }
  output_dir <- file.path(cosero_path, "output")
  if (!dir.exists(output_dir)) {
    stop("No 'output' folder found in: ", cosero_path, call. = FALSE)
  }

  # Stations layer (optional)
  stations <- NULL
  station_id_field <- NULL
  if (!is.null(stations_shp)) {
    stations <- resolve_sf_layer(stations_shp, "stations_shp")
    gtypes <- unique(as.character(sf::st_geometry_type(stations)))
    if (!all(gtypes %in% c("POINT", "MULTIPOINT"))) {
      stop("'stations_shp' must be a point layer (found geometry type: ",
           paste(gtypes, collapse = ", "), ")", call. = FALSE)
    }
    station_id_field <- detect_subbasin_id_field(stations, subbasin_id_field,
                                                 "stations")
    stations <- sf::st_transform(stations, 4326)
  }

  # Catchments layer (optional; clickable, with independent ID detection).
  # catchment_id_field falls back to subbasin_id_field, then to auto-detect,
  # so the two layers can use different ID columns (e.g. gauges via ID_,
  # catchments via NB_).
  catchments <- NULL
  catchment_id_resolved <- NULL
  if (!is.null(catchments_shp)) {
    catchments <- resolve_sf_layer(catchments_shp, "catchments_shp")
    catch_field <- if (!is.null(catchment_id_field)) catchment_id_field
                   else subbasin_id_field
    catchment_id_resolved <- detect_subbasin_id_field(catchments, catch_field,
                                                      "catchments")
    catchments <- sf::st_transform(catchments, 4326)
    if (catchment_simplify_pct < 100) {
      # Preserve the ID column through simplification
      catchments <- rmapshaper::ms_simplify(
        catchments,
        keep = catchment_simplify_pct / 100,
        keep_shapes = TRUE
      )
    }
  }

  # Build / validate the fst cache (binary fast path when *B files exist).
  # clean_cache = TRUE forces a full rebuild from the source files.
  if (clean_cache) {
    cache_dir <- file.path(output_dir, ".cache")
    if (dir.exists(cache_dir)) {
      message("clean_cache = TRUE: removing ", cache_dir)
      unlink(cache_dir, recursive = TRUE)
    }
  }
  cache <- build_output_cache(output_dir)

  # Per-subbasin performance metrics for the feature colouring (spinup
  # excluded). Cached alongside the fst files: the full pass reads every
  # QOBS_/QSIM_ column and costs ~9 s for 13,605 subbasins, while the result
  # is only ~1.4 MB. Keyed on the runoff fst identity AND spinup, so a cache
  # rebuild or a different spinup recomputes automatically.
  station_metrics <- load_or_compute_station_metrics(
    cache, spinup = spinup, cache_dir = file.path(output_dir, ".cache")
  )

  # Persist metrics + one coordinate per subbasin (station, else catchment
  # centroid) to output/.cache/station_metrics.csv -- e.g. for spatial clustering
  csv_path <- write_station_metrics_csv(
    station_metrics, stations, station_id_field,
    catchments, catchment_id_resolved,
    cache_dir = file.path(output_dir, ".cache"), spinup = spinup
  )
  if (!is.null(csv_path)) message("Wrote station metrics CSV: ", csv_path)

  ui <- shiny::fillPage(
    title = "COSERO Map Viewer",
    map_viewer_ui("cosero_map")
  )
  server <- function(input, output, session) {
    map_viewer_server("cosero_map",
                      stations = stations,
                      id_field = station_id_field,
                      catchments = catchments,
                      catchment_id_field = catchment_id_resolved,
                      cache = cache,
                      station_metrics = station_metrics,
                      spinup = spinup)
  }

  shiny::shinyApp(ui, server)
}

# 2 Package / input validation helpers #####

#' Check that suggested packages for the map viewer are installed
#' @keywords internal
check_map_viewer_packages <- function(need_simplify = FALSE) {
  required <- c("sf", "leaflet", "dygraphs", "xts", "fst", "base64enc")
  if (need_simplify) required <- c(required, "rmapshaper")
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1),
                                   quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("launch_cosero_map() requires additional packages. Install with:\n",
         "  install.packages(c(",
         paste0('"', missing_pkgs, '"', collapse = ", "), "))",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Resolve a path or sf object to an sf object
#' @keywords internal
resolve_sf_layer <- function(x, arg_name) {
  if (inherits(x, "sf")) return(x)
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) {
      stop("'", arg_name, "' file not found: ", x, call. = FALSE)
    }
    return(sf::st_read(x, quiet = TRUE))
  }
  stop("'", arg_name, "' must be a file path or an sf object", call. = FALSE)
}

#' Auto-detect the subbasin ID column of an sf layer
#'
#' When \code{field} is given, it is matched case-insensitively and also with
#' a trailing underscore (COSERO shapefiles often store \code{ID} as
#' \code{ID_}, \code{NB} as \code{NB_}). Auto-detection (\code{field = NULL})
#' tries NB, ID, SB, basin, subbasin, each with an optional trailing
#' underscore.
#' @keywords internal
detect_subbasin_id_field <- function(layer, field, layer_name, required = TRUE) {
  cols <- setdiff(names(layer), attr(layer, "sf_column"))
  match_col <- function(cand) {
    # exact, case-insensitive, and trailing-underscore variants
    variants <- unique(c(cand, paste0(cand, "_")))
    for (v in variants) {
      hit <- cols[tolower(cols) == tolower(v)]
      if (length(hit) >= 1) return(hit[1])
    }
    NULL
  }
  if (!is.null(field)) {
    hit <- match_col(field)
    if (!is.null(hit)) return(hit)
    stop("Column '", field, "' not found in ", layer_name,
         " layer (also tried '", field, "_'). Available columns: ",
         paste(cols, collapse = ", "), call. = FALSE)
  }
  for (cand in c("nb", "id", "sb", "basin", "subbasin")) {
    hit <- match_col(cand)
    if (!is.null(hit)) return(hit)
  }
  if (!required) return(NULL)
  stop("Could not auto-detect the subbasin ID column in the ", layer_name,
       " layer (tried NB, ID, SB, basin, subbasin; case-insensitive, with ",
       "optional trailing underscore). Available columns: ",
       paste(cols, collapse = ", "),
       ". Set it explicitly with 'subbasin_id_field' / 'catchment_id_field'.",
       call. = FALSE)
}

# 3 fst cache (with binary fast path) #####

#' Build or validate the fst sidecar cache for the four COSERO output files
#'
#' Converts COSERO.runoff/.prec/.plus/.plus1 to fst files in
#' \code{output/.cache/}. Prefers the binary twin files (e.g.
#' \code{COSERO.runoffB}) via \code{read_cosero_binary_file()}; falls back to
#' the ASCII readers. The cache is invalidated when the source file's mtime
#' or size changes (tracked in \code{cache_meta.rds}).
#' @keywords internal
build_output_cache <- function(output_dir, quiet = FALSE) {
  specs <- list(
    runoff = list(file = "COSERO.runoff", has_dates = TRUE,  header_ints = 1L),
    prec   = list(file = "COSERO.prec",   has_dates = FALSE, header_ints = 1L),
    plus   = list(file = "COSERO.plus",   has_dates = FALSE, header_ints = 2L),
    plus1  = list(file = "COSERO.plus1",  has_dates = FALSE, header_ints = 2L)
  )

  cache_dir <- file.path(output_dir, ".cache")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  meta_path <- file.path(cache_dir, "cache_meta.rds")
  meta <- if (file.exists(meta_path)) readRDS(meta_path) else list()

  # Bump when the cache CONTENT changes (e.g. v2 added the cumulative
  # *_SUM_* columns to plus1) -- discards caches built by older code
  cache_format_version <- 2L
  if (!identical(meta$format_version, cache_format_version)) {
    meta <- list(format_version = cache_format_version)
  }

  if (!quiet) message("Checking COSERO output cache in ", cache_dir)

  cache <- list()
  runoff_datetime <- NULL

  for (key in names(specs)) {
    spec <- specs[[key]]
    ascii_path <- file.path(output_dir, spec$file)
    bin_path <- paste0(ascii_path, "B")

    if (!file.exists(ascii_path)) {
      if (file.exists(bin_path)) {
        warning(spec$file, "B found without its ASCII twin -- skipped ",
                "(column names are only stored in the ASCII header)",
                call. = FALSE)
      }
      next
    }

    # Cache validity is governed by BOTH source files: the converter may
    # fall back from a stale binary to the ASCII twin, so a change in
    # either file must trigger reconversion
    finfo <- function(p) {
      if (file.exists(p)) c(as.numeric(file.mtime(p)), as.numeric(file.size(p)))
      else c(NA_real_, NA_real_)
    }
    src_info <- list(ascii = finfo(ascii_path), bin = finfo(bin_path))

    fst_path <- file.path(cache_dir, paste0(spec$file, ".fst"))
    valid <- file.exists(fst_path) && identical(meta[[key]], src_info)

    if (!valid) {
      df <- convert_output_for_cache(key, ascii_path, bin_path, spec,
                                     runoff_datetime, quiet = quiet)
      if (is.null(df)) next
      fst::write_fst(df, fst_path)
      meta[[key]] <- src_info
      saveRDS(meta, meta_path)
    }

    cache[[key]] <- list(
      path = fst_path,
      columns = fst::metadata_fst(fst_path)$columnNames
    )

    if (key == "runoff") {
      runoff_datetime <- fst::read_fst(fst_path, columns = "DateTime")$DateTime
    }
  }

  if (is.null(cache$runoff)) {
    stop("COSERO.runoff not found in ", output_dir,
         " -- run COSERO first (the map viewer needs at least the runoff file)",
         call. = FALSE)
  }

  cache
}

#' Convert one COSERO output file to its cache data frame
#'
#' Tries the binary fast path first (when the *B twin exists), then falls
#' back to the existing ASCII readers. Returns a data frame with DateTime
#' plus the value columns (subbasin-suffixed), or NULL if unreadable.
#' @keywords internal
convert_output_for_cache <- function(key, ascii_path, bin_path, spec,
                                     runoff_datetime, quiet = FALSE) {
  df <- NULL
  used <- NULL

  if (file.exists(bin_path)) {
    value_names <- read_output_header_names(ascii_path)
    edge_rows <- read_ascii_edge_rows(ascii_path)
    if (!is.null(value_names) && !is.null(edge_rows)) {
      df <- read_cosero_binary_file(bin_path, value_names,
                                    has_dates = spec$has_dates,
                                    header_ints = spec$header_ints,
                                    datetime = runoff_datetime,
                                    edge_rows = edge_rows)
      used <- "binary"
    }
    if (is.null(df) && !quiet) {
      message("  ", basename(bin_path), ": does not match current ",
              basename(ascii_path), " content -- falling back to ASCII")
    }
  }

  if (is.null(df)) {
    output_dir <- dirname(ascii_path)
    df <- switch(key,
      runoff = read_runoff(output_dir, quiet = TRUE),
      prec   = read_precipitation(output_dir, quiet = TRUE),
      plus   = read_plus(output_dir, quiet = TRUE),
      plus1  = read_plus1(output_dir, quiet = TRUE)
    )
    used <- "ascii"
  }
  if (is.null(df)) return(NULL)

  # Mirror read_cosero_runoff(): discharge cannot be negative.
  # Build the cleaned columns first and assign them in ONE `[<-` call:
  # `df[[col]][...] <- NA` inside a loop re-copies the whole frame on every
  # column, which on a 27,210-column runoff file costs ~40 s vs ~0.9 s here.
  if (key == "runoff" && used == "binary") {
    neg_cols <- grep("^(QOBS_|QSIM_|Qloc_)", colnames(df))
    if (length(neg_cols) > 0) {
      df[neg_cols] <- lapply(neg_cols, function(j) {
        v <- df[[j]]
        v[v < 0] <- NA
        v
      })
    }
  }

  # plus1: cumulative sums -> timestep values (ASCII reader already does
  # this). Both the converted timestep columns (e.g. PGEB) AND the original
  # cumulative *_SUM_* columns are kept -- the states panel offers both.
  if (key == "plus1" && used == "binary") {
    lt <- as.POSIXlt(df$DateTime)
    df <- convert_sum_columns_to_timestep(df, lt$mon + 1L, lt$mday)
  }

  # Subset / filter only when they actually change something -- each of these
  # copies the entire frame, and for the binary path they are usually no-ops.
  keep <- c("DateTime", grep("_\\d+$", colnames(df), value = TRUE))
  if (length(keep) != ncol(df)) df <- df[, keep, drop = FALSE]
  bad_dt <- is.na(df$DateTime)
  if (any(bad_dt)) df <- df[!bad_dt, , drop = FALSE]

  if (!quiet) {
    message("  Cached ", basename(ascii_path), " (", used, ", ",
            nrow(df), " rows, ", length(keep) - 1L, " columns)")
  }
  df
}

#' Read the value-column names from a COSERO ASCII output header
#'
#' Finds the header line starting with "yyyy mm dd" and returns the tokens
#' after the five date columns.
#' @keywords internal
read_output_header_names <- function(ascii_path, max_lines = 100) {
  lines <- readLines(ascii_path, n = max_lines, warn = FALSE)
  hdr <- grep("^\\s*yyyy\\s+mm\\s+dd", lines, value = TRUE)
  if (length(hdr) == 0) return(NULL)
  tokens <- strsplit(trimws(hdr[1]), "\\s+")[[1]]
  if (length(tokens) <= 5) return(NULL)
  tokens[-(1:5)]
}

#' Read a COSERO binary output file (fast path)
#'
#' Vectorized readBin() reader for COSERO's binary output twins. The Fortran
#' writers (\code{dev/datalog_ZRVIEW_RunoffB.f},
#' \code{dev/datalog_ZRVIEW_Output.f}) open these files with
#' \code{access='direct', Recl=4} and never truncate them: a shorter run
#' overwrites the first records in place and leaves the tail of an earlier,
#' longer run in the file. The file size is therefore NOT a reliable record
#' count. This reader instead determines the effective number of records
#' from the ASCII twin's first and last data rows (\code{edge_rows}):
#' record 1 must match the first ASCII row, and the effective end is the
#' first record whose date AND values match the last ASCII row (date-less
#' files take their length from the runoff time axis and are validated by
#' values only). Anything beyond is stale tail and is ignored.
#' \code{value_names} comes from the ASCII header; the runoff binary omits
#' the Qloc columns, so a name set without Qloc_* is tried as well. On any
#' mismatch NULL is returned and the caller falls back to the ASCII reader.
#'
#' @param bin_path Path to the binary file (e.g. COSERO.runoffB)
#' @param value_names Value column names from the ASCII header
#' @param has_dates TRUE if each record starts with 5 x int32 (Y M D H Min)
#' @param header_ints Number of leading int32 header values (1 for
#'   runoffB/precB, 2 for plusB/plus1B, per the Fortran writers)
#' @param datetime POSIXct time axis for date-less files (from the runoff
#'   cache); defines their effective record count
#' @param edge_rows First/last ASCII data rows from
#'   \code{read_ascii_edge_rows()}; required (NULL disables the binary path)
#' @return Data frame with DateTime + value columns, or NULL
#' @keywords internal
read_cosero_binary_file <- function(bin_path, value_names, has_dates,
                                    header_ints, datetime = NULL,
                                    edge_rows = NULL) {
  if (is.null(edge_rows)) return(NULL)
  sz <- file.size(bin_path)
  if (is.na(sz) || sz < 12) return(NULL)
  raw_data <- readBin(bin_path, what = "raw", n = sz)
  hdr_bytes <- 4L * header_ints

  # Candidate column sets: full ASCII header, and (runoff) without Qloc_*
  name_candidates <- list(value_names)
  is_qloc <- grepl("^Qloc_", value_names)
  if (any(is_qloc)) {
    name_candidates <- c(name_candidates, list(value_names[!is_qloc]))
  }

  for (nms in name_candidates) {
    n_vals <- length(nms)
    if (n_vals < 1) next
    rec_bytes <- (if (has_dates) 20L else 0L) + 4L * n_vals
    n_max <- (sz - hdr_bytes) %/% rec_bytes  # may include stale tail records
    if (n_max < 1) next
    sel <- match(nms, value_names)
    if (max(sel) > length(edge_rows$first$values)) next

    if (has_dates) {
      body <- raw_data[(hdr_bytes + 1L):(hdr_bytes + n_max * rec_bytes)]
      m <- matrix(body, nrow = rec_bytes)
      ints <- readBin(as.vector(m[1:20, , drop = FALSE]),
                      what = "integer", size = 4L, n = 5L * n_max,
                      endian = "little")
      dmat <- matrix(ints, nrow = 5L)
      # Record 1 must match the first ASCII data row exactly
      if (!all(dmat[, 1] == edge_rows$first$date)) next
      vals <- readBin(as.vector(m[21:rec_bytes, , drop = FALSE]),
                      what = "numeric", size = 4L, n = n_vals * n_max,
                      endian = "little")
      vmat <- matrix(vals, nrow = n_vals)
      if (!edge_values_match(vmat[, 1], edge_rows$first$values[sel])) next
      # Effective end: FIRST record matching the last ASCII row by date AND
      # values (a stale tail can coincidentally end on the same date)
      cand <- which(colSums(dmat == edge_rows$last$date) == 5L)
      n_eff <- NA_integer_
      for (i in cand) {
        if (edge_values_match(vmat[, i], edge_rows$last$values[sel])) {
          n_eff <- i
          break
        }
      }
      if (is.na(n_eff)) next
      dt <- ISOdatetime(dmat[1, 1:n_eff], dmat[2, 1:n_eff], dmat[3, 1:n_eff],
                        dmat[4, 1:n_eff], dmat[5, 1:n_eff], 0)
      vmat <- vmat[, 1:n_eff, drop = FALSE]
    } else {
      # Date-less file: length comes from the runoff time axis
      if (is.null(datetime)) next
      n_eff <- length(datetime)
      if (n_max < n_eff) next
      body <- raw_data[(hdr_bytes + 1L):(hdr_bytes + n_eff * rec_bytes)]
      vals <- readBin(body, what = "numeric", size = 4L,
                      n = n_vals * n_eff, endian = "little")
      vmat <- matrix(vals, nrow = n_vals)
      if (!edge_values_match(vmat[, 1], edge_rows$first$values[sel]) ||
          !edge_values_match(vmat[, n_eff], edge_rows$last$values[sel])) {
        next
      }
      dt <- datetime
    }

    df <- as.data.frame(t(vmat))
    colnames(df) <- nms
    df <- cbind(DateTime = dt, df)
    return(df)
  }
  NULL
}

#' Read the first and last data rows of a COSERO ASCII output file
#'
#' Used to validate binary twins against the current ASCII content without
#' parsing the whole file: the first data row follows the "yyyy mm dd"
#' header line; the last row is read by seeking to the end of the file.
#' Returns \code{list(first, last)}, each \code{list(date, values)}, or NULL.
#' @keywords internal
read_ascii_edge_rows <- function(ascii_path, max_lines = 100) {
  lines <- readLines(ascii_path, n = max_lines, warn = FALSE)
  hdr_idx <- grep("^\\s*yyyy\\s+mm\\s+dd", lines)
  if (length(hdr_idx) == 0 || length(lines) <= hdr_idx[1]) return(NULL)

  parse_row <- function(line) {
    if (is.null(line) || !nzchar(trimws(line))) return(NULL)
    vals <- suppressWarnings(as.numeric(strsplit(trimws(line), "\\s+")[[1]]))
    if (length(vals) <= 5) return(NULL)
    list(date = vals[1:5], values = vals[-(1:5)])
  }

  first <- parse_row(lines[hdr_idx[1] + 1])
  last <- parse_row(read_last_text_line(ascii_path))
  if (is.null(first) || is.null(last)) return(NULL)
  list(first = first, last = last)
}

#' Read the last non-empty line of a text file without reading the whole file
#'
#' Reads a tail chunk and grows it until it provably contains the complete
#' last line. COSERO ASCII lines can be very long (a 630-subbasin plus1
#' file has ~40 KB per line), so a fixed small chunk would return a mid-line
#' fragment and break the stale-binary validation.
#' @keywords internal
read_last_text_line <- function(path, chunk = 65536L) {
  sz <- file.size(path)
  if (is.na(sz) || sz == 0) return(NULL)
  repeat {
    con <- file(path, open = "rb")
    seek(con, max(0, sz - chunk))
    txt <- readChar(con, nchars = chunk, useBytes = TRUE)
    close(con)
    parts <- strsplit(txt, "\r?\n")[[1]]
    parts <- parts[nzchar(trimws(parts))]
    if (chunk >= sz) {
      # Chunk covers the whole file -- last line is complete by definition
      if (length(parts) == 0) return(NULL)
      return(parts[length(parts)])
    }
    # The first element may be a partial line (chunk started mid-line);
    # with >= 2 non-empty lines the LAST one is known to be complete
    if (length(parts) >= 2) return(parts[length(parts)])
    chunk <- chunk * 8L
  }
}

#' Compare a binary record against an ASCII data row within print precision
#'
#' COSERO prints 2-3 decimals in the ASCII files, so the maximum rounding
#' difference vs the full-precision float32 binary value is 0.005; a small
#' relative term covers float32 representation error of large values
#' (e.g. cumulative sums). NA / non-finite entries are skipped pairwise.
#' @keywords internal
edge_values_match <- function(bin_vals, ascii_vals, tol = 0.006) {
  if (is.null(ascii_vals) || length(ascii_vals) != length(bin_vals)) {
    return(FALSE)
  }
  comp <- is.finite(bin_vals) & is.finite(ascii_vals)
  if (!any(comp)) return(TRUE)
  tol_vec <- pmax(tol, abs(ascii_vals[comp]) * 1e-6)
  all(abs(bin_vals[comp] - ascii_vals[comp]) <= tol_vec)
}

#' Convert cumulative *_SUM_* columns to timestep values
#'
#' Same logic as \code{read_plus1()}: first value kept, then diff(); on
#' September 1 (water-year reset) a negative diff is replaced by the
#' cumulative value itself.
#' @keywords internal
convert_sum_columns_to_timestep <- function(df, month_vec, day_vec) {
  sum_cols <- grep("_SUM_", colnames(df), value = TRUE)
  if (length(sum_cols) == 0) return(df)

  # The water-year reset rows are the same for every column -- compute once,
  # not once per column (this used to sit inside the loop). Row 1 is excluded
  # here rather than re-tested per index (the old `idx > 1` guard).
  reset_idx <- which(month_vec == 9 & day_vec == 1)
  reset_idx <- reset_idx[reset_idx > 1]

  # Build the derived columns into a list and bind ONCE. Assigning into the
  # data frame inside the loop (df[[new_col]] <- ...) re-copies the whole
  # frame on each append, which is quadratic in the column count: measured
  # 0.15 ms/col at 500 columns but 0.61 ms/col at 5,000. On the Europe-wide
  # project (40,815 _SUM_ columns, growing the frame to ~122k columns) that
  # dominated the entire cache build. Vectorising the reset fix removes the
  # inner scalar loop as well.
  out <- vector("list", length(sum_cols))
  names(out) <- gsub("_SUM_", "_", sum_cols)
  for (i in seq_along(sum_cols)) {
    vals <- df[[sum_cols[i]]]
    timestep_vals <- c(vals[1], diff(vals))
    if (length(reset_idx) > 0) {
      # On Sep 1 the cumulative counter restarts, so a negative diff means
      # "counter reset" -- keep the cumulative value itself. NA-safe.
      hit <- reset_idx[which(timestep_vals[reset_idx] < 0)]
      if (length(hit) > 0) timestep_vals[hit] <- vals[hit]
    }
    out[[i]] <- timestep_vals
  }
  cbind(df, as.data.frame(out, stringsAsFactors = FALSE))
}

# 4 Per-click data access #####

#' Find the columns belonging to a subbasin (3/4/5-digit format probing)
#' @keywords internal
probe_subbasin_columns <- function(col_names, sb_id) {
  sb_num <- suppressWarnings(as.numeric(sb_id))
  fmts <- if (!is.na(sb_num)) {
    c(sprintf("%05d", sb_num), sprintf("%04d", sb_num),
      sprintf("%03d", sb_num), sprintf("%d", sb_num))
  } else {
    as.character(sb_id)
  }
  for (fmt in unique(fmts)) {
    hits <- grep(paste0("_", fmt, "$"), col_names, value = TRUE)
    if (length(hits) > 0) return(list(fmt = fmt, cols = hits))
  }
  NULL
}

#' Load all cached time series for one subbasin (with in-session memoization)
#'
#' Reads only the matching columns from each fst file and strips the
#' subbasin suffix from the column names (QOBS_0001 -> QOBS). Results are
#' memoized in \code{session_cache} so repeated clicks are instant. The
#' header-strip metrics exclude the first \code{spinup} rows; the returned
#' time series are full-length (the plots show the spin-up).
#' @keywords internal
load_station_data <- function(cache, sb_id, session_cache, spinup = 0L) {
  key <- as.character(sb_id)
  if (exists(key, envir = session_cache, inherits = FALSE)) {
    return(get(key, envir = session_cache, inherits = FALSE))
  }

  res <- list(subbasin = key, runoff = NULL, prec = NULL,
              plus = NULL, plus1 = NULL, metrics = NULL)

  for (part in c("runoff", "prec", "plus", "plus1")) {
    entry <- cache[[part]]
    if (is.null(entry)) next
    probe <- probe_subbasin_columns(entry$columns, sb_id)
    if (is.null(probe)) next
    df <- fst::read_fst(entry$path, columns = c("DateTime", probe$cols))
    names(df) <- c("DateTime", sub(paste0("_", probe$fmt, "$"), "", probe$cols))
    res[[part]] <- df
  }

  if (!is.null(res$runoff) && all(c("QOBS", "QSIM") %in% names(res$runoff))) {
    rn <- res$runoff
    obs <- rn$QOBS
    sim <- rn$QSIM
    keep <- if (spinup > 0 && spinup < length(obs)) -seq_len(spinup) else TRUE
    res$metrics <- compute_station_metrics(obs[keep], sim[keep])
    # Long-term monthly discharge regime (Jan-Dec mean), spinup excluded --
    # for the map hover popup
    res$regime <- compute_monthly_regime(rn$DateTime[keep], obs[keep], sim[keep])
  }

  # Long-term monthly rain/snow regime for the hover hyetograph
  if (!is.null(res$prec)) {
    pr <- res$prec
    rain <- if ("PRAINGEB" %in% names(pr)) pr$PRAINGEB else NULL
    snow <- if ("PSNOWGEB" %in% names(pr)) pr$PSNOWGEB else NULL
    dt <- pr$DateTime
    keep_p <- if (spinup > 0 && spinup < nrow(pr)) -seq_len(spinup) else TRUE
    res$prec_regime <- compute_monthly_prec_regime(
      dt[keep_p],
      if (!is.null(rain)) rain[keep_p] else NULL,
      if (!is.null(snow)) snow[keep_p] else NULL
    )
  }

  assign(key, res, envir = session_cache)
  res
}

#' Long-term monthly rain/snow regime (mean monthly TOTAL, mm/month)
#'
#' Returns a 12-row data frame (month, rain, snow). For each component the
#' precipitation is first summed within every (year, month) to a monthly
#' total, then those totals are averaged across years -- i.e. the typical
#' rain/snow depth per calendar month in mm/month (not mm/day). Either
#' component may be NULL (returns NA for that column).
#' @keywords internal
compute_monthly_prec_regime <- function(datetime, rain, snow) {
  lt <- as.POSIXlt(datetime)
  mon <- lt$mon + 1L
  yr <- lt$year + 1900L
  ym <- yr * 100L + mon
  ym_month <- (ym %% 100L)  # calendar month of each (year, month) group
  monthly_total_mean <- function(x) {
    if (is.null(x)) return(rep(NA_real_, 12))
    x[x <= -999] <- NA
    # sum to a per-(year,month) total, then average those totals by month
    tot <- tapply(x, ym, sum, na.rm = TRUE)
    tot_month <- ym_month[match(as.integer(names(tot)), ym)]
    mm <- tapply(as.numeric(tot), tot_month, mean, na.rm = TRUE)
    v <- rep(NA_real_, 12)
    v[as.integer(names(mm))] <- as.numeric(mm)
    v[is.nan(v)] <- NA_real_
    v
  }
  out <- data.frame(month = 1:12,
                    rain = monthly_total_mean(rain),
                    snow = monthly_total_mean(snow))
  out
}

#' Long-term monthly discharge regime (mean QOBS/QSIM per calendar month)
#'
#' Returns a 12-row data frame (month, QOBS, QSIM) of the multi-year mean
#' for each calendar month, used by the map hover preview. -999 / negative
#' observations are treated as NA and ignored per month.
#' @keywords internal
compute_monthly_regime <- function(datetime, obs, sim) {
  obs[obs <= -999] <- NA
  sim[sim <= -999] <- NA
  obs[obs < 0] <- NA
  mon <- as.POSIXlt(datetime)$mon + 1L
  out <- data.frame(month = 1:12, QOBS = NA_real_, QSIM = NA_real_)
  qobs_m <- tapply(obs, mon, mean, na.rm = TRUE)
  qsim_m <- tapply(sim, mon, mean, na.rm = TRUE)
  out$QOBS[as.integer(names(qobs_m))] <- as.numeric(qobs_m)
  out$QSIM[as.integer(names(qsim_m))] <- as.numeric(qsim_m)
  out$QOBS[is.nan(out$QOBS)] <- NA_real_
  out$QSIM[is.nan(out$QSIM)] <- NA_real_
  out
}

#' NSE, KGE and KGE components for the viewer header strip
#'
#' Follows the -999 / negative-Qobs convention of
#' \code{calculate_single_metric()}: values <= -999 and negative observations
#' become NA and incomplete pairs are dropped. Returns NULL when fewer than
#' 10 valid pairs remain ("no observations").
#' @keywords internal
compute_station_metrics <- function(obs, sim) {
  sim[sim <= -999] <- NA
  obs[obs <= -999] <- NA
  valid <- !is.na(sim) & !is.na(obs) & obs >= 0
  n_valid <- sum(valid)
  if (n_valid < 10) return(NULL)
  sim <- sim[valid]
  obs <- obs[valid]

  nse <- tryCatch(hydroGOF::NSE(sim = sim, obs = obs),
                  error = function(e) NA_real_)

  kge <- r <- alpha <- beta <- NA_real_
  kge_full <- tryCatch(hydroGOF::KGE(sim = sim, obs = obs, out.type = "full"),
                       error = function(e) NULL)
  if (!is.null(kge_full)) {
    kge <- as.numeric(kge_full$KGE.value)
    el <- kge_full$KGE.elements
    nm <- tolower(names(el))
    if (any(nm == "r"))     r     <- as.numeric(el[nm == "r"][1])
    if (any(nm == "alpha")) alpha <- as.numeric(el[nm == "alpha"][1])
    if (any(nm == "beta"))  beta  <- as.numeric(el[nm == "beta"][1])
  }
  # Manual fallback should hydroGOF's element naming differ
  if (is.na(r)) r <- suppressWarnings(stats::cor(sim, obs))
  if (is.na(alpha) && stats::sd(obs) > 0) alpha <- stats::sd(sim) / stats::sd(obs)
  if (is.na(beta) && mean(obs) != 0) beta <- mean(sim) / mean(obs)

  list(NSE = nse, KGE = kge, r = r, alpha = alpha, beta = beta, n = n_valid)
}

#' Compute NSE/KGE/r/BETA for every subbasin in the runoff cache
#'
#' Vectorized over the fst cache (read in column chunks to bound memory):
#' one pass at app launch supplies the station-coloring metrics for the map.
#' Same -999 / negative-obs handling as \code{compute_station_metrics()};
#' KGE 2009 components computed directly (r, alpha, beta). The first
#' \code{spinup} rows are dropped before any metric is computed.
#' @keywords internal
compute_all_station_metrics <- function(cache, spinup = 0L, chunk_size = 200L) {
  cols <- cache$runoff$columns
  suffixes <- sub("^QOBS_", "", grep("^QOBS_", cols, value = TRUE))
  suffixes <- suffixes[paste0("QSIM_", suffixes) %in% cols]
  out <- data.frame(suffix = suffixes, NSE = NA_real_, KGE = NA_real_,
                    r = NA_real_, alpha = NA_real_, beta = NA_real_,
                    n = NA_integer_, stringsAsFactors = FALSE)
  if (length(suffixes) == 0) return(out)
  drop <- if (spinup > 0) seq_len(spinup) else integer(0)

  chunks <- split(seq_along(suffixes),
                  ceiling(seq_along(suffixes) / chunk_size))
  for (ii in chunks) {
    df <- fst::read_fst(cache$runoff$path,
                        columns = c(paste0("QOBS_", suffixes[ii]),
                                    paste0("QSIM_", suffixes[ii])))
    if (length(drop) > 0 && length(drop) < nrow(df)) df <- df[-drop, , drop = FALSE]
    for (k in seq_along(ii)) {
      o <- df[[paste0("QOBS_", suffixes[ii][k])]]
      s <- df[[paste0("QSIM_", suffixes[ii][k])]]
      o[o <= -999] <- NA
      s[s <= -999] <- NA
      valid <- !is.na(o) & !is.na(s) & o >= 0
      nv <- sum(valid)
      if (nv < 10) next
      o <- o[valid]
      s <- s[valid]
      mo <- mean(o)
      denom <- sum((o - mo)^2)
      r <- suppressWarnings(stats::cor(s, o))
      alpha <- if (stats::sd(o) > 0) stats::sd(s) / stats::sd(o) else NA_real_
      beta <- if (mo != 0) mean(s) / mo else NA_real_
      out$NSE[ii[k]] <- if (denom > 0) 1 - sum((s - o)^2) / denom else NA_real_
      out$r[ii[k]] <- r
      out$alpha[ii[k]] <- alpha
      out$beta[ii[k]] <- beta
      out$n[ii[k]] <- nv
      out$KGE[ii[k]] <- if (!anyNA(c(r, alpha, beta))) {
        1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
      } else NA_real_
    }
  }
  out
}

#' Write per-subbasin metrics + coordinates to a CSV (for spatial clustering)
#'
#' Load cached per-subbasin metrics, or compute and cache them
#'
#' Wraps \code{compute_all_station_metrics()} with an rds sidecar in the same
#' \code{.cache} directory as the fst files. The full pass has to touch every
#' QOBS_/QSIM_ column (~9 s for 13,605 subbasins) while the result is ~1.4 MB,
#' so it is worth persisting between launches.
#'
#' The cache key is the runoff fst path, its mtime and size, plus \code{spinup}
#' -- so rebuilding the fst cache (\code{clean_cache = TRUE}, or a re-run of
#' COSERO) or asking for a different spin-up recomputes rather than returning
#' stale metrics. A corrupt or unreadable sidecar is silently recomputed.
#' @keywords internal
load_or_compute_station_metrics <- function(cache, spinup = 0L,
                                            cache_dir = NULL,
                                            chunk_size = 200L) {
  rds_path <- if (!is.null(cache_dir)) {
    file.path(cache_dir, "station_metrics.rds")
  } else {
    NULL
  }
  runoff_path <- cache$runoff$path
  key <- list(
    path   = runoff_path,
    mtime  = as.numeric(file.mtime(runoff_path)),
    size   = as.numeric(file.size(runoff_path)),
    spinup = as.integer(spinup)
  )

  if (!is.null(rds_path) && file.exists(rds_path)) {
    hit <- tryCatch(readRDS(rds_path), error = function(e) NULL)
    if (is.list(hit) && identical(hit$key, key) && is.data.frame(hit$metrics)) {
      message("Reusing cached station performance metrics")
      return(hit$metrics)
    }
  }

  message("Computing station performance metrics ...")
  metrics <- compute_all_station_metrics(cache, spinup = spinup,
                                         chunk_size = chunk_size)
  if (!is.null(rds_path)) {
    # A failed write must not break the launch -- the metrics are in hand
    tryCatch(saveRDS(list(key = key, metrics = metrics), rds_path),
             error = function(e) {
               warning("Could not cache station metrics: ",
                       conditionMessage(e), call. = FALSE)
             })
  }
  metrics
}

#' Joins the launch-time metrics (\code{compute_all_station_metrics()}) to one
#' WGS84 coordinate per subbasin: the gauging-station point when available,
#' otherwise the catchment representative point. Written to
#' \code{output/.cache/station_metrics.csv} at every launch. Columns:
#' subbasin, lon, lat, coord_source, NSE, KGE, r, alpha, beta, n, spinup.
#' @param station_metrics Data frame from \code{compute_all_station_metrics()}
#'   (keyed by \code{suffix} = the output-column subbasin id).
#' @param stations,catchments sf layers (WGS84) or NULL.
#' @param id_field,catchment_id_field ID columns in the respective layers.
#' @param cache_dir Directory to write into (the fst cache dir).
#' @param spinup Spin-up timesteps excluded from the metrics (recorded as a col).
#' @return Invisibly, the path to the written CSV (or NULL if nothing to write).
#' @keywords internal
write_station_metrics_csv <- function(station_metrics, stations, id_field,
                                      catchments, catchment_id_field,
                                      cache_dir, spinup = 0L) {
  if (is.null(station_metrics) || nrow(station_metrics) == 0) return(invisible(NULL))

  # Build an id -> (lon, lat) lookup, station first then catchment fallback.
  # Keys are normalized to the numeric subbasin where possible so they match
  # the output-column suffixes regardless of 3/4-digit zero-padding.
  norm_key <- function(x) {
    num <- suppressWarnings(as.numeric(as.character(x)))
    ifelse(is.na(num), as.character(x), as.character(as.integer(num)))
  }
  coord_lookup <- list()  # key -> c(lon, lat, source)
  add_coords <- function(layer, field, source) {
    if (is.null(layer)) return(invisible())
    xy <- suppressWarnings(tryCatch(
      sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(layer))),
      error = function(e) sf::st_coordinates(sf::st_centroid(sf::st_geometry(layer)))
    ))[, 1:2, drop = FALSE]
    keys <- norm_key(layer[[field]])
    for (i in seq_along(keys)) {
      k <- keys[i]
      if (is.null(coord_lookup[[k]])) {
        coord_lookup[[k]] <<- c(xy[i, 1], xy[i, 2], source)
      }
    }
  }
  # catchments first, then stations overwrite where both exist (station wins)
  add_coords(catchments, catchment_id_field, "catchment")
  coord_lookup_catch <- coord_lookup
  coord_lookup <- list()
  add_coords(stations, id_field, "station")
  # merge: station entries take precedence, catchment fills the rest
  for (k in names(coord_lookup_catch)) {
    if (is.null(coord_lookup[[k]])) coord_lookup[[k]] <- coord_lookup_catch[[k]]
  }

  keys <- norm_key(station_metrics$suffix)
  lon <- vapply(keys, function(k) {
    v <- coord_lookup[[k]]; if (is.null(v)) NA_real_ else as.numeric(v[1])
  }, numeric(1))
  lat <- vapply(keys, function(k) {
    v <- coord_lookup[[k]]; if (is.null(v)) NA_real_ else as.numeric(v[2])
  }, numeric(1))
  src <- vapply(keys, function(k) {
    v <- coord_lookup[[k]]; if (is.null(v)) NA_character_ else as.character(v[3])
  }, character(1))

  out <- data.frame(
    subbasin = station_metrics$suffix,
    lon = lon, lat = lat, coord_source = src,
    NSE = station_metrics$NSE, KGE = station_metrics$KGE,
    r = station_metrics$r, alpha = station_metrics$alpha,
    beta = station_metrics$beta, n = station_metrics$n,
    spinup = spinup, stringsAsFactors = FALSE
  )
  csv_path <- file.path(cache_dir, "station_metrics.csv")
  utils::write.csv(out, csv_path, row.names = FALSE)
  invisible(csv_path)
}

#' Performance classes and colors for station coloring
#'
#' Class boundaries: NSE above 0.8 excellent, 0.6-0.8 very good, 0.4-0.6
#' good, 0.2-0.4 poor, 0-0.2 very poor, below 0 unacceptable. KGE (also
#' used for r): above 0.9 excellent, 0.75-0.9 very good, 0.5-0.75 good,
#' 0-0.5 poor, below 0 very poor. NSE/KGE/r use a SEQUENTIAL blue
#' (good) -> red (bad) ramp. BETA uses a 5-class DIVERGING scheme around 1
#' so under- and over-estimation are read directly from the colour:
#' < 0.85 strong under, 0.85-0.95 mild under, 0.95-1.05 balanced
#' (within +-5\%), 1.05-1.15 mild over, > 1.15 strong over. Under = warm
#' ("dry"), over = cool/teal, balanced = vanilla centre (reserved for BETA,
#' not reused in the sequential scales). NA (no observations) is grey.
#' Returns per-station colors plus legend colors/labels.
#' @keywords internal
metric_color_classes <- function(metric, values) {
  na_color <- "#9e9e9e"
  if (metric == "beta") {
    # Diverging around 1.0: under-estimation (sim < obs) -> warm ("dry"),
    # over-estimation (sim > obs) -> cool/teal, balanced -> vanilla centre.
    # Vanilla (#e9d8a6) is reserved as the BETA centre and not reused in
    # the sequential scales below.
    breaks <- c(-Inf, 0.85, 0.95, 1.05, 1.15, Inf)
    labels <- c("Strong under (<0.85)",
                "Mild under (0.85-0.95)",
                "Balanced (0.95-1.05)",
                "Mild over (1.05-1.15)",
                "Strong over (>1.15)")
    pal <- c("#bb3e03", "#ee9b00", "#e9d8a6", "#0a9396", "#005f73")
    cls <- cut(values, breaks = breaks, labels = FALSE)
    title <- "BETA (bias)"
  } else if (metric == "NSE") {
    # Sequential blue (good) -> red (bad); vanilla skipped (BETA centre)
    breaks <- c(Inf, 0.8, 0.6, 0.4, 0.2, 0, -Inf)  # good -> bad
    labels <- c("Excellent (>0.8)", "Very good (0.6-0.8)", "Good (0.4-0.6)",
                "Poor (0.2-0.4)", "Very poor (0-0.2)", "Unacceptable (<0)")
    pal <- c("#005f73", "#0a9396", "#94d2bd", "#ee9b00", "#bb3e03", "#9b2226")
    cls <- cut(values, breaks = rev(breaks), labels = FALSE)
    cls <- length(pal) + 1L - cls  # reorder to good -> bad indexing
    title <- "NSE"
  } else {  # KGE and r share the KGE classes
    breaks <- c(Inf, 0.9, 0.75, 0.5, 0, -Inf)
    labels <- c("Excellent (>0.9)", "Very good (0.75-0.9)",
                "Good (0.5-0.75)", "Poor (0-0.5)", "Very poor (<0)")
    pal <- c("#005f73", "#0a9396", "#94d2bd", "#ee9b00", "#bb3e03")
    cls <- cut(values, breaks = rev(breaks), labels = FALSE)
    cls <- length(pal) + 1L - cls
    title <- if (metric == "r") "r (correlation)" else "KGE"
  }
  point_colors <- pal[cls]
  point_colors[is.na(point_colors)] <- na_color
  list(point_colors = point_colors,
       legend_colors = c(pal, na_color),
       legend_labels = c(labels, "No observations"),
       title = title)
}

# 5 Shiny module #####

#' CSS for the map viewer (near-fullscreen modal, tight margins)
#' @keywords internal
map_viewer_css <- function() {
  "
  html, body { height: 100%; }
  /* width AND max-width: Bootstrap 3 has no .modal-xl class and would
     otherwise keep the default 600px dialog width */
  .modal-xl { width: 98vw; max-width: 98vw; }
  .modal-dialog.modal-xl { margin: 1vh auto; }
  .modal-content { height: 97vh; display: flex; flex-direction: column; }
  /* overflow hidden: dygraphs + the range selector render a few px wider
     than their container, which otherwise triggers scrollbars. The body is
     a flex column so its single .cosero-viewer-panels child fills the height
     (the dialog already caps total height at 97vh via .modal-content). */
  .modal-body { padding: 0.3rem 0.5rem; overflow: hidden;
    box-sizing: border-box; display: flex; flex-direction: column;
    min-height: 0; flex: 1 1 auto; }
  /* Force every dygraph widget (and its sub-elements) to honour the
     container width so nothing spills past the modal edge. */
  .cosero-viewer-panels .dygraphs,
  .cosero-viewer-panels .dygraph-legend,
  .cosero-viewer-panels .dygraph-rangesel-zoomhandle { box-sizing: border-box; }
  .cosero-viewer-panels .dygraphs { max-width: 100% !important; }
  .cosero-viewer-panels .html-widget { max-width: 100% !important; }
  /* Flex column that fills the modal body: plot cells grow to take all the
     remaining height (no empty gap, no overflow), strip/selector rows stay
     at their natural size. */
  .cosero-viewer-panels { display: flex; flex-direction: column;
    height: 100%; min-height: 0; }
  .cosero-viewer-panels .cosero-panel-plot { min-height: 0; overflow: hidden; }
  .cosero-viewer-panels .cosero-panel-plot .dygraphs,
  .cosero-viewer-panels .cosero-panel-plot .html-widget {
    height: 100% !important; }
  .cosero-map-strip { display: flex; flex-wrap: wrap; gap: 1.1rem;
    align-items: baseline; font-size: 1.05rem; padding: 0.1rem 0.4rem 0.3rem; }
  .cosero-map-strip .strip-id { font-weight: 600; font-size: 1.25rem; }
  .cosero-map-strip .strip-metric b { font-weight: 600; }
  .cosero-map-varrow { display: flex; align-items: center; gap: 0.5rem;
    padding: 0.1rem 0.4rem; }
  .cosero-map-varrow .varrow-label { font-size: 0.8rem; white-space: nowrap; }
  .cosero-map-varrow .form-group, .cosero-map-varrow .shiny-input-container {
    margin-bottom: 0.1rem; flex: 1 1 auto; }
  .cosero-map-varrow .selectize-input { min-height: 1.7rem !important;
    padding: 0.15rem 0.5rem !important; font-size: 0.8rem !important; }
  .cosero-map-varrow .selectize-dropdown { font-size: 0.8rem !important; }
  .cosero-map-ctrl .form-group, .cosero-map-ctrl .shiny-input-container {
    margin-bottom: 4px; }
  .cosero-map-ctrl label { font-size: 0.8rem; margin-bottom: 2px; }
  .cosero-map-ctrl .selectize-input { min-height: 1.8rem !important;
    padding: 0.2rem 0.5rem !important; font-size: 0.85rem !important; }
  .cosero-map-ctrl .selectize-dropdown { font-size: 0.85rem !important; }
  .cosero-regime-popup .leaflet-popup-content { margin: 6px 8px; }
  .cosero-regime-popup .leaflet-popup-content-wrapper { border-radius: 6px; }
  "
}

#' Map viewer module UI
#' @keywords internal
map_viewer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$head(shiny::tags$style(shiny::HTML(map_viewer_css()))),
    leaflet::leafletOutput(ns("map"), width = "100%", height = "100%"),
    shiny::absolutePanel(
      top = 10, left = 55, class = "cosero-map-ctrl",
      style = paste0("z-index: 1000; background: rgba(255,255,255,0.92); ",
                     "padding: 4px 10px 0 10px; border-radius: 6px; ",
                     "box-shadow: 0 1px 4px rgba(0,0,0,0.3);"),
      shiny::div(
        style = "display: flex; align-items: flex-end; gap: 8px;",
        shiny::selectInput(ns("color_by"), label = "Colour by metric",
                           choices = c("Single colour" = "none",
                                       "NSE" = "NSE", "KGE" = "KGE",
                                       "r" = "r", "BETA (bias)" = "beta"),
                           selected = "none", width = "180px"),
        # Type-ahead subbasin finder: with thousands of features, scanning the
        # map by eye is hopeless. Choices are filled server-side
        # (updateSelectizeInput, server = TRUE) so the browser never receives
        # the full ID list.
        shiny::selectizeInput(
          ns("goto_id"), label = "Find subbasin", choices = NULL,
          selected = "", width = "150px",
          options = list(placeholder = "type an ID",
                         maxOptions = 100L, allowEmptyOption = TRUE)
        ),
        shiny::actionButton(ns("zoom_full"), label = "Full extent",
                            icon = shiny::icon("expand"),
                            class = "btn-sm", style = "margin-bottom: 4px;")
      )
    )
    # (The hover regime preview is shown as a leaflet popup anchored at the
    # hovered feature -- see regime_popup_img() in map_viewer_server.)
  )
}

#' Map viewer module server
#'
#' @param id Module id
#' @param stations Optional sf point layer (WGS84) with the gauging stations,
#'   or NULL
#' @param id_field Name of the subbasin ID column in \code{stations}
#' @param catchments Optional simplified sf polygon layer (WGS84) or NULL
#' @param catchment_id_field Name of the subbasin ID column in
#'   \code{catchments}
#' @param cache Cache list from \code{build_output_cache()}
#' @param station_metrics Data frame from \code{compute_all_station_metrics()}
#' @param spinup Integer leading model timesteps excluded from the
#'   header-strip metrics (the plotted series stay full-length)
#' @keywords internal
map_viewer_server <- function(id, stations, id_field, catchments,
                              catchment_id_field = NULL, cache,
                              station_metrics = NULL, spinup = 0L) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    session_cache <- new.env(parent = emptyenv())
    has_stations <- !is.null(stations)
    use_gl <- has_stations && requireNamespace("leafgl", quietly = TRUE)

    if (has_stations) {
      coords <- sf::st_coordinates(stations)[, 1:2, drop = FALSE]
      station_ids <- as.character(stations[[id_field]])
    } else {
      coords <- matrix(numeric(0), ncol = 2)
      station_ids <- character(0)
    }

    # Catchment popup anchor points (centroids) and IDs, when catchments are
    # the clickable layer or for the hover popup on polygons
    if (!is.null(catchments)) {
      catch_ids <- as.character(catchments[[catchment_id_field]])
      catch_xy <- suppressWarnings(
        sf::st_coordinates(sf::st_point_on_surface(sf::st_geometry(catchments)))
      )[, 1:2, drop = FALSE]
    } else {
      catch_ids <- character(0)
      catch_xy <- matrix(numeric(0), ncol = 2)
    }

    # Combined map bounds across whichever layers are present
    all_x <- c(coords[, 1], catch_xy[, 1])
    all_y <- c(coords[, 2], catch_xy[, 2])
    map_bounds <- list(lng1 = min(all_x), lat1 = min(all_y),
                       lng2 = max(all_x), lat2 = max(all_y))

    rv <- shiny::reactiveValues(station = NULL,
                                plus_sel = NULL, plus1_sel = NULL)

    # Map an ID vector -> rows in station_metrics (3/4/5-digit format probing)
    metric_rows_for <- function(ids) {
      if (is.null(station_metrics) || nrow(station_metrics) == 0 ||
          length(ids) == 0) {
        return(rep(NA_integer_, length(ids)))
      }
      vapply(ids, function(sb_id) {
        num <- suppressWarnings(as.numeric(sb_id))
        fmts <- if (!is.na(num)) {
          c(sprintf("%05d", num), sprintf("%04d", num),
            sprintf("%03d", num), sprintf("%d", num))
        } else {
          as.character(sb_id)
        }
        hit <- match(fmts, station_metrics$suffix)
        hit <- hit[!is.na(hit)]
        if (length(hit) > 0) hit[1] else NA_integer_
      }, integer(1), USE.NAMES = FALSE)
    }
    metric_rows <- metric_rows_for(station_ids)
    catch_metric_rows <- if (!is.null(catchments)) {
      metric_rows_for(catch_ids)
    } else {
      integer(0)
    }

    # Draw / redraw the catchment polygons, shaded by the selected metric.
    # Mirrors draw_stations(): works on the initial map object and on a
    # leafletProxy (clear = TRUE). When a station layer is present it owns
    # the legend, so polygons only carry one in catchments-only projects.
    draw_catchments <- function(m, metric, clear = FALSE) {
      if (is.null(catchments)) return(m)
      legend <- NULL
      if (metric == "none" || is.null(station_metrics)) {
        fill_colors <- "#bbbbbb"
        fill_opacity <- 0.15
        poly_labels <- catch_ids
      } else {
        vals <- station_metrics[[metric]][catch_metric_rows]
        legend <- metric_color_classes(metric, vals)
        fill_colors <- legend$point_colors
        # Opaque enough to read the metric, still lets the basemap through
        fill_opacity <- 0.75
        poly_labels <- ifelse(
          is.na(vals),
          paste0(catch_ids, " | no observations"),
          sprintf("%s | %s = %.2f", catch_ids, legend$title, vals)
        )
      }
      if (clear) {
        m <- leaflet::clearGroup(m, "Catchments")
        if (!has_stations) m <- leaflet::removeControl(m, "perf_legend")
      }
      m <- leaflet::addPolygons(
        m, data = catchments, layerId = catch_ids,
        weight = 1, color = "#666666",
        fillColor = fill_colors, fillOpacity = fill_opacity,
        group = "Catchments", label = poly_labels,
        highlightOptions = leaflet::highlightOptions(
          weight = 2, color = "#005f73", fillOpacity = 0.30,
          bringToFront = FALSE
        )
      )
      # Stations draw their own legend; avoid adding a second identical one
      if (!is.null(legend) && !has_stations) {
        m <- leaflet::addLegend(
          m, position = "bottomright", colors = legend$legend_colors,
          labels = legend$legend_labels, title = legend$title,
          opacity = 0.9, layerId = "perf_legend"
        )
      }
      m
    }

    # Draw / redraw the stations layer; works on the initial map object and
    # on a leafletProxy (clear = TRUE). No-op when there is no station layer.
    draw_stations <- function(m, metric, clear = FALSE) {
      if (!has_stations) return(m)
      if (metric == "none" || is.null(station_metrics)) {
        point_colors <- rep("#005f73", length(station_ids))
        point_labels <- station_ids
        legend <- NULL
      } else {
        vals <- station_metrics[[metric]][metric_rows]
        legend <- metric_color_classes(metric, vals)
        point_colors <- legend$point_colors
        point_labels <- ifelse(
          is.na(vals),
          paste0(station_ids, " | no observations"),
          sprintf("%s | %s = %.2f", station_ids, legend$title, vals)
        )
      }
      if (clear) {
        if (use_gl) m <- leafgl::clearGlLayers(m)
        else m <- leaflet::clearGroup(m, "Stations")
        m <- leaflet::removeControl(m, "perf_legend")
      }
      if (use_gl) {
        m <- leafgl::addGlPoints(
          m, data = stations, layerId = station_ids,
          fillColor = point_colors, radius = 14, group = "Stations"
        )
      } else {
        m <- leaflet::addCircleMarkers(
          m, lng = coords[, 1], lat = coords[, 2], layerId = station_ids,
          radius = 8, stroke = TRUE, weight = 1, color = "#ffffff",
          fillColor = point_colors, fillOpacity = 0.9,
          label = point_labels, group = "Stations"
        )
      }
      if (!is.null(legend)) {
        m <- leaflet::addLegend(
          m, position = "bottomright", colors = legend$legend_colors,
          labels = legend$legend_labels, title = legend$title,
          opacity = 0.9, layerId = "perf_legend"
        )
      }
      m
    }

    # Map (rendered once; modal never re-renders it, so zoom/pan persist)
    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet(
        options = leaflet::leafletOptions(preferCanvas = TRUE)
      )
      # Selectable base maps (switcher in the top-right layers control).
      # "Light" is added last so it is the one shown on load.
      m <- leaflet::addProviderTiles(m, "OpenStreetMap",
                                     group = "OpenStreetMap")
      m <- leaflet::addProviderTiles(m, "OpenTopoMap",
                                     group = "Topographic")
      m <- leaflet::addProviderTiles(m, "Esri.WorldShadedRelief",
                                     group = "Hillshade")
      m <- leaflet::addProviderTiles(m, "Esri.WorldImagery",
                                     group = "Satellite")
      # Esri.WorldGrayCanvas, not CartoDB.Positron: CARTO moved
      # basemaps.cartocdn.com behind an account, so the Positron tiles now
      # answer anonymous browser requests with an API-key challenge. The Esri
      # light-grey canvas is equivalent cartography and needs no key.
      m <- leaflet::addProviderTiles(m, "Esri.WorldGrayCanvas",
                                     group = "Light")
      overlay_groups <- character(0)
      sel_metric <- shiny::isolate(input$color_by)
      if (is.null(sel_metric)) sel_metric <- "none"
      if (!is.null(catchments)) {
        # Polygons are clickable (layerId = subbasin ID); highlight on hover
        m <- draw_catchments(m, sel_metric)
        overlay_groups <- c(overlay_groups, "Catchments")
      }
      if (has_stations) overlay_groups <- c(overlay_groups, "Stations")
      m <- draw_stations(m, sel_metric)
      m <- leaflet::addLayersControl(
        m,
        baseGroups = c("Light", "OpenStreetMap", "Topographic", "Hillshade",
                       "Satellite"),
        overlayGroups = overlay_groups,
        options = leaflet::layersControlOptions(collapsed = TRUE)
      )
      leaflet::fitBounds(m, map_bounds$lng1, map_bounds$lat1,
                         map_bounds$lng2, map_bounds$lat2)
    })

    # Recolor stations AND catchments when the metric selector changes
    shiny::observeEvent(input$color_by, {
      proxy <- leaflet::leafletProxy("map", session)
      draw_catchments(proxy, input$color_by, clear = TRUE)
      draw_stations(proxy, input$color_by, clear = TRUE)
    }, ignoreInit = TRUE)

    # Zoom to full extent of all layers
    shiny::observeEvent(input$zoom_full, {
      leaflet::flyToBounds(
        leaflet::leafletProxy("map", session),
        map_bounds$lng1, map_bounds$lat1, map_bounds$lng2, map_bounds$lat2
      )
    })

    # --- "Find subbasin": type-ahead ID search that zooms to the feature -----
    # Sorted numerically where the IDs are numbers, so the dropdown reads
    # 1, 2, 10 rather than 1, 10, 2.
    goto_ids <- unique(c(catch_ids, station_ids))
    if (length(goto_ids) > 0) {
      ord <- suppressWarnings(as.numeric(goto_ids))
      goto_ids <- if (anyNA(ord)) sort(goto_ids) else goto_ids[order(ord)]
    }
    shiny::updateSelectizeInput(session, "goto_id",
                                choices = c("", goto_ids), selected = "",
                                server = TRUE)

    shiny::observeEvent(input$goto_id, {
      sb <- input$goto_id
      if (is.null(sb) || !nzchar(sb)) return(invisible(NULL))
      proxy <- leaflet::leafletProxy("map", session)

      # Prefer the polygon: zoom to its real extent so the whole catchment
      # is visible. Fall back to the station point (no extent of its own).
      j <- match(sb, catch_ids)
      if (!is.na(j)) {
        bb <- as.numeric(sf::st_bbox(sf::st_geometry(catchments)[j]))
        # Pad a degenerate bbox (a tiny catchment) so flyToBounds still zooms
        if (bb[3] - bb[1] < 1e-4) bb[c(1, 3)] <- bb[c(1, 3)] + c(-5e-4, 5e-4)
        if (bb[4] - bb[2] < 1e-4) bb[c(2, 4)] <- bb[c(2, 4)] + c(-5e-4, 5e-4)
        leaflet::flyToBounds(proxy, bb[1], bb[2], bb[3], bb[4])
      } else {
        k <- match(sb, station_ids)
        if (is.na(k)) return(invisible(NULL))
        leaflet::flyTo(proxy, lng = coords[k, 1], lat = coords[k, 2], zoom = 11)
      }
    }, ignoreInit = TRUE)

    nearest_station <- function(lat, lng) {
      if (!has_stations) return(NULL)
      d2 <- (coords[, 1] - lng)^2 + (coords[, 2] - lat)^2
      station_ids[which.min(d2)]
    }

    handle_click <- function(sb_id) {
      if (is.null(sb_id) || !nzchar(sb_id)) return()
      d <- load_station_data(cache, sb_id, session_cache, spinup = spinup)
      if (all(vapply(d[c("runoff", "prec", "plus", "plus1")],
                     is.null, logical(1)))) {
        shiny::showModal(shiny::modalDialog(
          title = "No data",
          paste0("No COSERO output columns found for subbasin '", sb_id, "'."),
          easyClose = TRUE, footer = NULL
        ))
        return()
      }
      rv$station <- d
      shiny::showModal(build_viewer_modal(ns, d, rv$plus_sel, rv$plus1_sel,
                                          spinup = spinup))
    }

    shiny::observeEvent(input$map_marker_click, {
      handle_click(input$map_marker_click$id)
    })

    # leafgl click: payload differs across versions -> try id, attribute,
    # then nearest-station lookup by coordinates
    shiny::observeEvent(input$map_glify_click, {
      ev <- input$map_glify_click
      sb_id <- NULL
      if (!is.null(ev$id)) sb_id <- as.character(ev$id)
      if (is.null(sb_id) && !is.null(ev$data) && !is.null(ev$data[[id_field]])) {
        sb_id <- as.character(ev$data[[id_field]])
      }
      if (is.null(sb_id) && !is.null(ev$lat) && !is.null(ev$lng)) {
        sb_id <- nearest_station(ev$lat, ev$lng)
      }
      handle_click(sb_id)
    })

    # Catchment polygon click (layerId = subbasin ID)
    shiny::observeEvent(input$map_shape_click, {
      handle_click(input$map_shape_click$id)
    })

    # --- Hover preview: monthly regime as a popup anchored at the point ----
    # The regime + rain/snow hyetograph is rendered to a PNG and shown in a
    # leaflet popup at the hovered feature, so it tracks the point and pans
    # with the map. Canvas markers and catchment polygons emit mouseover with
    # the layerId; leafgl WebGL points emit map_glify_mouseover -> fall back
    # to nearest-station. The hovered id is debounced (~250 ms dwell).
    #
    # NO mouseout clearing: the popup itself sits over the map, so moving the
    # cursor onto it would fire mouseout on the point and create a show/hide
    # flicker loop. Instead the popup persists until a DIFFERENT feature is
    # hovered (or a click opens the modal). The render observer also ignores
    # repeat hovers of the same id, so no redundant flashing.
    hovered <- shiny::reactiveVal(NULL)  # list(id, lng, lat) or NULL
    hovered_dwell <- shiny::debounce(hovered, 250)
    shown_id <- NULL                     # id currently displayed in the popup

    set_hover <- function(id, lng = NULL, lat = NULL) {
      if (!is.null(id) && nzchar(as.character(id))) {
        hovered(list(id = as.character(id), lng = lng, lat = lat))
      }
    }

    if (has_stations && !use_gl) {
      shiny::observeEvent(input$map_marker_mouseover, {
        ev <- input$map_marker_mouseover
        set_hover(ev$id, ev$lng, ev$lat)
      })
    }
    if (use_gl) {
      shiny::observeEvent(input$map_glify_mouseover, {
        ev <- input$map_glify_mouseover
        id <- if (!is.null(ev$id)) ev$id else nearest_station(ev$lat, ev$lng)
        set_hover(id, ev$lng, ev$lat)
      })
    }
    if (!is.null(catchments)) {
      shiny::observeEvent(input$map_shape_mouseover, {
        ev <- input$map_shape_mouseover
        # anchor the popup at the catchment representative point
        i <- match(as.character(ev$id), catch_ids)
        lng <- if (!is.na(i)) catch_xy[i, 1] else ev$lng
        lat <- if (!is.na(i)) catch_xy[i, 2] else ev$lat
        set_hover(ev$id, lng, lat)
      })
    }

    # Render the popup when the debounced hover settles on a NEW feature
    shiny::observeEvent(hovered_dwell(), {
      h <- hovered_dwell()
      if (is.null(h) || identical(h$id, shown_id)) return()
      if (is.null(h$lng) || is.null(h$lat)) return()
      d <- load_station_data(cache, h$id, session_cache, spinup = spinup)
      if (is.null(d$regime)) return()
      img <- regime_popup_img(d$regime, d$prec_regime, d$subbasin, d$metrics)
      if (is.null(img)) return()
      shown_id <<- h$id
      proxy <- leaflet::leafletProxy("map", session)
      leaflet::clearPopups(proxy)
      leaflet::addPopups(proxy, lng = h$lng, lat = h$lat, popup = img,
                         options = leaflet::popupOptions(
                           closeButton = TRUE, maxWidth = 420,
                           className = "cosero-regime-popup"))
    })

    # When a popup is closed (X button), forget it so re-hovering re-renders
    shiny::observeEvent(input$map_popup_close, { shown_id <<- NULL })
    shiny::observeEvent(input$plus_vars,  { rv$plus_sel  <- input$plus_vars })
    shiny::observeEvent(input$plus1_vars, { rv$plus1_sel <- input$plus1_vars })

    # Panel 1: runoff (QOBS + QSIM)
    output$dy_runoff <- dygraphs::renderDygraph({
      d <- rv$station
      shiny::req(d, d$runoff)
      cols <- intersect(c("QOBS", "QSIM"), names(d$runoff))
      if ("QOBS" %in% cols && all(is.na(d$runoff$QOBS))) {
        cols <- setdiff(cols, "QOBS")
      }
      shiny::req(length(cols) > 0)
      labels <- c(QOBS = "Qobs", QSIM = "Qsim")[cols]
      colors <- c(QOBS = COLORS_DISCHARGE$Q_obs,
                  QSIM = COLORS_DISCHARGE$Q_sim)[cols]
      make_panel_dygraph(d$runoff, cols, labels = unname(labels),
                         colors = unname(colors),
                         ylab = "Q (m\u00b3/s)")
    })

    # Panel 2: precipitation
    output$dy_prec <- dygraphs::renderDygraph({
      d <- rv$station
      shiny::req(d, d$prec)
      cols <- names(d$prec)[names(d$prec) != "DateTime"]
      shiny::req(length(cols) > 0)
      labels <- cols
      labels[labels == "PRAINGEB"] <- "Rain"
      labels[labels == "PSNOWGEB"] <- "Snow"
      colors <- rep("#888888", length(cols))
      colors[cols == "PRAINGEB"] <- COLORS_PRECIPITATION$PRAIN
      colors[cols == "PSNOWGEB"] <- COLORS_PRECIPITATION$PSNOW
      # Stacked: rain (blue) + snow (black); stack height = total precip
      make_panel_dygraph(d$prec, cols, labels = labels, colors = colors,
                         step = TRUE, stacked = TRUE, ylab = "P (mm)")
    })

    # Panel 3: fluxes (COSERO.plus), user-selected variables
    output$dy_plus <- dygraphs::renderDygraph({
      d <- rv$station
      shiny::req(d, d$plus)
      vars <- intersect(input$plus_vars, names(d$plus))
      shiny::req(length(vars) > 0)
      make_panel_dygraph(d$plus, vars)
    })

    # Panel 4: states / water balance (COSERO.plus1), with range selector
    output$dy_plus1 <- dygraphs::renderDygraph({
      d <- rv$station
      shiny::req(d, d$plus1)
      vars <- intersect(input$plus1_vars, names(d$plus1))
      shiny::req(length(vars) > 0)
      make_panel_dygraph(d$plus1, vars, range_selector = TRUE)
    })
  })
}

#' Build the near-fullscreen viewer modal for one station
#' @keywords internal
build_viewer_modal <- function(ns, d, plus_sel = NULL, plus1_sel = NULL,
                               spinup = 0L) {
  plus_vars  <- if (!is.null(d$plus))  setdiff(names(d$plus),  "DateTime") else character(0)
  plus1_vars <- if (!is.null(d$plus1)) setdiff(names(d$plus1), "DateTime") else character(0)

  sel_plus <- intersect(plus_sel, plus_vars)
  if (length(sel_plus) == 0) {
    sel_plus <- intersect("QAB123GEB", plus_vars)
    if (length(sel_plus) == 0 && length(plus_vars) > 0) sel_plus <- plus_vars[1]
  }
  sel_plus1 <- intersect(plus1_sel, plus1_vars)
  if (length(sel_plus1) == 0) {
    sel_plus1 <- intersect(c("SWWGEB", "BW0GEB"), plus1_vars)[1]
    if (is.na(sel_plus1) && length(plus1_vars) > 0) sel_plus1 <- plus1_vars[1]
    sel_plus1 <- sel_plus1[!is.na(sel_plus1)]
  }

  # Each plotted panel is a flex cell that grows to fill the modal height.
  # The `grow` weights set the relative panel heights (top Q tallest); the
  # dygraph fills 100% of its cell, so the column always fits the window with
  # no empty gap and no overflow, on any screen height.
  panels <- list(metric_strip(d, spinup = spinup))

  plot_cell <- function(output_id, grow) {
    shiny::div(
      class = "cosero-panel-plot", style = sprintf("flex: %d 1 0;", grow),
      dygraphs::dygraphOutput(ns(output_id), width = "100%", height = "100%")
    )
  }
  selector_row <- function(label, input_id, choices, selected) {
    shiny::div(
      class = "cosero-map-varrow",
      shiny::span(label, class = "varrow-label"),
      shiny::selectizeInput(ns(input_id), label = NULL, choices = choices,
                            selected = selected, multiple = TRUE, width = "100%",
                            options = list(plugins = list("remove_button")))
    )
  }

  if (!is.null(d$runoff)) {
    panels <- c(panels, list(plot_cell("dy_runoff", 24)))
  }
  if (!is.null(d$prec)) {
    panels <- c(panels, list(plot_cell("dy_prec", 15)))
  }
  if (length(plus_vars) > 0) {
    panels <- c(panels, list(
      selector_row("Fluxes (COSERO.plus):", "plus_vars", plus_vars, sel_plus),
      plot_cell("dy_plus", 18)
    ))
  }
  if (length(plus1_vars) > 0) {
    panels <- c(panels, list(
      selector_row("States / water balance (COSERO.plus1):", "plus1_vars",
                   plus1_vars, sel_plus1),
      plot_cell("dy_plus1", 21)
    ))
  }

  shiny::modalDialog(
    shiny::div(class = "cosero-viewer-panels", panels),
    size = "xl", easyClose = TRUE, footer = NULL
  )
}

#' One-line header strip with subbasin ID and performance metrics
#'
#' The metrics are computed with the first \code{spinup} timesteps excluded;
#' this is shown in the strip so users know the OFs are not over the full
#' plotted period.
#' @keywords internal
metric_strip <- function(d, spinup = 0L) {
  fmt <- function(x) {
    if (is.null(x) || is.na(x)) "\u2013" else sprintf("%.3f", x)
  }
  spin_note <- if (spinup > 0) {
    paste0(", spin-up ", spinup, " excl.")
  } else {
    ""
  }
  m <- d$metrics
  if (is.null(m)) {
    metric_tags <- shiny::span(class = "strip-metric text-muted",
                               "no observations")
  } else {
    metric_tags <- shiny::tagList(
      shiny::span(class = "strip-metric",
                  shiny::HTML(paste0("NSE <b>", fmt(m$NSE), "</b>"))),
      shiny::span(class = "strip-metric",
                  shiny::HTML(paste0("KGE <b>", fmt(m$KGE), "</b>"))),
      shiny::span(class = "strip-metric",
                  shiny::HTML(paste0("r <b>", fmt(m$r), "</b>"))),
      shiny::span(class = "strip-metric",
                  shiny::HTML(paste0("\u03b1 <b>", fmt(m$alpha), "</b>"))),
      shiny::span(class = "strip-metric",
                  shiny::HTML(paste0("\u03b2 <b>", fmt(m$beta), "</b>"))),
      shiny::span(class = "strip-metric text-muted",
                  paste0("(n = ", m$n, spin_note, ")"))
    )
  }
  shiny::div(
    class = "cosero-map-strip",
    shiny::span(class = "strip-id", paste("Subbasin", d$subbasin)),
    metric_tags
  )
}

#' Compact base-R plot of the long-term monthly discharge regime
#'
#' Observed (blue) vs simulated (red) mean discharge by calendar month, with
#' dashed horizontal lines at the annual mean of each, and an inverted
#' rain/snow stacked hyetograph drawn down from the top axis. For the map
#' hover popup. Returns invisibly; called for its plotting side effect.
#' @param regime Data frame (month, QOBS, QSIM) from compute_monthly_regime()
#' @param prec_regime Optional data frame (month, rain, snow) from
#'   compute_monthly_prec_regime(); NULL skips the hyetograph
#' @keywords internal
plot_monthly_regime <- function(regime, prec_regime = NULL) {
  op <- graphics::par(mar = c(1.9, 2.7, 0.4, 2.7), mgp = c(1.5, 0.4, 0),
                      cex = 0.7, cex.axis = 0.75, cex.lab = 0.8, tcl = -0.2)
  on.exit(graphics::par(op))
  month_lab <- c("J","F","M","A","M","J","J","A","S","O","N","D")

  qmax <- max(c(regime$QOBS, regime$QSIM), na.rm = TRUE)
  if (!is.finite(qmax) || qmax <= 0) qmax <- 1
  # Leave a slim band at the top for the inverted hyetograph
  has_prec <- !is.null(prec_regime) &&
    any(is.finite(c(prec_regime$rain, prec_regime$snow)))
  ytop <- if (has_prec) qmax * 1.45 else qmax * 1.05

  graphics::plot(regime$month, regime$QOBS, type = "n",
                 xlim = c(0.5, 12.5), ylim = c(0, ytop),
                 xlab = "", ylab = expression("Q (m"^3*"/s)"),
                 xaxt = "n", yaxt = "n", bty = "l")
  graphics::axis(1, at = 1:12, labels = month_lab)
  # Only label the discharge part of the y-axis (lower part)
  q_ticks <- pretty(c(0, qmax), n = 4)
  q_ticks <- q_ticks[q_ticks <= qmax * 1.05]
  graphics::axis(2, at = q_ticks)

  # Inverted rain/snow stacked hyetograph from the top axis downwards
  if (has_prec) {
    pmax <- max(prec_regime$rain + prec_regime$snow, na.rm = TRUE)
    if (!is.finite(pmax) || pmax <= 0) pmax <- 1
    band <- ytop - qmax * 1.08        # slim vertical space for the hyetograph
    scale <- band / pmax
    rain <- ifelse(is.finite(prec_regime$rain), prec_regime$rain, 0)
    snow <- ifelse(is.finite(prec_regime$snow), prec_regime$snow, 0)
    w <- 0.38
    for (mo in 1:12) {
      # snow sits at the very top, rain hangs below it (looking down)
      graphics::rect(mo - w, ytop - snow[mo] * scale, mo + w, ytop,
                     col = COLORS_PRECIPITATION$PSNOW, border = NA)
      graphics::rect(mo - w, ytop - (snow[mo] + rain[mo]) * scale,
                     mo + w, ytop - snow[mo] * scale,
                     col = COLORS_PRECIPITATION$PRAIN, border = NA)
    }
    graphics::axis(4, at = c(ytop, ytop - band),
                   labels = c("0", sprintf("%.0f", pmax)),
                   col = "#777777", col.axis = "#777777")
    graphics::mtext("P (mm/month)", side = 4, line = 1.5, cex = 0.65,
                    col = "#777777")
  }

  # Dashed annual-mean reference lines
  mobs <- mean(regime$QOBS, na.rm = TRUE)
  msim <- mean(regime$QSIM, na.rm = TRUE)
  if (is.finite(mobs)) graphics::abline(h = mobs, col = COLORS_DISCHARGE$Q_obs,
                                        lty = 2, lwd = 1)
  if (is.finite(msim)) graphics::abline(h = msim, col = COLORS_DISCHARGE$Q_sim,
                                        lty = 2, lwd = 1)

  graphics::lines(regime$month, regime$QOBS, col = COLORS_DISCHARGE$Q_obs, lwd = 2)
  graphics::lines(regime$month, regime$QSIM, col = COLORS_DISCHARGE$Q_sim, lwd = 2)
  graphics::points(regime$month, regime$QOBS, col = COLORS_DISCHARGE$Q_obs, pch = 16, cex = 0.7)
  graphics::points(regime$month, regime$QSIM, col = COLORS_DISCHARGE$Q_sim, pch = 16, cex = 0.7)
  graphics::legend("bottomright", legend = c("Qobs", "Qsim"),
                   col = c(COLORS_DISCHARGE$Q_obs, COLORS_DISCHARGE$Q_sim),
                   lwd = 2, bty = "n", cex = 0.85, seg.len = 1.2)
  invisible(NULL)
}

#' Render the hover regime to a self-contained HTML popup (base64 PNG)
#'
#' Draws \code{plot_monthly_regime()} to a 400x230 temporary PNG,
#' base64-encodes it into an <img> tag, and prepends a one-line title with
#' the subbasin ID and (when available) NSE/KGE. Returns an HTML string for
#' leaflet::addPopups(), or NULL on failure.
#' @keywords internal
regime_popup_img <- function(regime, prec_regime, subbasin, metrics = NULL) {
  if (is.null(regime)) return(NULL)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp, width = 400, height = 230, res = 100, bg = "white")
  ok <- tryCatch({
    plot_monthly_regime(regime, prec_regime)
    TRUE
  }, error = function(e) FALSE)
  grDevices::dev.off()  # always flush/close the device
  if (!ok || !file.exists(tmp) || file.size(tmp) == 0) return(NULL)

  b64 <- base64enc::base64encode(tmp)
  title <- paste0("Subbasin ", subbasin)
  if (!is.null(metrics)) {
    title <- paste0(title,
                    " \u2013 NSE ", sprintf("%.2f", metrics$NSE),
                    ", KGE ", sprintf("%.2f", metrics$KGE),
                    ", BETA ", sprintf("%.2f", metrics$beta))
  }
  paste0(
    "<div style='font-size:0.8rem;font-weight:600;margin-bottom:2px;'>",
    title, "</div>",
    "<img src='data:image/png;base64,", b64,
    "' width='400' height='230' style='display:block;'/>"
  )
}

#' Build one linked dygraph panel
#'
#' All panels share \code{group = "cosero_map_viewer"} so x-zoom/pan is
#' synchronized client-side (no Shiny round-trip).
#' @keywords internal
make_panel_dygraph <- function(df, cols, labels = NULL, colors = NULL,
                               step = FALSE, fill = FALSE, stacked = FALSE,
                               ylab = NULL, range_selector = FALSE) {
  dt <- df$DateTime
  # fst-roundtripped POSIXct loses its tzone attribute; without it
  # dyOptions(useDataTimezone = TRUE) warns and falls back to UTC
  if (is.null(attr(dt, "tzone")) || !nzchar(attr(dt, "tzone")[1])) {
    attr(dt, "tzone") <- Sys.timezone()
  }
  x <- xts::xts(as.matrix(df[, cols, drop = FALSE]), order.by = dt)
  if (!is.null(labels)) colnames(x) <- labels
  g <- dygraphs::dygraph(x, group = "cosero_map_viewer")
  g <- dygraphs::dyOptions(g, colors = colors, stepPlot = step,
                           fillGraph = fill, stackedGraph = stacked,
                           useDataTimezone = TRUE,
                           retainDateWindow = TRUE)
  g <- dygraphs::dyAxis(g, "x", axisLabelFontSize = 10)
  g <- dygraphs::dyAxis(g, "y", label = ylab, axisLabelFontSize = 11)
  if (range_selector) g <- dygraphs::dyRangeSelector(g, height = 25)
  g <- dygraphs::dyLegend(g, show = "onmouseover", width = 420)
  g
}
