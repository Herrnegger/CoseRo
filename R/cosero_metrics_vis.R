# =============================================================================
# Simple metrics visualisation -- quick look at per-subbasin model performance
# =============================================================================
# A deliberately small companion to launch_cosero_map(): that viewer reads the
# full output set and offers hover regimes, four-panel time series and cache
# management. This one reads only output/statistics.txt and draws one map, to
# answer one question -- where are the badly performing catchments?
# =============================================================================

#' Map Per-Subbasin Model Performance
#'
#' Shiny map of the per-subbasin objective functions COSERO writes to
#' \code{output/statistics.txt}. Pick BETA, NSE or KGE and a range, and only
#' the catchments in that range are drawn, so the problem basins stand out
#' spatially.
#'
#' This is the lightweight counterpart to \code{\link{launch_cosero_map}}: it
#' reads one small text file rather than the full COSERO output, so it opens
#' in seconds even on projects with tens of GB of output, and needs no cache.
#'
#' @param cosero_path Character. COSERO project directory (the function appends
#'   \code{output/statistics.txt} itself), or the full path to a
#'   \code{statistics.txt}. Written by COSERO on every run, so no prior call to
#'   \code{\link{launch_cosero_map}} is needed.
#' @param catchments_shp Catchment polygons, as a file path or an \code{sf}
#'   object.
#' @param catchment_id_field Character (default \code{"NB_new"}). The column of
#'   \code{catchments_shp} holding subbasin IDs matching the \code{sb} column
#'   of \code{statistics.txt}.
#' @param simplify_tolerance Numeric (default 500). Tolerance in the layer's own
#'   CRS units passed to \code{sf::st_simplify()} before reprojecting to WGS84.
#'   Use 0 to skip simplification. See Details.
#'
#' @details
#' \strong{Source.} Values are read via \code{\link{read_cosero_statistics}},
#' which nulls the spurious metrics COSERO emits for ungauged subbasins
#' (NSE = NaN, KGE = 1.0), so those appear as NA rather than perfect scores.
#'
#' \strong{Range semantics differ by metric.} BETA = mean(Qsim) / mean(Qobs) is
#' a ratio centred on 1, so it selects catchments \emph{outside}
#' \code{[lo, hi]} -- bias in either direction, with the well-behaved middle
#' hidden. NSE and KGE are one-sided (1 = perfect), so they select catchments
#' \emph{inside} \code{[from, to)}; excluding a middle band there would hide
#' the good and the terrible together.
#'
#' \strong{Medians.} A line under the controls reports the median NSE, KGE and
#' BETA twice: over every subbasin with a value, and over the current
#' selection, each with the number of non-NA values behind it. The median
#' rather than the mean because NSE and KGE are unbounded below, so a few
#' failed subbasins move a mean arbitrarily far.
#'
#' \strong{Colours} come from \code{metric_color_classes()}, the same helper
#' \code{\link{launch_cosero_map}} uses, so a catchment has the same colour
#' here as in the full viewer: teal = good through red = bad for NSE and KGE,
#' diverging around 1 for BETA.
#'
#' \strong{Simplification.} Polygons are simplified with \code{sf::st_simplify()}
#' (GEOS, pure C++) \emph{before} reprojecting, so \code{simplify_tolerance} is
#' in the layer's own units -- metres for a projected CRS such as EPSG:3035.
#' \code{rmapshaper::ms_simplify()} is deliberately not used: it runs the
#' geometry through a V8 JavaScript engine with a hard ~1.4 GB heap cap and
#' crashes the R process on large layers (13,605 polygons / 205 MB is enough),
#' regardless of available RAM.
#'
#' \strong{Required packages}: \code{sf}, \code{leaflet} and \code{shiny}.
#'
#' @return A \code{shiny.appobj}; called for its side effect of launching the
#'   app.
#'
#' @seealso \code{\link{launch_cosero_map}} for the full viewer, and
#'   \code{\link{read_cosero_statistics}} for the reader used here.
#'
#' @export
#' @examples
#' \dontrun{
#' simple_metrics_vis(
#'   cosero_path        = "D:/KLIRES/working files/Model",
#'   catchments_shp     = "D:/KLIRES/working files/Merging/Final/merged_3035_final.shp",
#'   catchment_id_field = "NB_new"
#' )
#'
#' # Coarser polygons for a faster first look
#' simple_metrics_vis(
#'   cosero_path        = "D:/KLIRES/working files/Model",
#'   catchments_shp     = "D:/gis/catchments.shp",
#'   simplify_tolerance = 1000
#' )
#' }
simple_metrics_vis <- function(cosero_path,
                               catchments_shp,
                               catchment_id_field = "NB_new",
                               simplify_tolerance = 500) {

  stats_file <- resolve_statistics_file(cosero_path)
  m <- read_statistics_metrics(stats_file)
  message(sprintf("Subbasins in statistics.txt: %d", nrow(m)))

  check_metrics_vis_packages()
  poly <- prepare_metrics_polygons(catchments_shp, catchment_id_field, m,
                                   simplify_tolerance)
  build_metrics_map_app(poly)
}


# 1 Input helpers #####

#' Resolve a project path or file path to statistics.txt
#' @keywords internal
resolve_statistics_file <- function(cosero_path) {
  if (!is.character(cosero_path) || length(cosero_path) != 1) {
    stop("'cosero_path' must be a single path", call. = FALSE)
  }
  # Accept either the project directory or the statistics file itself.
  f <- if (grepl("\\.txt$", cosero_path, ignore.case = TRUE)) {
    cosero_path
  } else {
    file.path(cosero_path, "output", "statistics.txt")
  }
  if (!file.exists(f)) {
    stop("statistics.txt not found:\n  ", f,
         "\nCOSERO writes it on every run - check the project path, or that ",
         "the run completed.", call. = FALSE)
  }
  f
}

#' Read statistics.txt and normalise it to subbasin / NSE / KGE / beta
#' @keywords internal
read_statistics_metrics <- function(stats_file) {
  st <- read_cosero_statistics(stats_file, quiet = TRUE)

  # The column set follows the file header and varies with the COSERO build,
  # so require only what the map actually needs and name what is absent.
  needed <- c("NSE", "KGE", "BETA")
  missing_cols <- setdiff(needed, colnames(st))
  if (length(missing_cols)) {
    stop("statistics.txt has no ", paste(missing_cols, collapse = " / "),
         " column(s). Found: ", paste(colnames(st), collapse = ", "),
         "\n  ", stats_file, call. = FALSE)
  }

  # Subbasin ids are zero-padded strings in this file ("00001"); the map joins
  # them to a numeric shapefile column, so carry a numeric copy.
  m <- data.frame(
    subbasin = suppressWarnings(as.numeric(st$sb)),
    NSE      = st$NSE,
    KGE      = st$KGE,
    beta     = st$BETA,
    stringsAsFactors = FALSE
  )
  # n is the sample size the viewer's CSV carries; statistics.txt has no such
  # column in every build, so take it when present and leave NA otherwise.
  m$n <- if ("n" %in% colnames(st)) st$n else NA_integer_

  if (anyNA(m$subbasin)) {
    stop("Could not read subbasin ids as numbers from the 'sb' column of\n  ",
         stats_file, call. = FALSE)
  }
  m
}

#' Check that suggested packages for the metrics map are installed
#' @keywords internal
check_metrics_vis_packages <- function() {
  required <- c("sf", "leaflet", "shiny")
  missing_pkgs <- required[!vapply(required, requireNamespace, logical(1),
                                   quietly = TRUE)]
  if (length(missing_pkgs) > 0) {
    stop("simple_metrics_vis() requires additional packages. Install with:\n",
         "  install.packages(c(",
         paste0('"', missing_pkgs, '"', collapse = ", "), "))",
         call. = FALSE)
  }
  invisible(TRUE)
}


# 2 Map #####

#' Read, simplify and join the catchment polygons
#' @keywords internal
prepare_metrics_polygons <- function(catchments_shp, catchment_id_field, m,
                                     simplify_tolerance) {
  if (inherits(catchments_shp, "sf")) {
    poly <- catchments_shp
  } else {
    if (!file.exists(catchments_shp)) {
      stop("Catchment layer not found:\n  ", catchments_shp, call. = FALSE)
    }
    message("Reading and simplifying catchments ...")
    poly <- sf::st_read(catchments_shp, quiet = TRUE)
  }

  if (!catchment_id_field %in% colnames(poly)) {
    stop("Column '", catchment_id_field, "' not found in the catchment layer. ",
         "Available: ", paste(utils::head(colnames(poly), 20), collapse = ", "),
         call. = FALSE)
  }

  # Simplify BEFORE reprojecting, while the layer is still in its own CRS, so
  # the tolerance is a true distance (metres for EPSG:3035). st_simplify is
  # GEOS/C++ with no heap cap -- see the note in the function docs on why
  # rmapshaper::ms_simplify() is avoided here.
  if (is.numeric(simplify_tolerance) && simplify_tolerance > 0) {
    poly <- sf::st_simplify(poly, dTolerance = simplify_tolerance,
                            preserveTopology = TRUE)
    message("Simplified: ", format(utils::object.size(poly), units = "MB"))
  }
  poly <- sf::st_transform(poly, 4326)

  poly <- merge(poly[, catchment_id_field],
                m[, c("subbasin", "beta", "NSE", "KGE", "n")],
                by.x = catchment_id_field, by.y = "subbasin", all.x = TRUE)
  # Keep a subbasin if ANY of the three metrics is available. A row can carry
  # NSE/KGE but no usable beta, so dropping on beta alone would hide it.
  poly <- poly[!(is.na(poly$beta) & is.na(poly$NSE) & is.na(poly$KGE)), ]
  if (!nrow(poly)) {
    stop("No catchment matched a subbasin in statistics.txt. Check that '",
         catchment_id_field, "' holds the COSERO subbasin numbering.",
         call. = FALSE)
  }
  message(sprintf("Catchments with at least one metric: %d", nrow(poly)))
  attr(poly, "id_field") <- catchment_id_field
  poly
}

#' Build the Shiny app mapping the problem catchments
#' @keywords internal
build_metrics_map_app <- function(poly) {

  id_field <- attr(poly, "id_field")

  ui <- shiny::fluidPage(
    shiny::tags$h4("Problem subbasins - by BETA, NSE or KGE"),
    shiny::fluidRow(
      shiny::column(2,
                    shiny::selectInput("metric", "Metric",
                                       choices = c("BETA", "NSE", "KGE"),
                                       selected = "BETA")),
      # Both cases take two bounds, but they mean opposite things: BETA shows
      # what falls OUTSIDE [lo, hi] (bias in either direction), NSE/KGE show
      # what falls INSIDE [from, to) (a band of the skill range).
      shiny::column(4,
                    shiny::conditionalPanel(
                      "input.metric == 'BETA'",
                      shiny::fluidRow(
                        shiny::column(6,
                                      shiny::numericInput("lo", "Under, BETA <",
                                                          value = 0.5, min = 0,
                                                          max = 1, step = 0.05)),
                        shiny::column(6,
                                      shiny::numericInput("hi", "Over, BETA >",
                                                          value = 2, min = 1,
                                                          max = 100, step = 0.5)))),
                    shiny::conditionalPanel(
                      "input.metric != 'BETA'",
                      shiny::fluidRow(
                        shiny::column(6,
                                      shiny::numericInput("from", "From (>=)",
                                                          value = -1, min = -100,
                                                          max = 1, step = 0.05)),
                        shiny::column(6,
                                      shiny::numericInput("to", "To (<)",
                                                          value = 0.4, min = -100,
                                                          max = 1, step = 0.05))))),
      shiny::column(6, shiny::br(), shiny::textOutput("info"))
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::textOutput("medians"))
    ),
    leaflet::leafletOutput("map", height = "78vh")
  )

  server <- function(input, output, session) {

    sel <- shiny::reactive({
      if (input$metric == "BETA") {
        lo <- input$lo; hi <- input$hi
        shiny::req(is.finite(lo), is.finite(hi), lo < hi)
        d <- poly[!is.na(poly$beta) & (poly$beta < lo | poly$beta > hi), ]
        d$dir <- ifelse(d$beta < lo, "under", "over")
      } else {
        from <- input$from; to <- input$to
        shiny::req(is.finite(from), is.finite(to), from < to)
        v <- if (input$metric == "NSE") poly$NSE else poly$KGE
        # Band, not a one-sided cut: keeps what lies within [from, to).
        d <- poly[!is.na(v) & v >= from & v < to, ]
        dv <- if (input$metric == "NSE") d$NSE else d$KGE
        # Split at 0: below it the model is worse than the observed mean.
        d$dir <- ifelse(dv < 0, "unusable", "weak")
      }
      d
    })

    # Graded colours, shared with launch_cosero_map() so a catchment looks the
    # same in both. metric_color_classes() takes the viewer's metric names:
    # lowercase "beta", uppercase "NSE"/"KGE".
    shading <- shiny::reactive({
      d <- sel()
      v <- switch(input$metric,
                  BETA = d$beta, NSE = d$NSE, KGE = d$KGE)
      key <- if (input$metric == "BETA") "beta" else input$metric
      cc <- metric_color_classes(key, v)
      # Only the classes actually present are worth a legend row -- a narrow
      # band would otherwise carry five entries for one visible colour.
      present <- cc$legend_colors %in% unique(cc$point_colors)
      cc$legend_colors <- cc$legend_colors[present]
      cc$legend_labels <- cc$legend_labels[present]
      cc
    })

    output$info <- shiny::renderText({
      d <- sel()
      v <- switch(input$metric,
                  BETA = poly$beta, NSE = poly$NSE, KGE = poly$KGE)
      pool <- sum(!is.na(v))
      # BETA selects outside the range, NSE/KGE inside it, so the wording for
      # what is hidden has to follow the metric.
      if (input$metric == "BETA") {
        sprintf("%d of %d subbasins shown  (%d under, %d over)  -- %d within range are hidden",
                nrow(d), pool, sum(d$dir == "under"), sum(d$dir == "over"),
                pool - nrow(d))
      } else {
        sprintf("%d of %d subbasins shown  (%d below 0, %d from 0 up)  -- %d outside the band are hidden",
                nrow(d), pool, sum(d$dir == "unusable"), sum(d$dir == "weak"),
                pool - nrow(d))
      }
    })

    # Median, not mean: NSE and KGE are unbounded below, so a handful of
    # catastrophic subbasins (NSE in the hundreds negative is routine on an
    # uncalibrated run) drags a mean to a number that describes nothing.
    # Reported over all subbasins and over the current selection, since the
    # filter is normally set to isolate the problem cases.
    # n is per metric, not nrow(d): a subbasin can have a KGE but no NSE, so
    # one count over the row set would misstate what each median rests on.
    med_line <- function(d, label) {
      f <- function(x) {
        x <- x[!is.na(x)]
        if (!length(x)) "NA" else sprintf("%.2f (n=%d)", stats::median(x),
                                          length(x))
      }
      sprintf("%s: NSE %s | KGE %s | BETA %s",
              label, f(d$NSE), f(d$KGE), f(d$beta))
    }

    output$medians <- shiny::renderText({
      paste("Medians --", med_line(poly, "all"),
            "  ||  ", med_line(sel(), "shown"))
    })

    # Base map drawn once; only the polygon layer is redrawn on a change.
    output$map <- leaflet::renderLeaflet({
      bb <- as.numeric(sf::st_bbox(poly))
      leaflet::fitBounds(
        leaflet::addProviderTiles(
          leaflet::leaflet(options = leaflet::leafletOptions(preferCanvas = TRUE)),
          "Esri.WorldGrayCanvas"),   # CartoDB.Positron now demands an API key
        bb[1], bb[2], bb[3], bb[4]
      )
    })

    shiny::observe({
      d <- sel(); cc <- shading()
      m2 <- leaflet::clearGroup(leaflet::leafletProxy("map", session), "sel")
      m2 <- leaflet::removeControl(m2, "lg")
      if (nrow(d) == 0) return(invisible(NULL))
      # All three metrics go in the hover label whichever one is being mapped,
      # so a flagged catchment can be judged on the others without switching.
      fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.2f", x))
      leaflet::addLegend(
        leaflet::addPolygons(
          m2, data = d, weight = 0.5, color = "#444444",
          fillColor = cc$point_colors, fillOpacity = 0.8, group = "sel",
          label = sprintf("Subbasin %s | BETA = %s | NSE = %s | KGE = %s",
                          as.character(d[[id_field]]), fmt(d$beta), fmt(d$NSE),
                          fmt(d$KGE)),
          highlightOptions = leaflet::highlightOptions(
            weight = 2, color = "#000000", bringToFront = TRUE)
        ),
        position = "bottomright", layerId = "lg",
        colors = cc$legend_colors, labels = cc$legend_labels,
        title = cc$title, opacity = 0.9
      )
    })
  }

  shiny::shinyApp(ui, server)
}
