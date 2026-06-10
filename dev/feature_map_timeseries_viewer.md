# Feature: interactive map + linked time-series viewer (`launch_cosero_map()`)

New exported function that shows COSERO subbasins and gauging stations on a
map; clicking a station opens a near-fullscreen viewer with four linked
time-series panels (.runoff, .prec, .plus, .plus1) plus NSE/KGE metrics.
Speed and responsiveness are the top priorities (up to ~10,000 stations,
decades of daily data).

## Where

- New file `R/cosero_map.R`: exported `launch_cosero_map()` plus internal
  helpers. Implement as a self-contained `shiny::shinyApp()` but structure
  UI/server as a module (`map_viewer_ui()` / `map_viewer_server()`) so it can
  later become a tab in `inst/shiny-app/app.R`.
- Reuse existing readers from `R/cosero_readers.R`: `read_runoff()`,
  `read_precipitation()`, `read_plus()`, `read_plus1()` (and their underlying
  parsers) — see section "Data backend" for how they are wrapped.
- Metrics: `hydroGOF::KGE(sim, obs, out.type = "full")` for KGE + components
  (r, alpha/variability, beta/bias), `hydroGOF::NSE()`. Follow the -999 /
  negative-Qobs handling already used in `calculate_single_metric()`
  (`R/cosero_optimize.R:300-307`): values <= -999 and negative obs are NA and
  dropped pairwise. Borrow value-box/metric patterns from
  `inst/shiny-app/R/mod_timeseries.R`.

## Signature

```r
launch_cosero_map(
  stations_shp,                  # path to point shapefile OR sf object
  catchments_shp = NULL,         # path/sf of intermediate-catchment polygons; NULL = skip layer
  cosero_path,                   # COSERO project path (reads <cosero_path>/output/)
  subbasin_id_field = NULL,      # ID column; NULL = auto-detect NB/SB/basin/subbasin (case-insensitive)
  catchment_simplify_pct = 5     # % of polygon vertices kept for display; 100 = no simplification
)
```

Both `*_shp` args accept a file path (`sf::st_read()`) or an existing `sf`
object. `subbasin_id_field` is auto-detected on the **stations** layer (and
independently on catchments if present); error with the list of available
columns if no candidate matches. Transform both layers to WGS84
(`sf::st_transform(4326)`) for the map.

## Map layer

- `leaflet` with `leafletOptions(preferCanvas = TRUE)`.
- Stations: if `leafgl` is installed use `leafgl::addGlPoints()` (WebGL,
  handles 10k+ trivially); else fall back to canvas `addCircleMarkers()`.
  Attach the subbasin ID as `layerId`/group data so the click event returns it.
- Catchments (optional layer): simplify once at startup with
  `rmapshaper::ms_simplify(keep = catchment_simplify_pct / 100, keep_shapes = TRUE)`.
  Must be rmapshaper (Visvalingam, topology-preserving across shared
  boundaries) — NOT `sf::st_simplify()`, which simplifies polygons
  independently and creates slivers/gaps between adjacent intermediate
  catchments. Display only; never write to disk. Thin grey outlines,
  transparent or light fill; stations layer on top.

## Click -> viewer window

- `shiny::showModal(modalDialog(size = "xl", easyClose = TRUE, footer = NULL))`.
  `easyClose` = clicking beside the modal or Esc returns to the map; the map
  keeps zoom/pan state because it is never re-rendered.
- CSS for tight margins (goal: maximum plot area):

```css
.modal-xl { max-width: 98vw; }
.modal-content { height: 94vh; }
.modal-body { padding: 0.3rem 0.5rem; overflow-y: auto; }
```

- Header strip (one slim row, small font): subbasin ID + NSE, KGE, and KGE
  components r / alpha / beta computed on the fly from the Qobs/Qsim pair.
- Four stacked panels, each a `dygraphs::dygraph()` with the same
  `group = "cosero_viewer"` -> client-side linked x-zoom/pan across all four
  (no Shiny round-trip; this is the key responsiveness win). Drag = zoom,
  double-click = reset. Heights ~22vh each.
  1. Runoff: QOBS + QSIM for the clicked subbasin.
  2. Precipitation (.prec column of that subbasin).
  3. Fluxes (.plus): `selectInput` for variable(s), sensible default.
  4. States / water balance (.plus1, timestep-converted values from
     `read_plus1()`): variable selector as well.
- Use `dygraphs` not plotly here: canvas-based, fast at 100k+ points, native
  group sync. Do NOT add a date-range slider per panel; one optional
  `dyRangeSelector()` on the bottom panel only.

## Data backend — no ASCII parsing per click

Per-click latency budget is decided here.

1. On first launch, convert each of `COSERO.runoff`, `COSERO.prec`,
   `COSERO.plus`, `COSERO.plus1` to an `fst` sidecar
   (`<file>.fst` next to the source, or in `output/.cache/`). Reuse the
   existing readers for parsing so datetime columns and the .plus1
   cumulative-to-timestep conversion (`read_plus1()`,
   `R/cosero_readers.R:809+`) happen once, at conversion time.
   Invalidate the cache when source mtime or size changed.
2. Per click: `fst::read_fst(path, columns = c(...))` — random column access,
   single-digit ms even for very wide files. Select columns matching the
   clicked subbasin ID (reuse the 3/4-digit format probing from
   `calculate_single_metric()`, `R/cosero_optimize.R:268-290`).
3. In-session cache (environment keyed by subbasin ID) so repeated clicks are
   instant.

This is also the planned "binary files" step done R-side; if COSERO later
writes binary output natively, only the converter changes.

## Roxygen help (draft)

```
#' Interactive Map of COSERO Subbasins and Gauging Stations
#'
#' Launches a Shiny app showing gauging stations (and optionally the
#' intermediate-catchment polygons) on an interactive map. Clicking a
#' station opens a full-screen viewer with four linked time-series panels
#' -- runoff (observed/simulated), precipitation, fluxes (COSERO.plus) and
#' system states (COSERO.plus1) -- together with NSE, KGE and the KGE
#' components (r, alpha, beta) for the selected subbasin. All panels share
#' a common, zoomable time axis. Closing the window (or clicking next to
#' it) returns to the map.
#'
#' @param stations_shp Path to a point shapefile with the gauging stations,
#'   or an \code{sf} object. Must contain a subbasin ID column (see
#'   \code{subbasin_id_field}). Large station sets (10,000+) are supported.
#' @param catchments_shp Optional path to a polygon shapefile with the
#'   intermediate catchments (or an \code{sf} object). If NULL (default),
#'   the catchment layer is skipped.
#' @param cosero_path COSERO project path. Time series are read from
#'   \code{output/COSERO.runoff}, \code{.prec}, \code{.plus}, \code{.plus1}.
#'   On first launch these ASCII files are converted to a fast binary cache
#'   (fst format) next to the originals; later launches and every click read
#'   from the cache (milliseconds instead of seconds).
#' @param subbasin_id_field Name of the attribute column holding the COSERO
#'   subbasin ID. If NULL (default), the columns NB, SB, basin and subbasin
#'   are tried (case-insensitive).
#' @param catchment_simplify_pct Percentage of polygon vertices retained for
#'   map display (default 5). Uses topology-preserving simplification
#'   (rmapshaper), so shared boundaries between intermediate catchments stay
#'   seamless. Affects display only; the shapefile on disk is never
#'   modified. Use 100 to disable simplification.
#'
#' @return A \code{shiny.appobj}; called for its side effect of launching
#'   the app.
#' @export
```

Add an `@examples \dontrun{}` block with a minimal call and one with
catchments + explicit `subbasin_id_field`.

## Package changes

- `DESCRIPTION`: add to `Suggests`: `leaflet (>= 2.1.0)`, `dygraphs (>= 1.1.1)`,
  `fst (>= 0.9.8)`, `rmapshaper (>= 0.5.0)`, `leafgl` (optional WebGL
  speed-up). `sf` is already in Suggests. Guard every suggested package with
  `requireNamespace()` and a clear "install.packages('x') required for
  launch_cosero_map()" error; `leafgl` failure degrades silently to canvas
  markers.
- NAMESPACE: generated — just run `devtools::document()` after adding the
  roxygen (`@export` on `launch_cosero_map`). Do not import the suggested
  packages; always call them namespaced (`leaflet::`, `dygraphs::`, ...).
- Regenerate docs: `devtools::document()` -> new `man/launch_cosero_map.Rd`.
- README: short subsection with one screenshot-style description and the
  minimal example, listed next to the existing Shiny app section.
- Optional: mention in `launch_app()` docs as a related function
  (`@seealso`).

## Verification

- `devtools::document()` and `devtools::check()` clean (note: suggested-only
  deps must not break check on machines without them — all calls guarded).
- Synthetic test: stations sf with 10,000 random points + NB column ->
  map renders, panning stays smooth.
- Click a station: modal opens with 4 panels; zooming one panel moves all
  four; Esc/click-outside returns to the unchanged map view.
- First launch creates the .fst cache files; second click on the same or
  another station < ~100 ms; touching COSERO.runoff (mtime) triggers
  reconversion.
- Station whose subbasin has no observations (Qobs -999): runoff panel shows
  QSIM only, metrics display "no observations" instead of numbers (no
  spurious KGE = 1; consistent with dev/fix_ungauged_subbasins.md).
- `catchments_shp = NULL` skips the layer without error; wrong
  `subbasin_id_field` errors with the list of available columns.
