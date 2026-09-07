# Manual test: launch_cosero_map() on the KLIRES Europe-wide COSERO project
#
# Project:    D:/KLIRES/working files/Model          (output/ holds ~11 GB ASCII
#             plus the binary twins COSERO.*B -> binary fast path is used)
# Catchments: merged_3035_final.shp -- 13,605 polygons, EPSG:3035, link via
#             field NB_new (unique integers 1..13605)
# Gauges:     none supplied -> the catchment polygons are the clickable layer
#
# Simulation period in this output: 2005-01-01 .. 2020-12-31 (5,844 daily steps)
#
# One-time setup (suggested packages used by the map viewer):
# install.packages(c("sf", "leaflet", "dygraphs", "xts", "fst",
#                    "rmapshaper", "leafgl", "base64enc"))

devtools::load_all()

project   <- "D:/KLIRES/working files/Model"
catch_shp <- "D:/KLIRES/working files/Merging/Final/merged_3035_final.shp"
stopifnot(dir.exists(file.path(project, "output")), file.exists(catch_shp))


# --- Simplify the catchments OURSELVES, before handing them to the viewer -----
# Do NOT use the viewer's built-in catchment_simplify_pct here: that calls
# rmapshaper::ms_simplify(), which pushes the geometry through a V8 JavaScript
# engine with a hard ~1.4 GB heap cap. At 13,605 polygons / 205 MB this layer
# blows past that cap and kills the whole R process:
#     "V8 FATAL ERROR ... JavaScript heap out of memory"
#     R exited unexpectedly: exit code -2147483645   (0x80000003)
# It is not a RAM shortage -- the limit is internal to V8 and 31 GB of free
# memory does not help.
#
# sf::st_simplify() is pure C++ (GEOS) with no such cap: ~7 s, 205 MB -> 30 MB,
# no dropped geometries. Simplify BEFORE reprojecting, while the layer is still
# in EPSG:3035, so dTolerance is a true distance in metres.
message("Reading and simplifying catchments ...")
catchments <- sf::st_read(catch_shp, quiet = TRUE)          # EPSG:3035 (metres)
catchments <- sf::st_simplify(catchments, dTolerance = 500, # 200 m tolerance
                              preserveTopology = TRUE)
message("Simplified: ", format(utils::object.size(catchments), units = "MB"))

# --- First run is slow, later runs are instant --------------------------------
# Building output/.cache/ converts all four output files to fst. Expect roughly
# 10-25 min and ~4-5 GB of extra space on D: for this project (peak ~3.5 GB RAM
# while COSERO.plus1 is converted). Every launch afterwards reuses the cache.
#
# Note: output/.cosero_cache.rds (1.9 GB) belongs to the ASCII readers, NOT to
# the map viewer -- the viewer only ever writes into output/.cache/.

launch_cosero_map(
  # --- layers (need at least one of stations_shp / catchments_shp) ----------
  stations_shp   = NULL,            # no gauge point layer for this project
  catchments_shp = catchments,      # the PRE-SIMPLIFIED sf object from above
                                    # (the function accepts an sf object or a
                                    # path); becomes the clickable layer

  # --- project ---------------------------------------------------------------
  cosero_path = project,            # NOTE: the *project* dir; the function
                                    # appends /output itself

  # --- subbasin ID columns ---------------------------------------------------
  subbasin_id_field  = NULL,        # unused: no stations layer
  catchment_id_field = "NB_new",    # must be given -- auto-detect would pick
                                    # the plain "NB" column, which is NOT the
                                    # numbering used in the output columns

  # --- display / behaviour ---------------------------------------------------
  catchment_simplify_pct = 100,     # 100 = DO NOT call rmapshaper::ms_simplify()
                                    # -- it would crash R here (see note above).
                                    # The layer is already simplified.
  spinup      = 365,                # leading timesteps excluded from the OFs
                                    # (NSE/KGE/r/BETA); plots stay full-length
  clean_cache = FALSE               # TRUE = delete output/.cache and rebuild
)


# --- Alternative configurations (uncomment one) ------------------------------
# Faster first look -- coarser polygons, no spin-up exclusion. Raise the
# st_simplify tolerance rather than lowering catchment_simplify_pct:
# catchments_coarse <- sf::st_simplify(sf::st_read(catch_shp, quiet = TRUE),
#                                      dTolerance = 500, preserveTopology = TRUE)
# launch_cosero_map(
#   catchments_shp         = catchments_coarse,
#   cosero_path            = project,
#   catchment_id_field     = "NB_new",
#   catchment_simplify_pct = 100,
#   spinup                 = 0
# )
#
# Force a full cache rebuild (only after COSERO has been re-run):
# launch_cosero_map(
#   catchments_shp         = catchments,
#   cosero_path            = project,
#   catchment_id_field     = "NB_new",
#   catchment_simplify_pct = 100,
#   clean_cache            = TRUE
# )


# What to verify --------------------------------------------------------------
#  1. Console on first launch: all four files cached "(binary, 5844 rows, ...)".
#     output/.cache/ then holds COSERO.runoff.fst etc. + cache_meta.rds and
#     station_metrics.csv (subbasin, lon, lat, coord_source, NSE/KGE/r/alpha/
#     beta, n, spinup) -- coord_source is the catchment centroid here.
#     "does not match current ... falling back to ASCII" = no valid binary
#     region found (viewer still works, conversion is just much slower).
#  2. Map: basemap "Light" by default, switchable (Light/OSM/Topographic/
#     Satellite); "Full extent" re-zooms to the European extent.
#  3. "Colour by metric": None / NSE / KGE / r / BETA. Polygons recolour with a
#     legend (blue = good -> red = bad; BETA diverging around 1). Most of the
#     13,605 subbasins are ungauged and stay unshaded -- that is expected.
#  4. Hover a catchment (~250 ms): popup with the monthly regime (Qobs/Qsim +
#     dashed annual means) and the inverted rain/snow hyetograph.
#  5. Click a catchment: near-fullscreen viewer with up to four panels (runoff,
#     precipitation, fluxes, states) and the metric strip
#     (Subbasin <ID> | NSE | KGE | r | alpha | beta | n, spin-up 365 excl.).
#  6. Drag-zoom any panel -> all four move together; double-click resets.
#  7. Esc / click beside the modal -> back to the map, zoom/pan unchanged.
#  8. Click the same catchment again -> opens instantly (in-session memoization).
#  9. Ungauged subbasin (Qobs all -999): runoff panel shows Qsim only, strip
#     shows "no observations" (no spurious KGE = 1).
# 10. IDs resolve at all: this project writes 5-digit column suffixes
#     (QOBS_00001 .. QOBS_13605). If a click ever reports "No data" for a
#     subbasin you know is gauged, check probe_subbasin_columns() in
#     R/cosero_map.R -- it probes %05d/%04d/%03d/%d.
# 11. Second launch: cache reused (no conversion messages).
