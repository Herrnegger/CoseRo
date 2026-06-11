# Manual test: launch_cosero_map() on the Austria-wide calibrated project
#
# Project:  D:/temp/COSERO_AT/#COSERO_AT_calibrated  (~976 MB ASCII output,
#           binary twins COSERO.*B present -> exercises the binary fast path)
# Gauges:   gauges_basins_cosero.shp -- points, subbasin link via field ID_
# Basins:   basins_info_CE.shp       -- catchment polygons, link via field NB_
#
# One-time setup (install the suggested packages used by the map viewer):
# install.packages(c("sf", "leaflet", "dygraphs", "xts", "fst",
#                    "rmapshaper", "leafgl", "base64enc"))

devtools::load_all()

gis_dir <- "D:/temp/COSERO_AT/#COSERO_AT_GIS"
project <- "D:/temp/COSERO_AT/#COSERO_AT_calibrated"

# Full call -- every argument shown with its default, so all options are visible
launch_cosero_map(
  # --- layers (need at least one of stations_shp / catchments_shp) ----------
  stations_shp   = file.path(gis_dir, "gauges_basins_cosero.shp"),  # point layer; or sf object; or NULL
  catchments_shp = file.path(gis_dir, "basins_info_CE.shp"),        # polygon layer; or sf object; or NULL

  # --- project ---------------------------------------------------------------
  cosero_path = project,            # reads <cosero_path>/output/  (.runoff/.prec/.plus/.plus1)

  # --- subbasin ID columns (gauges and catchments use DIFFERENT columns) -----
  subbasin_id_field  = "ID",        # stations ID column; "ID" auto-matches "ID_". NULL = auto-detect (NB/ID/SB/basin/subbasin, +trailing "_")
  catchment_id_field = "NB",        # catchments ID column; "NB" auto-matches "NB_". NULL = fall back to subbasin_id_field, then auto-detect

  # --- display / behaviour ---------------------------------------------------
  catchment_simplify_pct = 5,       # % polygon vertices kept for display (rmapshaper, topology-preserving). 100 = no simplification
  spinup      = 365,                # leading model timesteps excluded from the OFs (NSE/KGE/r/BETA). Plots stay full-length. 0 = off
  clean_cache = FALSE               # TRUE = delete output/.cache and rebuild from source before launch
)

# --- Alternative layer configurations (uncomment one) ------------------------
# Catchments only (no gauges) -- polygons become the clickable layer:
# launch_cosero_map(
#   catchments_shp     = file.path(gis_dir, "basins_info_CE.shp"),
#   cosero_path        = project,
#   catchment_id_field = "NB"
# )
#
# Stations only (no catchment polygons):
# launch_cosero_map(
#   stations_shp      = file.path(gis_dir, "gauges_basins_cosero.shp"),
#   cosero_path       = project,
#   subbasin_id_field = "ID"
# )

# What to verify --------------------------------------------------------------
#  1. Console on first launch: all four files cached "(binary, ...)".
#     output/.cache/ holds COSERO.runoff.fst etc. + cache_meta.rds.
#     "does not match current ... falling back to ASCII" = no valid binary
#     region found (viewer still works, conversion just slower).
#  2. Map: basemap "Light" by default, switchable (Light/OSM/Topographic/
#     Satellite) via the layers control; "Full extent" button re-zooms.
#  3. "Colour by metric" selector: None / NSE / KGE / r / BETA. Points recolour
#     with a legend (sequential blue=good->red=bad; BETA diverging around 1).
#  4. Hover a gauge or a catchment (~250 ms): popup at the feature with the
#     monthly regime (Qobs/Qsim + dashed annual means) and the inverted
#     rain/snow hyetograph, titled with NSE/KGE.
#  5. Click a gauge OR a catchment polygon: near-fullscreen viewer with up to
#     four panels (runoff, precipitation, fluxes, states) and the metric strip
#     (Subbasin <ID> | NSE | KGE | r | alpha | beta | n, spin-up N excl.).
#  6. Drag-zoom any panel -> all four move together; double-click resets.
#  7. Esc / click beside the modal -> back to the map, zoom/pan unchanged.
#  8. Click the same feature again -> opens instantly (in-session memoization).
#  9. Ungauged subbasin (Qobs all -999): runoff panel shows Qsim only, strip
#     shows "no observations" (no spurious KGE = 1).
# 10. Variable selectors (panels 3/4) add/remove series; remembered next open.
# 11. Second launch: cache reused (no conversion messages). Force a rebuild
#     with clean_cache = TRUE, or by touching a source file:
#     Sys.setFileTime(file.path(project, "output", "COSERO.runoffB"), Sys.time())
