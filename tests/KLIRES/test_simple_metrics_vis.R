# Manual test: simple_metrics_vis() on the KLIRES Europe-wide COSERO project
#
# Project:    D:/KLIRES/working files/Model
# Catchments: merged_3035_final.shp -- 13,605 polygons, EPSG:3035, link via
#             field NB_new (unique integers 1..13605)
#
# The quick-look counterpart to test_map_viewer_klires.R. That script launches
# the full viewer, which reads ~11 GB of COSERO output and offers hover
# regimes, four-panel time series and cache management. This one reads only
# output/statistics.txt and draws one map: pick a metric and a range, and only
# the catchments in that range are shown. No cache build, no prior launch.
#
# Colours are the same graded scale the viewer uses (teal = good through red
# = bad), so a basin looks identical in both.
#
# One-time setup:
# install.packages(c("sf", "leaflet", "shiny"))

devtools::load_all()

project   <- "D:/KLIRES/working files/Model"
catch_shp <- "D:/KLIRES/working files/Merging/Final/merged_3035_final.shp"
stopifnot(dir.exists(project), file.exists(catch_shp))


# --- Launch -------------------------------------------------------------------
# Polygons are simplified with sf::st_simplify() at 500 m BEFORE reprojecting
# (so the tolerance is true metres in EPSG:3035). rmapshaper::ms_simplify() is
# NOT used -- its V8 engine has a ~1.4 GB heap cap and hard-crashes R on this
# 205 MB layer, regardless of free RAM. See ?simple_metrics_vis.

simple_metrics_vis(
  cosero_path        = project,      # the *project* dir; the function appends
                                     # output/statistics.txt itself
  catchments_shp     = catch_shp,    # path or an sf object
  catchment_id_field = "NB_new",     # NOT the plain "NB" column -- NB_new is
                                     # the numbering used in the output columns
  simplify_tolerance = 500           # metres here (EPSG:3035); 0 = no simplify
)


# --- Alternative configurations (uncomment one) ------------------------------
# Coarser polygons for a faster first look:
# simple_metrics_vis(project, catch_shp, "NB_new", simplify_tolerance = 1000)
#
# Point straight at a statistics.txt -- e.g. the stat_opt.txt an optimisation
# run saved, to map that calibration instead of the last full run:
# simple_metrics_vis(
#   cosero_path        = file.path(project, "output/stat_opt.txt"),
#   catchments_shp     = catch_shp,
#   catchment_id_field = "NB_new"
# )
#
# Re-use an already-loaded sf object across several calls (skips the re-read):
# poly <- sf::st_read(catch_shp, quiet = TRUE)
# simple_metrics_vis(project, poly, "NB_new")


# What to verify --------------------------------------------------------------
# 1. Console: "Subbasins in statistics.txt: 13605", then "Simplified: ~30 Mb"
#    and "Catchments with at least one metric: N".
# 2. Map opens on the European extent, basemap "Esri.WorldGrayCanvas".
#    Polygons are graded teal (good) -> red (bad), same colours as the full
#    viewer's "Colour by metric"; the legend title reads "NSE" / "KGE" /
#    "BETA (bias)" and lists only the classes actually on screen.
# 3. Metric = BETA: two boxes, "Under, BETA <" and "Over, BETA >". Shows what
#    falls OUTSIDE that range -- raise "Under" to 0.8 and the count must grow.
# 4. Metric = NSE or KGE: the boxes swap to "From (>=)" / "To (<)" and now
#    show what falls INSIDE the band. Set From = 0, To = 0.4 and the legend
#    must drop the "Unacceptable (<0)" row, since no such catchment can be in
#    the band.
# 5. The info line reads "within range are hidden" for BETA and "outside the
#    band are hidden" for NSE/KGE.
# 6. Hover any catchment: the label shows all three metrics, whichever one is
#    currently mapped.
# 7. Under the controls: "Medians -- all: NSE .. | KGE .. | BETA ..  ||
#    shown: ...". The "all" half must not move when the range boxes change;
#    the "shown" half must. Each value carries its own (n=..), and those n
#    may differ between metrics where a basin has one metric but not another.
# 8. Ungauged basins (COSERO writes NSE = NaN and a spurious KGE = 1.0) must
#    come through as NA and drop out, NOT map as perfect scores --
#    read_cosero_statistics() nulls them.
# 9. A missing statistics.txt gives a clear error naming the path -- test with
#    a bogus project path.
