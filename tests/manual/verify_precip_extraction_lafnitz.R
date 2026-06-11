# =============================================================================
# Verify SPARTACUS precipitation extraction — Lafnitz catchment
# =============================================================================
# Goal: confirm that write_spartacus_precip() extracts the correct zonal rainfall
# from the SPARTACUS RR NetCDFs, by cross-checking against the raw raster in QGIS.
#
# What this script does:
#   1. Extract 2 years of precipitation for the Lafnitz zones -> P_NZ_<years>.txt
#   2. Export the FIRST and LAST SPARTACUS day as GeoTIFFs (in the shapefile CRS)
#      so they can be loaded in QGIS alongside cosero_zones.shp for manual checks.
#   3. Print a small table of 5 sample zones with their extracted P value on the
#      first and last day, so the numbers can be compared visually against the
#      rasters in QGIS.
#
# Manual QGIS check:
#   - Load the exported GeoTIFF + cosero_zones.shp.
#   - For a sample zone, the extracted P value should be the area-weighted mean
#      of the raster cells the zone polygon overlaps. Coarse single-cell zones
#      should match the underlying cell almost exactly; multi-cell zones will be
#      an area-weighted blend.
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude
# Date: 2026-06-11
# =============================================================================

devtools::load_all()

library(sf)
library(terra)
library(data.table)

# =============================================================================
# USER SETTINGS
# =============================================================================

shp_path <- "D:/temp/P_T_ETO_NZ/Lafnitz/cosero_zones.shp"

# Directory with SPARTACUS RR NetCDFs (pattern: SPARTACUS2-DAILY_RR_YYYY.nc)
rr_dir <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/RR"

output_dir <- "D:/temp/P_T_ETO_NZ/Lafnitz/output_verify"

years   <- 1991:2024   # 2 years only, for a quick verification
nz_col  <- "NZ"        # zone-ID column in the shapefile (adjust if different)
n_cores <- 4

n_sample_zones <- 55    # how many zones to show in the comparison table

# Which days of the extraction period to export/compare (1-based, counting
# across the full set of daily layers in chronological order). Day 1 = first
# day of the first year. Pick days with actual rainfall for a meaningful check.
check_days <- c(4, 5, 41, 729)

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# 1. LOAD ZONES + INSPECT FIELDS
# =============================================================================

cat("=== 1. Load Lafnitz zones ===\n")
zones <- st_read(shp_path, quiet = TRUE)
cat("Loaded", nrow(zones), "zones. Fields:",
    paste(names(zones), collapse = ", "), "\n")

if (!nz_col %in% names(zones)) {
  stop(sprintf("Column '%s' not found. Available: %s\nAdjust 'nz_col'.",
               nz_col, paste(names(zones), collapse = ", ")), call. = FALSE)
}

# Sort zones by NZ exactly as write_spartacus_precip() does internally, so the
# output column order matches our sample-zone lookup below.
zones <- zones[order(zones[[nz_col]]), ]
zone_ids <- as.character(zones[[nz_col]])
cat("Zone ID range:", min(zones[[nz_col]]), "to", max(zones[[nz_col]]), "\n\n")

# =============================================================================
# 2. EXTRACT PRECIPITATION (2 years) -> P_NZ_<years>.txt
# =============================================================================

cat("=== 2. Extract precipitation with write_spartacus_precip() ===\n")
out_p <- write_spartacus_precip(
  nc_dir       = rr_dir,
  output_dir   = output_dir,
  model_zones  = zones,
  nz_col       = nz_col,
  years        = years,
  n_cores      = n_cores,
  write_binary = TRUE,
  time_shift   = TRUE,
  na_fill      = "mean"
)
cat("Precipitation file written:", out_p$txt, "\n\n")

# =============================================================================
# 3. EXPORT SELECTED DAYS AS GeoTIFF (for QGIS)
# =============================================================================
# IMPORTANT: write_spartacus_precip() applies time_shift = TRUE, which mixes
# consecutive days. The raw rasters exported here are the UN-shifted SPARTACUS
# fields. The independent zonal means in section 4 are computed from these same
# raw rasters (no shift), so the table's "independent" column matches what the
# GeoTIFFs show in QGIS. The package column will differ on non-boundary days by
# the time-shift blend - that is expected.
# =============================================================================

cat("=== 3. Export selected days as GeoTIFF ===\n")

rr_files <- sort(list.files(rr_dir,
                            pattern = "SPARTACUS2-DAILY_RR_\\d{4}\\.nc$",
                            full.names = TRUE))
file_years <- as.integer(gsub(".*_(\\d{4})\\.nc$", "\\1", basename(rr_files)))
rr_files <- rr_files[file_years %in% years]
if (length(rr_files) == 0) stop("No SPARTACUS RR files for years ", paste(years, collapse = ","))

# Layers per file (days per year), in chronological order, to map an absolute
# day index -> (file, layer-within-file).
layers_per_file <- vapply(rr_files, function(f) as.integer(terra::nlyr(terra::rast(f))),
                          integer(1))
cum_layers <- cumsum(layers_per_file)
total_days <- sum(layers_per_file)
cat("Total daily layers across", length(rr_files), "file(s):", total_days, "\n")

if (any(check_days > total_days)) {
  stop("check_days contains day(s) beyond the available period (max = ",
       total_days, "): ",
       paste(check_days[check_days > total_days], collapse = ", "), call. = FALSE)
}

# IMPORTANT — SPARTACUS row order:
# SPARTACUS NetCDFs store rows top-to-bottom (row 0 = north), the opposite of
# standard GIS orientation. write_spartacus_precip() corrects this internally
# with terra::flip(r, "vertical") before extraction. We apply the SAME flip
# here, or (a) the GeoTIFF appears upside-down in QGIS and (b) the independent
# zonal extraction samples the mirror-image location.

# Resolve an absolute day index to a flipped raster + its date.
read_day <- function(day_idx) {
  file_idx  <- which(cum_layers >= day_idx)[1]
  prev_cum  <- if (file_idx == 1) 0L else cum_layers[file_idx - 1]
  layer_idx <- day_idx - prev_cum
  r_unflip  <- terra::rast(rr_files[file_idx])[[layer_idx]]
  list(
    raster = terra::flip(r_unflip, "vertical"),
    date   = terra::time(r_unflip),
    crs    = terra::crs(r_unflip)
  )
}

day_rasters <- lapply(check_days, read_day)
r_crs <- day_rasters[[1]]$crs

# Export each selected day as a GeoTIFF
tif_files <- character(length(check_days))
for (k in seq_along(check_days)) {
  d   <- check_days[k]
  obj <- day_rasters[[k]]
  tag <- format(as.Date(obj$date), "%Y%m%d")
  tif_files[k] <- file.path(output_dir, sprintf("RR_day%04d_%s.tif", d, tag))
  terra::writeRaster(obj$raster, tif_files[k], overwrite = TRUE)
  cat(sprintf("  Day %d (%s) -> %s\n", d, format(obj$date), basename(tif_files[k])))
}

# Export zones in the raster CRS so QGIS overlay is trivial
zones_rcrs <- sf::st_transform(zones, r_crs)
shp_out <- file.path(output_dir, "cosero_zones_rastercrs.shp")
sf::st_write(zones_rcrs, shp_out, quiet = TRUE, delete_dsn = TRUE)
cat("Wrote zones in raster CRS:", basename(shp_out), "\n\n")

# =============================================================================
# 4. INDEPENDENT ZONAL MEANS FROM RAW RASTERS (no time shift)
# =============================================================================
# exactextractr directly on the (flipped) raw rasters -> area-weighted zonal
# mean. This is what the GeoTIFFs show and the ground truth for the QGIS check.
# Independent of the package's sparse-matrix path, so it's a genuine cross-check.
# =============================================================================

cat("=== 4. Independent zonal means from raw rasters (un-shifted) ===\n")

zones_match <- sf::st_transform(zones, r_crs)

# Per selected day, a vector of zonal means (one per zone, in zone_ids order)
indep_by_day <- lapply(day_rasters, function(obj) {
  exactextractr::exact_extract(obj$raster, zones_match, "mean", progress = FALSE)
})

# =============================================================================
# 5. READ PACKAGE OUTPUT + BUILD COMPARISON TABLE
# =============================================================================

cat("=== 5. Compare package output vs independent extraction ===\n")

dt <- fread(out_p$txt)
nz <- ncol(dt) - 5L
setnames(dt, c("Y", "M", "D", "H", "Min", zone_ids))

# Package value rows for the selected days (row index == day index, since the
# output has one row per day in chronological order)
pkg_by_day <- lapply(check_days, function(d) as.numeric(dt[d, (zone_ids), with = FALSE]))

# Sample zones evenly spread across the ID range
sample_idx <- unique(round(seq(1, length(zone_ids), length.out = n_sample_zones)))

# Build a comparison table: NZ + package/independent columns per selected day
comparison <- data.table(NZ = zone_ids[sample_idx])
for (k in seq_along(check_days)) {
  d <- check_days[k]
  comparison[[sprintf("P_day%d_package", d)]]     <- round(pkg_by_day[[k]][sample_idx], 3)
  comparison[[sprintf("P_day%d_independent", d)]] <- round(indep_by_day[[k]][sample_idx], 3)
}

cat("\nNOTE: 'package' values include the 07:00->00:00 time shift; 'independent'\n")
cat("values are the raw un-shifted rasters shown in the GeoTIFFs. The two match\n")
cat("exactly only on boundary days; on interior days they differ by the shift\n")
cat("blend (package[t] = 7/24*raw[t-1] + 17/24*raw[t]) - this is expected.\n\n")

print(comparison)

# Per-day full-zone agreement summary
cat("\nPer-day max |package - independent| across all", nz, "zones:\n")
for (k in seq_along(check_days)) {
  d    <- check_days[k]
  diff <- abs(pkg_by_day[[k]] - indep_by_day[[k]])
  cat(sprintf("  Day %d: %.4f mm  (independent mean = %.2f mm)\n",
              d, max(diff, na.rm = TRUE), mean(indep_by_day[[k]], na.rm = TRUE)))
}

# Save comparison table to CSV
csv_out <- file.path(output_dir, "precip_extraction_check.csv")
fwrite(comparison, csv_out)
cat("\nComparison table saved to:", csv_out, "\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n============================================================\n")
cat("Verification artifacts in:", output_dir, "\n")
cat("  -", basename(out_p$txt), "  (COSERO precip input)\n")
for (k in seq_along(check_days)) {
  cat("  -", basename(tif_files[k]), "  (day", check_days[k], "raster for QGIS)\n")
}
cat("  -", basename(shp_out), "  (zones in raster CRS)\n")
cat("  - precip_extraction_check.csv\n")
cat("\nIn QGIS: load a GeoTIFF + zones, use 'Identify' on a cell and compare to\n")
cat("the P_dayN_independent column for that zone.\n")
cat("============================================================\n")
