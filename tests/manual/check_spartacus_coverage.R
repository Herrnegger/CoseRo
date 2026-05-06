# Diagnostic: Check SPARTACUS NetCDF spatial coverage vs model zones shapefile
#
# Problem: write_spartacus_precip() produces fewer data columns (704) than zones
# in the shapefile (764). Hypothesis: the NetCDF domain does not fully cover the
# shapefile, so zones outside the NetCDF extent get all-NA values and may be
# dropped or produce NA columns.
#
# What this script does:
#   1. Load one representative SPARTACUS NetCDF and the model zones shapefile
#   2. Reproject zones to raster CRS if needed
#   3. Check which zones intersect/overlap the NetCDF bounding box
#   4. Compute exactextractr coverage per zone (how much of each zone lies
#      within the NetCDF extent) — zones fully outside get 0 coverage
#   5. Classify zones: fully covered, partially covered, not covered
#   6. Report counts and zone IDs
#   7. Map the result: NetCDF extent + zones coloured by coverage status

library(sf)
library(terra)
library(exactextractr)
library(ggplot2)
library(dplyr)
devtools::load_all()

# ── Paths ──────────────────────────────────────────────────────────────────────
shp_path <- "D:/temp/P_T_ETO_NZ/Ammer/shp/cosero_zones.shp"
rr_dir   <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/RR"
nz_col   <- "NZ"

# Use the first available RR file as the spatial reference
nc_files <- sort(list.files(rr_dir, pattern = "SPARTACUS2-DAILY_RR_\\d{4}\\.nc$",
                             full.names = TRUE))
stopifnot("No SPARTACUS RR files found" = length(nc_files) > 0)
nc_ref <- nc_files[1]
cat("Reference NetCDF:", basename(nc_ref), "\n")

# ── Load data ──────────────────────────────────────────────────────────────────
zones <- st_read(shp_path, quiet = TRUE)
cat("Zones loaded:", nrow(zones), "\n")

# Load first layer/band of the NetCDF (we only need the grid geometry)
r_raw    <- terra::rast(nc_ref)[[1]]
r_flip   <- terra::flip(r_raw, "vertical")   # standard GIS orientation

# Reproject zones if CRS differs
if (sf::st_crs(zones) != sf::st_crs(r_flip)) {
  cat("Reprojecting zones to raster CRS...\n")
  zones <- sf::st_transform(zones, sf::st_crs(r_flip))
}

# ── Export flipped raster as GeoTIFF ──────────────────────────────────────────
tif_file <- "D:/temp/P_T_ETO_NZ/Ammer/output/spartacus_ref_flipped.tif"
terra::writeRaster(r_flip, tif_file, overwrite = TRUE)
cat("Flipped raster exported to:", tif_file, "\n")

# ── NetCDF bounding box as sf polygon ──────────────────────────────────────────
nc_bbox_sf <- st_as_sf(st_as_sfc(st_bbox(r_flip)))
st_crs(nc_bbox_sf) <- st_crs(r_flip)

# ── Compute per-zone coverage using exactextractr ──────────────────────────────
# exact_extract returns coverage_fraction per cell; a zone with no overlapping
# cells at all returns an empty data frame → we get sum(coverage_fraction) == 0.
cat("Extracting per-zone cell coverage (this may take ~30 s)...\n")
extracted <- exact_extract(r_flip, zones, include_cell = TRUE, progress = TRUE)

# For each zone: sum of coverage fractions (proxy for how many cells it touches)
# and number of non-NA values in those cells
zone_summary <- mapply(function(df, z_idx) {
  if (nrow(df) == 0) {
    data.frame(zone_idx = z_idx, n_cells = 0L, sum_cov = 0, n_valid = 0L)
  } else {
    data.frame(
      zone_idx = z_idx,
      n_cells  = nrow(df),
      sum_cov  = sum(df$coverage_fraction, na.rm = TRUE),
      n_valid  = sum(!is.na(df$value))  # non-NA raster cells within zone
    )
  }
}, extracted, seq_along(extracted), SIMPLIFY = FALSE)

zone_df <- do.call(rbind, zone_summary)
zone_df[[nz_col]] <- zones[[nz_col]]

# Classification
zone_df$status <- dplyr::case_when(
  zone_df$n_cells == 0                        ~ "Not covered",
  zone_df$n_valid == 0 & zone_df$n_cells > 0  ~ "Outside NC domain",
  zone_df$n_valid < zone_df$n_cells           ~ "Partially covered",
  TRUE                                         ~ "Fully covered"
)

# ── Summary ────────────────────────────────────────────────────────────────────
cat("\n=== Coverage Summary ===\n")
print(table(zone_df$status))
cat("\nZone IDs with status 'Not covered' or 'Outside NC domain':\n")
problem_zones <- zone_df[zone_df$status %in% c("Not covered", "Outside NC domain"), nz_col]
if (length(problem_zones) == 0) {
  cat("  None — all zones have at least some NetCDF coverage.\n")
} else {
  cat(" ", paste(sort(problem_zones), collapse = ", "), "\n")
}
cat("\nZone IDs partially covered (some cells outside NetCDF extent):\n")
partial_zones <- zone_df[zone_df$status == "Partially covered", nz_col]
if (length(partial_zones) == 0) {
  cat("  None.\n")
} else {
  cat(" ", paste(sort(partial_zones), collapse = ", "), "\n")
}

# Count expected vs actual columns in output
n_covered <- sum(zone_df$status %in% c("Fully covered", "Partially covered"))
cat(sprintf("\nExpected output columns: %d (zones with any coverage)\n", n_covered))
cat(sprintf("Zones with NO coverage:  %d\n", nrow(zones) - n_covered))

# ── Spatial Plot ───────────────────────────────────────────────────────────────
# Merge status back onto sf object for plotting
zones_plot <- zones
zones_plot$status <- zone_df$status[match(zones[[nz_col]], zone_df[[nz_col]])]

# NetCDF extent polygon in WGS84 for reference (reproject for display if needed)
wgs84 <- st_crs(4326)
nc_bbox_wgs  <- st_transform(nc_bbox_sf, wgs84)
zones_wgs    <- st_transform(zones_plot, wgs84)

status_colors <- c(
  "Fully covered"      = "#2166ac",
  "Partially covered"  = "#f4a582",
  "Outside NC domain"  = "#d6604d",
  "Not covered"        = "#b2182b"
)

p_map <- ggplot() +
  geom_sf(data = nc_bbox_wgs, fill = NA, color = "black", linewidth = 1,
          linetype = "dashed") +
  geom_sf(data = zones_wgs, aes(fill = status), color = "white", linewidth = 0.1) +
  scale_fill_manual(values = status_colors, name = "Coverage status") +
  labs(
    title    = "SPARTACUS NetCDF vs Model Zones: Spatial Coverage",
    subtitle = sprintf("Reference: %s  |  Total zones: %d  |  Covered: %d  |  Problem: %d",
                       basename(nc_ref), nrow(zones),
                       n_covered, nrow(zones) - n_covered),
    caption  = "Dashed rectangle = NetCDF bounding box"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# ── Bar chart of coverage counts ───────────────────────────────────────────────
status_counts <- as.data.frame(table(Status = zone_df$status))
p_bar <- ggplot(status_counts, aes(x = Status, y = Freq, fill = Status)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = Freq), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = status_colors) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Zone counts by coverage status",
       x = NULL, y = "Number of zones") +
  theme_bw()

# ── Print & save ───────────────────────────────────────────────────────────────
print(p_map)
print(p_bar)

out_pdf <- "D:/temp/P_T_ETO_NZ/Ammer/output/spartacus_coverage_check.pdf"
pdf(out_pdf, width = 12, height = 8)
print(p_map)
print(p_bar)
dev.off()
cat("\nPlots saved to:", out_pdf, "\n")

# ── Optional: inspect a specific partially-covered zone ────────────────────────
# Uncomment to see which raster cells fall within zone NZ = X
# z_id <- 1
# z_sf <- zones[zones[[nz_col]] == z_id, ]
# ex   <- exact_extract(r_flip, z_sf, include_cell = TRUE, progress = FALSE)
# print(ex[[1]])
