# Quick WINFORE NetCDF inspection - run interactively in RStudio
library(terra)
library(sf)
library(ncdf4)

# ── Paths ──
nc_file <- "D:/temp/Geosphere_data/WINFORE_ET0/WINFORE2_ET0_2025.nc"
shp_path <- "D:/temp/T_NZ_complex/shp/wildalpen_zones_cosero_parameters.shp"

# ── Inspect NetCDF structure with ncdf4 ──
nc <- nc_open(nc_file)
print(nc)  # Full dump of dimensions, variables, attributes
nc_close(nc)

# ── Load single layer with terra ──
r <- rast(nc_file)
cat("Layers:", nlyr(r), "\n")
cat("Grid:", nrow(r), "x", ncol(r), "\n")
cat("Resolution:", res(r), "\n")
cat("CRS:", crs(r, describe = TRUE)$name, "\n")

# Extract just layer 1 (day 1)
r1 <- r[[1]]
cat("\nLayer 1 name:", names(r1), "\n")
cat("Range:", range(values(r1), na.rm = TRUE), "\n")
cat("Non-NA cells:", sum(!is.na(values(r1))), "of", ncell(r1), "\n")

# ── Load zones shapefile ──
zones <- st_read(shp_path, quiet = TRUE)
cat("\nZones CRS:", st_crs(zones)$Name, "\n")
cat("N zones:", nrow(zones), "\n")

# Reproject zones to raster CRS if needed
zones_proj <- st_transform(zones, crs(r))

# ── Plot (raw vs flipped) ──
r1_flipped <- flip(r1, direction = "vertical")

par(mfrow = c(1, 2))
plot(r1, main = "Raw (as stored in NetCDF)")
plot(st_geometry(zones_proj), add = TRUE, border = "red", lwd = 0.5)

plot(r1_flipped, main = "Flipped (vertical)")
plot(st_geometry(zones_proj), add = TRUE, border = "red", lwd = 0.5)
par(mfrow = c(1, 1))

# ── Compare with SPARTACUS (optional) ──
sp_file <- "D:/OneDrive - Universit\u00e4t f\u00fcr Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TN/SPARTACUS2-DAILY_TN_2020.nc"
if (file.exists(sp_file)) {
  sp <- rast(sp_file)
  cat("\n--- SPARTACUS vs WINFORE ---\n")
  cat("Same grid:", all(res(r) == res(sp)), "\n")
  cat("Same extent:", all(as.vector(ext(r)) == as.vector(ext(sp))), "\n")
  cat("Same CRS:", crs(r, proj = TRUE) == crs(sp, proj = TRUE), "\n")
  cat("Same nrow/ncol:", nrow(r) == nrow(sp), ncol(r) == ncol(sp), "\n")
}
