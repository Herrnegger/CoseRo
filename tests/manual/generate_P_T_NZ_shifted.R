# Generate precipitation and temperature input for COSERO
# Method: Dall'Amico (2006) for Tmean, time_shift = TRUE for both
# Period: 1991-2024
# Output: D:/temp/P_T_NZ_shifted/

library(sf)
devtools::load_all()

# ── Paths ──
shp_path   <- "D:/temp/T_NZ_complex/shp/wildalpen_zones_cosero_parameters.shp"
nc_dir     <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/RR"
tmin_dir   <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TN"
tmax_dir   <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TX"
output_dir <- "D:/temp/P_T_NZ_shifted"

years <- 1991:2024

# ── Load zones ──
zones <- st_read(shp_path, quiet = TRUE)
cat("Loaded", nrow(zones), "zones\n")

# ── Precipitation ──
cat("\n--- Processing precipitation (RR) ---\n")
write_spartacus_precip(
  nc_dir      = nc_dir,
  output_dir  = output_dir,
  model_zones = zones,
  years       = years,
  n_cores     = 8,
  write_binary = TRUE,
  time_shift  = TRUE
)

# ── Temperature (Dall'Amico method) ──
cat("\n--- Processing temperature (Dall'Amico) ---\n")
write_spartacus_temp(
  tmin_dir     = tmin_dir,
  tmax_dir     = tmax_dir,
  output_dir   = output_dir,
  model_zones  = zones,
  years        = years,
  tmean_method = "dall_amico",
  n_cores      = 8,
  write_binary = TRUE,
  time_shift   = TRUE
)

cat("\nDone. Files saved to:", output_dir, "\n")
