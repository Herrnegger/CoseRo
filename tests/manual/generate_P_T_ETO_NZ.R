# Generate COSERO meteorological input files: precipitation (P), temperature (T), ET0
# Method: Dall'Amico (2006) for Tmean; time_shift = TRUE for P and T; no shift for ET0
# Period: 1991-2024
# Output: D:/temp/P_T_ETO_NZ/

library(sf)
library(ggplot2)
library(data.table)
devtools::load_all()

# ── Paths ──────────────────────────────────────────────────────────────────────
shp_path   <- "D:/temp/T_NZ_complex/shp/wildalpen_zones_cosero_parameters.shp"
rr_dir     <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/RR"
tmin_dir   <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TN"
tmax_dir   <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TX"
et0_dir    <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/WINFORE_Daily/ET0"
output_dir <- "D:/temp/P_T_ETO_NZ"

years   <- 1991:2024
n_cores <- 6
nz_col  <- "NZ"

# ── Load zones ─────────────────────────────────────────────────────────────────
zones   <- st_read(shp_path, quiet = TRUE)
n_zones <- nrow(zones)
cat("Loaded", n_zones, "zones\n")

# ── Process precipitation (P) ──────────────────────────────────────────────────
cat("\n--- Processing precipitation (RR) ---\n")
out_p <- write_spartacus_precip(
  nc_dir       = rr_dir,
  output_dir   = output_dir,
  model_zones  = zones,
  nz_col       = nz_col,
  years        = years,
  n_cores      = n_cores,
  write_binary = TRUE,
  time_shift   = TRUE
)

# ── Process temperature (T, Dall'Amico method) ─────────────────────────────────
cat("\n--- Processing temperature (Dall'Amico) ---\n")
out_t <- write_spartacus_temp(
  tmin_dir     = tmin_dir,
  tmax_dir     = tmax_dir,
  output_dir   = output_dir,
  model_zones  = zones,
  nz_col       = nz_col,
  years        = years,
  tmean_method = "dall_amico",
  n_cores      = n_cores,
  write_binary = TRUE,
  time_shift   = TRUE
)

# ── Process ET0 (WINFORE, no time shift) ───────────────────────────────────────
cat("\n--- Processing ET0 (WINFORE) ---\n")
out_et0 <- write_winfore_et0(
  nc_dir       = et0_dir,
  output_dir   = output_dir,
  model_zones  = zones,
  nz_col       = nz_col,
  years        = years,
  n_cores      = n_cores,
  write_binary = TRUE
)

cat("\nAll files saved to:", output_dir, "\n")

# ── Helper: read COSERO-format txt and return long-format data.table ───────────
read_cosero_txt <- function(file, var_name) {
  dt <- fread(file)
  nz <- ncol(dt) - 5L
  setnames(dt, c("Y", "M", "D", "H", "Min", paste0("Z", seq_len(nz))))
  dt[, Date := as.Date(paste(Y, M, D, sep = "-"))]
  # Compute spatial mean across all zones
  dt[, Mean := rowMeans(.SD), .SDcols = patterns("^Z")]
  # Long format for per-zone plots
  dt_long <- melt(dt, id.vars = "Date",
                  measure.vars = paste0("Z", seq_len(nz)),
                  variable.name = "Zone", value.name = var_name)
  list(wide = dt, long = dt_long)
}

# ── Read outputs ───────────────────────────────────────────────────────────────
cat("\n--- Reading output files for verification plots ---\n")

p_file   <- out_p$txt
t_file   <- out_t$txt
et0_file <- out_et0$txt

res_p   <- read_cosero_txt(p_file,   "P")
res_t   <- read_cosero_txt(t_file,   "T")
res_et0 <- read_cosero_txt(et0_file, "ET0")

# ── Summary statistics ─────────────────────────────────────────────────────────
cat("\n--- Summary statistics ---\n")
cat(sprintf("P   (mm/day):  mean = %.2f  min = %.2f  max = %.2f\n",
            mean(res_p$wide$Mean),   min(res_p$wide$Mean),   max(res_p$wide$Mean)))
cat(sprintf("T   (°C):      mean = %.2f  min = %.2f  max = %.2f\n",
            mean(res_t$wide$Mean),   min(res_t$wide$Mean),   max(res_t$wide$Mean)))
cat(sprintf("ET0 (mm/day):  mean = %.2f  min = %.2f  max = %.2f\n",
            mean(res_et0$wide$Mean), min(res_et0$wide$Mean), max(res_et0$wide$Mean)))

# ── Plot helpers ───────────────────────────────────────────────────────────────
# Time series of spatial mean
plot_timeseries <- function(dt_wide, var_name, unit, color) {
  ggplot(dt_wide, aes(x = Date, y = Mean)) +
    geom_line(color = color, linewidth = 0.4, alpha = 0.8) +
    labs(title = sprintf("%s — Daily Spatial Mean (all zones)", var_name),
         y = sprintf("%s (%s)", var_name, unit), x = NULL) +
    theme_bw()
}

# Monthly boxplot of spatial mean
plot_monthly_box <- function(dt_wide, var_name, unit, color) {
  dt <- copy(dt_wide)
  dt[, Month := factor(M, labels = month.abb)]
  ggplot(dt, aes(x = Month, y = Mean)) +
    geom_boxplot(fill = color, alpha = 0.4, outlier.size = 0.5) +
    labs(title = sprintf("%s — Monthly Distribution", var_name),
         y = sprintf("%s (%s)", var_name, unit), x = NULL) +
    theme_bw()
}

# Time series per zone (thin lines, one color per zone)
plot_per_zone <- function(dt_long, var_name, unit) {
  ggplot(dt_long, aes(x = Date, y = .data[[var_name]], color = Zone)) +
    geom_line(linewidth = 0.25, alpha = 0.6) +
    labs(title = sprintf("%s — Daily Values per Zone", var_name),
         y = sprintf("%s (%s)", var_name, unit), x = NULL) +
    theme_bw() +
    theme(legend.position = "none")  # Too many zones for a legend
}

# Boxplot of zone means (distribution across zones)
plot_zone_box <- function(dt_long, var_name, unit) {
  dt_zone_mean <- dt_long[, .(ZoneMean = mean(.SD[[1]], na.rm = TRUE)),
                           .SDcols = var_name, by = Zone]
  ggplot(dt_long, aes(x = Zone, y = .data[[var_name]])) +
    geom_boxplot(fill = "grey70", alpha = 0.5, outlier.size = 0.3) +
    labs(title = sprintf("%s — Distribution per Zone (full period)", var_name),
         y = sprintf("%s (%s)", var_name, unit), x = "Zone") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7))
}

# ── Generate plots ─────────────────────────────────────────────────────────────
cat("\n--- Generating verification plots ---\n")

# Precipitation
p_ts    <- plot_timeseries(res_p$wide, "Precipitation", "mm/day", "steelblue")
p_box_m <- plot_monthly_box(res_p$wide, "Precipitation", "mm/day", "steelblue")
p_zones <- plot_per_zone(res_p$long, "P", "mm/day")
p_box_z <- plot_zone_box(res_p$long, "P", "mm/day")

# Temperature
t_ts    <- plot_timeseries(res_t$wide, "Temperature (Tmean)", "°C", "firebrick")
t_box_m <- plot_monthly_box(res_t$wide, "Temperature (Tmean)", "°C", "firebrick")
t_zones <- plot_per_zone(res_t$long, "T", "°C")
t_box_z <- plot_zone_box(res_t$long, "T", "°C")

# ET0
e_ts    <- plot_timeseries(res_et0$wide, "ET0", "mm/day", "darkgreen")
e_box_m <- plot_monthly_box(res_et0$wide, "ET0", "mm/day", "darkgreen")
e_zones <- plot_per_zone(res_et0$long, "ET0", "mm/day")
e_box_z <- plot_zone_box(res_et0$long, "ET0", "mm/day")

# ── Save PDF ───────────────────────────────────────────────────────────────────
pdf_file <- file.path(output_dir, "P_T_ETO_NZ_verification.pdf")
pdf(pdf_file, width = 12, height = 6)

# Precipitation (4 pages)
print(p_ts)
print(p_box_m)
print(p_zones)
print(p_box_z)

# Temperature (4 pages)
print(t_ts)
print(t_box_m)
print(t_zones)
print(t_box_z)

# ET0 (4 pages)
print(e_ts)
print(e_box_m)
print(e_zones)
print(e_box_z)

dev.off()
cat("\nVerification PDF saved to:", pdf_file, "\n")
