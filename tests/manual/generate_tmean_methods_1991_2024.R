# Test script: Compare Tmean calculation methods
# Compares simple, dall_amico, and parton_logan methods with station data

library(sf)
library(ggplot2)
library(data.table)
devtools::load_all()

# ── Paths ──
shp_path <- "D:/temp/T_NZ_complex/shp/wildalpen_zones_cosero_parameters.shp"
tmin_dir <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TN"
tmax_dir <- "D:/OneDrive - Universität für Bodenkultur Wien/DROBAUT_WTZ_Brasilien/Analysis/download_geosphere/raw/SPARTACUS_Daily/TX"
output_dir <- "D:/temp/T_NZ_complex/output"
station_file <- "D:/temp/T_NZ_complex/stationdata_geosphere_2020/Messstationen Stundendaten v2 Datensatz_20200101T0000_20201231T0000.csv"

# ── Load zones ──
zones <- st_read(shp_path, quiet = TRUE)
cat("Loaded", nrow(zones), "zones\n")

# ── Process with all three methods (use 1 year for testing) ──
years <- 1991:2024

cat("\n--- Method 1: Simple (50/50) ---\n")
write_spartacus_temp(tmin_dir, tmax_dir, output_dir, zones,
                     tmean_method = "simple", tmin_weight = 0.5,
                     years = years, n_cores = 8,
                    write_binary = TRUE)
file.rename(file.path(output_dir, "T_NZ_1991_2024.txt"),
            file.path(output_dir, "T_NZ_1991_2024_simple.txt"))
file.rename(file.path(output_dir, "T_NZ_1991_2024.bin"),
            file.path(output_dir, "T_NZ_1991_2024_simple.bin"))

cat("\n--- Method 2: Dall'Amico ---\n")
write_spartacus_temp(tmin_dir, tmax_dir, output_dir, zones,
                     tmean_method = "dall_amico",
                     years = years, n_cores = 8,
                    write_binary = TRUE)
file.rename(file.path(output_dir, "T_NZ_1991_2024.txt"),
            file.path(output_dir, "T_NZ_1991_2024_dall_amico.txt"))
file.rename(file.path(output_dir, "T_NZ_1991_2024.bin"),
            file.path(output_dir, "T_NZ_1991_2024_dall_amico.bin"))


cat("\n--- Method 3: Parton-Logan ---\n")
write_spartacus_temp(tmin_dir, tmax_dir, output_dir, zones,
                     tmean_method = "parton_logan",
                     years = years, n_cores = 8,
                    write_binary = TRUE)
file.rename(file.path(output_dir, "T_NZ_1991_2024.txt"),
            file.path(output_dir, "T_NZ_1991_2024_parton_logan.txt"))
file.rename(file.path(output_dir, "T_NZ_1991_2024.bin"),
            file.path(output_dir, "T_NZ_1991_2024_parton_logan.bin"))

# ── Read results and compute daily mean across all zones ──
read_tmean <- function(file) {
  dt <- fread(file, col.names = c("Y", "M", "D", "H", "Min", paste0("Z", 1:(ncol(fread(file, nrows = 1)) - 5))))
  dt[, Date := as.Date(paste(Y, M, D, sep = "-"))]
  dt[, Tmean := rowMeans(.SD), .SDcols = patterns("^Z")]
  dt[, .(Date, Tmean)]
}

df_simple <- read_tmean(file.path(output_dir, "T_simple.txt"))[, Method := "Simple (Tmin+Tmax)/2"]
df_dall <- read_tmean(file.path(output_dir, "T_dall_amico.txt"))[, Method := "Dall'Amico (2006)"]
df_parton <- read_tmean(file.path(output_dir, "T_parton_logan.txt"))[, Method := "Parton-Logan (1981)"]

df_all <- rbindlist(list(df_simple, df_dall, df_parton))

# ── Read station data and compute daily Tmean from hourly observations ──
if (file.exists(station_file)) {
  cat("\n--- Loading station data ---\n")
  st <- fread(station_file, sep = ",")
  st <- st[!is.na(tl)]
  st[, datetime := as.POSIXct(time, format = "%Y-%m-%dT%H:%M", tz = "UTC")]
  st[, Date := as.Date(datetime)]

  # Daily mean per station (keep separate)
  df_station <- st[, .(Tmean = mean(tl, na.rm = TRUE)), by = .(Date, station)]
  df_station[, Method := paste0("Station ", station, " (hourly)")]

  df_all <- rbindlist(list(df_all, df_station[, .(Date, Tmean, Method)]), fill = TRUE)
  cat("Stations:", paste(unique(df_station$station), collapse = ", "), "\n")
}

# ── Calculate differences ──
df_wide <- dcast(df_all, Date ~ Method, value.var = "Tmean")
# Clean column names
clean_names <- gsub("[^a-zA-Z0-9]", "_", names(df_wide))
clean_names <- gsub("_+", "_", clean_names)
clean_names <- gsub("_$|^_", "", clean_names)
clean_names <- gsub("Simple_Tmin_Tmax_2", "Simple", clean_names)
clean_names <- gsub("Dall_Amico_2006", "Dall_Amico", clean_names)
clean_names <- gsub("Parton_Logan_1981", "Parton_Logan", clean_names)
clean_names <- gsub("Station_1_hourly", "Station_1", clean_names)
clean_names <- gsub("Station_7221_hourly", "Station_7221", clean_names)
setnames(df_wide, clean_names)

df_wide[, Diff_Dall := Simple - Dall_Amico]
df_wide[, Diff_Parton := Simple - Parton_Logan]

cat("\n--- Summary: Simple minus alternative methods (°C) ---\n")
cat(sprintf("Dall'Amico:   mean = %.2f, range = [%.2f, %.2f]\n",
            mean(df_wide$Diff_Dall, na.rm = TRUE), min(df_wide$Diff_Dall, na.rm = TRUE), max(df_wide$Diff_Dall, na.rm = TRUE)))
cat(sprintf("Parton-Logan: mean = %.2f, range = [%.2f, %.2f]\n",
            mean(df_wide$Diff_Parton, na.rm = TRUE), min(df_wide$Diff_Parton, na.rm = TRUE), max(df_wide$Diff_Parton, na.rm = TRUE)))

# Compare methods to each station
station_cols <- grep("^Station_", names(df_wide), value = TRUE)
if (length(station_cols) > 0) {
  cat("\n--- Summary: Method minus Station (°C) ---\n")
  for (st_col in station_cols) {
    cat(sprintf("\n%s:\n", st_col))
    for (method in c("Simple", "Dall_Amico", "Parton_Logan")) {
      if (method %in% names(df_wide)) {
        diff <- df_wide[[method]] - df_wide[[st_col]]
        cat(sprintf("  %s: bias = %.2f, RMSE = %.2f\n", method,
                    mean(diff, na.rm = TRUE), sqrt(mean(diff^2, na.rm = TRUE))))
      }
    }
  }
}

# ── Plot ──
p1 <- ggplot(df_all, aes(x = Date, y = Tmean, color = Method)) +
  geom_line(alpha = 0.7) +
  labs(title = "Daily Mean Temperature by Method", y = "Tmean (°C)") +
  theme_bw() +
  theme(legend.position = "bottom")

df_diff <- melt(df_wide, id.vars = "Date", measure.vars = c("Diff_Dall", "Diff_Parton"),
                variable.name = "Comparison", value.name = "Difference")
df_diff[, Comparison := fifelse(Comparison == "Diff_Dall", "Simple - Dall'Amico", "Simple - Parton-Logan")]

p2 <- ggplot(df_diff, aes(x = Date, y = Difference, color = Comparison)) +
  geom_line(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Temperature Difference: Simple vs Alternative Methods", y = "Difference (°C)") +
  theme_bw() +
  theme(legend.position = "bottom")

p3 <- ggplot(df_diff, aes(x = Difference, fill = Comparison)) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  labs(title = "Distribution of Temperature Differences", x = "Difference (°C)") +
  theme_bw() +
  theme(legend.position = "bottom")

p4 <- ggplot(df_all, aes(x = Tmean, color = Method, fill = Method)) +
  geom_density(alpha = 0.3) +
  labs(title = "Distribution of Daily Mean Temperature by Method",
       x = "Tmean (°C)", y = "Density") +
  theme_bw() +
  theme(legend.position = "bottom")

# ── Save PDF ──
pdf_file <- file.path(output_dir, "tmean_method_comparison.pdf")
pdf(pdf_file, width = 10, height = 12)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()
cat("\nPDF saved to:", pdf_file, "\n")
