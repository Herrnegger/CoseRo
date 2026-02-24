# Test script: WINFORE ET0 preprocessing
# Tests write_winfore_et0() with real data and validates output

library(sf)
library(ggplot2)
library(data.table)
devtools::load_all()

# ── Paths ──
shp_path <- "D:/temp/T_NZ_complex/shp/wildalpen_zones_cosero_parameters.shp"
et0_dir  <- "D:/temp/Geosphere_data/WINFORE_ET0"
output_dir <- "D:/temp/winfore_et0_test"

# ── Load zones ──
zones <- st_read(shp_path, quiet = TRUE)
cat("Loaded", nrow(zones), "zones\n")

# ── Process ET0 (single year for quick test) ──
cat("\n--- Processing WINFORE ET0 ---\n")
write_winfore_et0(
  nc_dir      = et0_dir,
  output_dir  = output_dir,
  model_zones = zones,
  nz_col      = "NZ",
  years       = 2025,
  n_cores     = 4,
  write_binary = TRUE
)

# ── Read and validate output ──
et0_file <- list.files(output_dir, pattern = "ET0_NZ.*\\.txt$", full.names = TRUE)
cat("\nOutput file:", et0_file, "\n")

dt <- fread(et0_file)
n_zones <- ncol(dt) - 5
cat("Columns:", ncol(dt), "(5 date + ", n_zones, " zones)\n")
cat("Rows:", nrow(dt), "\n")

# Rename columns for easier handling
setnames(dt, c("Y", "M", "D", "H", "Min", paste0("Z", seq_len(n_zones))))
dt[, Date := as.Date(paste(Y, M, D, sep = "-"))]

# Compute daily mean across all zones
dt[, ET0_mean := rowMeans(.SD), .SDcols = patterns("^Z")]

cat("\n--- ET0 Summary (mm/day) ---\n")
cat(sprintf("  Mean:   %.2f\n", mean(dt$ET0_mean, na.rm = TRUE)))
cat(sprintf("  Median: %.2f\n", median(dt$ET0_mean, na.rm = TRUE)))
cat(sprintf("  Min:    %.2f\n", min(dt$ET0_mean, na.rm = TRUE)))
cat(sprintf("  Max:    %.2f\n", max(dt$ET0_mean, na.rm = TRUE)))

# Check for reasonable range (ET0 should be 0-10 mm/day)
out_of_range <- dt[ET0_mean < 0 | ET0_mean > 15]
if (nrow(out_of_range) > 0) {
  cat("\n  WARNING:", nrow(out_of_range), "days with ET0 outside [0, 15] mm/day!\n")
} else {
  cat("\n  All values within expected range [0, 15] mm/day.\n")
}

# ── Plot ──
p1 <- ggplot(dt, aes(x = Date, y = ET0_mean)) +
  geom_line(color = "steelblue", linewidth = 0.5) +
  labs(title = "WINFORE ET0 - Daily Mean Across All Zones",
       y = "ET0 (mm/day)", x = NULL) +
  theme_bw()

# Monthly boxplot
dt[, Month := factor(M, labels = month.abb)]
p2 <- ggplot(dt, aes(x = Month, y = ET0_mean)) +
  geom_boxplot(fill = "steelblue", alpha = 0.3) +
  labs(title = "Monthly ET0 Distribution", y = "ET0 (mm/day)", x = NULL) +
  theme_bw()

# Plot per-zone comparison (first 6 zones)
n_show <- min(6, n_zones)
dt_long <- melt(dt, id.vars = "Date",
                measure.vars = paste0("Z", 1:n_show),
                variable.name = "Zone", value.name = "ET0")
p3 <- ggplot(dt_long, aes(x = Date, y = ET0, color = Zone)) +
  geom_line(alpha = 0.7, linewidth = 0.3) +
  labs(title = paste("ET0 per Zone (first", n_show, "zones)"),
       y = "ET0 (mm/day)", x = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Save plots
pdf_file <- file.path(output_dir, "winfore_et0_test.pdf")
pdf(pdf_file, width = 10, height = 10)
print(p1)
print(p2)
print(p3)
dev.off()
cat("\nPlots saved to:", pdf_file, "\n")
