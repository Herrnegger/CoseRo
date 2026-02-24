# Generate COSERO QOBS valid files from eHYD downloaded raw CSV data
# Method: Reads all raw CSV files in Q_Tagesmittel_*.csv format from eHYD.
# Output: D:/OneDrive - Universität für Bodenkultur Wien/SurfaceHydrology/Quarto/Q_ehyd/Qobs_COSERO/Qobs.txt

library(data.table)
library(ggplot2)
devtools::load_all()

# ── Paths ──────────────────────────────────────────────────────────────────────
input_dir      <- "D:/temp/Qobs_ehyd/raw"
output_file    <- "D:/temp/Qobs_ehyd/Qobs_COSERO/Qobs.txt"

# ── Gauge Mapping ──────────────────────────────────────────────────────────────
# Map HZB-Nummern from eHYD (extracted from filenames) directly to COSERO 
# NB (Subbasin) Indices.
gauge_mapping <- c(
  "210864" = 1,  # Gusswerk / Salza 
  "210880" = 2,  # Weichselboden / Radmerbach
  "210898" = 3   # Wildalpen / Salza 
)

# ── Process Output ─────────────────────────────────────────────────────────────
cat("\n--- Processing eHYD discharge (QOBS) data ---\n")
qobs_data <- write_ehyd_qobs(
  input_dir      = input_dir,
  output_file    = output_file,
  gauge_to_nb    = gauge_mapping,
  catchment_name = "COSERO-Wildalpen"
)

cat("\nFile saved to:", output_file, "\n")

# ── Verify Output Format ───────────────────────────────────────────────────────
# Quickly review output by reading the QOBS file structure
cat("\n--- Reading output file for verification plots ---\n")
lines <- readLines(output_file, n = 5)
cat("First 5 lines of the output file:\n")
cat(paste(lines, collapse = "\n"), "\n\n")

# Prepare for visualizations
qobs_dt <- setDT(qobs_data)
# Create Date 
qobs_dt[, Date := as.Date(paste(YYYY, MM, DD, sep = "-"))]

# Subbasin columns are named QOBS_1, QOBS_2, etc. (3 total mapped)
qobs_long <- melt(qobs_dt, id.vars = "Date", 
                  measure.vars = patterns("^QOBS_"),
                  variable.name = "Subbasin", 
                  value.name = "Discharge")

# Filter out the na_value code (-0.01) for plotting logic
qobs_long_valid <- qobs_long[Discharge > -0.01]

# ── Summary statistics ─────────────────────────────────────────────────────────
cat("\n--- Summary statistics (ignoring -0.01 NAs) ---\n")
agg_stats <- qobs_long_valid[, .(
  Mean = mean(Discharge),
  Min  = min(Discharge),
  Max  = max(Discharge),
  N_Valid = .N
), by = Subbasin]
print(agg_stats)

# ── Generate plots ─────────────────────────────────────────────────────────────
cat("\n--- Generating verification plots ---\n")

# Discard zero-days inside plots
p_ts <- ggplot(qobs_long_valid, aes(x = Date, y = Discharge, color = Subbasin)) +
  geom_line(linewidth = 0.4, alpha = 0.8) +
  labs(title = "eHYD Discharge — Daily Values per Subbasin",
       y = "Discharge (m3/s)", x = NULL) +
  theme_bw() +
  facet_wrap(~Subbasin, ncol = 1, scales = "free_y") +
  theme(legend.position = "none")

# Boxplots for distributions
p_box <- ggplot(qobs_long_valid, aes(x = Subbasin, y = Discharge, fill = Subbasin)) +
  geom_boxplot(alpha = 0.6, outlier.size = 0.5) +
  labs(title = "eHYD Discharge — Distribution per Subbasin (full period)",
       y = "Discharge (m3/s)", x = "Subbasin") +
  theme_bw()

# ── Save PDF ───────────────────────────────────────────────────────────────────
output_dir <- dirname(output_file)
pdf_file   <- file.path(output_dir, "eHYD_QOBS_verification.pdf")
pdf(pdf_file, width = 10, height = 6)
print(p_ts)
print(p_box)
dev.off()

cat("\nVerification PDF saved to:", pdf_file, "\n")
