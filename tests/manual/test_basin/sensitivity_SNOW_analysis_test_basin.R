# =============================================================================
# COSERO SNOW SENSITIVITY ANALYSIS - Test Basin (D:/temp/COSERO_test)
# =============================================================================
# Complete workflow: Parameter sampling → Model runs → Analysis → Visualization
# Data period: 2009-01-01 to 2020-12-31
# Warm start with 2-year spinup
# Snow parameters: CTMIN, CTMAX, NVAR, RAINTRT, SNOWTRT
# =============================================================================

# Load package
devtools::load_all("D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R")
library(ggplot2)
library(dplyr)
library(tidyr)

# Memory optimization
options(expressions = 500000)
gc(full = TRUE, verbose = FALSE)

cat("=============================================================================\n")
cat("COSERO SNOW SENSITIVITY ANALYSIS - Test Basin\n")
cat("=============================================================================\n\n")

# =============================================================================
# CONFIGURATION
# =============================================================================

# Project paths
project_path <- "D:/temp/COSERO_test"
param_file <- file.path(project_path, "input", "para_ini.txt")

# Check for statevar.dmp
statevar_input <- file.path(project_path, "input", "statevar.dmp")
statevar_output <- file.path(project_path, "output", "statevar.dmp")

# Copy statevar.dmp from output to input if needed

if (!file.exists(statevar_input) && file.exists(statevar_output)) {
  cat("Copying statevar.dmp from output to input folder...\n")
  file.copy(statevar_output, statevar_input, overwrite = TRUE)
  cat("✓ statevar.dmp copied\n\n")
} else if (file.exists(statevar_input)) {
  cat("✓ statevar.dmp found in input folder\n\n")
} else {
  warning("statevar.dmp not found! Will use cold start (parameter file initialization)")
}

# Sensitivity analysis configuration
n_samples <- 50  # Small sample for testing (increase to 100-500 for production)

# Snow parameters to analyze
param_names <- c("CTMIN", "CTMAX", "NVAR", "RAINTRT", "SNOWTRT")

# Subbasin for analysis
plot_subbasin <- "0003"

# Visualization settings
viz_dpi <- 300  # 300 for quick review, 600 for publication
viz_width <- 14
viz_height <- 10
viz_sobol_width <- 12
viz_sobol_height <- 7

cat("Configuration:\n")
cat("  Project:", project_path, "\n")
cat("  Parameter file:", basename(param_file), "\n")
cat("  Sample size:", n_samples, "(Total simulations:", n_samples * (length(param_names) + 2), ")\n")
cat("  Parameters:", paste(param_names, collapse=", "), "\n")
cat("  Target subbasin:", plot_subbasin, "\n")
cat("  Date range: 2009-01-01 to 2020-12-31\n")
cat("  Spinup: 730 days (2 years)\n")
cat("  Warm start:", file.exists(statevar_input), "\n")
cat("  Visualization:", viz_dpi, "DPI\n\n")

# Validate paths
if (!file.exists(project_path)) stop("❌ Project path not found!")
if (!file.exists(param_file)) stop("❌ Parameter file not found!")
if (!file.exists(file.path(project_path, "COSERO.exe"))) stop("❌ COSERO.exe not found!")
cat("✓ All paths validated\n\n")

# =============================================================================
# STEP 1: DEFINE PARAMETER BOUNDS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 1: PARAMETER BOUNDS DEFINITION\n")
cat("=============================================================================\n\n")

# Load parameter bounds from package CSV file
# This loads the default bounds for snow parameters from parameter_bounds.csv
# The CSV contains:
#   - CTMIN: Minimum snow melt factor 
#   - CTMAX: Maximum snow melt factor 
#   - NVAR: Variance for creating new snow 
#   - RAINTRT: Transition temperature for pure rain 
#   - SNOWTRT: Transition temperature for pure snow 

bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameter bounds defined:\n")
print(bounds)
cat("\n✓ Bounds successfully created\n\n")

# =============================================================================
# STEP 2: GENERATE SOBOL SAMPLES
# =============================================================================

cat("=============================================================================\n")
cat("STEP 2: SOBOL SAMPLE GENERATION\n")
cat("=============================================================================\n\n")

# Convert bounds to Sobol format
sobol_bounds <- create_sobol_bounds(bounds)

cat("Generating Sobol quasi-random samples...\n")
cat("  Base sample size: n =", n_samples, "\n")
cat("  Number of parameters: k =", length(param_names), "\n")
cat("  Total parameter sets: n × (k+2) =", n_samples * (length(param_names) + 2), "\n\n")

# Generate samples
set.seed(42)  # For reproducibility
samples <- generate_sobol_samples(sobol_bounds, n = n_samples)

cat("✓ Samples generated successfully\n")
cat("  Dimensions:", nrow(samples$parameter_sets), "sets ×", ncol(samples$parameter_sets), "parameters\n\n")

# Verify sample properties
cat("Sample validation:\n")
for (param in param_names) {
  values <- samples$parameter_sets[[param]]
  cat(sprintf("  %-8s: range [%.3f, %.3f], mean = %.3f\n",
              param, min(values), max(values), mean(values)))
}
cat("\n")

# =============================================================================
# STEP 3: SAVE CONFIGURATION
# =============================================================================

cat("=============================================================================\n")
cat("STEP 3: SAVE CONFIGURATION\n")
cat("=============================================================================\n\n")

# Create results directory
results_base_dir <- file.path(project_path, "sensitivity_results")
dir.create(results_base_dir, recursive = TRUE, showWarnings = FALSE)

# Save configuration
config_file <- file.path(results_base_dir, "config.rds")
saveRDS(list(
  n_samples = n_samples,
  param_names = param_names,
  bounds = bounds,
  sobol_bounds = sobol_bounds,
  samples = samples,
  plot_subbasin = plot_subbasin,
  project_path = project_path,
  param_file = param_file,
  generated_date = Sys.time(),
  total_simulations = nrow(samples$parameter_sets)
), file = config_file)

cat("✓ Configuration saved:", config_file, "\n")
cat("  File size:", round(file.info(config_file)$size / 1024, 2), "KB\n\n")

# =============================================================================
# STEP 4: RUN SEQUENTIAL MONTE CARLO SIMULATIONS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 4: SEQUENTIAL MONTE CARLO SIMULATIONS\n")
cat("=============================================================================\n\n")

n_total <- nrow(samples$parameter_sets)
cat("⚠ WARNING: About to run", n_total, "sequential COSERO simulations\n")
cat("Estimated runtime:", round(n_total * 5 / 60, 1), "minutes (assuming 2 sec/run)\n")
cat("Memory management: Batch processing (10 runs/batch) to prevent crashes\n\n")

# Memory cleanup before starting
gc(full = TRUE)
closeAllConnections()

# BATCHED SEQUENTIAL EXECUTION
batch_size <- 10
n_batches <- ceiling(n_total / batch_size)

cat("\nStarting batched sequential execution:\n")
cat(sprintf("  Total runs: %d\n", n_total))
cat(sprintf("  Batch size: %d\n", batch_size))
cat(sprintf("  Number of batches: %d\n\n", n_batches))

# Create temporary batch directory
batch_dir <- file.path(results_base_dir, "batches")
if (dir.exists(batch_dir)) unlink(batch_dir, recursive = TRUE)
dir.create(batch_dir, showWarnings = FALSE)

# Determine statevar_source based on file existence
statevar_source <- if (file.exists(statevar_input)) 2 else 1
cat("State variable source:", if(statevar_source == 2) "Warm start (statevar.dmp)" else "Cold start (parameter file)", "\n\n")

# Execute batches
time_start <- Sys.time()
batch_times <- numeric(n_batches)

for (batch_idx in 1:n_batches) {
  batch_start <- Sys.time()

  start_idx <- (batch_idx - 1) * batch_size + 1
  end_idx <- min(batch_idx * batch_size, n_total)
  batch_samples <- samples$parameter_sets[start_idx:end_idx, ]

  cat(sprintf("Batch %d/%d (simulations %d-%d)... ",
              batch_idx, n_batches, start_idx, end_idx))

  batch_results <- run_cosero_ensemble(
    project_path = project_path,
    parameter_sets = batch_samples,
    par_bounds = bounds,
    base_settings = list(
      STARTDATE = "2009 1 1 0 0",
      ENDDATE = "2020 12 31 0 0",
      SPINUP = 730,  # 2 years
      OUTPUTTYPE = 1
    ),
    statevar_source = statevar_source,  # Use warm start if available
    quiet = TRUE
  )

  # Save batch immediately
  batch_file <- file.path(batch_dir, sprintf("batch_%04d.rds", batch_idx))
  saveRDS(batch_results, batch_file)

  # Calculate time for this batch
  batch_times[batch_idx] <- as.numeric(difftime(Sys.time(), batch_start, units = "secs"))

  # Display progress with time estimation
  if (batch_idx == 1) {
    avg_batch_time <- batch_times[1]
    cat(sprintf("✓ (%.1fs) | Est. total: %.1f min\n",
                batch_times[1], (avg_batch_time * n_batches) / 60))
  } else {
    avg_batch_time <- mean(batch_times[1:batch_idx], na.rm = TRUE)
    est_remaining <- avg_batch_time * (n_batches - batch_idx)

    if (est_remaining < 60) {
      time_str <- sprintf("%.0fs", est_remaining)
    } else {
      time_str <- sprintf("%.1f min", est_remaining / 60)
    }

    cat(sprintf("✓ (%.1fs) | %.0f%% done | Remaining: %s\n",
                batch_times[batch_idx],
                (batch_idx / n_batches) * 100,
                time_str))
  }

  # Clean up memory
  rm(batch_results)
  closeAllConnections()
  gc(full = TRUE)
}

time_elapsed <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

cat(sprintf("\n✓ All batches complete: %.1f sec (%.2f sec/run)\n\n",
            time_elapsed, time_elapsed / n_total))

# Combine batches
cat("Combining batch results...")
batch_files <- list.files(batch_dir, pattern = "^batch_.*\\.rds$", full.names = TRUE)

if (length(batch_files) == 0) {
  stop("❌ No batch files found!")
}

results_seq <- readRDS(batch_files[1])

if (length(batch_files) > 1) {
  for (i in 2:length(batch_files)) {
    cat(sprintf("\r  Combining batch %d/%d...", i, length(batch_files)))
    flush.console()
    batch_i <- readRDS(batch_files[i])
    results_seq$results <- c(results_seq$results, batch_i$results)
    results_seq$parameter_sets <- rbind(results_seq$parameter_sets, batch_i$parameter_sets)
    rm(batch_i)
    gc(full = TRUE)
  }
  cat("\n")
}

results_seq$runtime_minutes <- time_elapsed / 60
cat(" ✓\n\n")

# Save complete results
results_file <- file.path(results_base_dir, "ensemble_results.rds")
saveRDS(results_seq, file = results_file)
cat("✓ Complete results saved:", results_file, "\n")
cat("  File size:", round(file.info(results_file)$size / 1024^2, 2), "MB\n")
cat("  Runtime:", round(results_seq$runtime_minutes, 2), "minutes\n\n")

# Cleanup batch directory
unlink(batch_dir, recursive = TRUE)

# =============================================================================
# STEP 5: EXTRACT PERFORMANCE METRICS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 5: PERFORMANCE METRICS EXTRACTION\n")
cat("=============================================================================\n\n")

cat("Extracting metrics for subbasin", plot_subbasin, "...\n")

# Extract NSE and KGE from COSERO statistics
nse_seq <- extract_cosero_metrics(results_seq, subbasin_id = plot_subbasin,
                                   metric = "NSE", warn_nan = TRUE)
kge_seq <- extract_cosero_metrics(results_seq, subbasin_id = plot_subbasin,
                                   metric = "KGE", warn_nan = FALSE)

# Calculate metrics with spinup
nse_calc <- calculate_ensemble_metrics(results_seq, subbasin_id = plot_subbasin,
                                        metric = "NSE", spinup = 730)
kge_calc <- calculate_ensemble_metrics(results_seq, subbasin_id = plot_subbasin,
                                        metric = "KGE", spinup = 730)

cat("\nMetrics summary:\n")
cat(sprintf("  NSE: %d/%d valid (mean=%.4f, median=%.4f, sd=%.4f)\n",
            sum(!is.na(nse_seq)), length(nse_seq),
            mean(nse_seq, na.rm=TRUE), median(nse_seq, na.rm=TRUE), sd(nse_seq, na.rm=TRUE)))
cat(sprintf("  KGE: %d/%d valid (mean=%.4f, median=%.4f, sd=%.4f)\n\n",
            sum(!is.na(kge_seq)), length(kge_seq),
            mean(kge_seq, na.rm=TRUE), median(kge_seq, na.rm=TRUE), sd(kge_seq, na.rm=TRUE)))

# =============================================================================
# STEP 6: CONVERT TO ABSOLUTE PARAMETER VALUES
# =============================================================================

cat("=============================================================================\n")
cat("STEP 6: PARAMETER VALUE CONVERSION\n")
cat("=============================================================================\n\n")

# Read original parameter values
original_all <- read_parameter_table(param_file, param_names, zone_id = "all")

# Convert relative/absolute changes to actual values
cat("Converting sampled values to absolute parameter values...\n")
absolute_params <- as.data.frame(samples$parameter_sets)

for (param in param_names) {
  # Target Value Strategy:
  # The sampled values ARE the absolute parameter values.
  absolute_params[[param]] <- samples$parameter_sets[[param]]

  # Clip to physical bounds (redundant if sampling was correct, but good for safety)
  param_bounds_row <- bounds[bounds$parameter == param, ]
  absolute_params[[param]] <- pmax(param_bounds_row$min,
                                   pmin(param_bounds_row$max,
                                        absolute_params[[param]]))
}


cat("✓ Conversion complete\n\n")
cat("Absolute parameter ranges:\n")
for (param in param_names) {
  cat(sprintf("  %-8s: [%.2f, %.2f]\n", param,
              min(absolute_params[[param]]), max(absolute_params[[param]])))
}
cat("\n")

# =============================================================================
# STEP 7: SOBOL SENSITIVITY ANALYSIS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 7: SOBOL SENSITIVITY INDICES\n")
cat("=============================================================================\n\n")

# Calculate Sobol indices for NSE
cat("Calculating Sobol sensitivity indices for NSE...\n")
sobol_indices_nse <- calculate_sobol_indices(nse_seq, samples, boot = TRUE, R = 100)
cat("✓ NSE Sobol indices calculated\n\n")
print(sobol_indices_nse)

# Calculate Sobol indices for KGE
cat("\nCalculating Sobol sensitivity indices for KGE...\n")
sobol_indices_kge <- calculate_sobol_indices(kge_calc, samples, boot = TRUE, R = 100)
cat("✓ KGE Sobol indices calculated\n\n")
print(sobol_indices_kge)

# =============================================================================
# STEP 8: VISUALIZATIONS
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 8: SENSITIVITY VISUALIZATIONS\n")
cat("=============================================================================\n\n")

plots_dir <- file.path(results_base_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# 8.1: NSE Sobol Indices
cat("Creating NSE Sobol indices plot...\n")
p_sobol_nse <- plot_sobol(sobol_indices_nse,
                           title = "Sobol Sensitivity Indices (NSE)")
ggsave(
  filename = file.path(plots_dir, "01_sobol_nse.png"),
  plot = p_sobol_nse,
  width = viz_sobol_width, height = viz_sobol_height, dpi = viz_dpi
)
cat("  ✓ Saved: 01_sobol_nse.png\n")

# 8.2: KGE Sobol Indices
cat("Creating KGE Sobol indices plot...\n")
p_sobol_kge <- plot_sobol(sobol_indices_kge,
                           title = "Sobol Sensitivity Indices (KGE)")
ggsave(
  filename = file.path(plots_dir, "02_sobol_kge.png"),
  plot = p_sobol_kge,
  width = viz_sobol_width, height = viz_sobol_height, dpi = viz_dpi
)
cat("  ✓ Saved: 02_sobol_kge.png\n")

# 8.3: NSE Dotty Plots
cat("Creating NSE dotty plots...\n")
p_dotty_nse <- plot_dotty(
  parameter_sets = absolute_params,
  Y = nse_seq,
  y_label = "NSE",
  n_col = 2,
  y_min = 0
)
ggsave(
  filename = file.path(plots_dir, "03_dotty_nse.png"),
  plot = p_dotty_nse,
  width = viz_width, height = viz_height, dpi = viz_dpi
)
cat("  ✓ Saved: 03_dotty_nse.png\n")

# 8.4: KGE Dotty Plots
cat("Creating KGE dotty plots...\n")
p_dotty_kge <- plot_dotty(
  parameter_sets = absolute_params,
  Y = kge_calc,
  y_label = "KGE",
  n_col = 2,
  y_min = 0
)
ggsave(
  filename = file.path(plots_dir, "04_dotty_kge.png"),
  plot = p_dotty_kge,
  width = viz_width, height = viz_height, dpi = viz_dpi
)
cat("  ✓ Saved: 04_dotty_kge.png\n")

# 8.5: NSE vs KGE Comparison
cat("Creating NSE vs KGE comparison plot...\n")
comparison_data <- data.frame(NSE = nse_seq, KGE = kge_calc) %>%
  filter(!is.na(NSE) & !is.na(KGE))

p_comparison <- ggplot(comparison_data, aes(x = NSE, y = KGE)) +
  geom_point(alpha = 0.5, size = 2, color = "#2E86AB") +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  theme_minimal(base_size = 14) +
  labs(
    title = "NSE vs KGE Performance Comparison",
    subtitle = sprintf("Correlation: r = %.3f, n = %d simulations",
                      cor(comparison_data$NSE, comparison_data$KGE),
                      nrow(comparison_data)),
    x = "Nash-Sutcliffe Efficiency (NSE)",
    y = "Kling-Gupta Efficiency (KGE)"
  )

ggsave(
  filename = file.path(plots_dir, "05_nse_vs_kge.png"),
  plot = p_comparison,
  width = 10, height = 8, dpi = viz_dpi
)
cat("  ✓ Saved: 05_nse_vs_kge.png\n")

cat("\n✓ All visualizations created\n")
cat("  Location:", plots_dir, "\n\n")

# =============================================================================
# STEP 9: EXPORT BEST PARAMETER SETS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 9: BEST PARAMETER SETS\n")
cat("=============================================================================\n\n")

best_params_dir <- file.path(results_base_dir, "best_params")
dir.create(best_params_dir, recursive = TRUE, showWarnings = FALSE)

# Find best NSE simulation
best_nse_idx <- which.max(nse_seq)
best_nse_value <- nse_seq[best_nse_idx]
best_nse_params <- as.list(absolute_params[best_nse_idx, ])

cat("Best NSE simulation:\n")
cat(sprintf("  Index: %d / %d\n", best_nse_idx, length(nse_seq)))
cat(sprintf("  NSE: %.4f, KGE: %.4f\n", best_nse_value, kge_calc[best_nse_idx]))
cat("  Parameter values:\n")
for (p in param_names) {
  cat(sprintf("    %-8s = %.4f\n", p, best_nse_params[[p]]))
}
cat("\n")

# Find best KGE simulation
best_kge_idx <- which.max(kge_calc)
best_kge_value <- kge_calc[best_kge_idx]
best_kge_params <- as.list(absolute_params[best_kge_idx, ])

cat("Best KGE simulation:\n")
cat(sprintf("  Index: %d / %d\n", best_kge_idx, length(kge_calc)))
cat(sprintf("  KGE: %.4f, NSE: %.4f\n", best_kge_value, nse_seq[best_kge_idx]))
cat("  Parameter values:\n")
for (p in param_names) {
  cat(sprintf("    %-8s = %.4f\n", p, best_kge_params[[p]]))
}
cat("\n")

# Export best NSE parameter file
cat("Exporting best NSE parameter file...\n")
best_nse_file <- file.path(best_params_dir, "para_BEST_NSE.txt")
file.copy(param_file, best_nse_file, overwrite = TRUE)
original_values <- read_parameter_table(param_file, param_names, zone_id = 1)
modify_parameter_table(
  par_file = best_nse_file,
  params = best_nse_params,
  par_bounds = bounds,
  original_values = original_values,
  zones = NULL
)
cat("  ✓ Saved:", best_nse_file, "\n")

# Export best KGE parameter file
cat("Exporting best KGE parameter file...\n")
best_kge_file <- file.path(best_params_dir, "para_BEST_KGE.txt")
file.copy(param_file, best_kge_file, overwrite = TRUE)
modify_parameter_table(
  par_file = best_kge_file,
  params = best_kge_params,
  par_bounds = bounds,
  original_values = original_values,
  zones = NULL
)
cat("  ✓ Saved:", best_kge_file, "\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("SENSITIVITY ANALYSIS COMPLETE\n")
cat("=============================================================================\n\n")

cat("✓ Analysis completed successfully\n\n")

cat("Key Results:\n")
cat("  • Total simulations:", n_total, "\n")
cat("  • Runtime:", round(results_seq$runtime_minutes, 2), "minutes\n")
cat("  • Best NSE:", round(best_nse_value, 4), "(simulation #", best_nse_idx, ")\n")
cat("  • Best KGE:", round(best_kge_value, 4), "(simulation #", best_kge_idx, ")\n\n")

cat("Output Directories:\n")
cat("  📁", results_base_dir, "\n")
cat("     ├── plots/ (5 sensitivity plots)\n")
cat("     ├── best_params/ (COSERO parameter files)\n")
cat("     ├── ensemble_results.rds\n")
cat("     └── config.rds\n\n")

cat("Next Steps:\n")
cat("  1. Review plots in:", plots_dir, "\n")
cat("  2. Use best parameter files for validation runs\n")
cat("  3. Adjust parameter bounds and re-run if needed\n\n")

cat("=============================================================================\n")
cat("Completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("=============================================================================\n")

# Memory cleanup
gc(full = TRUE)
closeAllConnections()
