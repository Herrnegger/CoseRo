# =============================================================================
# PART 1: RUN SIMULATIONS AND SAVE BATCHES
# =============================================================================
# This workflow generates Sobol samples and runs all COSERO simulations
# Results are saved as batches to prevent memory crashes
# After completion, use workflow_2_generate_results.R to analyze results
# =============================================================================

# Load required libraries
devtools::load_all()

# =============================================================================
# CONFIGURATION: Adjust these settings as needed
# =============================================================================

cat("=============================================================================\n")
cat("PART 1: SIMULATION EXECUTION - COSERO SENSITIVITY ANALYSIS\n")
cat("=============================================================================\n\n")

# Project paths
project_path <- "D:/Structural Exercise/COSERO_MonteCarlo_Optimierung_SH/COSERO_MonteCarlo_Optimierung_SH"
param_file <- file.path(project_path, "input", "para_run_opt6_final.txt")

# Sensitivity analysis configuration
n_samples <- 500  # Base sample size (Total simulations = n × (k+2) where k=parameters)

# Parameters to analyze (all 9 core hydrological parameters)
param_names <- c("M", "TAB1", "TAB2", "TAB3", "TVS1", "TVS2", "KBF", "H1", "H2")

# Subbasin for analysis
plot_subbasin <- "0001"

cat("Configuration:\n")
cat("  Project:", project_path, "\n")
cat("  Parameter file:", basename(param_file), "\n")
cat("  Sample size:", n_samples, "(Total simulations:", n_samples * (length(param_names) + 2), ")\n")
cat("  Parameters:", paste(param_names, collapse=", "), "\n")
cat("  Target subbasin:", plot_subbasin, "\n\n")

# Validate paths
if (!file.exists(project_path)) stop("❌ Project path not found!")
if (!file.exists(param_file)) stop("❌ Parameter file not found!")
cat("✓ All paths validated\n\n")

# =============================================================================
# STEP 1: DEFINE PARAMETER BOUNDS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 1: PARAMETER BOUNDS DEFINITION\n")
cat("=============================================================================\n\n")

bounds <- create_custom_bounds(
  parameter = c("M",    "TAB1", "TAB2", "TAB3", "TVS1", "TVS2", "KBF",  "H1",  "H2"),

  # Physical bounds (absolute limits - for clipping)
  min =       c(50,     1,      25,     500,    5,      45,     2000,   0,     0),
  max =       c(1000,   50,     300,    5000,   150,    1500,   12000,  20,    20),

  # Sampling ranges (for Sobol) - relative multipliers (0.5× to 2.0×)
  sample_min = c(0.5,   0.5,    0.5,    0.5,    0.5,    0.5,    0.5,    0.5,   0.5),
  sample_max = c(2.0,   2.0,    2.0,    2.0,    2.0,    2.0,    2.0,    2.0,   2.0),

  # Modification type (all use relative change - preserves spatial patterns)
  modification_type = c("relchg", "relchg", "relchg", "relchg", "relchg",
                       "relchg", "relchg", "relchg", "relchg"),

  # Parameter descriptions (for documentation)
  description = c(
    "Maximum soil moisture storage",
    "Baseflow recession constant 1",
    "Baseflow recession constant 2",
    "Baseflow recession constant 3",
    "Interflow recession constant 1",
    "Interflow recession constant 2",
    "Baseflow storage coefficient",
    "Percolation rate coefficient 1",
    "Percolation rate coefficient 2"
  )
)

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
# STEP 3: SAVE SAMPLES AND CONFIGURATION
# =============================================================================

cat("=============================================================================\n")
cat("STEP 3: SAVE CONFIGURATION AND SAMPLES\n")
cat("=============================================================================\n\n")

# Create results directory
results_base_dir <- file.path(project_path, "sensitivity_analysis_results")
dir.create(results_base_dir, recursive = TRUE, showWarnings = FALSE)

# Save configuration
config_file <- file.path(results_base_dir, "analysis_configuration.rds")
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
cat("  File size:", round(file.info(config_file)$size / 1024^2, 2), "MB\n\n")

# =============================================================================
# STEP 4: RUN SEQUENTIAL MONTE CARLO SIMULATIONS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 4: SEQUENTIAL MONTE CARLO SIMULATIONS\n")
cat("=============================================================================\n\n")

n_total <- nrow(samples$parameter_sets)
cat("⚠ WARNING: About to run", n_total, "sequential COSERO simulations\n")
cat("Estimated runtime:", round(n_total * 2 / 60, 1), "minutes (assuming 2 sec/run)\n")
cat("Memory management: Batch processing (25 runs/batch) to prevent crashes\n\n")
cat("Press Enter to continue or Ctrl+C to cancel: ")
readline()

# Memory cleanup before starting
gc(full = TRUE)
closeAllConnections()

# BATCHED SEQUENTIAL EXECUTION (Memory-efficient)
# Process in small batches to prevent memory overflow
batch_size <- 25
n_batches <- ceiling(n_total / batch_size)

cat("\nStarting batched sequential execution:\n")
cat(sprintf("  Total runs: %d\n", n_total))
cat(sprintf("  Batch size: %d\n", batch_size))
cat(sprintf("  Number of batches: %d\n\n", n_batches))

# Create batch directory in results folder
batch_dir <- file.path(results_base_dir, "simulation_batches")
if (dir.exists(batch_dir)) unlink(batch_dir, recursive = TRUE)
dir.create(batch_dir, showWarnings = FALSE)

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
    base_settings = list(OUTPUTTYPE = 1),
    quiet = TRUE
  )

  # Save batch immediately
  batch_file <- file.path(batch_dir, sprintf("batch_%04d.rds", batch_idx))
  saveRDS(batch_results, batch_file)

  # Calculate time for this batch
  batch_times[batch_idx] <- as.numeric(difftime(Sys.time(), batch_start, units = "secs"))

  # Display progress with time estimation
  if (batch_idx == 1) {
    # First batch: estimate total time
    avg_batch_time <- batch_times[1]
    est_remaining <- avg_batch_time * (n_batches - 1)
    cat(sprintf("✓ (%.1fs) | Est. total: %.1f min\n",
                batch_times[1], (avg_batch_time * n_batches) / 60))
  } else {
    # Subsequent batches: use running average
    avg_batch_time <- mean(batch_times[1:batch_idx], na.rm = TRUE)
    est_remaining <- avg_batch_time * (n_batches - batch_idx)
    elapsed_total <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

    # Format time remaining
    if (est_remaining < 60) {
      time_str <- sprintf("%.0fs", est_remaining)
    } else if (est_remaining < 3600) {
      time_str <- sprintf("%.1f min", est_remaining / 60)
    } else {
      time_str <- sprintf("%.1f hrs", est_remaining / 3600)
    }

    cat(sprintf("✓ (%.1fs) | %.0f%% done | Est. remaining: %s\n",
                batch_times[batch_idx],
                (batch_idx / n_batches) * 100,
                time_str))
  }

  # CRITICAL: Clean up immediately to free memory
  rm(batch_results)
  closeAllConnections()
  gc(full = TRUE)
}

time_elapsed <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))

cat(sprintf("\n✓ All batches complete: %.1f sec (%.2f sec/run)\n\n",
            time_elapsed, time_elapsed / n_total))

# Save timing information
timing_info <- list(
  total_simulations = n_total,
  batch_size = batch_size,
  n_batches = n_batches,
  batch_times = batch_times,
  total_runtime_seconds = time_elapsed,
  total_runtime_minutes = time_elapsed / 60,
  avg_time_per_run = time_elapsed / n_total,
  completed_date = Sys.time()
)

timing_file <- file.path(results_base_dir, "simulation_timing.rds")
saveRDS(timing_info, timing_file)

cat("✓ Timing information saved:", timing_file, "\n\n")

# =============================================================================
# SIMULATION COMPLETE
# =============================================================================

cat("=============================================================================\n")
cat("SIMULATION PHASE COMPLETE!\n")
cat("=============================================================================\n\n")

cat("Summary:\n")
cat("  Total simulations:", n_total, "\n")
cat("  Batches saved:", n_batches, "\n")
cat("  Batch directory:", batch_dir, "\n")
cat("  Total runtime:", round(time_elapsed / 60, 2), "minutes\n")
cat("  Average per run:", round(time_elapsed / n_total, 2), "seconds\n\n")

cat("Next step:\n")
cat("  Run workflow_2_generate_results.R to combine batches and generate all plots/results\n\n")

cat("Files created:\n")
cat("  1.", config_file, "\n")
cat("  2.", batch_dir, "/ (", n_batches, "batch files)\n")
cat("  3.", timing_file, "\n\n")

cat("=============================================================================\n")
