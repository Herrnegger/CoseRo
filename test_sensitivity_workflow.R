# Manual Test: Sensitivity Analysis Workflow (Step by Step)
# This tests each stage of sensitivity analysis WITHOUT running full ensemble
# Author: COSERO R Package
# Date: 2025-11-06

# Load package
devtools::load_all()

cat("=== Sensitivity Analysis Workflow Test ===\n\n")
cat("This test walks through each step of sensitivity analysis:\n")
cat("1. Define parameter bounds\n")
cat("2. Generate Sobol samples\n")
cat("3. Verify sample properties\n")
cat("4. Test parameter modification with one sample\n")
cat("5. Verify COSERO can be called\n\n")

# =============================================================================
# Setup: Define paths
# =============================================================================

cat("=== Setup ===\n")

# Your actual COSERO project
project_path <- "C:/Users/AHMED/OneDrive/Desktop/BOKU/Structural Exercise/COSERO_MonteCarlo_Optimierung_SH/COSERO_MonteCarlo_Optimierung_SH"
param_file <- file.path(project_path, "input", "para_run_opt6_final.txt")

cat("Project path:", project_path, "\n")
cat("Parameter file:", param_file, "\n")

# Check if files exist
if (!file.exists(project_path)) {
  stop("Project path not found!")
}
if (!file.exists(param_file)) {
  stop("Parameter file not found!")
}

cat("✓ All paths exist\n\n")

# =============================================================================
# Step 1: Define Parameter Bounds
# =============================================================================

cat("=== Step 1: Define Parameter Bounds ===\n\n")

# Define parameters for sensitivity analysis
param_names <- c("BETA", "CTMAX", "TCOR")

cat("Parameters for sensitivity analysis:\n")
for (p in param_names) {
  cat("  -", p, "\n")
}

# Load bounds from CSV (recommended) or create custom
cat("\nLoading parameter bounds from CSV...\n")
bounds <- load_parameter_bounds(parameters = param_names)

# Alternative: Create custom bounds
bounds <- create_custom_bounds(
   parameter = c("BETA", "CTMAX", "TCOR"),

   # Physical bounds (clipping limits)
   min = c(0.1,  2.0,  -3.0),    # BETA: physical min, CTMAX: 2°C, TCOR: -3°C
   max = c(10.0, 12.0,  3.0),    # BETA: physical max, CTMAX: 12°C, TCOR: +3°C

   # Sampling ranges (for Sobol) - optional
   sample_min = c(0.5,  0.5,  -2.0),  # BETA: 0.5× to 2×, CTMAX: 0.5× to 2×, TCOR: -2 to +2°C
   sample_max = c(2.0,  2.0,   2.0),

   modification_type = c("relchg", "relchg", "abschg")  # relchg: multiply, abschg: add/subtract
 )

cat("\nParameter bounds:\n")
print(bounds)

cat("\nModification types:\n")

for (i in 1:nrow(bounds)) {
  cat(sprintf("  %-10s: %s (range: %.2f to %.2f)\n",
              bounds$parameter[i],
              bounds$modification_type[i],
              bounds$min[i],
              bounds$max[i]))
}

cat("\n✓ Bounds defined successfully\n\n")

# =============================================================================
# Step 2: Generate Sobol Samples
# =============================================================================

cat("=== Step 2: Generate Sobol Samples ===\n\n")

# Convert to Sobol format
sobol_bounds <- create_sobol_bounds(bounds)

cat("Sobol bounds structure:\n")
cat("  - Number of parameters:", ncol(sobol_bounds), "\n")
cat("  - Parameter names:", paste(colnames(sobol_bounds), collapse = ", "), "\n")
cat("  - Dimensions:", nrow(sobol_bounds), "rows ×", ncol(sobol_bounds), "columns\n")
cat("\nSobol bounds table:\n")
print(sobol_bounds)

# Generate small sample for testing
n_samples <- 3
cat("\nGenerating", n_samples, "Sobol samples...\n")

samples <- generate_sobol_samples(sobol_bounds, n = n_samples)

cat("✓ Samples generated\n")
cat("  - Number of sets:", nrow(samples$parameter_sets), "\n")
cat("  - Number of parameters per set:", ncol(samples$parameter_sets), "\n")

# =============================================================================
# Step 3: Verify Sample Properties
# =============================================================================

cat("\n=== Step 3: Verify Sample Properties ===\n\n")

cat("First 5 parameter sets:\n")
print(head(samples$parameter_sets, 5))

cat("\nSample statistics:\n")
for (param in param_names) {
  values <- samples$parameter_sets[[param]]
  cat(sprintf("  %-10s: min=%.3f, max=%.3f, mean=%.3f, sd=%.3f\n",
              param,
              min(values),
              max(values),
              mean(values),
              sd(values)))
}

cat("\nCheck if samples are within bounds:\n")
all_within_bounds <- TRUE
for (param in param_names) {
  values <- samples$parameter_sets[[param]]
  param_bounds <- bounds[bounds$parameter == param, ]

  # Check against SAMPLING bounds (sample_min/sample_max), not physical bounds (min/max)
  # For relchg: sample_min/max are multipliers (e.g., 0.5 to 2.0)
  # For abschg: sample_min/max are additive values (e.g., -2 to +2)
  within <- all(values >= param_bounds$sample_min & values <= param_bounds$sample_max)

  cat(sprintf("  %-10s (%s): %s (%.2f to %.2f, expected: %.2f to %.2f)\n",
              param,
              param_bounds$modification_type,
              ifelse(within, "✓ Within bounds", "✗ OUT OF BOUNDS"),
              min(values),
              max(values),
              param_bounds$sample_min,
              param_bounds$sample_max))

  if (!within) all_within_bounds <- FALSE
}

if (all_within_bounds) {
  cat("\n✓ All samples within specified bounds\n")
} else {
  cat("\n✗ Some samples outside bounds - check bounds definition!\n")
}

# =============================================================================
# Helper Function: Create Parameter Comparison Histogram
# =============================================================================

plot_parameter_comparison <- function(original_vals, modified_vals, param_name,
                                      param_bounds, sample_value = NULL,
                                      save_file = NULL, step_label = NULL) {

  # Validate inputs
  if (length(original_vals) == 0 || length(modified_vals) == 0) {
    warning(sprintf("Empty value vectors for %s - skipping plot", param_name))
    return(invisible(NULL))
  }

  # Check if values are identical (no modification occurred)
  if (all(abs(original_vals - modified_vals) < 1e-10, na.rm = TRUE)) {
    warning(sprintf("%s: Original and modified values are identical", param_name))
  }

  # Warn if sample size too small
  if (length(modified_vals) < 20) {
    cat(sprintf("    [INFO] Only %d samples for %s - using side-by-side boxplots for clarity\n",
                length(modified_vals), param_name))
  }

  # Set up plot device if saving
  if (!is.null(save_file)) {
    png(save_file, width = 800, height = 600, res = 100)
  }

  # Create title
  title_text <- paste("Parameter:", param_name)
  if (!is.null(step_label)) {
    title_text <- paste(step_label, "-", title_text)
  }

  # If sample size is small (< 20), use boxplots instead of histograms
  if (length(modified_vals) < 20) {
    # Create side-by-side boxplots for better clarity with small samples
    boxplot(list(Original = original_vals, Modified = modified_vals),
            main = title_text,
            ylab = paste(param_name, "Value"),
            col = c("lightblue", "orange"),
            border = c("darkblue", "darkorange"),
            las = 1,
            notch = FALSE,
            outline = TRUE)

    # Add horizontal lines for bounds
    abline(h = param_bounds$min, col = "red", lty = 2, lwd = 2)
    abline(h = param_bounds$max, col = "red", lty = 2, lwd = 2)

    # Add text labels for bounds
    text(x = 2.4, y = param_bounds$min, labels = sprintf("Min = %.1f", param_bounds$min),
         col = "red", cex = 0.8, pos = 3)
    text(x = 2.4, y = param_bounds$max, labels = sprintf("Max = %.1f", param_bounds$max),
         col = "red", cex = 0.8, pos = 1)

    # Add sample size info
    mtext(sprintf("n_original = %d, n_modified = %d",
                  length(original_vals), length(modified_vals)),
          side = 3, line = 0.5, cex = 0.9)

    # Add modification info if available
    if (!is.null(sample_value)) {
      mtext(sprintf("Sample value applied: %.3f (%s)",
                    sample_value, param_bounds$modification_type),
            side = 3, line = -1, cex = 0.9, col = "darkgreen")
    }

    # Add grid for easier reading
    grid(nx = NA, ny = NULL, col = "gray90", lty = 1)

  } else {
    # For larger samples, use histogram
    n_breaks <- max(10, min(30, ceiling(length(original_vals) / 5)))

    all_vals <- c(original_vals, modified_vals, param_bounds$min, param_bounds$max)
    x_range <- range(all_vals, na.rm = TRUE)
    x_margin <- diff(x_range) * 0.05
    x_range <- x_range + c(-x_margin, x_margin)

    hist(original_vals,
         main = title_text,
         xlab = paste(param_name, "Value"),
         ylab = "Frequency",
         col = rgb(0.2, 0.6, 0.8, 0.5),
         border = "darkblue",
         xlim = x_range,
         breaks = n_breaks,
         las = 1)

    hist(modified_vals,
         col = rgb(0.9, 0.6, 0.0, 0.5),
         border = "darkorange",
         breaks = n_breaks,
         add = TRUE)

    abline(v = param_bounds$min, col = "red", lty = 2, lwd = 2)
    abline(v = param_bounds$max, col = "red", lty = 2, lwd = 2)

    legend("topright",
           legend = c("Original", "Modified", "Bounds"),
           fill = c(rgb(0.2, 0.6, 0.8, 0.5), rgb(0.9, 0.6, 0.0, 0.5), NA),
           border = c("darkblue", "darkorange", NA),
           lty = c(NA, NA, 2),
           col = c(NA, NA, "red"),
           lwd = c(NA, NA, 2),
           bty = "n")
  }

  # Check if any clipping occurred and add warning
  n_clipped_low <- sum(modified_vals <= param_bounds$min, na.rm = TRUE)
  n_clipped_high <- sum(modified_vals >= param_bounds$max, na.rm = TRUE)

  if (n_clipped_low > 0 || n_clipped_high > 0) {
    mtext(sprintf("⚠ WARNING: %d values clipped!", n_clipped_low + n_clipped_high),
          side = 1, line = 4, col = "red", font = 2, cex = 1.1)
  }

  # Close device if saving
  if (!is.null(save_file)) {
    dev.off()
    cat("  ✓ Plot saved to:", save_file, "\n")
  }

  invisible(NULL)
}

# =============================================================================
# Visualize parameter distributions
# =============================================================================

cat("\nGenerating histograms for sampled parameters...\n")

# Read original values from parameter file (all zones)
original_all <- read_parameter_table(param_file, param_names, zone_id = "all")

# Create histogram for each parameter
for (param in param_names) {
  # Get original values and sampled values
  original_vals <- original_all[[param]]
  sampled_vals <- samples$parameter_sets[[param]]

  # Get bounds for this parameter
  param_bounds <- bounds[bounds$parameter == param, ]

  # Calculate MODIFIED values (what will actually be used in model runs)
  # This applies the same logic as modify_parameter_table()
  modified_vals <- numeric(length(sampled_vals))

  if (param_bounds$modification_type == "relchg") {
    # Multiply: modified = original × sampled
    # Use median for robustness against outliers
    reference_val <- median(original_vals, na.rm = TRUE)
    modified_vals <- reference_val * sampled_vals
  } else if (param_bounds$modification_type == "abschg") {
    # Add: modified = original + sampled
    reference_val <- median(original_vals, na.rm = TRUE)
    modified_vals <- reference_val + sampled_vals

    # Debug info for abschg
    cat(sprintf("    [%s] Original median: %.2f, Sampled range: %.2f to %.2f, Modified range before clipping: %.2f to %.2f\n",
                param, reference_val, min(sampled_vals), max(sampled_vals),
                min(modified_vals), max(modified_vals)))
  } else {
    # absval: use sampled values directly
    modified_vals <- sampled_vals
  }

  # Clip to physical bounds
  modified_vals_before_clip <- modified_vals
  modified_vals <- pmax(param_bounds$min, pmin(param_bounds$max, modified_vals))

  # Check if any clipping occurred
  n_clipped <- sum(modified_vals != modified_vals_before_clip)
  if (n_clipped > 0) {
    cat(sprintf("    [%s] Clipped %d values to bounds [%.2f, %.2f]\n",
                param, n_clipped, param_bounds$min, param_bounds$max))
  }

  # Create plot using improved helper function (display only, no saving)
  plot_parameter_comparison(
    original_vals = original_vals,
    modified_vals = modified_vals,
    param_name = param,
    param_bounds = param_bounds,
    save_file = NULL,  # Don't save to file
    step_label = "Step 3"
  )

  cat(sprintf("  ✓ Histogram created for %s (original: %.2f±%.2f, modified: %.2f±%.2f)\n",
              param,
              mean(original_vals, na.rm = TRUE),
              sd(original_vals, na.rm = TRUE),
              mean(modified_vals),
              sd(modified_vals)))
}

# =============================================================================
# Step 4: Test Parameter Modification with One Sample
# =============================================================================

cat("\n=== Step 4: Test Parameter Modification ===\n\n")

# Create a test copy of parameter file
test_file <- file.path(project_path, "input", "para_TEST_SA.txt")
file.copy(param_file, test_file, overwrite = TRUE)

cat("Created test copy:", test_file, "\n\n")

# Read original values
cat("Reading original parameter values (zone 1)...\n")
original_values <- read_parameter_table(test_file, param_names, zone_id = 1)

cat("Original values:\n")
for (param in param_names) {
  cat(sprintf("  %-10s = %.4f\n", param, original_values[[param]]))
}

# Select first parameter set from samples
test_params <- as.list(samples$parameter_sets[1, ])

cat("\nFirst Sobol sample to apply:\n")
for (param in param_names) {
  param_bounds <- bounds[bounds$parameter == param, ]
  cat(sprintf("  %-10s = %.4f (%s)\n", param, test_params[[param]], param_bounds$modification_type))
}

# Apply modifications
cat("\nApplying modifications to test file...\n")
modify_parameter_table(
  par_file = test_file,
  params = test_params,
  par_bounds = bounds,
  original_values = original_values,
  zones = NULL  # Modify all zones
)

cat("✓ Modifications applied\n\n")

# Read modified values
cat("Reading modified parameter values (zone 1)...\n")
modified_values <- read_parameter_table(test_file, param_names, zone_id = 1)

cat("Modified values:\n")
for (param in param_names) {
  cat(sprintf("  %-10s = %.4f\n", param, modified_values[[param]]))
}

# Verify modifications based on modification type
cat("\nVerification (accounting for modification type):\n")
all_correct <- TRUE
for (param in param_names) {
  sampled <- test_params[[param]]
  original <- original_values[[param]]
  actual <- modified_values[[param]]

  # Get modification type
  mod_type <- bounds$modification_type[bounds$parameter == param]
  param_min <- bounds$min[bounds$parameter == param]
  param_max <- bounds$max[bounds$parameter == param]

  # Calculate expected value
  if (mod_type == "relchg") {
    expected <- original * sampled
  } else if (mod_type == "abschg") {
    expected <- original + sampled
  } else {
    expected <- sampled  # Shouldn't happen
  }

  # Clip to bounds
  expected <- max(param_min, min(param_max, expected))

  diff <- abs(expected - actual)
  match <- diff < 0.001

  cat(sprintf("  %-10s: %s -> Expected %.4f, Got %.4f %s\n",
              param,
              mod_type,
              expected,
              actual,
              ifelse(match, "✓", "✗")))

  if (!match) all_correct <- FALSE
}

if (all_correct) {
  cat("\n✓ All parameter modifications correct!\n")
} else {
  cat("\n✗ Some modifications incorrect - check modify_parameter_table() logic\n")
}

# Visualize modifications across all zones
cat("\nGenerating histograms for modified parameters (all zones)...\n")
cat("NOTE: This shows ONE specific modification (first Sobol sample)\n")
cat("      For relchg: all zones multiplied by same factor\n")
cat("      For abschg: all zones shifted by same amount\n\n")

# Read original and modified values for ALL zones
original_all_zones <- read_parameter_table(param_file, param_names, zone_id = "all")
modified_all_zones <- read_parameter_table(test_file, param_names, zone_id = "all")

# Create histogram for each parameter using improved helper function
for (param in param_names) {
  # Get original and modified values
  original_vals <- original_all_zones[[param]]
  modified_vals <- modified_all_zones[[param]]

  # Get bounds for this parameter
  param_bounds <- bounds[bounds$parameter == param, ]

  # Get the sample value that was applied
  sample_val <- test_params[[param]]

  # Create plot using improved helper function (display only, no saving)
  plot_parameter_comparison(
    original_vals = original_vals,
    modified_vals = modified_vals,
    param_name = param,
    param_bounds = param_bounds,
    sample_value = sample_val,
    save_file = NULL,  # Don't save to file
    step_label = "Step 4"
  )

  cat(sprintf("  ✓ Histogram created for %s (sample value: %.4f)\n", param, sample_val))
}

# =============================================================================
# Step 5: Verify COSERO Can Be Called
# =============================================================================

cat("\n=== Step 5: Verify COSERO Executable ===\n\n")

# Find COSERO executable

cosero_exe <- file.path(project_path, "COSERO.exe")

if (file.exists(cosero_exe)) {
  cat("✓ COSERO executable found:\n")
  cat("  ", cosero_exe, "\n")

  # Check file size
  file_info <- file.info(cosero_exe)
  cat("  File size:", round(file_info$size / 1024 / 1024, 2), "MB\n")

  cat("\nTo run COSERO with modified parameters, use:\n")
  cat("  run_cosero(\n")
  cat("    project_path = \"", project_path, "\",\n", sep = "")
  cat("    silent = FALSE\n")
  cat("  )\n")

} else {
  cat("✗ COSERO executable not found at:\n")
  cat("  ", cosero_exe, "\n")
  cat("\nExpected executable name pattern: [project_name].exe\n")

  # Try to find any .exe files
  exe_files <- list.files(project_path, pattern = "\\.exe$", full.names = TRUE)
  if (length(exe_files) > 0) {
    cat("\nFound .exe files in project directory:\n")
    for (exe in exe_files) {
      cat("  -", basename(exe), "\n")
    }
  }
}

# =============================================================================
# Step 6: Test Ensemble Runs (Sequential & Parallel)
# =============================================================================

cat("\n=== Step 6: Test Ensemble Runs ===\n\n")
cat("WARNING: This will run COSERO multiple times!\n")
cat("Press Enter to continue or Ctrl+C to skip...\n")
readline()

# Use all samples for the test
devtools::load_all()
n_test <- nrow(samples$parameter_sets)
test_samples <- samples$parameter_sets[1:n_test, ]

cat("Running", n_test, "COSERO simulations...\n\n")

# Sequential run
cat("--- Sequential execution ---\n")
time_seq <- system.time({
  results_seq <- run_cosero_ensemble(
    project_path = project_path,
    parameter_sets = test_samples,
    par_bounds = bounds,
    base_settings = list(OUTPUTTYPE = 1)  # Minimal output for speed
  )
})
cat("Sequential time:", round(time_seq[3], 1), "seconds\n\n")

# Parallel run
cat("--- Parallel execution (4 cores) ---\n")
time_par <- system.time({
  results_par <- run_cosero_ensemble_parallel(
    project_path = project_path,
    parameter_sets = test_samples,
    par_bounds = bounds,
    base_settings = list(OUTPUTTYPE = 1),
     temp_dir = "D:/temp",
    n_cores = 4
  )
})
cat("Parallel time:", round(time_par[3], 1), "seconds\n")
cat("Speedup:", round(time_seq[3] / time_par[3], 2), "x\n\n")


# =============================================================================
# Step 7: Using the New Helper Functions for Sensitivity Analysis
# =============================================================================

cat("\n=== Step 7: Sensitivity Analysis with New Functions ===\n\n")

# Define which subbasin to analyze
plot_subbasin <- "0001"  # or use numeric: plot_subbasin <- 1

# Example 1: Extract pre-calculated NSE (recommended for standard metrics)
cat("Extracting pre-calculated NSE for subbasin", plot_subbasin, "...\n")
nse_extracted <- extract_cosero_metrics(results_par,
                                        subbasin_id = plot_subbasin,
                                        metric = "NSE",
                                        warn_nan = TRUE)

cat("NSE values extracted:", paste(round(nse_extracted, 3), collapse = ", "), "\n")
cat("Valid values:", sum(!is.na(nse_extracted) & !is.nan(nse_extracted)), "/", length(nse_extracted), "\n")

# Diagnose why NAs might occur
if (any(is.na(nse_extracted))) {
  cat("\nDiagnosing NA values:\n")
  for (i in which(is.na(nse_extracted))) {
    result <- results_par$results[[i]]
    cat(sprintf("  Run %d: success=%s, output_data=%s, statistics=%s\n",
                i,
                result$success,
                !is.null(result$output_data),
                !is.null(result$output_data$statistics)))

    if (!is.null(result$output_data$statistics)) {
      stats <- result$output_data$statistics
      cat(sprintf("    - Subbasins in statistics: %s\n", paste(unique(stats$sb), collapse = ", ")))
      cat(sprintf("    - Columns in statistics: %s\n", paste(colnames(stats), collapse = ", ")))
    }
  }
}
cat("\n")

# Example 2: Extract KGE
cat("Extracting pre-calculated KGE for subbasin", plot_subbasin, "...\n")
kge_extracted <- extract_cosero_metrics(results_par,
                                        subbasin_id = plot_subbasin,
                                        metric = "KGE",
                                        warn_nan = FALSE)

cat("KGE values extracted:", paste(round(kge_extracted, 3), collapse = ", "), "\n")
cat("Valid values:", sum(!is.na(kge_extracted) & !is.nan(kge_extracted)), "/", length(kge_extracted), "\n\n")

# Example 3: Calculate custom metric (PBIAS) using calculate_ensemble_metrics
cat("Calculating PBIAS using calculate_ensemble_metrics()...\n")
cat("(This recalculates from QOBS/QSIM data with spin-up exclusion)\n")

# First, check what column names exist in runoff data
cat("\nDiagnostic - Runoff columns in first run:\n")
if (!is.null(results_par$results[[1]]$output_data$runoff)) {
  runoff_cols <- colnames(results_par$results[[1]]$output_data$runoff)
  qsim_cols <- grep("QSIM", runoff_cols, value = TRUE)
  qobs_cols <- grep("QOBS", runoff_cols, value = TRUE)
  cat("  QSIM columns:", paste(qsim_cols, collapse = ", "), "\n")
  cat("  QOBS columns:", paste(qobs_cols, collapse = ", "), "\n")
} else {
  cat("  No runoff data available\n")
}

# Check spin-up settings
cat("\nDiagnostic - Spin-up settings in first run:\n")
if (!is.null(results_par$results[[1]]$defaults_settings$SPINUP)) {
  cat("  SPINUP:", results_par$results[[1]]$defaults_settings$SPINUP, "\n")
} else {
  cat("  SPINUP: Not found in defaults_settings\n")
}

pbias_calculated <- calculate_ensemble_metrics(results_par,
                                               subbasin_id = plot_subbasin,
                                               metric = "PBIAS")
pbias_calculated <- calculate_ensemble_metrics(results_par, subbasin_id = "001", 
                      metric = "NSE", spinup = 365)

cat("\nPBIAS values calculated:", paste(round(pbias_calculated, 3), collapse = ", "), "\n")
cat("Valid values:", sum(!is.na(pbias_calculated) & !is.nan(pbias_calculated)), "/", length(pbias_calculated), "\n\n")

# Show the structure of what extract_cosero_metrics expects
cat("Structure of statistics data (first run):\n")
if (!is.null(results_par$results[[1]]$output_data$statistics)) {
  print(head(results_par$results[[1]]$output_data$statistics, 3))
} else {
  cat("  No statistics data available\n")
}

cat("\n=== How the OF calculation functions work ===\n\n")
cat("1. extract_cosero_metrics() - RECOMMENDED for standard metrics\n")
cat("   - Location: R/sensitivity_analysis.R:1187-1231\n")
cat("   - Extracts from COSERO's statistics.txt (pre-calculated)\n")
cat("   - Fast (no recalculation)\n")
cat("   - Metrics: NSE, KGE, RMSE, etc.\n")
cat("   - Usage: extract_cosero_metrics(results, subbasin_id = '001', metric = 'KGE')\n\n")

cat("2. calculate_ensemble_metrics() - For custom metrics\n")
cat("   - Location: R/sensitivity_analysis.R:1251-1320\n")
cat("   - Recalculates from QOBS vs QSIM discharge data\n")
cat("   - Uses hydroGOF package (KGE, NSE, RMSE, PBIAS)\n")
cat("   - Automatically applies spin-up period exclusion\n")
cat("   - Usage: calculate_ensemble_metrics(results, subbasin_id = '001', metric = 'PBIAS')\n\n")

cat("3. Key hydroGOF functions used:\n")
cat("   - hydroGOF::KGE(sim, obs)     - Kling-Gupta Efficiency\n")
cat("   - hydroGOF::NSE(sim, obs)     - Nash-Sutcliffe Efficiency\n")
cat("   - hydroGOF::rmse(sim, obs)    - Root Mean Square Error\n")
cat("   - hydroGOF::pbias(sim, obs)   - Percent Bias\n\n")

# These extracted metrics can be used directly in sensitivity analysis:
cat("Example workflow for sensitivity analysis:\n")
cat("  sobol_indices <- calculate_sobol_indices(nse_extracted, samples)\n")
cat("  plot_sobol(sobol_indices)\n")
cat("  plot_dotty(samples$parameter_sets, nse_extracted, y_label = 'NSE')\n\n")

cat("✓ Metric extraction demonstration completed\n")

sobol_indices <- calculate_sobol_indices(nse_extracted, samples)

# =============================================================================
# Step 8: Cleanup and Summary
# =============================================================================

cat("\n=== Step 8: Cleanup ===\n\n")

cat("Test file location:", test_file, "\n")
cat("You can:\n")
cat("  - Delete it if test passed\n")
cat("  - Inspect it to verify structure\n")
cat("  - Use it for a manual COSERO run\n")

# Optional: Delete test file
# file.remove(test_file)
# cat("✓ Test file deleted\n")

# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat(paste(rep("=", 70), collapse = ""), "\n")
cat("SUMMARY: Sensitivity Analysis Workflow Test\n")
cat(paste(rep("=", 70), collapse = ""), "\n\n")

cat("Steps completed:\n")
cat("  ✓ Step 1: Parameter bounds defined (", nrow(bounds), " parameters)\n", sep = "")
cat("  ✓ Step 2: Sobol samples generated (", n_samples, " sets)\n", sep = "")
cat("  ✓ Step 3: Sample properties verified\n")
cat("  ✓ Step 4: Parameter modification tested\n")
cat("  ✓ Step 5: COSERO executable checked\n")

cat("\nNext steps:\n")
cat("  1. If all tests passed, run a small ensemble (n=10)\n")
cat("  2. Test with run_cosero_ensemble() for sequential runs\n")
cat("  3. Test with run_cosero_ensemble_parallel() for parallel runs\n")
cat("  4. Scale up to full sensitivity analysis (n=1000+)\n")

cat("\nExample command for small ensemble:\n")
cat("```r\n")
cat("results <- run_cosero_ensemble(\n")
cat("  project_path = \"", project_path, "\",\n", sep = "")
cat("  parameter_sets = samples$parameter_sets,\n")
cat("  par_bounds = bounds\n")
cat(")\n")
cat("```\n")

cat("\n=== Test Complete ===\n")
