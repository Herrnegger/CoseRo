# Manual Test: Sensitivity Analysis Workflow (Step by Step)
# This tests each stage of sensitivity analysis WITHOUT running full ensemble
# Author: COSERO R Package
# Date: 2025-12-03

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

   # Sampling ranges (for Sobol)
   sample_min = c(0.5,  -2.0,  -2.0),  # BETA: 0.5× to 2×, CTMAX: -2.0°C to +2.0°C, TCOR: -2 to +2°C
   sample_max = c(2.0,   2.0,   2.0),

   modification_type = c("relchg", "abschg", "abschg")  # relchg: multiply, abschg: add/subtract
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
n_samples <- 20
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

  col_original <- "#5B8FA3"       # Muted slate blue
  col_original_dark <- "#3D5A6C"  # Darker blue for borders
  col_modified <- "#D4A574"       # Soft tan/gold
  col_modified_dark <- "#9A7654"  # Darker tan for borders
  col_bounds <- "#C65353"         # Muted red for bounds
  col_grid <- "#E8E8E8"           # Very light gray for grid
  col_bg <- "#FAFAFA"             # Off-white background

  # Set up plot device if saving
  if (!is.null(save_file)) {
    png(save_file, width = 1000, height = 700, res = 110, bg = col_bg)
  }
  # For screen display, use default device (RStudio plot pane or current device)
  # Don't open new windows

  # Set margins and background
  par(mar = c(4.5, 4.5, 3.5, 1.5), bg = col_bg, family = "sans")

  # Create title
  title_text <- param_name
  if (!is.null(step_label)) {
    title_text <- paste(step_label, "-", param_name)
  }

  # For small samples, use detailed comparison plots
  if (length(modified_vals) < 20) {
    cat(sprintf("    [INFO] %d samples for %s - using detailed comparison plot\n",
                length(modified_vals), param_name))

    # Prepare data
    all_data <- data.frame(
      value = c(original_vals, modified_vals),
      group = factor(rep(c("Original", "Modified"),
                        c(length(original_vals), length(modified_vals))))
    )

    # Calculate y-axis limits with generous padding
    y_range <- range(c(original_vals, modified_vals, param_bounds$min, param_bounds$max), na.rm = TRUE)
    y_margin <- diff(y_range) * 0.15
    y_lim <- y_range + c(-y_margin, y_margin)

    # Create empty plot with clean design
    plot(1, type = "n", xlim = c(0.5, 2.5), ylim = y_lim,
         xlab = "", ylab = param_name, xaxt = "n",
         main = title_text, cex.main = 1.3, cex.lab = 1.2,
         las = 1, bty = "n", col.axis = "#333333", fg = "#333333")

    # Add subtle horizontal grid lines
    grid_pos <- axTicks(2)
    abline(h = grid_pos, col = col_grid, lwd = 0.8)

    # Add very subtle bounds region (barely visible shading)
    rect(0.4, param_bounds$min, 2.6, param_bounds$max,
         col = rgb(0.7, 0.9, 0.7, 0.12), border = NA)

    # Add bounds as dashed lines (softer)
    abline(h = param_bounds$min, col = col_bounds, lty = 2, lwd = 1.8)
    abline(h = param_bounds$max, col = col_bounds, lty = 2, lwd = 1.8)

    # Plot boxplots with muted colors
    boxplot(value ~ group, data = all_data, add = TRUE, at = c(1, 2),
            col = c(col_original, col_modified),
            border = c(col_original_dark, col_modified_dark),
            boxwex = 0.5, outline = FALSE, axes = FALSE, lwd = 1.5)

    # Overlay individual points with subtle jitter
    set.seed(123)
    points(jitter(rep(1, length(original_vals)), amount = 0.12), original_vals,
           pch = 21, bg = col_original, col = col_original_dark,
           cex = 1.5, lwd = 1.2)
    points(jitter(rep(2, length(modified_vals)), amount = 0.12), modified_vals,
           pch = 21, bg = col_modified, col = col_modified_dark,
           cex = 1.5, lwd = 1.2)

    # Add mean lines (horizontal bars)
    segments(0.75, mean(original_vals, na.rm = TRUE),
             1.25, mean(original_vals, na.rm = TRUE),
             col = col_original_dark, lwd = 2.5)
    segments(1.75, mean(modified_vals, na.rm = TRUE),
             2.25, mean(modified_vals, na.rm = TRUE),
             col = col_modified_dark, lwd = 2.5)

    # Add x-axis labels
    axis(1, at = c(1, 2), labels = c("Original", "Modified"),
         cex.axis = 1.1, tick = FALSE, col.axis = "#333333")

    # Add clean legend with statistics
    legend("topright",
           legend = c(
             sprintf("Original: %.2f ± %.2f",
                    mean(original_vals, na.rm = TRUE),
                    sd(original_vals, na.rm = TRUE)),
             sprintf("Modified: %.2f ± %.2f",
                    mean(modified_vals, na.rm = TRUE),
                    sd(modified_vals, na.rm = TRUE)),
             sprintf("Bounds: [%.2f, %.2f]", param_bounds$min, param_bounds$max)
           ),
           fill = c(col_original, col_modified, rgb(0.7, 0.9, 0.7, 0.3)),
           border = c(col_original_dark, col_modified_dark, col_bounds),
           bty = "n", cex = 0.95, pt.cex = 1.2,
           box.col = "#DDDDDD", bg = "white")

    # Add modification info if available
    if (!is.null(sample_value)) {
      mtext(sprintf("Modification: %s = %.3f",
                    param_bounds$modification_type, sample_value),
            side = 3, line = 0.3, cex = 0.95, col = "#2C7A3D")
    }

  } else {
    # For larger samples, use refined histograms
    n_breaks <- max(15, min(35, ceiling(length(original_vals) / 8)))

    all_vals <- c(original_vals, modified_vals, param_bounds$min, param_bounds$max)
    x_range <- range(all_vals, na.rm = TRUE)
    x_margin <- diff(x_range) * 0.1
    x_lim <- x_range + c(-x_margin, x_margin)

    # Create histograms
    hist_orig <- hist(original_vals, breaks = n_breaks, plot = FALSE)
    hist_mod <- hist(modified_vals, breaks = n_breaks, plot = FALSE)

    y_max <- max(c(hist_orig$counts, hist_mod$counts)) * 1.12

    # Plot with clean design
    hist(original_vals,
         main = title_text,
         xlab = param_name,
         ylab = "Frequency",
         col = col_original,
         border = col_original_dark,
         xlim = x_lim,
         ylim = c(0, y_max),
         breaks = n_breaks,
         las = 1,
         cex.main = 1.3,
         cex.lab = 1.2,
         cex.axis = 1.0,
         lwd = 1.2,
         bty = "l",
         col.axis = "#333333",
         fg = "#666666")

    # Add subtle grid
    abline(h = axTicks(2), col = col_grid, lwd = 0.8)

    # Overlay modified histogram with nice transparency
    hist(modified_vals,
         col = rgb(0.83, 0.65, 0.45, 0.75),  # col_modified with transparency
         border = col_modified_dark,
         breaks = n_breaks,
         add = TRUE,
         lwd = 1.2)

    # Add bounds as softer dashed lines
    abline(v = param_bounds$min, col = col_bounds, lty = 2, lwd = 2)
    abline(v = param_bounds$max, col = col_bounds, lty = 2, lwd = 2)

    # Add subtle bound labels
    text(param_bounds$min, y_max * 0.92,
         labels = sprintf("Min\n%.2f", param_bounds$min),
         col = col_bounds, cex = 0.9, pos = 4, font = 2)
    text(param_bounds$max, y_max * 0.92,
         labels = sprintf("Max\n%.2f", param_bounds$max),
         col = col_bounds, cex = 0.9, pos = 2, font = 2)

    # Add mean lines (solid, but subtle)
    abline(v = mean(original_vals, na.rm = TRUE),
           col = col_original_dark, lwd = 2.2, lty = 1)
    abline(v = mean(modified_vals, na.rm = TRUE),
           col = col_modified_dark, lwd = 2.2, lty = 1)

    # Clean legend
    legend("topright",
           legend = c(
             sprintf("Original (μ=%.2f, σ=%.2f)",
                    mean(original_vals, na.rm = TRUE),
                    sd(original_vals, na.rm = TRUE)),
             sprintf("Modified (μ=%.2f, σ=%.2f)",
                    mean(modified_vals, na.rm = TRUE),
                    sd(modified_vals, na.rm = TRUE)),
             "Physical bounds"
           ),
           fill = c(col_original, rgb(0.83, 0.65, 0.45, 0.75), NA),
           border = c(col_original_dark, col_modified_dark, NA),
           lty = c(1, 1, 2),
           col = c(col_original_dark, col_modified_dark, col_bounds),
           lwd = c(2.2, 2.2, 2),
           bty = "n",
           cex = 0.95,
           box.col = "#DDDDDD",
           bg = "white")

    # Add sample info
    if (!is.null(sample_value)) {
      mtext(sprintf("Modification: %s = %.3f",
                    param_bounds$modification_type, sample_value),
            side = 3, line = 0.3, cex = 0.95, col = "#2C7A3D")
    }
  }

  # Check if any clipping occurred
  n_clipped_low <- sum(modified_vals <= param_bounds$min, na.rm = TRUE)
  n_clipped_high <- sum(modified_vals >= param_bounds$max, na.rm = TRUE)

  if (n_clipped_low > 0 || n_clipped_high > 0) {
    mtext(sprintf("⚠ WARNING: %d values clipped to bounds",
                  n_clipped_low + n_clipped_high),
          side = 1, line = 3.5, col = col_bounds, font = 2, cex = 1.0)
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

  # Pause to allow viewing the plot
  if (param != param_names[length(param_names)]) {
    cat("    Press Enter to continue to next plot...\n")
    readline()
  }
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

  # Pause to allow viewing the plot
  if (param != param_names[length(param_names)]) {
    cat("    Press Enter to continue to next plot...\n")
    readline()
  }
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

# Check if previous results exist
results_rds_file <- file.path(project_path, "ensemble_results.rds")
load_existing <- FALSE

if (file.exists(results_rds_file)) {
  cat("Found existing ensemble results:", results_rds_file, "\n")
  cat("  File size:", round(file.info(results_rds_file)$size / 1024^2, 2), "MB\n")
  cat("  Last modified:", format(file.info(results_rds_file)$mtime, "%Y-%m-%d %H:%M:%S"), "\n\n")
  cat("Do you want to:\n")
  cat("  1. Load existing results (skip ensemble runs)\n")
  cat("  2. Run new ensemble (overwrite existing file)\n")
  cat("Enter choice (1 or 2): ")
  choice <- readline()

  if (choice == "1") {
    load_existing <- TRUE
    cat("\nLoading existing results...\n")
    results_par <- readRDS(results_rds_file)
    cat("✓ Loaded", length(results_par$results), "simulation results\n")
    cat("  Parameter sets:", nrow(results_par$parameter_sets), "\n")
    cat("  Runtime:", round(results_par$runtime_minutes, 2), "minutes\n\n")
  } else {
    cat("\nWill run new ensemble and overwrite existing file.\n\n")
  }
}

if (!load_existing) {
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

  # Save results_par immediately after ensemble run
  cat("Saving ensemble results to RDS file...\n")
  saveRDS(results_par, file = results_rds_file)
  cat("✓ Ensemble results saved to:", results_rds_file, "\n")
  cat("  File size:", round(file.info(results_rds_file)$size / 1024^2, 2), "MB\n")
  cat("  To load later: results_par <- readRDS('", basename(results_rds_file), "')\n\n")
} # End of if (!load_existing)

# =============================================================================
# Step 6b: Visualize Ensemble Uncertainty
# =============================================================================

cat("=== Step 6b: Ensemble Uncertainty Visualization ===\n\n")

cat("Generating ensemble uncertainty plot...\n")
tryCatch({
  p_uncertainty <- plot_ensemble_uncertainty(
    ensemble_output = results_par,
    observed = NULL,
    subbasin_id = "0001",  # Specify which subbasin to plot
    variable = "QSIM"       # Plot simulated discharge
  )
  ggsave(
    filename = file.path(project_path, "ensemble_uncertainty.png"),
    plot = p_uncertainty,
    width = 12,
    height = 6,
    dpi = 300
  )
  cat("✓ Ensemble uncertainty plot saved to:", file.path(project_path, "ensemble_uncertainty.png"), "\n\n")
}, error = function(e) {
  cat("✗ Could not create ensemble uncertainty plot:", e$message, "\n")
  cat("  Error details:", conditionMessage(e), "\n\n")
})


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
nse_calculated <- calculate_ensemble_metrics(results_par, subbasin_id = "0001",
                      metric = "NSE", spinup = 365)
kge_calculated <- calculate_ensemble_metrics(results_par, subbasin_id = "0001",
                      metric = "KGE", spinup = 365)

cat("\nPBIAS values calculated:", paste(round(pbias_calculated, 3), collapse = ", "), "\n")
cat("Valid values:", sum(!is.na(pbias_calculated) & !is.nan(pbias_calculated)), "/", length(pbias_calculated), "\n")

cat("\nNSE values calculated:", paste(round(nse_calculated, 3), collapse = ", "), "\n")
cat("Valid values:", sum(!is.na(nse_calculated) & !is.nan(nse_calculated)), "/", length(nse_calculated), "\n")

cat("\nKGE values calculated:", paste(round(kge_calculated, 3), collapse = ", "), "\n")
cat("Valid values:", sum(!is.na(kge_calculated) & !is.nan(kge_calculated)), "/", length(kge_calculated), "\n\n")

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
cat("   - Usage: extract_cosero_metrics(results, subbasin_id = '0001', metric = 'KGE')\n\n")

cat("2. calculate_ensemble_metrics() - For custom metrics\n")
cat("   - Location: R/sensitivity_analysis.R:1251-1320\n")
cat("   - Recalculates from QOBS vs QSIM discharge data\n")
cat("   - Uses hydroGOF package (KGE, NSE, RMSE, PBIAS)\n")
cat("   - Automatically applies spin-up period exclusion\n")
cat("   - Usage: calculate_ensemble_metrics(results, subbasin_id = '0001', metric = 'PBIAS')\n\n")

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

# Calculate Sobol indices (only if we have enough samples)
cat("\n=== Sobol Sensitivity Analysis ===\n\n")
if (n_samples >= 10) {

  # Check for existing plot files and notify user they will be overwritten
  plot_files <- c("sobol_indices_nse.png", "sobol_indices_kge.png",
                  "dotty_plots_nse.png", "dotty_plots_kge.png")
  existing_plots <- plot_files[file.exists(file.path(project_path, plot_files))]

  if (length(existing_plots) > 0) {
    cat("Note: The following existing plots will be overwritten:\n")
    for (pf in existing_plots) {
      cat("  -", pf, "\n")
    }
    cat("\n")
  }

  # NSE-based Sobol analysis
  cat("Calculating Sobol sensitivity indices for NSE...\n")
  tryCatch({
    sobol_indices_nse <- calculate_sobol_indices(nse_extracted, samples)
    cat("✓ NSE Sobol indices calculated successfully\n")
    print(sobol_indices_nse)

    # Generate sensitivity plots
    cat("\n=== Generating NSE Sensitivity Plots ===\n")

    # Plot 1: Sobol indices bar chart for NSE
    cat("Creating NSE Sobol indices plot...\n")
    p_sobol_nse <- plot_sobol(sobol_indices_nse, title = "Sobol Sensitivity Indices (NSE)")
    nse_sobol_file <- file.path(project_path, "sobol_indices_nse.png")
    ggsave(
      filename = nse_sobol_file,
      plot = p_sobol_nse,
      width = 10,
      height = 6,
      dpi = 300
    )
    cat("✓ NSE Sobol plot saved to:", nse_sobol_file, "\n")

    # Convert sampled values to absolute parameter values
    cat("\nConverting sampled values to absolute parameter values...\n")
    absolute_params <- samples$parameter_sets
    for (param in param_names) {
      # Get original values and bounds
      original_vals <- original_all[[param]]
      param_bounds <- bounds[bounds$parameter == param, ]
      reference_val <- median(original_vals, na.rm = TRUE)

      # Convert based on modification type
      if (param_bounds$modification_type == "relchg") {
        # Multiply: absolute = reference × sampled
        absolute_params[[param]] <- reference_val * samples$parameter_sets[[param]]
      } else if (param_bounds$modification_type == "abschg") {
        # Add: absolute = reference + sampled
        absolute_params[[param]] <- reference_val + samples$parameter_sets[[param]]
      } else {
        # absval: already absolute
        absolute_params[[param]] <- samples$parameter_sets[[param]]
      }

      # Clip to physical bounds
      absolute_params[[param]] <- pmax(param_bounds$min,
                                       pmin(param_bounds$max,
                                            absolute_params[[param]]))
    }

    cat("✓ Absolute parameter values calculated\n")
    cat("  Parameter ranges:\n")
    for (param in param_names) {
      cat(sprintf("    %-10s: %.3f to %.3f\n", param,
                  min(absolute_params[[param]]), max(absolute_params[[param]])))
    }

    # Plot 2: Dotty plots with absolute values (parameter scatter plots)
    cat("\nCreating NSE dotty plots with absolute parameter values...\n")
    p_dotty_nse <- plot_dotty(
      parameter_sets = absolute_params,
      Y = nse_extracted,
      y_label = "NSE",
      n_col = 3,
      y_min = 0  # Start Y-axis from 0
    )
    nse_dotty_file <- file.path(project_path, "dotty_plots_nse.png")
    ggsave(
      filename = nse_dotty_file,
      plot = p_dotty_nse,
      width = 12,
      height = 8,
      dpi = 300
    )
    cat("✓ NSE dotty plots saved to:", nse_dotty_file, "\n")

  }, error = function(e) {
    cat("✗ Error in NSE sensitivity analysis:", e$message, "\n")
  })

  # KGE-based Sobol analysis
  cat("\n=== KGE Sensitivity Analysis ===\n\n")
  cat("Calculating Sobol sensitivity indices for KGE...\n")
  tryCatch({
    sobol_indices_kge <- calculate_sobol_indices(kge_calculated, samples)
    cat("✓ KGE Sobol indices calculated successfully\n")
    print(sobol_indices_kge)

    # Generate KGE sensitivity plots
    cat("\n=== Generating KGE Sensitivity Plots ===\n")

    # Plot 1: Sobol indices bar chart for KGE
    cat("Creating KGE Sobol indices plot...\n")
    p_sobol_kge <- plot_sobol(sobol_indices_kge, title = "Sobol Sensitivity Indices (KGE)")
    kge_sobol_file <- file.path(project_path, "sobol_indices_kge.png")
    ggsave(
      filename = kge_sobol_file,
      plot = p_sobol_kge,
      width = 10,
      height = 6,
      dpi = 300
    )
    cat("✓ KGE Sobol plot saved to:", kge_sobol_file, "\n")

    # Plot 2: Dotty plots for KGE with absolute values
    cat("\nCreating KGE dotty plots with absolute parameter values...\n")
    p_dotty_kge <- plot_dotty(
      parameter_sets = absolute_params,
      Y = kge_calculated,
      y_label = "KGE",
      n_col = 3,
      y_min = 0  # Start Y-axis from 0
    )
    kge_dotty_file <- file.path(project_path, "dotty_plots_kge.png")
    ggsave(
      filename = kge_dotty_file,
      plot = p_dotty_kge,
      width = 12,
      height = 8,
      dpi = 300
    )
    cat("✓ KGE dotty plots saved to:", kge_dotty_file, "\n")

  }, error = function(e) {
    cat("✗ Error in KGE sensitivity analysis:", e$message, "\n")
  })

  # Export sensitivity results
  cat("\n=== Exporting Sensitivity Results ===\n")
  results_dir <- file.path(project_path, "sensitivity_results")

  # Clean and recreate sensitivity results directory
  if (dir.exists(results_dir)) {
    cat("Removing old sensitivity results folder...\n")
    unlink(results_dir, recursive = TRUE)
    cat("✓ Old sensitivity results removed\n")
  }
  cat("Creating fresh sensitivity results directory...\n")
  dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  cat("✓ Directory created:", results_dir, "\n\n")

  # Export NSE results
  tryCatch({
    export_sensitivity_results(
      sobol_indices = sobol_indices_nse,
      parameter_sets = absolute_params,
      metrics = nse_extracted,
      output_dir = results_dir,
      prefix = "nse_sensitivity"
    )
    cat("✓ NSE sensitivity results exported to:", results_dir, "\n")
    cat("  Files created:\n")
    cat("    - nse_sensitivity_sobol.rds\n")
    cat("    - nse_sensitivity_parameters.csv\n")
    cat("    - nse_sensitivity_metrics.csv\n")
    cat("    - nse_sensitivity_summary.csv\n")
  }, error = function(e) {
    cat("✗ Could not export NSE results:", e$message, "\n")
  })

  # Export KGE results
  tryCatch({
    export_sensitivity_results(
      sobol_indices = sobol_indices_kge,
      parameter_sets = absolute_params,
      metrics = kge_calculated,
      output_dir = results_dir,
      prefix = "kge_sensitivity"
    )
    cat("✓ KGE sensitivity results exported to:", results_dir, "\n")
    cat("  Files created:\n")
    cat("    - kge_sensitivity_sobol.rds\n")
    cat("    - kge_sensitivity_parameters.csv\n")
    cat("    - kge_sensitivity_metrics.csv\n")
    cat("    - kge_sensitivity_summary.csv\n")
  }, error = function(e) {
    cat("✗ Could not export KGE results:", e$message, "\n")
  })
} else {
  cat("⚠ Skipping Sobol index calculation - need at least 10 samples (currently:", n_samples, ")\n")
  cat("  Sobol sensitivity analysis requires n ≥ 10 to produce meaningful results\n")
  cat("  For proper sensitivity analysis, use n ≥ 100 (recommended: n = 500-1000)\n\n")
  cat("Example for proper sensitivity analysis:\n")
  cat("  samples <- generate_sobol_samples(sobol_bounds, n = 500)\n")
  cat("  results <- run_cosero_ensemble_parallel(..., parameter_sets = samples$parameter_sets)\n")
  cat("  nse <- extract_cosero_metrics(results, subbasin_id = '0001', metric = 'NSE')\n")
  cat("  sobol_indices <- calculate_sobol_indices(nse, samples)\n")
  cat("  plot_sobol(sobol_indices)\n")
}

# =============================================================================
# Step 7b: Final Save of All Results
# =============================================================================

cat("\n=== Final Save ===\n\n")
cat("Saving final ensemble results (overwrites previous)...\n")
saveRDS(results_par, file = results_rds_file)
cat("✓ Final results saved to:", results_rds_file, "\n")
cat("  File size:", round(file.info(results_rds_file)$size / 1024^2, 2), "MB\n")
cat("  Contains:\n")
cat("    -", length(results_par$results), "simulation results\n")
cat("    -", nrow(results_par$parameter_sets), "parameter sets\n")
cat("    - Runtime:", round(results_par$runtime_minutes, 2), "minutes\n")
cat("    - Cores used:", results_par$n_cores, "\n\n")

cat("To reload these results in a new R session:\n")
cat("  results_par <- readRDS(file.path(project_path, 'ensemble_results.rds'))\n\n")

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
cat("  ✓ Step 6: Ensemble runs completed (", length(results_par$results), " simulations)\n", sep = "")
cat("  ✓ Step 7: Sensitivity analysis performed\n")
cat("  ✓ Step 8: Results saved to RDS file\n")

cat("\nFiles generated:\n")
cat("  Data files:\n")
cat("    - ensemble_results.rds (", round(file.info(results_rds_file)$size / 1024^2, 2), " MB)\n", sep = "")
cat("  Plots:\n")
cat("    - ensemble_uncertainty.png\n")
if (n_samples >= 10) {
  cat("    - sobol_indices_nse.png\n")
  cat("    - sobol_indices_kge.png\n")
  cat("    - dotty_plots_nse.png\n")
  cat("    - dotty_plots_kge.png\n")
  cat("  Sensitivity results:\n")
  cat("    - sensitivity_results/nse_sensitivity_*.csv\n")
  cat("    - sensitivity_results/kge_sensitivity_*.csv\n")
}

cat("\nTo reload results in future sessions:\n")
cat("  results_par <- readRDS('", results_rds_file, "')\n", sep = "")

cat("\n=== Test Complete ===\n")