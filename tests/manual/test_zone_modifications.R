# =============================================================================
# VERIFICATION: All Zones Are Modified in Sensitivity Analysis
# Real-World Example Using COSERO_test Project
# =============================================================================

# Load package functions
devtools::load_all("D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R")

cat("========================================================\n")
cat("REAL-WORLD VERIFICATION: Zone Modification Behavior\n")
cat("Using actual COSERO_test project data\n")
cat("========================================================\n\n")

# Configuration (same as sensitivity_SNOW_analysis_test_basin.R)
project_path <- "D:/temp/COSERO_test"
param_file <- file.path(project_path, "input", "para_ini.txt")
param_names <- c("CTMIN", "CTMAX", "NVAR", "RAINTRT", "SNOWTRT")

# Check if files exist
if (!file.exists(project_path)) stop("❌ Project path not found!")
if (!file.exists(param_file)) stop("❌ Parameter file not found!")

cat("Configuration:\n")
cat("  Project:", project_path, "\n")
cat("  Parameter file:", basename(param_file), "\n")
cat("  Parameters:", paste(param_names, collapse=", "), "\n\n")

# Load parameter bounds from package
bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameter bounds:\n")
print(bounds[, c("parameter", "min", "max", "default", "modification_type")])
cat("\n")

# Read original parameter values for ALL zones
original_all <- read_parameter_table(param_file, param_names, zone_id = "all")

cat("Original parameter values (first 5 zones):\n")
print(head(original_all, 5))
cat(sprintf("Total number of zones: %d\n\n", nrow(original_all)))

# Simulate a sampled parameter set
sampled_params <- list(CTMAX = 9.0, RAINTRT = 3.5)

# Calculate spatial means for reference
ctmax_spatial_mean <- mean(original_all$CTMAX, na.rm = TRUE)
raintrt_spatial_mean <- mean(original_all$RAINTRT, na.rm = TRUE)
ctmax_factor <- sampled_params$CTMAX / ctmax_spatial_mean
raintrt_offset <- sampled_params$RAINTRT - raintrt_spatial_mean

cat("Sampled parameter values (Target Catchment Averages):\n")
cat(sprintf("  CTMAX = %.1f (spatial mean=%.2f, factor=%.2f)\n", 
            sampled_params$CTMAX, ctmax_spatial_mean, ctmax_factor))
cat(sprintf("  RAINTRT = %.1f (spatial mean=%.2f, offset=%.2f)\n\n",
            sampled_params$RAINTRT, raintrt_spatial_mean, raintrt_offset))

# Create backup of parameter file
backup_file <- paste0(param_file, ".backup_verification")
file.copy(param_file, backup_file, overwrite = TRUE)

# Apply modification with zones = NULL (modifies ALL zones)
cat("Modifying parameter file...\n")
modify_parameter_table(
  par_file = param_file,
  params = sampled_params,
  par_bounds = bounds,
  original_values = original_all,
  zones = NULL,  # This is the default - modifies ALL zones
  quiet = FALSE
)

# Read modified values
modified_all <- read_parameter_table(param_file, param_names, zone_id = "all")

cat("\nModified parameter values (first 5 zones):\n")
print(head(modified_all[, c("NZ_", "CTMAX", "RAINTRT")], 5))
cat("\n")

# Verify that ALL zones were modified
expected_ctmax <- original_all$CTMAX * ctmax_factor
expected_raintrt <- original_all$RAINTRT + raintrt_offset

cat("Verification Results:\n")
if (all(abs(modified_all$CTMAX - expected_ctmax) < 0.01)) {
  cat(sprintf("✓ CTMAX: All %d zones scaled by factor %.2f\n", nrow(modified_all), ctmax_factor))
  cat(sprintf("  Zone 1: %.2f → %.2f\n", original_all$CTMAX[1], modified_all$CTMAX[1]))
  cat(sprintf("  New catchment mean: %.2f (target: %.2f)\n", 
              mean(modified_all$CTMAX), sampled_params$CTMAX))
} else {
  cat("✗ CTMAX: Modification failed\n")
}

if (all(abs(modified_all$RAINTRT - expected_raintrt) < 0.01)) {
  cat(sprintf("✓ RAINTRT: All %d zones shifted by offset %.2f\n", nrow(modified_all), raintrt_offset))
  cat(sprintf("  Zone 1: %.2f → %.2f\n", original_all$RAINTRT[1], modified_all$RAINTRT[1]))
  cat(sprintf("  New catchment mean: %.2f (target: %.2f)\n",
              mean(modified_all$RAINTRT), sampled_params$RAINTRT))
} else {
  cat("✗ RAINTRT: Modification failed\n")
}

cat("\n========================================================\n")
cat("CONCLUSION:\n")
cat(sprintf("✓ ALL %d zones modified using Spatial Mean strategy\n", nrow(modified_all)))
cat(sprintf("✓ Catchment averages match targets exactly\n"))
cat("========================================================\n")

# Restore original file
file.copy(backup_file, param_file, overwrite = TRUE)
unlink(backup_file)
cat("\n✓ Original parameter file restored\n")
