# COSERO Sensitivity Analysis - Custom Bounds Examples
# Demonstrates how to use custom parameter bounds instead of CSV file
# Author: COSERO R Interface
# Date: 2025-11-04

# Load libraries and source sensitivity analysis functions
source("05_cosero_sensitivity_analysis.R")

# =============================================================================
# Example 1: Using create_custom_bounds() function
# =============================================================================
# This is the most flexible approach - you define everything explicitly

cat("\n=== Example 1: Using create_custom_bounds() ===\n")

# Define custom bounds for 3 parameters
custom_bounds_1 <- create_custom_bounds(
  parameters = c("BETA", "CTMAX", "TAB1"),
  min = c(0.5, 3, 0.5),           # Custom minimum values
  max = c(8, 10, 3),               # Custom maximum values
  modification_type = c("absval", "absval", "relchg"),  # How to apply changes
  default = c(4.5, 5, 1),          # Optional: default values
  description = c(                  # Optional: descriptions
    "Runoff generation parameter (custom bounds)",
    "Maximum snow melt factor (custom bounds)",
    "Temperature adjustment factor (custom bounds)"
  ),
  category = c("runoff", "snow", "snow")  # Optional: categories
)

print(custom_bounds_1)

# Use these custom bounds in sensitivity analysis
sobol_bounds_1 <- create_sobol_bounds(custom_bounds_1)
sobol_samples_1 <- generate_sobol_samples(sobol_bounds_1, n = 50)

cat("\nGenerated", nrow(sobol_samples_1$parameter_sets), "parameter sets\n")

# =============================================================================
# Example 2: Using load_parameter_bounds() with custom parameters
# =============================================================================
# This is more concise when you just need to override bounds

cat("\n\n=== Example 2: Using load_parameter_bounds() with custom parameters ===\n")

# Define custom bounds directly in load_parameter_bounds()
custom_bounds_2 <- load_parameter_bounds(
  parameters = c("BETA", "CTMAX", "FK"),
  custom_min = c(1, 4, 0.1),
  custom_max = c(6, 8, 0.5),
  custom_modification_type = c("absval", "absval", "absval")
)

print(custom_bounds_2)

# =============================================================================
# Example 3: Using load_parameter_bounds() with pre-built custom_bounds
# =============================================================================
# Useful when you want to reuse the same bounds or modify them

cat("\n\n=== Example 3: Using pre-built custom_bounds object ===\n")

# Create custom bounds once
my_bounds <- create_custom_bounds(
  parameters = c("BETA", "CTMAX", "CTMIN", "TAB1", "TAB2"),
  min = c(0.5, 3, 0.5, 0.5, 0.5),
  max = c(8, 10, 4, 3, 3),
  modification_type = c("absval", "absval", "absval", "relchg", "relchg")
)

# Use them in load_parameter_bounds()
custom_bounds_3 <- load_parameter_bounds(custom_bounds = my_bounds)

print(custom_bounds_3)

# You can also filter specific parameters
custom_bounds_3_filtered <- load_parameter_bounds(
  custom_bounds = my_bounds,
  parameters = c("BETA", "CTMAX")  # Only use these two
)

cat("\nFiltered to 2 parameters:\n")
print(custom_bounds_3_filtered)

# =============================================================================
# Example 4: CSV as fallback (default behavior)
# =============================================================================
# If no custom bounds are provided, the CSV file is used automatically

cat("\n\n=== Example 4: CSV as fallback (default behavior) ===\n")

# This will use the CSV file (cosero_parameter_bounds.csv)
csv_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "TAB1"))

print(csv_bounds)

# =============================================================================
# Example 5: Mixing CSV and custom bounds
# =============================================================================
# You can load from CSV and then modify specific parameters

cat("\n\n=== Example 5: Mixing CSV and custom bounds ===\n")

# Load initial bounds from CSV
base_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "TAB1", "TAB2"))

# Modify specific parameter bounds
library(dplyr)

mixed_bounds <- base_bounds %>%
  mutate(
    min = case_when(
      parameter == "BETA" ~ 1.0,      # Narrow BETA range
      parameter == "CTMAX" ~ 4.0,     # Narrow CTMAX range
      TRUE ~ min
    ),
    max = case_when(
      parameter == "BETA" ~ 6.0,
      parameter == "CTMAX" ~ 8.0,
      TRUE ~ max
    )
  )

cat("Original CSV bounds:\n")
print(base_bounds)

cat("\nModified bounds:\n")
print(mixed_bounds)

# =============================================================================
# Example 6: Full sensitivity analysis workflow with custom bounds
# =============================================================================

cat("\n\n=== Example 6: Full workflow with custom bounds ===\n")

# Step 1: Define custom bounds
workflow_bounds <- create_custom_bounds(
  parameters = c("BETA", "CTMAX", "TAB1"),
  min = c(1, 4, 0.5),
  max = c(6, 8, 2),
  modification_type = c("absval", "absval", "relchg")
)

cat("Custom bounds for workflow:\n")
print(workflow_bounds)

# Step 2: Create Sobol bounds
workflow_sobol_bounds <- create_sobol_bounds(workflow_bounds)

# Step 3: Generate Sobol samples
workflow_samples <- generate_sobol_samples(workflow_sobol_bounds, n = 10)

cat("\nGenerated", nrow(workflow_samples$parameter_sets), "parameter sets\n")
cat("First 5 parameter sets:\n")
print(head(workflow_samples$parameter_sets, 5))

# Step 4: Run ensemble (commented out - requires COSERO project)
# ensemble_results <- run_cosero_ensemble(
#   project_path = "path/to/cosero/project",
#   parameter_sets = workflow_samples$parameter_sets,
#   par_bounds = workflow_bounds
# )

# Step 5: Extract outputs and calculate sensitivity indices (commented out)
# runoff_output <- extract_ensemble_output(ensemble_results,
#                                          variable = "runoff",
#                                          aggregation = "monthly")
# runoff_means <- sapply(runoff_output, function(x) mean(x$value, na.rm = TRUE))
# sobol_indices <- calculate_sobol_indices(Y = runoff_means,
#                                          sobol_samples = workflow_samples)
# plot_sobol(sobol_indices)

# =============================================================================
# Summary of approaches
# =============================================================================

cat("\n\n=== Summary of Approaches ===\n")
cat("
1. create_custom_bounds():
   - Most flexible
   - Define all parameters explicitly
   - Good for complex custom configurations

2. load_parameter_bounds() with custom_min/max/type:
   - More concise
   - Quick way to define custom bounds
   - Parameters must be specified

3. load_parameter_bounds() with custom_bounds:
   - Reuse bounds across multiple analyses
   - Can filter to specific parameters
   - Good for modular code

4. load_parameter_bounds() with just parameters:
   - Uses CSV file as fallback
   - Default behavior preserved
   - Safe and reliable

5. Mixing CSV and custom:
   - Load from CSV, then modify
   - Combines safety of CSV with custom tweaks
   - Best of both worlds

MODIFICATION TYPES:
  - 'absval': Replace with sampled value (e.g., BETA = 2.5)
  - 'relchg': Multiply original by sampled value (e.g., TAB1 = original * 1.3)
  - 'abschg': Add sampled value to original (e.g., TCOR = original + 1.5)
")

cat("\n=== Examples completed ===\n")
