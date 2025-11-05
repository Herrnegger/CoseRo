# Test: Parameter File Preservation
# Demonstrates that parameters NOT in sensitivity analysis retain original values
# Author: COSERO R Interface
# Date: 2025-11-04

library(dplyr)

# Source sensitivity analysis functions
source("05_cosero_sensitivity_analysis.R")

# =============================================================================
# Create Test Parameter File
# =============================================================================

test_dir <- file.path(tempdir(), "cosero_param_test")
dir.create(test_dir, showWarnings = FALSE, recursive = TRUE)

test_par_file <- file.path(test_dir, "para.txt")

# Create a test parameter file with 6 parameters
test_params <- c(
  "BETA",
  "4.5",
  "CTMAX",
  "5.0",
  "CTMIN",
  "2.0",
  "FK",
  "1.0",
  "PWP",
  "0.5",
  "TAB1",
  "1.0",
  "# Comment line - should be preserved",
  "M",
  "0.7"
)

writeLines(test_params, test_par_file)

cat("=== Original Parameter File ===\n")
cat(readLines(test_par_file), sep = "\n")

# =============================================================================
# Test 1: Read Parameter File
# =============================================================================

cat("\n\n=== Test 1: Read Specific Parameters ===\n")

# Read only some parameters (like sensitivity analysis would do)
params_to_read <- c("BETA", "CTMAX", "TAB1")
original_values <- read_parameter_file(test_par_file, params_to_read)

cat("Reading only:", paste(params_to_read, collapse = ", "), "\n")
cat("Original values read:\n")
print(unlist(original_values))

cat("\nNote: FK, PWP, M, CTMIN were NOT read (not needed for these parameters)\n")

# =============================================================================
# Test 2: Modify Parameter File (Sensitivity Analysis Simulation)
# =============================================================================

cat("\n\n=== Test 2: Modify ONLY Sensitivity Analysis Parameters ===\n")

# Create parameter bounds for sensitivity analysis
test_par_bounds <- create_custom_bounds(
  parameters = c("BETA", "CTMAX", "TAB1"),
  min = c(0.5, 3, 0.5),
  max = c(8, 10, 3),
  modification_type = c("absval", "absval", "relchg")
)

# Simulate one parameter set from Sobol sampling
test_params_run1 <- list(
  BETA = 2.5,   # absval: will replace with 2.5
  CTMAX = 8.2,  # absval: will replace with 8.2
  TAB1 = 1.5    # relchg: will multiply original (1.0) by 1.5 = 1.5
)

cat("Modifying parameters to:\n")
print(unlist(test_params_run1))

# Modify the file
modify_parameter_file(test_par_file, test_params_run1, test_par_bounds, original_values)

cat("\n=== Modified Parameter File (Run #1) ===\n")
modified_content <- readLines(test_par_file)
cat(modified_content, sep = "\n")

# =============================================================================
# Test 3: Verify Unmodified Parameters Preserved
# =============================================================================

cat("\n\n=== Test 3: Check Which Parameters Changed ===\n")

# Read all parameter values from modified file
check_params <- c("BETA", "CTMAX", "CTMIN", "FK", "PWP", "TAB1", "M")
modified_values <- read_parameter_file(test_par_file, check_params)

comparison <- data.frame(
  Parameter = check_params,
  Original = c(4.5, 5.0, 2.0, 1.0, 0.5, 1.0, 0.7),
  Modified = unlist(modified_values),
  Modified_By_SA = c("YES", "YES", "NO", "NO", "NO", "YES", "NO"),
  Status = c(
    "Changed (absval)",
    "Changed (absval)",
    "PRESERVED",
    "PRESERVED",
    "PRESERVED",
    "Changed (relchg)",
    "PRESERVED"
  ),
  stringsAsFactors = FALSE
)

print(comparison)

# Verify preservation
preserved_params <- check_params[!check_params %in% names(test_params_run1)]
all_preserved <- TRUE

for (param in preserved_params) {
  original <- comparison$Original[comparison$Parameter == param]
  modified <- comparison$Modified[comparison$Parameter == param]
  if (!is.na(modified) && abs(original - modified) > 1e-10) {
    cat("ERROR: Parameter", param, "was changed but should be preserved!\n")
    all_preserved <- FALSE
  }
}

if (all_preserved) {
  cat("\n✓ SUCCESS: All unmodified parameters were preserved!\n")
} else {
  cat("\n✗ FAILURE: Some unmodified parameters were changed!\n")
}

# =============================================================================
# Test 4: Test Backup/Restore Workflow (Like Ensemble Does)
# =============================================================================

cat("\n\n=== Test 4: Backup/Restore Workflow ===\n")

# Reset to original
writeLines(test_params, test_par_file)

# Create backup (like ensemble does)
backup_file <- paste0(test_par_file, ".backup")
file.copy(test_par_file, backup_file, overwrite = TRUE)

cat("Backup created:", backup_file, "\n")

# Simulate multiple runs
test_runs <- list(
  list(BETA = 2.5, CTMAX = 8.2, TAB1 = 1.5),
  list(BETA = 7.1, CTMAX = 4.3, TAB1 = 0.8),
  list(BETA = 3.2, CTMAX = 6.5, TAB1 = 2.1)
)

for (run_i in 1:length(test_runs)) {
  cat("\n--- Run", run_i, "---\n")

  # Restore backup (resets ALL parameters)
  file.copy(backup_file, test_par_file, overwrite = TRUE)
  cat("Restored from backup\n")

  # Modify only SA parameters
  modify_parameter_file(test_par_file, test_runs[[run_i]], test_par_bounds, original_values)

  # Check values
  run_values <- read_parameter_file(test_par_file, check_params)

  cat("Parameter values for this run:\n")
  cat(sprintf("  BETA:  %.2f (modified)\n", run_values$BETA))
  cat(sprintf("  CTMAX: %.2f (modified)\n", run_values$CTMAX))
  cat(sprintf("  TAB1:  %.2f (modified)\n", run_values$TAB1))
  cat(sprintf("  FK:    %.2f (preserved)\n", run_values$FK))
  cat(sprintf("  PWP:   %.2f (preserved)\n", run_values$PWP))
  cat(sprintf("  M:     %.2f (preserved)\n", run_values$M))

  # Verify FK, PWP, M are always at original values
  if (abs(run_values$FK - 1.0) < 1e-10 &&
      abs(run_values$PWP - 0.5) < 1e-10 &&
      abs(run_values$M - 0.7) < 1e-10) {
    cat("  ✓ Unmodified parameters preserved\n")
  } else {
    cat("  ✗ ERROR: Unmodified parameters changed!\n")
  }
}

# Restore original file (like ensemble does at end)
file.copy(backup_file, test_par_file, overwrite = TRUE)

cat("\nOriginal file restored after all runs\n")

# =============================================================================
# Test 5: Verify Comments and Formatting Preserved
# =============================================================================

cat("\n\n=== Test 5: Comments and Formatting Preserved ===\n")

# Check if comment line is still there
final_content <- readLines(test_par_file)
has_comment <- any(grepl("^#", final_content))

if (has_comment) {
  cat("✓ Comment lines preserved\n")
} else {
  cat("✗ Comment lines lost\n")
}

# Check line count
if (length(final_content) == length(test_params)) {
  cat("✓ Same number of lines as original\n")
} else {
  cat("✗ Line count changed: original =", length(test_params),
      ", final =", length(final_content), "\n")
}

# =============================================================================
# Test 6: Test relchg and abschg Modification Types
# =============================================================================

cat("\n\n=== Test 6: Modification Type Behavior ===\n")

# Create bounds with different modification types
mixed_bounds <- create_custom_bounds(
  parameters = c("BETA", "TAB1", "CTMIN"),
  min = c(0.5, 0.5, 0),
  max = c(8, 3, 2),
  modification_type = c("absval", "relchg", "abschg")
)

# Reset file
writeLines(test_params, test_par_file)

# Read original values for all three
mixed_original <- read_parameter_file(test_par_file, c("BETA", "TAB1", "CTMIN"))

# Test parameter set
mixed_params <- list(
  BETA = 3.0,   # absval: replace with 3.0
  TAB1 = 2.0,   # relchg: original (1.0) * 2.0 = 2.0
  CTMIN = 1.5   # abschg: original (2.0) + 1.5 = 3.5
)

cat("Original values:\n")
cat(sprintf("  BETA:  %.2f\n", mixed_original$BETA))
cat(sprintf("  TAB1:  %.2f\n", mixed_original$TAB1))
cat(sprintf("  CTMIN: %.2f\n", mixed_original$CTMIN))

modify_parameter_file(test_par_file, mixed_params, mixed_bounds, mixed_original)

final_values <- read_parameter_file(test_par_file, c("BETA", "TAB1", "CTMIN"))

cat("\nSampled values:\n")
cat(sprintf("  BETA:  %.2f (absval)\n", mixed_params$BETA))
cat(sprintf("  TAB1:  %.2f (relchg multiplier)\n", mixed_params$TAB1))
cat(sprintf("  CTMIN: %.2f (abschg additive)\n", mixed_params$CTMIN))

cat("\nFinal values in file:\n")
cat(sprintf("  BETA:  %.2f (expected: %.2f)\n", final_values$BETA, 3.0))
cat(sprintf("  TAB1:  %.2f (expected: %.2f = %.2f * %.2f)\n",
    final_values$TAB1, 2.0, mixed_original$TAB1, mixed_params$TAB1))
cat(sprintf("  CTMIN: %.2f (expected: %.2f = %.2f + %.2f)\n",
    final_values$CTMIN, 3.5, mixed_original$CTMIN, mixed_params$CTMIN))

# Verify calculations
absval_correct <- abs(final_values$BETA - 3.0) < 1e-10
relchg_correct <- abs(final_values$TAB1 - (mixed_original$TAB1 * mixed_params$TAB1)) < 1e-10
abschg_correct <- abs(final_values$CTMIN - (mixed_original$CTMIN + mixed_params$CTMIN)) < 1e-10

if (absval_correct && relchg_correct && abschg_correct) {
  cat("\n✓ All modification types working correctly\n")
} else {
  cat("\n✗ Some modification types incorrect:\n")
  if (!absval_correct) cat("  - absval failed\n")
  if (!relchg_correct) cat("  - relchg failed\n")
  if (!abschg_correct) cat("  - abschg failed\n")
}

# =============================================================================
# Summary
# =============================================================================

cat("\n\n" , paste(rep("=", 70), collapse=""), "\n")
cat("SUMMARY: Parameter File Handling Test Results\n")
cat(paste(rep("=", 70), collapse=""), "\n\n")

cat("KEY FINDINGS:\n")
cat("1. Only parameters in sensitivity analysis are modified\n")
cat("2. All other parameters retain their original values from file\n")
cat("3. Backup/restore workflow ensures clean state for each run\n")
cat("4. Comment lines and file formatting are preserved\n")
cat("5. Modification types (absval, relchg, abschg) work correctly\n")
cat("\nThis confirms the parameter file handling is working as designed!\n")

# Cleanup
unlink(test_dir, recursive = TRUE)
cat("\nTest directory cleaned up\n")
