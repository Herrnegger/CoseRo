# Check if COSERO functions are properly exported
# Run this after reloading the package

cat("Checking COSERO package exports...\n\n")

# Try to load the package
if (requireNamespace("COSERO", quietly = TRUE)) {
  library(COSERO)
  cat("✓ COSERO package loaded\n\n")
} else {
  cat("! Package not installed - using devtools::load_all()\n")
  devtools::load_all()
  cat("✓ Package functions loaded\n\n")
}

# Check for key functions
functions_to_check <- c(
  "run_cosero",
  "run_cosero_ensemble",
  "run_cosero_ensemble_parallel",
  "launch_cosero_app",
  "read_cosero_output",
  "plot_discharge"
)

cat("Checking exported functions:\n")
for (fn in functions_to_check) {
  if (exists(fn, mode = "function")) {
    cat(sprintf("  ✓ %s\n", fn))
  } else {
    cat(sprintf("  ✗ %s - NOT FOUND\n", fn))
  }
}

cat("\nTo get help on any function, use:\n")
cat("  ?run_cosero\n")
cat("  ?launch_cosero_app\n")
