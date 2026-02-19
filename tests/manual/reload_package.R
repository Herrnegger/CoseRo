# Reload COSERO Package
# Run this script after making changes to the package code

# Option 1: Load all functions for development (fastest)
cat("Loading package functions...\n")
devtools::load_all()

cat("\n✓ Package reloaded successfully!\n")
cat("\nYou can now use:\n")
cat("  - run_cosero()\n")
cat("  - run_cosero_ensemble()\n")
cat("  - run_cosero_ensemble_parallel()\n")
cat("  - launch_cosero_app()\n")
cat("\nTry: ?run_cosero for help\n")
