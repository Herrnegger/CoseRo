# COSERO Workbench Launcher
# This script ensures the working directory is set correctly before launching the app

# Set working directory to the script's location
script_path <- normalizePath(dirname(sys.frame(1)$ofile), winslash = "/")
setwd(script_path)
cat("Working directory set to:", getwd(), "\n")

# Verify www directory exists
if (!dir.exists("www")) {
  stop("ERROR: www directory not found! Please ensure you are in the correct directory.")
}
if (!file.exists("www/logo.svg")) {
  stop("ERROR: www/logo.svg not found!")
}

cat("Logo file found at: www/logo.svg\n")
cat("Starting COSERO Workbench...\n\n")

# Run the Shiny app
shiny::runApp("04_cosero_visualization_app.R")
