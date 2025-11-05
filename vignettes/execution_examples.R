# COSERO Configuration Examples
# Documentation and examples for running COSERO with custom settings
# Author: COSERO R Interface Examples
# Date: 2025-09-26

# Load the COSERO package
library(COSERO)

# ==============================================================================
# COSERO PARAMETER DOCUMENTATION
# ==============================================================================

# All parameters that can be modified in defaults.txt:

# DATE AND TIME PARAMETERS:
# STARTDATE = "2015 1 1 0 0"      # Start date: year month day hour minute
# ENDDATE   = "2016 12 31 23 59"  # End date: year month day hour minute
# SPINUP    = 365                 # Warm-up period in time steps

# OUTPUT CONTROL:
# OUTPUTTYPE = 1                  # 1=basic outputs (runoff, plus, prec, stats), 2=+monthly/glacier files, 3=+annual/seasonal means
# OUTCONTROL = 0                  # 0=no zonal outputs, 1=timestep zonal outputs to cdr/output/ (requires OUTPUTTYPE=3)
# SC_FLAG    = 1                  # 0=local subbasin area (EZFL_B), 1=total upstream catchment area (EZFL_T) for flux calculation

# FILE SETTINGS:
# PROJECTINFO = "My_Project"      # Project name for output files
# DATAFILE    = "data.txt"        # Input runoff data file
# PARAFILE    = "parameters.txt"  # Parameter file to use
# RUNOFFFILE  = "COSERO.runoff"      # Output runoff filename
# STATSFILE   = "statistics.txt"  # Output statistics filename

# ADVANCED SETTINGS:
# IKL         = 5                 # Number of snow classes
# NCLASS      = 10                # Number of land use classes
# ADDFLUXCONT = 0                 # Additional inflow control
# ADDFLUXFILE = "inflow.txt"      # Additional inflow file

# RUN OPTIONS (function parameters, not in defaults.txt):
# statevar_source = 1             # State variables from parameter file (default)
#                 = 2             # State variables from statevar.dmp (warm start)
# tmmon_option    = 1             # Monthly temperature from parameter file (default)
#                 = 2             # Calculate monthly temperature from data

# ==============================================================================
# USAGE EXAMPLES
# ==============================================================================

# Set your project path
project_path <- "D:/OneDrive - Universität für Bodenkultur Wien/COSERO_Run_R/COSERO_MonteCarlo_Optimierung_SH"


# ==============================================================================
# EXAMPLE 1: Comprehensive run with all common options
# ==============================================================================

example1_settings <- list(
  # Date and time
  STARTDATE = "2010 1 1 0 0",        # Start: year month day hour minute
  ENDDATE = "2022 12 31 0 0",      # End: year month day hour minute
  SPINUP = 365,                      # Warm-up period in time steps

  # Output control
  OUTPUTTYPE = 3,                    # 1=basic, 2=+monthly/glacier, 3=+annual/seasonal means
  SC_FLAG = 1,                       # 0=local subbasin (EZFL_B), 1=total upstream (EZFL_T)
  OUTCONTROL = 0,                    # 0=no zonal outputs, 1=timestep zonal to cdr/output/ (needs OUTPUTTYPE=3)

  # Run options
  tmmon_option = 1,                  # 1=from parameter file, 2=calculate from data
  statevar_source = 1,               # 1=from parameter file, 2=from statevar.dmp (warm start)

  # File settings
  PROJECTINFO = "COSERO_R",          # Project name for output files
  #DATAFILE = "data.txt",             # Input runoff data file
  #PARAFILE = "para.txt",             # Parameter file name
  #RUNOFFFILE = "COSERO.runoff",      # Output runoff file name
  #STATSFILE = "statistics.txt",      # Output statistics file name

  # Advanced settings
  IKL = 9,                           # Number of snow classes
  NCLASS = 10,                       # Number of land use classes
  ADDFLUXCONT = 0,                   # Additional inflow control: 0=off, 1=on
  ADDFLUXFILE = "addflux.txt"       # Additional inflow file
)

# Uncomment to run:
# result1 <- run_cosero(project_path, defaults_settings = example1_settings)

# Run without reading outputs (faster for optimization/batch runs):
# result1 <- run_cosero(project_path, defaults_settings = example1_settings, read_outputs = FALSE)

# Run without changing anything
cosero_run_result1 <- run_cosero(project_path, quiet = FALSE, read_outputs = TRUE)

# Run in quiet mode (only shows runtime and errors):
cosero_run_result1 <- run_cosero(project_path, defaults_settings = example1_settings, 
  quiet = FALSE, read_outputs = TRUE)


# Read
source("02_cosero_readers.R")

output_dir <- "COSERO_MonteCarlo_Optimierung_SH/output"

cat("Testing COSERO readers...\n\n")

# Read all data
result <- read_cosero_output(output_dir)

cat("\n=== Summary ===\n")
cat("OUTPUTTYPE:", result$metadata$outputtype, "\n")
cat("Subbasins:", paste(result$metadata$subbasins, collapse=", "), "\n\n")

cat("Loaded datasets:\n")
for (name in names(result)) {
  if (name == "metadata") next
  obj <- result[[name]]
  if (is.data.frame(obj)) {
    cat(sprintf("  %s: %d rows, %d cols\n", name, nrow(obj), ncol(obj)))
  } else if (is.list(obj)) {
    cat(sprintf("  %s: list with %d elements\n", name, length(obj)))
  } else if (is.null(obj)) {
    cat(sprintf("  %s: NULL\n", name))
  }
}

# Test monitor_subbasins
if (!is.null(result$monitor_subbasins)) {
  cat("\nmonitor_subbasins structure:\n")
  sb1 <- result$monitor_subbasins[["0001"]]
  cat("  Subbasin 0001:", nrow(sb1), "rows,", ncol(sb1), "cols\n")
  cat("  Variables:", paste(head(colnames(sb1), 10), collapse=", "), "...\n")
}

# Test water_balance cumulative conversion
if (!is.null(result$water_balance)) {
  cat("\nwater_balance columns:\n")
  wb_cols <- colnames(result$water_balance)
  et_cols <- grep("ETAG|QABG", wb_cols, value = TRUE)
  cat("  ET/Runoff cols:", paste(head(et_cols, 5), collapse=", "), "\n")
}

cat("\nTest completed successfully!\n")
