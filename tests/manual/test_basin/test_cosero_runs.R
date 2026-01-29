# COSERO Run Function Test Script
# Test different run configurations on D:\temp\COSERO_test
# Based on examples from 01_cosero_core_run_README.md and 03_cosero_execution_examples_README.md
devtools::load_all("D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R")
#devtools::document()
library(COSERO)  # Assuming the package is loaded

# Project path
project_path <- "D:/temp/COSERO_test"

# Check if project exists
if (!dir.exists(project_path)) {
  stop("Project directory does not exist: ", project_path)
}

if (!file.exists(file.path(project_path, "COSERO.exe"))) {
  stop("COSERO.exe not found in project directory")
}

settings <- read_defaults(file.path(project_path, "input/defaults.txt"))


cat("To launch the app with this project, run:\n")
launch_cosero_app(project_path)

cat("==============================================\n")
cat("COSERO RUN FUNCTION TEST SCRIPT\n")
cat("Project:", project_path, "\n")
cat("==============================================\n\n")

# ==============================================================================
# TEST 1: Basic run with date range and OUTPUTTYPE 1
# ==============================================================================
cat("\n--- TEST 1: Basic run with date range (OUTPUTTYPE 1) ---\n")

result1 <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "1991 1 1 0 0",
    ENDDATE = "2020 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 1,
    PARAFILE = "para_ini.txt" #"para_ini.txt"# "para_BEST_NSE.txt"
  ),
  read_outputs = TRUE,
  quiet = FALSE
)

??COSERO

extract_run_metrics(result1, subbasin_id = "all", metric = "KGE")
calculate_run_metrics(result1, subbasin_id = "all", metric = "NSE", spinup = 365)
calculate_ensemble_metrics()


result1$output_data$statistics

if (result1$success) {
  cat("SUCCESS - Runtime:", round(result1$runtime_seconds, 2), "seconds\n")
  cat("Available outputs:", names(result1$output_data), "\n")
  if (!is.null(result1$output_data$runoff)) {
    cat("Runoff data rows:", nrow(result1$output_data$runoff), "\n")
  }
  if (!is.null(result1$output_data$statistics)) {
    print(result1$output_data$statistics)
  }
} else {
  cat("FAILED - Error:", result1$error_message, "\n")
}

# ==============================================================================
# TEST 2: Complete configuration with OUTPUTTYPE 3
# ==============================================================================
cat("\n--- TEST 2: Complete configuration (OUTPUTTYPE 3) ---\n")

result2 <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2021 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 3,
    SC_FLAG = 1,
    OUTCONTROL = 0,
    PROJECTINFO = "TestRun"
  ),
  read_outputs = TRUE,
  quiet = FALSE
)

if (result2$success) {
  cat("SUCCESS - Runtime:", round(result2$runtime_seconds, 2), "seconds\n")
  cat("Available outputs:", names(result2$output_data), "\n")

  # Check for OUTPUTTYPE 3 specific files
  if (!is.null(result2$output_data$monitor)) {
    cat("Monitor data rows:", nrow(result2$output_data$monitor), "\n")
  }
  if (!is.null(result2$output_data$rundepth)) {
    cat("Rundepth data rows:", nrow(result2$output_data$rundepth), "\n")
  }
} else {
  cat("FAILED - Error:", result2$error_message, "\n")
}

# ==============================================================================
# TEST 3: Fast batch run without reading outputs
# ==============================================================================
cat("\n--- TEST 3: Fast batch run (read_outputs = FALSE, quiet = TRUE) ---\n")

result3 <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2021 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 1
  ),
  read_outputs = FALSE,
  quiet = TRUE
)

if (result3$success) {
  cat("SUCCESS - Runtime:", round(result3$runtime_seconds, 2), "seconds\n")
  cat("Output data present:", !is.null(result3$output_data), "\n")
} else {
  cat("FAILED - Error:", result3$error_message, "\n")
}

# ==============================================================================
# TEST 4: Warm start continuation run (two-period simulation)
# ==============================================================================
cat("\n--- TEST 4: Warm start continuation run ---\n")

# First run (creates statevar.dmp)
cat("\nPart A: Initial run (2015-2017, cold start)\n")
result4a <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2015 1 1 0 0",
    ENDDATE = "2017 12 31 0 0",
    SPINUP = 365,
    OUTPUTTYPE = 2
  ),
  statevar_source = 1,  # Cold start
  read_outputs = FALSE,
  quiet = TRUE
)

if (result4a$success) {
  cat("SUCCESS - Runtime:", round(result4a$runtime_seconds, 2), "seconds\n")

  # Check if statevar.dmp was created
  statevar_file <- file.path(project_path, "output", "statevar.dmp")
  if (file.exists(statevar_file)) {
    cat("statevar.dmp created successfully\n")

    # Second run (warm start from statevar.dmp)
    cat("\nPart B: Continuation run (2018-2020, warm start)\n")
    result4b <- run_cosero(
      project_path,
      defaults_settings = list(
        STARTDATE = "2018 1 1 0 0",
        ENDDATE = "2020 12 31 0 0",
        SPINUP = 1,  # Minimum required (warm start minimizes spinup need)
        OUTPUTTYPE = 2
      ),
      statevar_source = 2,  # Warm start
      read_outputs = FALSE,
      quiet = TRUE
    )

    if (result4b$success) {
      cat("SUCCESS - Runtime:", round(result4b$runtime_seconds, 2), "seconds\n")
      cat("Warm start successful\n")
    } else {
      cat("FAILED - Error:", result4b$error_message, "\n")
    }
  } else {
    cat("WARNING: statevar.dmp not created\n")
  }
} else {
  cat("FAILED - Error:", result4a$error_message, "\n")
}

# ==============================================================================
# TEST 5: Different tmmon_option settings
# ==============================================================================
cat("\n--- TEST 5: TMMon option comparison ---\n")

# Option 1: Use TMMon from parameter file
cat("\nOption 1: TMMon from parameter file\n")
result5a <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2020 6 30 0 0",
    SPINUP = 100,
    OUTPUTTYPE = 1
  ),
  tmmon_option = 1,
  read_outputs = FALSE,
  quiet = TRUE
)

if (result5a$success) {
  cat("SUCCESS - Runtime:", round(result5a$runtime_seconds, 2), "seconds\n")
} else {
  cat("FAILED - Error:", result5a$error_message, "\n")
}

# Option 2: Calculate TMMon from input data
cat("\nOption 2: TMMon calculated from input data\n")
result5b <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "1991 1 1 0 0",
    ENDDATE = "2020 12 31 0 0",
    SPINUP = 1,
    OUTPUTTYPE = 1
  ),
  tmmon_option = 2,
  read_outputs = FALSE,
  quiet = TRUE
)

if (result5b$success) {
  cat("SUCCESS - Runtime:", round(result5b$runtime_seconds, 2), "seconds\n")
} else {
  cat("FAILED - Error:", result5b$error_message, "\n")
}

# ==============================================================================
# TEST 6: Different SC_FLAG settings
# ==============================================================================
cat("\n--- TEST 6: SC_FLAG comparison (local vs upstream area) ---\n")

# SC_FLAG = 0: Local subbasin area
cat("\nSC_FLAG = 0: Local subbasin area (EZFL_B)\n")
result6a <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2020 6 30 0 0",
    SPINUP = 100,
    OUTPUTTYPE = 1,
    SC_FLAG = 0
  ),
  read_outputs = FALSE,
  quiet = TRUE
)

if (result6a$success) {
  cat("SUCCESS - Runtime:", round(result6a$runtime_seconds, 2), "seconds\n")
} else {
  cat("FAILED - Error:", result6a$error_message, "\n")
}

# SC_FLAG = 1: Total upstream catchment area
cat("\nSC_FLAG = 1: Total upstream area (EZFL_T)\n")
result6b <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = "2020 1 1 0 0",
    ENDDATE = "2020 6 30 0 0",
    SPINUP = 100,
    OUTPUTTYPE = 1,
    SC_FLAG = 1
  ),
  read_outputs = FALSE,
  quiet = TRUE
)

if (result6b$success) {
  cat("SUCCESS - Runtime:", round(result6b$runtime_seconds, 2), "seconds\n")
} else {
  cat("FAILED - Error:", result6b$error_message, "\n")
}

# ==============================================================================
# SUMMARY
# ==============================================================================
cat("\n==============================================\n")
cat("TEST SUMMARY\n")
cat("==============================================\n")

tests <- list(
  "Test 1: Basic run (OUTPUTTYPE 1)" = result1$success,
  "Test 2: Complete config (OUTPUTTYPE 3)" = result2$success,
  "Test 3: Fast batch run" = result3$success,
  "Test 4a: Cold start (first period)" = result4a$success,
  "Test 4b: Warm start (continuation)" = if(exists("result4b")) result4b$success else FALSE,
  "Test 5a: TMMon from file" = result5a$success,
  "Test 5b: TMMon calculated" = result5b$success,
  "Test 6a: SC_FLAG = 0" = result6a$success,
  "Test 6b: SC_FLAG = 1" = result6b$success
)

for (test_name in names(tests)) {
  status <- if(tests[[test_name]]) "PASS" else "FAIL"
  cat(sprintf("%-40s %s\n", test_name, status))
}

total_tests <- length(tests)
passed_tests <- sum(unlist(tests))
cat(sprintf("\nTotal: %d/%d tests passed\n", passed_tests, total_tests))

cat("\n==============================================\n")
cat("Test script completed\n")
cat("==============================================\n")
