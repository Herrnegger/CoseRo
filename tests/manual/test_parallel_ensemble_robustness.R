# =============================================================================
# Manual test — parallel ensemble robustness (persistent-worker fix)
# =============================================================================
# Regression for the thread-directory reuse race that caused diffuse "hard_fail"
# runs in run_cosero_ensemble_parallel() under cold start (statevar_source = 1).
#
# History: with the old batched/modulo thread assignment, a cold-start NDC
# ensemble produced ~13% hard fails (291/2500) that REPRODUCED FINE in isolation
# -> the failures were a parallel shared-resource (thread-dir reuse) problem, not
# a parameter problem. A serial run of the SAME ensemble gave 100% success.
# The fix binds each worker permanently to its own directory (no reuse across
# runs), so this should now pass at 100%.
#
# This test runs a SMALL cold-start NDC ensemble and asserts (near-)100% success
# AND that the parallel result matches a serial reference run-for-run.
#
# Run: "C:/Program Files/R/R-4.5.2/bin/Rscript.exe" tests/manual/test_parallel_ensemble_robustness.R
# (or source interactively after devtools::load_all())
# =============================================================================

devtools::load_all()

# --- Settings ----------------------------------------------------------------
project_path <- "D:/temp/Wildalpen_Example_0.9.3"   # NDC=5 aggregated example
n_sobol      <- 8        # small: 8 * (k+2) runs — enough to exercise concurrency
n_cores      <- 8

base_settings <- list(
  STARTDATE  = c(2000, 10, 1, 0, 0),
  ENDDATE    = c(2010, 9, 30, 0, 0),   # shorter period -> faster test
  SPINUP     = 365,
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini_agg.txt"
)

# Include NDC so the cold-start regime (the one that exposed the race) is active
param_names <- c("NDC", "LAPSE_P", "SOILVAR", "BETA", "M", "PCOR", "FHL")

stopifnot(dir.exists(project_path),
          file.exists(file.path(project_path, "COSERO.exe")))

# --- Build a small Sobol design ---------------------------------------------
par_bounds   <- load_parameter_bounds(parameters = param_names)
sobol_bounds <- create_sobol_bounds(par_bounds)
samples      <- generate_sobol_samples(sobol_bounds, n = n_sobol, order = "first")
n_runs       <- nrow(samples$parameter_sets)
cat(sprintf("Test ensemble: %d runs (%d params), cold start\n", n_runs, length(param_names)))

reducer <- make_var_reducer(c("runoff"))   # keep it light

# --- Parallel run (the function under test) ----------------------------------
cat("\n=== PARALLEL (persistent workers) ===\n")
ens_par <- run_cosero_ensemble_parallel(
  project_path    = project_path,
  parameter_sets  = samples$parameter_sets,
  par_bounds      = par_bounds,
  base_settings   = base_settings,
  n_cores         = n_cores,
  quiet           = FALSE,
  statevar_source = 1,            # cold start = the regime that exposed the race
  result_reducer  = reducer
)
ok_par <- vapply(ens_par$results, function(r) isTRUE(r$success), logical(1))
cat(sprintf("Parallel success: %d / %d (%.1f%%)\n",
            sum(ok_par), n_runs, 100 * mean(ok_par)))

# --- Serial reference (known-good) -------------------------------------------
cat("\n=== SERIAL (reference) ===\n")
ens_ser <- run_cosero_ensemble(
  project_path    = project_path,
  parameter_sets  = samples$parameter_sets,
  par_bounds      = par_bounds,
  base_settings   = base_settings,
  quiet           = TRUE,
  statevar_source = 1,
  result_reducer  = reducer
)
ok_ser <- vapply(ens_ser$results, function(r) isTRUE(r$success), logical(1))
cat(sprintf("Serial success:   %d / %d (%.1f%%)\n",
            sum(ok_ser), n_runs, 100 * mean(ok_ser)))

# --- Assertions --------------------------------------------------------------
cat("\n=== CHECKS ===\n")

# 1. Parallel must succeed on every run the serial reference succeeds on
fail_only_parallel <- which(ok_ser & !ok_par)
if (length(fail_only_parallel) == 0) {
  cat("PASS: no run fails in parallel that succeeds serially (race fixed)\n")
} else {
  cat(sprintf("FAIL: %d run(s) fail ONLY in parallel: %s\n",
              length(fail_only_parallel),
              paste(fail_only_parallel, collapse = ", ")))
}

# 2. Parallel NSE matches serial NSE run-for-run (same params -> same result).
# NSE here is read from statistics.txt, which COSERO writes rounded to ~4
# decimals, so the realistic tolerance is ~1e-3 (text rounding), NOT machine
# epsilon. A diff at the 1e-4 level is rounding noise, not a real divergence.
nse_par <- extract_ensemble_metrics(ens_par, subbasin_id = "003", metric = "NSE", warn_nan = FALSE)
nse_ser <- extract_ensemble_metrics(ens_ser, subbasin_id = "003", metric = "NSE", warn_nan = FALSE)
both_ok <- ok_par & ok_ser
max_diff <- if (any(both_ok)) max(abs(nse_par[both_ok] - nse_ser[both_ok]), na.rm = TRUE) else NA
tol <- 1e-3   # statistics.txt rounding tolerance
if (!is.na(max_diff) && max_diff < tol) {
  cat(sprintf("PASS: parallel NSE matches serial run-for-run within %.0e (max diff %.2e)\n",
              tol, max_diff))
} else {
  cat(sprintf("FAIL: parallel vs serial NSE max diff = %s (exceeds %.0e tolerance)\n",
              format(max_diff), tol))
}

cat("\nDone.\n")
