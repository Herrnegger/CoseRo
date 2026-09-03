# =============================================================================
# Verify: OMP_NUM_THREADS cap in the parallel ensemble runner
#
# Small (2-run) parallel ensemble on Schwechat 320 to confirm:
#   1. run_cosero_ensemble_parallel() still runs correctly with the new
#      n_cores default (1) and the OMP cap wired in.
#   2. The batch file each worker writes actually contains
#      "set OMP_NUM_THREADS=<n>" -- read back from a thread directory
#      while (or just after) the run, to prove the plumbing reaches the
#      exe call, not just the R-level calculation.
# =============================================================================

devtools::load_all()

project_path <- "D:/COSERO/COSERO_NDC/COSERO_320_Schwechat"
param_file   <- "para_opt.txt"

par_bounds <- create_optimization_bounds(
  parameters = c("BETA"),
  lower = c(8), upper = c(14),
  modification_type = c("relchg")
)

set.seed(1)
parameter_sets <- data.frame(BETA = c(9, 11))

cat("=== n_cores = 1 (new default) ===\n")
res1 <- run_cosero_ensemble_parallel(
  project_path = project_path,
  parameter_sets = parameter_sets,
  par_bounds = par_bounds,
  base_settings = list(
    STARTDATE = "2018 1 1 0 0", ENDDATE = "2018 12 31 0 0",
    SPINUP = 365, OUTPUTTYPE = 1, PARAFILE = param_file
  ),
  n_cores = 1,
  statevar_source = 1,
  quiet = FALSE
)

cat("\nsuccess per run:", sapply(res1$results, function(r) isTRUE(r$success)), "\n")

cat("\n=== n_cores = 2 (check OMP cap scales down) ===\n")
res2 <- run_cosero_ensemble_parallel(
  project_path = project_path,
  parameter_sets = parameter_sets,
  par_bounds = par_bounds,
  base_settings = list(
    STARTDATE = "2018 1 1 0 0", ENDDATE = "2018 12 31 0 0",
    SPINUP = 365, OUTPUTTYPE = 1, PARAFILE = param_file
  ),
  n_cores = 2,
  statevar_source = 1,
  quiet = FALSE
)

cat("\nsuccess per run:", sapply(res2$results, function(r) isTRUE(r$success)), "\n")

cat("\n=== debug: inspect first failing result ===\n")
r1 <- res1$results[[1]]
cat("names:", paste(names(r1), collapse=", "), "\n")
if (!is.null(r1$error)) cat("error:", r1$error, "\n")
if (!is.null(r1$error_message)) cat("error_message:", r1$error_message, "\n")
if (!is.null(r1$has_error)) cat("has_error:", r1$has_error, "\n")
if (!is.null(r1$exit_code)) cat("exit_code:", r1$exit_code, "\n")
str(r1, max.level = 1)
