# =============================================================================
# Test: PDIFF metric implementation and time-step window scaling
#
# Project: D:/COSERO/COSERO_NDC/COSERO_320_Schwechat
#
# Goals:
#   1. Synthetic tests: exact peak matching, lag detection, details = TRUE.
#   2. Automatic window scaling: daily (2), hourly (48), 15-min (192).
#   3. Fortran agreement on a series long enough for all n_maxima peaks.
#   4. Missing-value / sentinel handling (the fixes that matter for calibration).
#   5. Real Schwechat 320 hourly data vs statistics.txt.
#   6. Package integration: extract_run_metrics, calculate_run_metrics,
#      calculate_single_metric + metric_args forwarding.
#   7. logNSE (incl. spin-up handling and the scaled log offset).
# =============================================================================

devtools::load_all("D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R")

ok <- function(label, condition) {
  cat(sprintf("%-58s [%s]\n", label, if (isTRUE(condition)) "PASS" else "FAIL"))
}

# Faithful R port of optimCOS_objectiveFunctions.f (lines 253-318), used as the
# independent reference for the Fortran-agreement checks.
pdiff_fortran_ref <- function(sim, obs, n_max = 15, w = 48) {
  q_o <- obs; q_s <- sim; n <- length(obs)
  na_val <- -99
  q_o[is.na(q_o)] <- na_val
  sel <- matrix(na_val, n_max, 2)
  for (i in seq_len(n_max)) {
    idx <- which.max(q_o)
    sel[i, 1] <- q_o[idx]
    lb <- max(idx - w, 1); ub <- min(idx + w, n)
    sel[i, 2] <- max(q_s[lb:ub])
    q_o[lb:ub] <- na_val; q_s[lb:ub] <- na_val
  }
  mean_obs <- sum(sel[, 1]) / n_max
  1 - sum((sel[, 1] - sel[, 2])^2) / sum((sel[, 1] - mean_obs)^2)
}

cat("\n========================================================\n")
cat(" 1. SYNTHETIC UNIT TESTS\n")
cat("========================================================\n")

set.seed(42)
t_len <- 500
obs_synth <- pmax(0, sin(seq(0, 10 * pi, length.out = t_len)) * 50 + rnorm(t_len, 10, 2))
sim_synth <- obs_synth * 0.95 + rnorm(t_len, 0, 1)

pdiff_perfect <- pdiff(obs_synth, obs_synth, n_maxima = 5, window = 20)
ok(sprintf("perfect simulation == 1.0 (got %.6f)", pdiff_perfect),
   abs(pdiff_perfect - 1) < 1e-9)

res_det <- pdiff(sim_synth, obs_synth, n_maxima = 5, window = 20, details = TRUE)
cat("\nExtracted peaks table:\n")
print(res_det$peaks)
ok("details returns n_peaks / n_maxima / window",
   all(c("n_peaks", "n_maxima", "window") %in% names(res_det)))

cat("\n========================================================\n")
cat(" 2. TIME-STEP WINDOW SCALING (window_hours = 48)\n")
cat("========================================================\n")

get_window <- function(tt) {
  suppressWarnings(
    pdiff(rep(1, length(tt)), rep(1, length(tt)), time = tt, details = TRUE)$window
  )
}

time_daily  <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "1 day",  length.out = 500)
time_hourly <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "1 hour", length.out = 5000)
time_15min  <- seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "15 min", length.out = 10000)
time_date   <- seq(as.Date("2020-01-01"), by = "1 day", length.out = 500)

ok(sprintf("daily   POSIXct -> 2   (got %d)", get_window(time_daily)),  get_window(time_daily) == 2)
ok(sprintf("hourly  POSIXct -> 48  (got %d)", get_window(time_hourly)), get_window(time_hourly) == 48)
ok(sprintf("15-min  POSIXct -> 192 (got %d)", get_window(time_15min)),  get_window(time_15min) == 192)
ok(sprintf("daily   Date    -> 2   (got %d)", get_window(time_date)),   get_window(time_date) == 2)

# Without timestamps the physical window cannot be converted: must error rather
# than silently assume hourly (which would give a +/-48-DAY window on daily data).
err <- tryCatch({
  pdiff(sim_synth, obs_synth, n_maxima = 5)
  "no error"
}, error = function(e) conditionMessage(e))
ok("window=NULL without time errors (no hourly guess)", grepl("Cannot convert", err))

cat("\n========================================================\n")
cat(" 3. FORTRAN AGREEMENT\n")
cat("========================================================\n")

# Long series: all n_maxima windows fit, so R equals the Fortran result.
set.seed(2)
n_long <- 5000
obs_long <- pmax(0, sin(seq(0, 50 * pi, length.out = n_long)) * 50 + rnorm(n_long, 10, 3))
sim_long <- obs_long * 0.85 + rnorm(n_long, 0, 2)

ref_long <- pdiff_fortran_ref(sim_long, obs_long, 15, 48)
def_long <- pdiff(sim_long, obs_long, n_maxima = 15, window = 48)
cat(sprintf("long series  ref: %.10f  R: %.10f\n", ref_long, def_long))
ok("long series: R == Fortran ref", abs(def_long - ref_long) < 1e-9)

# Short series: fewer independent peaks than n_maxima. The Fortran divides by
# n_maxima and counts unfilled -99 slots; R computes NSE over the peaks it
# actually found and warns, so a divergence here is expected and documented.
set.seed(1)
n_short <- 200
obs_short <- pmax(0, sin(seq(0, 4 * pi, length.out = n_short)) * 20 + rnorm(n_short, 5, 1))
sim_short <- obs_short * 0.9 + rnorm(n_short, 0, 0.5)

warn_msg <- NULL
def_short <- withCallingHandlers(
  pdiff(sim_short, obs_short, n_maxima = 15, window = 48),
  warning = function(w) { warn_msg <<- conditionMessage(w); invokeRestart("muffleWarning") }
)
cat(sprintf("short series: default %.6f (over found peaks only)\n", def_short))
ok("default path warns when k < n_maxima",
   !is.null(warn_msg) && grepl("independent peaks", warn_msg))

cat("\n========================================================\n")
cat(" 4. MISSING VALUES AND SENTINELS\n")
cat("========================================================\n")

# A -999 sentinel in QSIM must never enter the sums as a number.
n_s <- 300
obs_s <- rep(1, n_s); sim_s <- rep(1, n_s)
obs_s[200] <- 50; sim_s[195:205] <- -999
r_sent <- pdiff(sim_s, obs_s, n_maxima = 2, window = 5, details = TRUE)
ok("-999 in QSIM does not leak into sim_peak",
   !any(r_sent$peaks$sim_peak <= -999, na.rm = TRUE))

# An invalid simulated value must NOT remove the observed peak: otherwise the
# evaluated event set would change with the simulation, and an optimiser would
# score different candidate parameter sets against different peaks.
obs_n <- rep(1, 400); sim_n <- rep(1, 400)
obs_n[200] <- 50; sim_n[190:210] <- NA
r_na <- pdiff(sim_n, obs_n, n_maxima = 2, window = 5, details = TRUE)
ok("NA in QSIM keeps the observed peak in the ranking",
   50 %in% r_na$peaks$obs_peak)

# Invalid observations are excluded from peak selection.
obs_g <- c(rep(1, 200), rep(-999, 50), rep(1, 250))
obs_g[300] <- 99   # inside the -999 gap region? no: index 300 is past it
r_gap <- pdiff(rep(1, 500), obs_g, n_maxima = 3, window = 5, details = TRUE)
ok("-999 observations never selected as peaks",
   !any(r_gap$peaks$obs_peak <= -999, na.rm = TRUE))

cat("\n========================================================\n")
cat(" 5. REAL DATA: SCHWECHAT 320 HOURLY\n")
cat("========================================================\n")

project_path <- "D:/COSERO/COSERO_NDC/COSERO_320_Schwechat"
stats_file   <- file.path(project_path, "output", "statistics.txt")
runoff_file  <- file.path(project_path, "output", "COSERO.runoff")
stopifnot(file.exists(stats_file), file.exists(runoff_file))

stats  <- read_cosero_statistics(stats_file)
runoff <- read_cosero_runoff(runoff_file, quiet = TRUE)
fortran_pdiff <- stats$PDIFF[1]
cat(sprintf("COSERO Fortran statistics.txt PDIFF: %.6f\n", fortran_pdiff))

spinup_steps <- 365
keep      <- (spinup_steps + 1):nrow(runoff)
sim_eval  <- runoff$QSIM_0001[keep]
obs_eval  <- runoff$QOBS_0001[keep]
time_eval <- runoff$DateTime[keep]
cat(sprintf("Runoff rows: %d  |  Spin-up steps: %d\n", nrow(runoff), spinup_steps))

res_auto <- pdiff(sim_eval, obs_eval, n_maxima = 15, time = time_eval, details = TRUE)
cat(sprintf("R pdiff auto (%d steps): %.6f   [%d of %d peaks found]\n",
            res_auto$window, res_auto$val, res_auto$n_peaks, res_auto$n_maxima))
ok("hourly data auto-scales to 48 steps", res_auto$window == 48)
ok("all 15 peaks found (modes must agree)", res_auto$n_peaks == 15)
ok("matches Fortran statistics.txt", abs(res_auto$val - fortran_pdiff) < 1e-4)


res_72h <- pdiff(sim_eval, obs_eval, n_maxima = 15, time = time_eval,
                 window_hours = 72, details = TRUE)
cat(sprintf("R pdiff custom 72h (%d steps): %.6f\n", res_72h$window, res_72h$val))

cat("\nTop 5 independent peaks (48 h Fortran standard):\n")
print(head(res_auto$peaks, 5))

cat("\n========================================================\n")
cat(" 6. PACKAGE INTEGRATION\n")
cat("========================================================\n")

run_result <- list(
  success = TRUE,
  output_data = list(runoff = runoff, statistics = stats),
  defaults_settings = list(SPINUP = 365)
)

ext_pdiff <- extract_run_metrics(run_result, subbasin_id = "001", metric = "PDIFF")
ok(sprintf("extract_run_metrics    %.6f", ext_pdiff),
   abs(ext_pdiff - fortran_pdiff) < 1e-4)

calc_auto <- calculate_run_metrics(run_result, subbasin_id = "001", metric = "PDIFF")
ok(sprintf("calculate_run_metrics  %.6f", calc_auto),
   abs(calc_auto - fortran_pdiff) < 1e-4)

calc_72h <- calculate_run_metrics(run_result, subbasin_id = "001",
                                  metric = "PDIFF", window_hours = 72)
ok(sprintf("... window_hours = 72  %.6f", calc_72h),
   abs(calc_72h - res_72h$val) < 1e-9)

calc_n25 <- calculate_run_metrics(run_result, subbasin_id = "001", metric = "PDIFF",
                                  n_maxima = 25, window_hours = 24)
ok(sprintf("... n_maxima = 25      %.6f", calc_n25), is.finite(calc_n25))

# Optimizer path: calculate_single_metric + metric_args forwarding.
opt_result <- list(output_data = list(runoff = runoff, statistics = stats))

single_default <- calculate_single_metric(opt_result, "001", "PDIFF", spinup_value = 365)
ok(sprintf("calculate_single_metric %.6f", single_default),
   abs(single_default - fortran_pdiff) < 1e-4)

single_args <- calculate_single_metric(
  opt_result, "001", "PDIFF", spinup_value = 365,
  metric_args = list(n_maxima = 25, window_hours = 24)
)
ok("metric_args reaches pdiff() from optimizer path",
   abs(single_args - calc_n25) < 1e-9)

ok("optimize_cosero_dds accepts metric_args",
   "metric_args" %in% names(formals(optimize_cosero_dds)))
ok("optimize_cosero_sce accepts metric_args",
   "metric_args" %in% names(formals(optimize_cosero_sce)))

cat("\n========================================================\n")
cat(" 7. logNSE\n")
cat("========================================================\n")

log_run <- calculate_run_metrics(run_result, subbasin_id = "001", metric = "logNSE")
nse_run <- calculate_run_metrics(run_result, subbasin_id = "001", metric = "NSE")
cat(sprintf("NSE %.4f   logNSE %.4f\n", nse_run, log_run))
ok("logNSE is finite and <= 1", is.finite(log_run) && log_run <= 1)
ok("logNSE differs from NSE (low-flow weighting)", abs(log_run - nse_run) > 1e-6)

ok("lnNSE is accepted as a synonym",
   isTRUE(all.equal(calculate_run_metrics(run_result, "001", "lnNSE"), log_run)))
ok("optimizer path agrees with calculate_run_metrics",
   isTRUE(all.equal(calculate_single_metric(opt_result, "001", "logNSE",
                                           spinup_value = 365), log_run)))
ok("optimizer lnNSE == logNSE",
   isTRUE(all.equal(calculate_single_metric(opt_result, "001", "lnNSE", spinup_value = 365),
                    calculate_single_metric(opt_result, "001", "logNSE", spinup_value = 365))))

# Spin-up must actually change the result, and must match a hand-trimmed
# calculation. This is the same exclusion path all metrics share.
log_sp0 <- calculate_run_metrics(run_result, subbasin_id = "001",
                                 metric = "logNSE", spinup = 0)
ok(sprintf("spinup=0 differs from spinup=365 (%.4f vs %.4f)", log_sp0, log_run),
   abs(log_sp0 - log_run) > 1e-9)

manual_log <- local({
  s <- runoff$QSIM_0001[(365 + 1):nrow(runoff)]
  o <- runoff$QOBS_0001[(365 + 1):nrow(runoff)]
  k <- !is.na(s) & !is.na(o)
  s <- s[k]; o <- o[k]
  eps <- mean(o) / 100
  hydroGOF::NSE(log(s + eps), log(o + eps))
})
ok(sprintf("matches hand-trimmed calculation (%.6f)", manual_log),
   isTRUE(all.equal(log_run, manual_log)))

# The offset scales with mean observed flow, so it cannot dominate small
# catchments the way a fixed 0.01 would.
ok(sprintf("log_offset scales with flow (mean/100 = %.4f)", mean(obs_eval, na.rm = TRUE) / 100),
   isTRUE(all.equal(log_offset(obs_eval[!is.na(obs_eval)]),
                    mean(obs_eval, na.rm = TRUE) / 100)))
ok("log_offset falls back for all-zero obs",
   log_offset(rep(0, 100)) == 0.01)

# Zero-flow timesteps must stay finite (the reason for the offset at all).
zero_run <- list(
  success = TRUE,
  output_data = list(runoff = data.frame(
    DateTime = seq(as.POSIXct("2020-01-01", tz = "UTC"), by = "1 day", length.out = 200),
    QSIM_0001 = c(rep(0, 50), abs(rnorm(150, 2, 1))),
    QOBS_0001 = c(rep(0, 50), abs(rnorm(150, 2, 1)))
  )),
  defaults_settings = list(SPINUP = 0)
)
zr <- calculate_run_metrics(zero_run, subbasin_id = "001", metric = "logNSE")
ok(sprintf("zero-flow timesteps stay finite (%.4f)", zr), is.finite(zr))

cat("\nDone. PDIFF + logNSE test suite complete.\n")
