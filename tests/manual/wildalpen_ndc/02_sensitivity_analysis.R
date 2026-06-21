# =============================================================================
# Wildalpen NDC — Script 2: Sobol Sensitivity Analysis
# =============================================================================
# Global sensitivity analysis for the NDC disaggregation workflow.
# Combines standard hydrological parameters with the five NDC-specific
# disaggregation parameters (LAPSE_T, LAPSE_P, SOILVAR, HYDROVAR, CTVAR).
#
# Workflow:
#   1. Setup / verify aggregated example project
#   2. Load parameter bounds (standard + disaggregation)
#   3. Generate Sobol samples
#   4. Baseline run for reference metrics
#   5. Run parallel ensemble
#   6. Extract and summarise performance metrics
#   7. Calculate Sobol indices
#   8. Diagnostic plots (Sobol bars, dotty plots, uncertainty band,
#      metric distributions, behavioural filtering)
#   9. Export results
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude
# Date: 2026-05-06
# Branch: dev/spatial-disaggregation
# =============================================================================

devtools::load_all()

library(ggplot2)
library(dplyr)
library(tidyr)

# =============================================================================
# USER SETTINGS
# =============================================================================

project_path    <- "D:/temp/Wildalpen_Example_0.9.3"
target_subbasin <- "003"    # primary subbasin for sensitivity indices

base_settings <- list(
  STARTDATE  = c(2000, 10, 1, 0, 0),
  ENDDATE    = c(2015, 9, 30, 0, 0),
  SPINUP     = 365,
  # OUTPUTTYPE must be >= 1 here: the ET section (10) analyses simulated actual
  # ET (ETAGEB from COSERO.plus1), which OUTPUTTYPE = 0 does NOT write. If you
  # ever drop section 10 and only use NSE/KGE + QSIM, you may set this to 0
  # (calibration mode: runoff + statistics only) for a faster ensemble.
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini_agg.txt"
)

# Sobol sample size: total runs = n × (n_params + 2)
# Use 30–50 for a smoke test; 200–500 for publication-quality indices
n_sobol <- 50
n_cores <- 8

# Where the parallel runner places its per-worker project copies. Keep this on a
# roomy drive on the SAME volume as the project (D:) — each worker copy is
# ~100+ MB, and AppData\Local\Temp (C:) was filling up and crashing the session.
ens_temp_dir <- "D:/temp/cosero_parallel"

parallel::detectCores()

# Parameters to analyse:
#   disaggregation params first, then standard hydro params
param_names <- c(
  # NDC disaggregation
  "LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR", "NDC",
  # Snow
  "CTMAX", "CTMIN",
  # Soil and runoff
  "M", "BETA", "KBF",
  # Flow recession
  "H1", "H2", "TVS1", "TVS2", "TAB1", "TAB2", "TAB3",
  # Meteorological corrections
  "PCOR", "TCOR",
  # Evapotranspiration (incl. hydraulic lift FHL)
  "ETSYSCOR", "FKFAK", "FHL"#"ETSLPCOR", 
)

# =============================================================================
# 1. SETUP / VERIFY EXAMPLE PROJECT
# =============================================================================

cat("=== Verifying aggregated example project ===\n")

if (!dir.exists(project_path)) {
  setup_cosero_project_example_aggregated(project_path)
  cat("Project created at:", project_path, "\n")
} else {
  cat("Using existing project at:", project_path, "\n")
}

stopifnot(
  dir.exists(project_path),
  file.exists(file.path(project_path, "COSERO.exe")),
  file.exists(file.path(project_path, "input", "para_ini_agg.txt"))
)
cat("Project structure OK\n\n")

# =============================================================================
# 2. LOAD PARAMETER BOUNDS
# =============================================================================

par_bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameter bounds for sensitivity analysis:\n")
print(par_bounds[, c("parameter", "description", "min", "max", "default",
                      "modification_type", "category")])
cat("\n")

# =============================================================================
# 3. GENERATE SOBOL SAMPLES
# =============================================================================

sobol_bounds <- create_sobol_bounds(par_bounds)
samples      <- generate_sobol_samples(sobol_bounds, n = n_sobol, order = "first")

cat(sprintf("Generated %d parameter sets for %d parameters\n",
            nrow(samples$parameter_sets), length(param_names)))
cat(sprintf("(n = %d × (%d + 2) = %d total runs)\n\n",
            n_sobol, length(param_names), nrow(samples$parameter_sets)))

# =============================================================================
# 4. BASELINE RUN (reference metrics)
# =============================================================================

cat("=== Baseline run ===\n")

result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = base_settings,
  statevar_source   = 1,
  quiet             = FALSE,
  read_outputs      = TRUE
)

baseline_metrics <- sapply(c("001", "002", "003"), function(sb) {
  c(NSE = tryCatch(extract_run_metrics(result_base, sb, "NSE"), error = function(e) NA_real_),
    KGE = tryCatch(extract_run_metrics(result_base, sb, "KGE"), error = function(e) NA_real_))
}, simplify = FALSE)

cat("\nBaseline metrics:\n")
for (sb in names(baseline_metrics)) {
  cat(sprintf("  Subbasin %s — NSE = %.4f  |  KGE = %.4f\n",
              sb, baseline_metrics[[sb]]["NSE"], baseline_metrics[[sb]]["KGE"]))
}

baseline_nse <- baseline_metrics[[target_subbasin]]["NSE"]
baseline_kge <- baseline_metrics[[target_subbasin]]["KGE"]
cat(sprintf("\nTarget subbasin (%s) used for Sobol indices: NSE = %.4f  |  KGE = %.4f\n\n",
            target_subbasin, baseline_nse, baseline_kge))

# =============================================================================
# 5. RUN ENSEMBLE (parallel or serial)
# =============================================================================
# run_mode = "parallel" -> run_cosero_ensemble_parallel() (8 cores, fast)
#          = "serial"   -> run_cosero_ensemble()  (one core, no workers/sockets,
#                          no shared-file contention; slower but robust). Use
#                          this when parallel runs show diffuse "hard_fail"s that
#                          reproduce fine in isolation (i.e. worker/contention
#                          failures, not parameter problems).
run_mode <- "parallel"   # "parallel" or "serial"

# Same memory reducer for both modes: keep only runoff (QOBS/QSIM) + ET (ETAGEB)
# columns per run, plus statistics & defaults_settings. Categories:
#   runoff = QOBS/QSIM | ET = ETAGEB | states = BW0/BW3 | snow = SWW
# NULL/empty = keep everything. Prevents the multi-GB ensemble at OUTPUTTYPE = 1.
ens_reducer <- make_var_reducer(c("runoff", "ET"))

rds_dir <- file.path(project_path, "sensitivity_results")
dir.create(rds_dir, showWarnings = FALSE, recursive = TRUE)
# Separate cache per mode so a serial run doesn't clobber the parallel one
rds_file <- file.path(rds_dir,
                      sprintf("ensemble_ndc_sensitivity_%s.rds", run_mode))

if (file.exists(rds_file)) {
  cat("=== Loading cached ensemble from disk (", run_mode, ") ===\n", sep = "")
  ensemble <- readRDS(rds_file)
} else if (run_mode == "serial") {
  cat("=== Running SERIAL ensemble (this is slow but robust) ===\n")
  ensemble <- run_cosero_ensemble(
    project_path    = project_path,
    parameter_sets  = samples$parameter_sets,
    par_bounds      = par_bounds,
    base_settings   = base_settings,   # OUTPUTTYPE set here (see base_settings)
    quiet           = FALSE,
    statevar_source = 1,
    result_reducer  = ens_reducer
  )
  saveRDS(ensemble, rds_file)
  cat("Ensemble saved to:", rds_file, "\n")
} else {
  cat("=== Running PARALLEL ensemble ===\n")
  ensemble <- run_cosero_ensemble_parallel(
    project_path    = project_path,
    parameter_sets  = samples$parameter_sets,
    par_bounds      = par_bounds,
    base_settings   = base_settings,   # OUTPUTTYPE set here (see base_settings)
    n_cores         = n_cores,
    temp_dir        = ens_temp_dir,    # worker copies on D: (roomy, same volume)
    quiet           = FALSE,
    statevar_source = 1,
    result_reducer  = ens_reducer
  )
  saveRDS(ensemble, rds_file)
  cat("Ensemble saved to:", rds_file, "\n")
}

n_runs <- length(ensemble$results)
n_ok   <- sum(sapply(ensemble$results, function(r) isTRUE(r$success)))
cat(sprintf("Successful runs: %d / %d (%.0f%%)\n\n", n_ok, n_runs,
            100 * n_ok / n_runs))

# =============================================================================
# 5b. FAILURE DIAGNOSTICS — is there a systematic cause?
# =============================================================================
# Two distinct failure modes:
#   hard_fail = COSERO did not complete (result$success == FALSE)
#   na_metric = ran, but the target-subbasin NSE could not be computed (NA)
# We compare the parameter distributions of failed vs successful sample points
# to see whether failure is driven by a particular parameter region.

hard_fail <- !vapply(ensemble$results, function(r) isTRUE(r$success), logical(1))
na_metric <- !hard_fail & is.na(nse_values_pre <- {
  # cheap pre-extract of NSE per run (same source extract_ensemble_metrics uses)
  vapply(ensemble$results, function(r) {
    s <- r$output_data$statistics
    if (is.null(s)) return(NA_real_)
    row <- s[s$sb == sprintf("%04d", as.numeric(target_subbasin)) |
               s$sb == as.numeric(target_subbasin), ]
    if (nrow(row) == 0 || !"NSE" %in% names(row)) return(NA_real_)
    row$NSE[1]
  }, numeric(1))
})
failed <- hard_fail | na_metric

cat(sprintf("Failure breakdown: %d hard fails (COSERO crashed), %d NA-metric (ran, no valid NSE), %d OK\n",
            sum(hard_fail), sum(na_metric), sum(!failed)))

if (any(failed)) {
  ps <- as.data.frame(samples$parameter_sets)
  # Compare median of each parameter: failed vs successful runs
  cmp <- data.frame(
    parameter = names(ps),
    median_ok     = vapply(ps, function(x) median(x[!failed], na.rm = TRUE), numeric(1)),
    median_failed = vapply(ps, function(x) median(x[failed],  na.rm = TRUE), numeric(1))
  )
  cmp$shift <- cmp$median_failed - cmp$median_ok
  # Normalise the shift by the parameter's sampled range -> comparable across params
  rng <- vapply(ps, function(x) diff(range(x, na.rm = TRUE)), numeric(1))
  cmp$shift_frac <- cmp$shift / rng
  cmp <- cmp[order(-abs(cmp$shift_frac)), ]
  cat("\nParameters whose FAILED-run median differs most from OK-run median\n")
  cat("(shift_frac = (median_failed - median_ok) / sampled_range; large |value| = suspect):\n")
  print(head(cmp[, c("parameter", "median_ok", "median_failed", "shift_frac")], 8),
        row.names = FALSE)

  # Save the failed sample points for inspection
  failed_tbl <- cbind(run_id = which(failed),
                      mode = ifelse(hard_fail[failed], "hard_fail", "na_metric"),
                      ps[failed, , drop = FALSE])
  utils::write.csv(failed_tbl,
                   file.path(rds_dir, "failed_runs.csv"), row.names = FALSE)
  cat("\nFailed-run parameter sets saved to: ",
      file.path(rds_dir, "failed_runs.csv"), "\n", sep = "")
}
cat("\n")

# =============================================================================
# 5c. REPRODUCE A SINGLE FAILED RUN (verbose, isolated)
# =============================================================================
# Pick one failed sample point, apply its parameters to the parameter file, and
# run COSERO once with full output so you can SEE why it fails. The original
# parameter file is backed up and ALWAYS restored afterwards (on.exit), so this
# is non-destructive.
#
# Set repro_run_id to any run_id from failed_runs.csv; default = first failure.

repro_run_id <- if (exists("failed") && any(failed)) which(failed)[2] else NA_integer_

if (!is.na(repro_run_id)) {
  cat(sprintf("=== Reproducing failed run %d (%s) ===\n",
              repro_run_id,
              if (hard_fail[repro_run_id]) "hard_fail" else "na_metric"))

  # Parameter row for this run (drop the run_id/mode bookkeeping cols)
  repro_params <- samples$parameter_sets[repro_run_id, , drop = FALSE]
  cat("Parameter values:\n")
  print(round(unlist(repro_params), 4))

  # Resolve the parameter file COSERO will read (PARAFILE from base_settings)
  repro_par_file <- file.path(project_path, "input", base_settings$PARAFILE)

  # Back up and guarantee restore (fires on normal exit, error, or interrupt)
  repro_backup <- paste0(repro_par_file, ".repro_backup")
  file.copy(repro_par_file, repro_backup, overwrite = TRUE)
  on.exit({
    if (file.exists(repro_backup)) {
      file.copy(repro_backup, repro_par_file, overwrite = TRUE)
      unlink(repro_backup)
      cat("Original parameter file restored.\n")
    }
  }, add = TRUE)

  # Original values needed by modify_parameter_table (same call the ensemble uses)
  repro_orig <- read_parameter_table(repro_par_file, names(repro_params),
                                     zone_id = NULL, quiet = TRUE)

  modify_parameter_table(repro_par_file, repro_params, par_bounds,
                         repro_orig, quiet = TRUE)

  # Run COSERO with full output visible (NOT quiet) so the failure is exposed
  cat("\n--- COSERO run (verbose) ---\n")
  repro_result <- tryCatch(
    run_cosero(project_path = project_path,
               defaults_settings = base_settings,
               statevar_source   = 1,
               quiet             = FALSE,
               read_outputs      = TRUE),
    error = function(e) { cat("run_cosero ERROR:", conditionMessage(e), "\n"); list(success = FALSE) }
  )

  cat("\n--- Outcome ---\n")
  cat("success:", isTRUE(repro_result$success), "\n")
  repro_nse <- tryCatch(extract_run_metrics(repro_result, target_subbasin, "NSE"),
                        error = function(e) NA_real_)
  cat(sprintf("NSE (subbasin %s): %s\n", target_subbasin,
              ifelse(is.na(repro_nse), "NA", sprintf("%.4f", repro_nse))))

  # COSERO's own stdout log is kept in the project root after each run
  stdout_log <- file.path(project_path, "cosero_stdout.txt")
  if (file.exists(stdout_log)) {
    cat("\n--- Last 25 lines of cosero_stdout.txt ---\n")
    cat(paste(utils::tail(readLines(stdout_log, warn = FALSE), 25), collapse = "\n"), "\n")
  }
  # (Original parameter file is restored by on.exit above.)
}

# =============================================================================
# 6. EXTRACT METRICS
# =============================================================================

nse_values <- extract_ensemble_metrics(ensemble, subbasin_id = target_subbasin, metric = "NSE")
kge_values <- extract_ensemble_metrics(ensemble, subbasin_id = target_subbasin, metric = "KGE")

cat(sprintf("Metric summary for subbasin %s (n = %d valid runs):\n",
            target_subbasin, sum(!is.na(nse_values))))
cat(sprintf("  NSE: median = %.3f  range = [%.3f, %.3f]\n",
            median(nse_values, na.rm = TRUE),
            min(nse_values, na.rm = TRUE),
            max(nse_values, na.rm = TRUE)))
cat(sprintf("  KGE: median = %.3f  range = [%.3f, %.3f]\n\n",
            median(kge_values, na.rm = TRUE),
            min(kge_values, na.rm = TRUE),
            max(kge_values, na.rm = TRUE)))

# =============================================================================
# 7. CALCULATE SOBOL INDICES
# =============================================================================

sobol_nse <- calculate_sobol_indices(
  Y             = nse_values,
  sobol_samples = samples,
  boot          = TRUE,
  R             = 500
)

sobol_kge <- calculate_sobol_indices(
  Y             = kge_values,
  sobol_samples = samples,
  boot          = TRUE,
  R             = 500
)

# Print top-5 parameters by total-effect index (Ti) for NSE.
# calculate_sobol_indices() returns a sensobol object: indices are in $results
# in LONG format (columns: parameters, sensitivity = "Si"/"Ti", original, ...).
# Reshape Si/Ti into one row per parameter, then sort by Ti.
res_nse  <- as.data.frame(sobol_nse$results)
si_nse   <- res_nse[res_nse$sensitivity == "Si", c("parameters", "original")]
ti_nse_l <- res_nse[res_nse$sensitivity == "Ti", c("parameters", "original")]
names(si_nse)[2]   <- "Si"
names(ti_nse_l)[2] <- "Ti"
ti_nse <- merge(si_nse, ti_nse_l, by = "parameters")
ti_nse <- ti_nse[order(-ti_nse$Ti), ]

cat("Top 5 parameters by Ti (NSE):\n")
print(utils::head(ti_nse, 5), row.names = FALSE)
cat("\n")

# =============================================================================
# 8. DIAGNOSTIC PLOTS
# =============================================================================

plot_dir <- file.path(project_path, "sensitivity_results")
sb_tag   <- paste0("NB", target_subbasin)

# --- 8a. Sobol bar plots ---

p_sobol_nse <- plot_sobol(sobol_nse,
                           title = paste("Sobol Indices — NSE, Subbasin", target_subbasin))
print(p_sobol_nse)
ggsave(file.path(plot_dir, paste0("sobol_NSE_", sb_tag, ".png")),
       p_sobol_nse, width = 10, height = 6, dpi = 150)
cat("Saved: sobol_NSE_", sb_tag, ".png\n", sep = "")

p_sobol_kge <- plot_sobol(sobol_kge,
                           title = paste("Sobol Indices — KGE, Subbasin", target_subbasin))
print(p_sobol_kge)
ggsave(file.path(plot_dir, paste0("sobol_KGE_", sb_tag, ".png")),
       p_sobol_kge, width = 10, height = 6, dpi = 150)
cat("Saved: sobol_KGE_", sb_tag, ".png\n", sep = "")

# --- 8b. Dotty plots — colour disaggregation params differently ---

p_dotty_nse <- plot_dotty(
  parameter_sets = samples$parameter_sets,
  Y              = nse_values,
  y_label        = "NSE",
  n_col          = 4,
  reference_line = baseline_nse,
  y_min          = -0.5,
  show_envelope  = TRUE,
  envelope_quantile = 0.95
) +
  labs(title    = paste("Dotty Plots — NSE, Subbasin", target_subbasin),
       subtitle = "Red dashed = baseline NSE | Orange curve = 95th-percentile LOESS envelope")

print(p_dotty_nse)
ggsave(file.path(plot_dir, paste0("dotty_NSE_", sb_tag, ".png")),
       p_dotty_nse, width = 14, height = 10, dpi = 150)
cat("Saved: dotty_NSE_", sb_tag, ".png\n", sep = "")

p_dotty_kge <- plot_dotty(
  parameter_sets = samples$parameter_sets,
  Y              = kge_values,
  y_label        = "KGE",
  n_col          = 4,
  reference_line = baseline_kge,
  y_min          = -0.5,
  show_envelope  = TRUE,
  envelope_quantile = 0.95
) +
  labs(title    = paste("Dotty Plots — KGE, Subbasin", target_subbasin),
       subtitle = "Red dashed = baseline KGE | Orange curve = 95th-percentile LOESS envelope")

print(p_dotty_kge)
ggsave(file.path(plot_dir, paste0("dotty_KGE_", sb_tag, ".png")),
       p_dotty_kge, width = 14, height = 10, dpi = 150)
cat("Saved: dotty_KGE_", sb_tag, ".png\n", sep = "")

# --- 8c. Ensemble uncertainty band ---

p_uncertainty <- plot_ensemble_uncertainty(
  ensemble,
  subbasin_id    = sprintf("%04d", as.numeric(target_subbasin)),
  output_variable = "QSIM",
  lower_quantile = 0.10,
  upper_quantile = 0.90
) +
  labs(title    = paste("Ensemble Discharge Uncertainty — Subbasin", target_subbasin),
       subtitle = sprintf("n = %d runs | 10–90%% ribbon (orange) | Median (red)",
                           nrow(samples$parameter_sets)))

print(p_uncertainty)
ggsave(file.path(plot_dir, paste0("ensemble_uncertainty_", sb_tag, ".png")),
       p_uncertainty, width = 14, height = 6, dpi = 150)
cat("Saved: ensemble_uncertainty_", sb_tag, ".png\n", sep = "")

# --- 8d. Metric distributions ---

p_nse_dist <- plot_metric_distribution(
  nse_values,
  metric_name     = "NSE",
  reference_value = baseline_nse,
  show_mean       = TRUE
)
print(p_nse_dist)
ggsave(file.path(plot_dir, paste0("dist_NSE_", sb_tag, ".png")),
       p_nse_dist, width = 7, height = 5, dpi = 150)
cat("Saved: dist_NSE_", sb_tag, ".png\n", sep = "")

p_kge_dist <- plot_metric_distribution(
  kge_values,
  metric_name     = "KGE",
  reference_value = baseline_kge,
  show_mean       = TRUE
)
print(p_kge_dist)
ggsave(file.path(plot_dir, paste0("dist_KGE_", sb_tag, ".png")),
       p_kge_dist, width = 7, height = 5, dpi = 150)
cat("Saved: dist_KGE_", sb_tag, ".png\n", sep = "")

# --- 8e. Behavioural filtering ---

behav_out <- extract_behavioral_runs(
  ensemble_output  = ensemble,
  subbasin_id      = target_subbasin,
  nse_thresh       = 0.5,
  kge_thresh       = 0.5,
  pbias_thresh     = c(-25, 25),
  plot_uncertainty = TRUE,
  lower_quantile   = 0.0,
  upper_quantile   = 1.0,
  xlim             = c(0, 1),
  ylim             = c(0, 1)
)

n_behav <- length(behav_out$behavioral_run_ids)
cat(sprintf("\nBehavioural runs (NSE > 0.5, KGE > 0.5): %d / %d (%.1f%%)\n",
            n_behav, n_runs, 100 * n_behav / n_runs))

print(behav_out$scatter_plot)
ggsave(file.path(plot_dir, paste0("behavioral_scatter_", sb_tag, ".png")),
       behav_out$scatter_plot, width = 8, height = 7, dpi = 150)
cat("Saved: behavioral_scatter_", sb_tag, ".png\n", sep = "")

if (!is.null(behav_out$uncertainty_plot)) {
  print(behav_out$uncertainty_plot)
  ggsave(file.path(plot_dir, paste0("behavioral_uncertainty_", sb_tag, ".png")),
         behav_out$uncertainty_plot, width = 14, height = 6, dpi = 150)
  cat("Saved: behavioral_uncertainty_", sb_tag, ".png\n", sep = "")
}

# Dotty plots restricted to behavioural runs only
if (n_behav >= 5) {
  behav_nse <- behav_out$metrics_df$NSE[behav_out$metrics_df$category == "Behavioral"]
  p_dotty_behav <- plot_dotty(
    parameter_sets = behav_out$filtered_ensemble$parameter_sets,
    Y              = behav_nse,
    y_label        = "NSE",
    n_col          = 4,
    reference_line = baseline_nse,
    y_min          = 0.4,
    show_envelope  = TRUE,
    envelope_quantile = 0.99
  ) +
    labs(title    = paste("Dotty Plots — Behavioural Runs Only, Subbasin", target_subbasin),
         subtitle = sprintf("n = %d behavioural runs", n_behav))
  print(p_dotty_behav)
  ggsave(file.path(plot_dir, paste0("dotty_behavioural_", sb_tag, ".png")),
         p_dotty_behav, width = 14, height = 10, dpi = 150)
  cat("Saved: dotty_behavioural_", sb_tag, ".png\n", sep = "")
} else {
  cat(sprintf("Note: only %d behavioural runs — skipping behavioural dotty plot\n", n_behav))
}

# --- 8f. Disaggregation vs standard parameter comparison ---
# Side-by-side Sobol Ti for the two parameter groups

# NDC counts as a disaggregation parameter here. Note: NDC is an INTEGER (1-10)
# sampled continuously then rounded by COSERO, and NDC<=1 switches disaggregation
# OFF entirely -- so its response is a step change that Sobol (a continuous-input
# variance method) only approximates. Read its Ti as indicative, not exact.
disag_params  <- c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR", "NDC")
std_params    <- setdiff(param_names, disag_params)

# ti_nse (built in section 7) is the per-parameter Si/Ti table derived from
# sobol_nse$results (long format). Reuse it here; the column is `parameters`.
ti_df <- ti_nse %>%
  mutate(
    parameter = parameters,
    group = ifelse(parameters %in% disag_params, "Disaggregation", "Standard hydro"),
    Ti    = pmax(Ti, 0)   # clip negative (near-zero Ti) to 0 for display
  ) %>%
  arrange(desc(Ti))

p_groups <- ggplot(ti_df, aes(x = reorder(parameter, Ti), y = Ti, fill = group)) +
  geom_col(width = 0.7) +
  coord_flip() +
  scale_fill_manual(values = c("Disaggregation" = "#e74c3c", "Standard hydro" = "#2980b9")) +
  labs(title    = paste("Total-Effect Sobol Indices (Ti) — NSE, Subbasin", target_subbasin),
       subtitle = "Red = NDC disaggregation params | Blue = standard hydro params",
       x = NULL, y = "Ti (total-effect index)", fill = NULL) +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

print(p_groups)
ggsave(file.path(plot_dir, paste0("sobol_Ti_grouped_NSE_", sb_tag, ".png")),
       p_groups, width = 8, height = 6, dpi = 150)
cat("Saved: sobol_Ti_grouped_NSE_", sb_tag, ".png\n", sep = "")

# =============================================================================
# 10. ET / HYDRAULIC-LIFT SENSITIVITY (reuses the same ensemble)
# =============================================================================
# Plain NSE/KGE on discharge is dominated by PCOR and the water-balance/timing
# parameters, masking the ET correctors (ETSLPCOR, ETSYSCOR, FKFAK) and the new
# hydraulic-lift parameter FHL. To surface the ET signal we re-extract two
# different targets Y from the SAME ensemble (no extra model runs):
#
#   (a) logNSE on discharge  -> low-flow / dry-season performance, where deep-
#       root hydraulic lift actually acts. Computed from COSERO.runoff (QSIM/QOBS).
#   (b) summer-mean actual ET (ETAGEB, Jun-Aug) -> the process the ET params drive
#       directly. Output-variance based (no ET observations), so it answers
#       "what controls modelled ET", not "performance".
#
# NB FHL and ETVEGCOR are strongly correlated by construction; ETVEGCOR is a
# fixed a-priori lookup (not sampled here), so FHL calibrates against it cleanly.
# Expect the ET group to act largely through INTERACTIONS (Ti >> Si).

cat("\n=== Section 10: ET / hydraulic-lift sensitivity ===\n")

spinup_ts <- as.numeric(base_settings$SPINUP)
sb4       <- sprintf("%04d", as.numeric(target_subbasin))

# --- 10a. Build the two target vectors from the existing ensemble ---

# logNSE on discharge (offset avoids log(0); negatives already NA in runoff)
extract_lognse <- function(ens, sb_col, spinup) {
  qs <- paste0("QSIM_", sb_col); qo <- paste0("QOBS_", sb_col)
  vapply(ens$results, function(r) {
    if (!isTRUE(r$success) || is.null(r$output_data$runoff)) return(NA_real_)
    ro <- r$output_data$runoff
    if (!all(c(qs, qo) %in% names(ro))) return(NA_real_)
    sim <- ro[[qs]]; obs <- ro[[qo]]
    if (spinup > 0 && length(sim) > spinup) {
      sim <- sim[-seq_len(spinup)]; obs <- obs[-seq_len(spinup)]
    }
    ok <- !is.na(sim) & !is.na(obs)
    if (sum(ok) < 10) return(NA_real_)
    tryCatch(hydroGOF::NSE(log(sim[ok] + 0.01), log(obs[ok] + 0.01)),
             error = function(e) NA_real_)
  }, numeric(1))
}

# Summer (JJA) mean simulated actual ET, from COSERO.plus1 (ETAGEB, timestep mm)
extract_summer_eta <- function(ens, sb_col, spinup) {
  eta_col <- paste0("ETAGEB_", sb_col)
  vapply(ens$results, function(r) {
    wb <- r$output_data$water_balance
    if (!isTRUE(r$success) || is.null(wb) || !eta_col %in% names(wb)) return(NA_real_)
    eta <- wb[[eta_col]]
    mon <- if (!is.null(wb$DateTime)) as.integer(format(wb$DateTime, "%m")) else NA
    if (spinup > 0 && length(eta) > spinup) {
      eta <- eta[-seq_len(spinup)]; mon <- mon[-seq_len(spinup)]
    }
    jja <- mon %in% 6:8
    if (!any(jja, na.rm = TRUE)) return(NA_real_)
    mean(eta[jja], na.rm = TRUE)
  }, numeric(1))
}

lognse_values <- extract_lognse(ensemble, sb4, spinup_ts)
eta_values    <- extract_summer_eta(ensemble, sb4, spinup_ts)

cat(sprintf("  logNSE: %d/%d valid | median = %.3f\n",
            sum(!is.na(lognse_values)), length(lognse_values),
            stats::median(lognse_values, na.rm = TRUE)))
cat(sprintf("  Summer ETA (mm/step): %d/%d valid | median = %.3f\n",
            sum(!is.na(eta_values)), length(eta_values),
            stats::median(eta_values, na.rm = TRUE)))

# --- 10b. Sobol indices for both ET targets ---

sobol_lognse <- calculate_sobol_indices(Y = lognse_values, sobol_samples = samples,
                                        boot = TRUE, R = 500)
sobol_eta    <- calculate_sobol_indices(Y = eta_values, sobol_samples = samples,
                                        boot = TRUE, R = 500)

# Helper: long $results -> per-parameter Si/Ti table, ET params flagged
et_params <- c("ETSLPCOR", "ETSYSCOR", "FKFAK", "FHL")
sobol_to_table <- function(sob) {
  res <- as.data.frame(sob$results)
  si  <- res[res$sensitivity == "Si", c("parameters", "original")]
  ti  <- res[res$sensitivity == "Ti", c("parameters", "original")]
  names(si)[2] <- "Si"; names(ti)[2] <- "Ti"
  tab <- merge(si, ti, by = "parameters")
  tab$Si <- pmax(tab$Si, 0); tab$Ti <- pmax(tab$Ti, 0)  # clip near-zero negatives
  tab$is_et <- tab$parameters %in% et_params
  tab[order(-tab$Ti), ]
}

ti_lognse <- sobol_to_table(sobol_lognse)
ti_eta    <- sobol_to_table(sobol_eta)

cat("\nTop parameters by Ti (logNSE, low-flow discharge):\n")
print(utils::head(ti_lognse[, c("parameters", "Si", "Ti")], 8), row.names = FALSE)
cat("\nET-parameter ranks for summer ETA:\n")
print(ti_eta[ti_eta$is_et, c("parameters", "Si", "Ti")], row.names = FALSE)

# --- 10c. Plots: Ti for both ET targets, ET params highlighted ---

et_ti_plot <- function(tab, target_label) {
  ggplot(tab, aes(x = reorder(parameters, Ti), y = Ti, fill = is_et)) +
    geom_col(width = 0.7) +
    coord_flip() +
    scale_fill_manual(values = c("FALSE" = "#95a5a6", "TRUE" = "#27ae60"),
                      labels = c("FALSE" = "Other", "TRUE" = "ET / FHL"),
                      name = NULL) +
    labs(title = paste0("Total-Effect Sobol Indices (Ti) — ", target_label,
                        ", Subbasin ", target_subbasin),
         subtitle = "Green = ET correctors + hydraulic lift (FHL)",
         x = NULL, y = "Ti (total-effect index)") +
    theme_bw(base_size = 11) + theme(legend.position = "top")
}

p_lognse <- et_ti_plot(ti_lognse, "logNSE (low flow)")
print(p_lognse)
ggsave(file.path(plot_dir, paste0("sobol_Ti_logNSE_", sb_tag, ".png")),
       p_lognse, width = 8, height = 6, dpi = 150)
cat("Saved: sobol_Ti_logNSE_", sb_tag, ".png\n", sep = "")

p_eta <- et_ti_plot(ti_eta, "Summer actual ET (JJA)")
print(p_eta)
ggsave(file.path(plot_dir, paste0("sobol_Ti_summerETA_", sb_tag, ".png")),
       p_eta, width = 8, height = 6, dpi = 150)
cat("Saved: sobol_Ti_summerETA_", sb_tag, ".png\n", sep = "")

# Dotty plot of FHL vs both targets — does hydraulic lift move anything?
p_fhl <- plot_dotty(
  parameter_sets = samples$parameter_sets[, "FHL", drop = FALSE],
  Y              = eta_values,
  y_label        = "Summer ETA (mm/step)",
  n_col          = 1,
  show_envelope  = TRUE
) + labs(title = paste("FHL vs summer ET — Subbasin", target_subbasin))
print(p_fhl)
ggsave(file.path(plot_dir, paste0("dotty_FHL_summerETA_", sb_tag, ".png")),
       p_fhl, width = 6, height = 5, dpi = 150)
cat("Saved: dotty_FHL_summerETA_", sb_tag, ".png\n", sep = "")

# =============================================================================
# 11. EXPORT RESULTS
# =============================================================================

export_sensitivity_results(
  output_dir     = plot_dir,
  sobol_indices  = sobol_nse,
  parameter_sets = samples$parameter_sets,
  metrics        = nse_values,
  prefix         = paste0("sobol_NSE_NB", target_subbasin)
)

export_sensitivity_results(
  output_dir     = plot_dir,
  sobol_indices  = sobol_kge,
  parameter_sets = samples$parameter_sets,
  metrics        = kge_values,
  prefix         = paste0("sobol_KGE_NB", target_subbasin)
)

# ET targets (section 10)
export_sensitivity_results(
  output_dir     = plot_dir,
  sobol_indices  = sobol_lognse,
  parameter_sets = samples$parameter_sets,
  metrics        = lognse_values,
  prefix         = paste0("sobol_logNSE_NB", target_subbasin)
)

export_sensitivity_results(
  output_dir     = plot_dir,
  sobol_indices  = sobol_eta,
  parameter_sets = samples$parameter_sets,
  metrics        = eta_values,
  prefix         = paste0("sobol_summerETA_NB", target_subbasin)
)

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n============================================================\n")
cat("Script 2 complete.\n")
cat("Results saved to:", plot_dir, "\n")
cat("============================================================\n")
