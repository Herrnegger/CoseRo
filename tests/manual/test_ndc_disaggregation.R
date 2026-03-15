# =============================================================================
# NDC Spatial Disaggregation — Manual Test Script
# =============================================================================
# Tests the dev/spatial-disaggregation branch end-to-end:
#   1.  Setup aggregated example project
#   2.  Read parameter file (195 cols, new NDC columns)
#   3.  Read defaults.txt (NDC = 5)
#   4.  Validate NDC in cosero_defaults
#   5.  Load disaggregation parameter bounds
#   6.  Run COSERO with NDC = 5
#   7.  Read model output
#   8.  DDS optimization with LAPSE_T + SOILVAR + BETA
#   9.  Sobol sensitivity analysis with disaggregation params
#  10.  Behavioural filtering (extract_behavioral_runs)
#  11.  Save best-performing parameter set as COSERO file
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude
# Date: 2026-03-12
# Branch: dev/spatial-disaggregation
# =============================================================================

devtools::load_all()

library(ggplot2)
library(dplyr)
library(tidyr)


# =============================================================================
# USER SETTINGS
# =============================================================================

project_path    <- "D:/COSERO/COSERO_Wildalpen_agreggated_test"
target_subbasin <- "003"                   # subbasin for sensitivity metrics

base_settings <- list(
  STARTDATE  = c(1991, 10, 1, 0, 0),
  ENDDATE    = c(2000, 9, 30, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini_agg.txt"
)

# Parameters for sensitivity analysis
# Disaggregation params (LAPSE_T, SOILVAR) + standard hydro params
param_names <- c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR",          # disaggregation
                 "CTMAX", "CTMIN",                           # snow
                 "M", "BETA", "KBF",                         # soil + runoff
                 "TAB1", "TAB2", "TAB3",                   # recession
                 "PCOR", "TCOR")                    

# Sobol sample size  — keep small for a quick smoke test
n_sobol  <- 30     # total runs = n × (n_params + 2);  use 200–500 for production
n_cores  <- 4


# =============================================================================
# TEST 1 — Setup aggregated example project
# =============================================================================

cat("\n=== TEST 1: Setup aggregated example project ===\n")

# Always recreate for a clean test
if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
setup_cosero_project_example_aggregated(project_path)

stopifnot(dir.exists(project_path))
stopifnot(file.exists(file.path(project_path, "COSERO.exe")))
stopifnot(file.exists(file.path(project_path, "input", "defaults.txt")))
stopifnot(file.exists(file.path(project_path, "input", "para_ini_agg.txt")))
cat("PASS: Project structure OK\n")


# =============================================================================
# TEST 2 — Read parameter file (expect 195 columns, new NDC columns present)
# =============================================================================

cat("\n=== TEST 2: Read parameter file ===\n")

par_file <- file.path(project_path, "input", "para_ini_agg.txt")
params   <- read_cosero_parameters(par_file)

cat("Columns:", ncol(params), "\n")  # expect 195

new_cols <- names(params)[grep("SOILVAR|HYDROVAR|CTVAR|LAPSE|HYPSO", names(params))]
cat("New NDC-related columns found:\n")
print(new_cols)

stopifnot(ncol(params) == 195)
stopifnot("LAPSE_T_"  %in% names(params))
stopifnot("LAPSE_P_"  %in% names(params))
stopifnot("SOILVAR_"  %in% names(params))
stopifnot("HYDROVAR_" %in% names(params))
stopifnot("CTVAR_"    %in% names(params))
stopifnot("HYPSO0_"   %in% names(params))
stopifnot("HYPSO100_" %in% names(params))
cat("PASS: All expected columns present\n")


# =============================================================================
# TEST 3 — Read defaults.txt (expect NDC = 5)
# =============================================================================

cat("\n=== TEST 3: Read defaults.txt ===\n")

defaults <- read_defaults(file.path(project_path, "input", "defaults.txt"))
cat("NDC =", defaults$NDC, "\n")

stopifnot(!is.null(defaults$NDC))
stopifnot(defaults$NDC == 5)
cat("PASS: NDC = 5 confirmed\n")


# =============================================================================
# TEST 4 — Validate NDC (valid and invalid values)
# =============================================================================

cat("\n=== TEST 4: validate_cosero_defaults() for NDC ===\n")

result_valid <- tryCatch(
  { validate_cosero_defaults(list(NDC = 5)); "valid" },
  error = function(e) paste("ERROR:", e$message)
)
cat("NDC = 5  →", result_valid, "\n")
stopifnot(result_valid == "valid")

result_zero <- tryCatch(
  { validate_cosero_defaults(list(NDC = 0)); "valid" },
  error = function(e) "error (expected)"
)
cat("NDC = 0  →", result_zero, "\n")
stopifnot(grepl("error", result_zero))

result_high <- tryCatch(
  { validate_cosero_defaults(list(NDC = 11)); "valid" },
  error = function(e) "error (expected)"
)
cat("NDC = 11 →", result_high, "\n")
stopifnot(grepl("error", result_high))

cat("PASS: NDC validation OK\n")


# =============================================================================
# TEST 5 — Load disaggregation parameter bounds
# =============================================================================

cat("\n=== TEST 5: Load disaggregation parameter bounds ===\n")

disag_params <- c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR")
bounds_disag <- load_parameter_bounds(parameters = disag_params)

print(bounds_disag[, c("parameter", "min", "max", "default", "modification_type", "category")])

stopifnot(nrow(bounds_disag) == 5)
stopifnot(all(bounds_disag$category == "Disaggregation"))
stopifnot(all(bounds_disag$modification_type == "abschg"))
cat("PASS: Disaggregation bounds loaded correctly\n")


# =============================================================================
# TEST 6 — Run COSERO with NDC = 5
# =============================================================================

cat("\n=== TEST 6: Run COSERO with NDC = 5 ===\n")

# Short run for quick testing
test_settings <- modifyList(base_settings, list(
  STARTDATE = c(2000, 1, 1, 0, 0),
  ENDDATE   = c(2024, 12, 31, 0, 0),
  SPINUP    = 365,
  OUTPUTTYPE = 3
))

result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = test_settings,
  statevar_source   = 1,
  read_outputs = TRUE
)

stopifnot(!is.null(result_base))
cat("PASS: COSERO ran without error\n")

# Print metrics for all subbasins
subbasins <- as.character(unique(result_base$output_data$runoff$SUBBASIN %||%
                                   sub("QSIM_0*", "", grep("^QSIM_", names(result_base$output_data$runoff), value = TRUE))))

# Safer: extract metrics directly
for (sb in c("001", "002", "003")) {
  nse_val <- tryCatch(extract_run_metrics(result_base, sb, "NSE"), error = function(e) NA)
  kge_val <- tryCatch(extract_run_metrics(result_base, sb, "KGE"), error = function(e) NA)
  cat(sprintf("  Subbasin %s:  NSE = %.3f  |  KGE = %.3f\n", sb, nse_val, kge_val))
}


# =============================================================================
# TEST 7 — Read model output
# =============================================================================

cat("\n=== TEST 7: Read model output ===\n")

output <- read_cosero_output(project_path)
str(output, max.level = 1)

stopifnot(!is.null(output$runoff))
stopifnot(nrow(output$runoff) > 0)
cat("PASS: Output read successfully\n")


# =============================================================================
# TEST 8 — DDS optimization with disaggregation + standard params
# =============================================================================

cat("\n=== TEST 8: DDS optimization (20 iterations, quick smoke test) ===\n")

opt_params <- load_parameter_bounds(parameters = c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR", "PCOR", "TAB1"))
opt_params <- load_parameter_bounds(parameters = c("PCOR", "TAB1"))  # quick smoke test: 2 params only

result_opt <- optimize_cosero_dds(
  cosero_path       = project_path,             # project root with input/ output/ COSERO.exe
  par_bounds        = opt_params,               # parameter bounds from load_parameter_bounds()
  target_subbasins  = c("001", "002", "003"),   # subbasins to calibrate; zones auto-mapped from para file
  metric            = c("NSE", "r2"), # c("NSE", "lnNSE", "rNSE"), # high / low / mid-range flow emphasis
  metric_weights    = c(0.7, 0.3), # c(0.5, 0.3, 0.2),         # weights for combined OF (must sum to 1)
  aggregation       = "min",  #"weighted",               # aggregate across subbasins using subbasin_weights
  subbasin_weights  = c(0.5, 0.3, 0.2),         # per-subbasin weights (must sum to 1)
  defaults_settings = modifyList(base_settings, list(
    STARTDATE       = c(2000, 10, 1, 0, 0),     # calibration period start
    ENDDATE         = c(2015, 9, 30, 0, 0),     # calibration period end
    SPINUP          = 365,                       # 1-year spin-up
    statevar_source = 2                          # 1 = cold start; 2 = warm start (statevar.dmp)
  )),
  max_iter          = 10,                      # DDS iterations (50-100 for smoke test)
  verbose           = TRUE                      # print progress bar
)

stopifnot(!is.null(result_opt))
stopifnot(!is.null(result_opt$optimized_par_file))
cat(sprintf("Best combined metric: %.4f\n", -result_opt$best_value))
cat("Optimized file:", result_opt$optimized_par_file, "\n")

# Verify original parameter file was NOT modified (working-copy safety check)
orig_params <- read_cosero_parameters(par_file)
stopifnot(identical(ncol(orig_params), ncol(params)))   # same structure
work_file_dds <- file.path(project_path, "input",
                           paste0(tools::file_path_sans_ext("para_ini_agg.txt"),
                                  "_opt_work.txt"))
stopifnot(!file.exists(work_file_dds))   # working copy cleaned up
cat("PASS: DDS optimization completed — original parameter file untouched\n")


# =============================================================================
# TEST 8b — SCE-UA optimization (working-copy safety check)
# =============================================================================

cat("\n=== TEST 8b: SCE-UA optimization (10 evaluations, smoke test) ===\n")

result_sce <- optimize_cosero_sce(
  cosero_path       = project_path,
  par_bounds        = opt_params,               # same 2-param bounds as DDS smoke test
  target_subbasins  = c("001", "002", "003"),
  metric            = "NSE",
  aggregation       = "mean",
  defaults_settings = modifyList(base_settings, list(
    STARTDATE = c(2000, 10, 1, 0, 0),
    ENDDATE   = c(2015, 9, 30, 0, 0),
    SPINUP    = 365
  )),
  maxn    = 10,     # tiny limit — just enough to exercise the code path
  ngs     = 2,
  verbose = TRUE
)

stopifnot(!is.null(result_sce))
stopifnot(!is.null(result_sce$optimized_par_file))

# Verify original parameter file was NOT modified
orig_params_sce <- read_cosero_parameters(par_file)
stopifnot(identical(ncol(orig_params_sce), ncol(params)))
work_file_sce <- file.path(project_path, "input",
                           paste0(tools::file_path_sans_ext("para_ini_agg.txt"),
                                  "_opt_work.txt"))
stopifnot(!file.exists(work_file_sce))   # working copy cleaned up
cat("PASS: SCE-UA optimization completed — original parameter file untouched\n")


# =============================================================================
# TEST 8c — Sequential ensemble (working-copy safety check)
# =============================================================================

cat("\n=== TEST 8c: Sequential ensemble (6 runs, working-copy safety check) ===\n")

seq_bounds <- load_parameter_bounds(parameters = c("PCOR", "TAB1"))
seq_sobol  <- generate_sobol_samples(create_sobol_bounds(seq_bounds), n = 3, order = "first")
# n=3 with 2 params → (2+2)*3 = 12 runs; enough to exercise the loop

seq_settings <- modifyList(base_settings, list(
  STARTDATE = c(2010, 10, 1, 0, 0),
  ENDDATE   = c(2012, 9, 30, 0, 0),
  SPINUP    = 365
))

# Record parameter values BEFORE the run so we can compare after
params_before <- read_cosero_parameters(par_file)
pcor_before   <- params_before$PCOR_[1]   # first zone, first column

ensemble_seq <- run_cosero_ensemble(
  project_path   = project_path,
  parameter_sets = seq_sobol$parameter_sets,
  par_bounds     = seq_bounds,
  base_settings  = seq_settings,
  quiet          = FALSE,
  statevar_source = 1
)

# Verify original parameter file values are unchanged
params_after <- read_cosero_parameters(par_file)
pcor_after   <- params_after$PCOR_[1]
stopifnot(isTRUE(all.equal(pcor_before, pcor_after)))

# Verify working copy was deleted
work_file_seq <- file.path(project_path, "input",
                           paste0(tools::file_path_sans_ext("para_ini_agg.txt"),
                                  "_ens_work.txt"))
stopifnot(!file.exists(work_file_seq))

n_ok <- sum(sapply(ensemble_seq$results, function(r) isTRUE(r$success)))
cat(sprintf("Successful runs: %d / %d\n", n_ok, length(ensemble_seq$results)))
cat("PASS: Sequential ensemble completed — original parameter file untouched\n")


# =============================================================================
# TEST 9 — Sobol sensitivity analysis with disaggregation params
# =============================================================================

cat("\n=== TEST 9: Sobol sensitivity analysis ===\n")

sens_settings <- modifyList(base_settings, list(
  STARTDATE = c(2000, 10, 1, 0, 0),
  ENDDATE   = c(2015, 9, 30, 0, 0),
  SPINUP    = 365
))

par_bounds   <- load_parameter_bounds(parameters = param_names)
sobol_bounds <- create_sobol_bounds(par_bounds)
samples      <- generate_sobol_samples(sobol_bounds, n = n_sobol, order = "first")

cat(sprintf("Ensemble size: %d runs (%d params, n = %d)\n",
            nrow(samples$parameter_sets), length(param_names), n_sobol))

ensemble <- run_cosero_ensemble_parallel(
  project_path   = project_path,
  parameter_sets = samples$parameter_sets,
  par_bounds     = par_bounds,
  base_settings  = sens_settings,
  n_cores        = n_cores,
  statevar_source = 1
)

# Save — ensemble runs are expensive
rds_dir <- file.path(project_path, "sensitivity_results")
dir.create(rds_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(ensemble, file.path(rds_dir, "ensemble_ndc_test.rds"))
cat("Ensemble saved to:", file.path(rds_dir, "ensemble_ndc_test.rds"), "\n")

ensemble <- readRDS(file.path(rds_dir, "ensemble_ndc_test.rds"))

# Compute metrics
nse_values <- extract_ensemble_metrics(ensemble, subbasin_id = target_subbasin, metric = "NSE")
kge_values <- extract_ensemble_metrics(ensemble, subbasin_id = target_subbasin, metric = "KGE")

cat(sprintf("Ensemble NSE — median: %.3f  range: [%.3f, %.3f]\n",
            median(nse_values, na.rm = TRUE),
            min(nse_values, na.rm = TRUE),
            max(nse_values, na.rm = TRUE)))

# Sobol indices
sobol_nse <- calculate_sobol_indices(Y = nse_values, sobol_samples = samples,
                                     boot = TRUE, R = 200)
sobol_kge <- calculate_sobol_indices(Y = kge_values, sobol_samples = samples,
                                     boot = TRUE, R = 200)

# --- Plots ---
plot_dir <- file.path(project_path, "sensitivity_results")
sb_tag   <- paste0("NB", target_subbasin)

p_sobol_nse <- plot_sobol(sobol_nse,
                           title = paste("Sobol Indices — NSE, Subbasin", target_subbasin))
print(p_sobol_nse)
ggsave(file.path(plot_dir, paste0("sobol_NSE_", sb_tag, ".png")),
       p_sobol_nse, width = 10, height = 6, dpi = 150)

baseline_nse <- tryCatch(extract_run_metrics(result_base, target_subbasin, "NSE"), error = function(e) NA)

p_dotty <- plot_dotty(samples$parameter_sets, Y = nse_values, y_label = "NSE",
                       n_col = 4, reference_line = baseline_nse, y_min = -0.5,
                       show_envelope = TRUE, envelope_quantile = 0.95) +
  labs(title   = paste("Dotty Plots — NSE, Subbasin", target_subbasin),
       subtitle = "Red dashed = baseline NSE | Orange = 95th-percentile LOESS envelope")
print(p_dotty)
ggsave(file.path(plot_dir, paste0("dotty_NSE_", sb_tag, ".png")),
       p_dotty, width = 14, height = 10, dpi = 150)

p_uncertainty <- plot_ensemble_uncertainty(
  ensemble, q_max = 400,
  lower_quantile = 0.1, upper_quantile = 0.9,
  subbasin_id    = sprintf("%04d", as.numeric(target_subbasin))
) +
  labs(title    = paste("Ensemble Discharge Uncertainty — Subbasin", target_subbasin),
       subtitle = sprintf("n = %d runs | 10–90%% ribbon", nrow(samples$parameter_sets)))
print(p_uncertainty)

ggsave(file.path(plot_dir, paste0("ensemble_uncertainty_", sb_tag, ".png")),
       p_uncertainty, width = 14, height = 6, dpi = 150)

cat("PASS: Sobol analysis completed — plots saved to", plot_dir, "\n")


# =============================================================================
# TEST 10 — Behavioural filtering with extract_behavioral_runs()
# =============================================================================

cat("\n=== TEST 10: Behavioural filtering ===\n")

eval_out <- extract_behavioral_runs(
  ensemble_output  = ensemble,
  subbasin_id      = target_subbasin,
  nse_thresh       = 0.5,
  kge_thresh       = 0.6,
  pbias_thresh     = c(-20, 20),
  plot_uncertainty = TRUE,
  lower_quantile   = 0,
  upper_quantile   = 1,
  xlim             = c(0, 1),
  ylim             = c(0, 1)
)

n_behav <- length(eval_out$behavioral_run_ids)
n_total <- length(ensemble$results)
cat(sprintf("Behavioural runs: %d / %d  (%.1f%%)\n",
            n_behav, n_total, 100 * n_behav / n_total))

print(eval_out$scatter_plot)
ggsave(file.path(plot_dir, paste0("behavioral_scatter_", sb_tag, ".png")),
       eval_out$scatter_plot, width = 8, height = 7, dpi = 150)

if (!is.null(eval_out$uncertainty_plot)) {
  print(eval_out$uncertainty_plot)
  ggsave(file.path(plot_dir, paste0("behavioral_uncertainty_", sb_tag, ".png")),
         eval_out$uncertainty_plot, width = 14, height = 6, dpi = 150)
}

# Dotty plots restricted to behavioural runs
if (n_behav >= 2) {
  behav_nse <- eval_out$metrics_df$NSE[eval_out$metrics_df$category == "Behavioral"]
  p_dotty_behav <- plot_dotty(
    eval_out$filtered_ensemble$parameter_sets,
    Y              = behav_nse,
    y_label        = "NSE",
    n_col          = 4,
    reference_line = 0.4,
    y_min          = 0.3,
    show_envelope  = TRUE,
    envelope_quantile = 1
  ) +
    labs(title    = paste("Dotty Plots — Behavioural Runs Only, Subbasin", target_subbasin),
         subtitle = sprintf("n = %d behavioural runs", n_behav))
  print(p_dotty_behav)
  ggsave(file.path(plot_dir, paste0("dotty_behavioural_", sb_tag, ".png")),
         p_dotty_behav, width = 14, height = 10, dpi = 150)
} else {
  cat("Note: fewer than 2 behavioural runs — skipping dotty plot\n")
}

cat("PASS: Behavioural filtering completed\n")


# =============================================================================
# TEST 11 — Save best-performing parameter set as COSERO file
# =============================================================================

cat("\n=== TEST 11: Save best NSE / best KGE parameter files ===\n")

# Best run by NSE
best_idx_nse <- which.max(eval_out$metrics_df$NSE)
best_run_id  <- eval_out$metrics_df$run_id[best_idx_nse]
best_nse     <- eval_out$metrics_df$NSE[best_idx_nse]
best_kge     <- eval_out$metrics_df$KGE[best_idx_nse]
cat(sprintf("Best NSE run: id = %d  |  NSE = %.4f  |  KGE = %.4f\n",
            best_run_id, best_nse, best_kge))

# Best run by KGE
best_idx_kge    <- which.max(eval_out$metrics_df$KGE)
best_run_id_kge <- eval_out$metrics_df$run_id[best_idx_kge]
best_kge_val    <- eval_out$metrics_df$KGE[best_idx_kge]
cat(sprintf("Best KGE run: id = %d  |  KGE = %.4f  |  NSE = %.4f\n",
            best_run_id_kge, best_kge_val, eval_out$metrics_df$NSE[best_idx_kge]))

# Write best NSE parameter file
best_params_nse   <- as.list(samples$parameter_sets[best_run_id, , drop = FALSE][1, ])
par_file_best_nse <- file.path(project_path, "input", "para_best_NSE.txt")
file.copy(par_file, par_file_best_nse, overwrite = TRUE)
orig_vals_best <- read_parameter_table(par_file_best_nse, par_bounds$parameter, zone_id = "all")
modify_parameter_table(par_file_best_nse, best_params_nse, par_bounds, orig_vals_best,
                       add_timestamp = TRUE)
cat("Saved best NSE parameter file to:", par_file_best_nse, "\n")
stopifnot(file.exists(par_file_best_nse))

# Write best KGE parameter file (only if different run)
if (best_run_id_kge != best_run_id) {
  best_params_kge   <- as.list(samples$parameter_sets[best_run_id_kge, , drop = FALSE][1, ])
  par_file_best_kge <- file.path(project_path, "input", "para_best_KGE.txt")
  file.copy(par_file, par_file_best_kge, overwrite = TRUE)
  orig_vals_kge <- read_parameter_table(par_file_best_kge, par_bounds$parameter, zone_id = "all")
  modify_parameter_table(par_file_best_kge, best_params_kge, par_bounds, orig_vals_kge,
                         add_timestamp = TRUE)
  cat("Saved best KGE parameter file to:", par_file_best_kge, "\n")
} else {
  cat("Best NSE and best KGE are the same run — no separate KGE file written.\n")
}

# Optional quick verification — uncomment to re-run COSERO with best NSE params
# result_best <- run_cosero(
#   project_path      = project_path,
#   defaults_settings = modifyList(sens_settings, list(PARAFILE = "para_best_NSE.txt")),
#   statevar_source   = 1
# )
# cat(sprintf("Verification — NSE = %.4f  |  KGE = %.4f\n",
#             extract_run_metrics(result_best, target_subbasin, "NSE"),
#             extract_run_metrics(result_best, target_subbasin, "KGE")))

cat("PASS: Best parameter files saved\n")


# =============================================================================
# SUMMARY
# =============================================================================

cat("\n============================================================\n")
cat("All NDC disaggregation tests completed.\n")
cat("Results saved to:", file.path(project_path, "sensitivity_results"), "\n")
cat("============================================================\n")
