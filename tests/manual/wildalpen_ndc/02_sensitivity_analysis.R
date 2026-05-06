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

project_path    <- "D:/temp/Wildalpen_Example"
target_subbasin <- "003"    # primary subbasin for sensitivity indices

base_settings <- list(
  STARTDATE  = c(2000, 10, 1, 0, 0),
  ENDDATE    = c(2015, 9, 30, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini_agg.txt"
)

# Sobol sample size: total runs = n × (n_params + 2)
# Use 30–50 for a smoke test; 200–500 for publication-quality indices
n_sobol <- 50
n_cores <- 8

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
  "PCOR", "TCOR"
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
# 5. RUN PARALLEL ENSEMBLE
# =============================================================================

rds_dir <- file.path(project_path, "sensitivity_results")
dir.create(rds_dir, showWarnings = FALSE, recursive = TRUE)
rds_file <- file.path(rds_dir, "ensemble_ndc_sensitivity.rds")

if (file.exists(rds_file)) {
  cat("=== Loading cached ensemble from disk ===\n")
  ensemble <- readRDS(rds_file)
} else {
  cat("=== Running parallel ensemble ===\n")
  ensemble <- run_cosero_ensemble_parallel(
    project_path    = project_path,
    parameter_sets  = samples$parameter_sets,
    par_bounds      = par_bounds,
    base_settings   = base_settings,
    n_cores         = n_cores,
    quiet           = FALSE,
    statevar_source = 1
  )
  saveRDS(ensemble, rds_file)
  cat("Ensemble saved to:", rds_file, "\n")
}

n_runs <- length(ensemble$results)
n_ok   <- sum(sapply(ensemble$results, function(r) isTRUE(r$success)))
cat(sprintf("Successful runs: %d / %d (%.0f%%)\n\n", n_ok, n_runs,
            100 * n_ok / n_runs))

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

# Print top-5 parameters by total-effect index (Ti) for NSE
ti_nse <- sobol_nse$indices[order(-sobol_nse$indices$Ti), ]
cat("Top 5 parameters by Ti (NSE):\n")
print(head(ti_nse[, c("parameter", "Si", "Ti")], 5))
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

disag_params  <- c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR")
std_params    <- setdiff(param_names, disag_params)

ti_df <- sobol_nse$indices %>%
  mutate(
    group = ifelse(parameter %in% disag_params, "Disaggregation", "Standard hydro"),
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
# 9. EXPORT RESULTS
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

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n============================================================\n")
cat("Script 2 complete.\n")
cat("Results saved to:", plot_dir, "\n")
cat("============================================================\n")
