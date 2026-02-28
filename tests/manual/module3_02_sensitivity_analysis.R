# =============================================================================
# Module 3, Script 2: Sobol Sensitivity Analysis
# =============================================================================
# Performs global sensitivity analysis using Sobol quasi-random sampling.
# Runs an ensemble of COSERO simulations and calculates first-order (Si) and
# total-effect (STi) sensitivity indices for key model parameters.
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude/Gemini
# Date: 2026-02-26
# =============================================================================

devtools::load_all()

library(ggplot2)
library(dplyr)
library(tidyr)

# =============================================================================
# USER SETTINGS
# =============================================================================

project_path <- "C:/COSERO/MyCatchment"  # <-- adjust to your project

start_date <- "2014 1 1 0 0"
end_date   <- "2020 12 31 0 0"
spinup     <- 365

# Sobol sample size (total runs = n × (n_params + 2))
n_sobol    <- 50       # use 200–500 for production
n_cores    <- 4        # parallel cores (NULL = auto-detect)

target_subbasin <- "003"   # subbasin for sensitivity metrics

# Parameters to analyse
param_names <- c("BETA", "M", "CTMAX", "TAB1", "TAB2", "TAB3", "KBF", "PCOR", "TCOR")

# =============================================================================
# 1. SETUP EXAMPLE PROJECT (optional)
# =============================================================================

# Uncomment to create a fresh Wildalpen example project:
# project_path <- "D:/temp/COSERO_Sensitivity"
# if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
# setup_cosero_project_example(project_path)

# =============================================================================
# 2. LOAD PARAMETER BOUNDS
# =============================================================================

# Load pre-defined bounds from package CSV
par_bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameter bounds for sensitivity analysis:\n")
print(par_bounds[, c("parameter", "description", "min", "max", "default",
                      "modification_type")])

# =============================================================================
# 3. GENERATE SOBOL SAMPLES
# =============================================================================

sobol_bounds <- create_sobol_bounds(par_bounds)
samples      <- generate_sobol_samples(sobol_bounds, n = n_sobol, order = "first")

cat(sprintf("\nGenerated %d parameter sets for %d parameters\n",
            nrow(samples$parameter_sets), length(param_names)))
cat(sprintf("(n=%d × (%d + 2) = %d total runs)\n",
            n_sobol, length(param_names), nrow(samples$parameter_sets)))

# Quick look at the sampled parameter space
head(samples$parameter_sets)

# =============================================================================
# 4. BASELINE RUN (for reference metrics)
# =============================================================================

base_settings <- list(
  STARTDATE  = start_date,
  ENDDATE    = end_date,
  SPINUP     = spinup,
  OUTPUTTYPE = 1
)

cat("\n--- Baseline run ---\n")
result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = base_settings,
  statevar_source   = 1,
  quiet             = FALSE
)

baseline_nse <- extract_run_metrics(result_base, target_subbasin, "NSE")
baseline_kge <- extract_run_metrics(result_base, target_subbasin, "KGE")
cat(sprintf("Baseline NSE = %.4f  |  KGE = %.4f\n", baseline_nse, baseline_kge))

# =============================================================================
# 5. RUN ENSEMBLE (parallel)
# =============================================================================

cat("\n--- Running ensemble ---\n")
ensemble <- run_cosero_ensemble_parallel(
  project_path   = project_path,
  parameter_sets = samples$parameter_sets,
  par_bounds     = par_bounds,
  base_settings  = base_settings,
  n_cores        = n_cores,
  quiet          = FALSE,
  statevar_source = 1
)

cat(sprintf("Ensemble completed in %.1f min\n", ensemble$runtime_minutes))

# =============================================================================
# 6. EXTRACT METRICS
# =============================================================================

# Pre-calculated by COSERO (fast)
nse_values <- extract_ensemble_metrics(ensemble, subbasin_id = target_subbasin, metric = "NSE")
kge_values <- extract_ensemble_metrics(ensemble, subbasin_id = target_subbasin, metric = "KGE")

cat(sprintf("\nMetric summary for subbasin %s (n = %d valid runs):\n",
            target_subbasin, sum(!is.na(nse_values))))
cat(sprintf("  NSE: median = %.3f, range = [%.3f, %.3f]\n",
            median(nse_values, na.rm = TRUE),
            min(nse_values, na.rm = TRUE),
            max(nse_values, na.rm = TRUE)))
cat(sprintf("  KGE: median = %.3f, range = [%.3f, %.3f]\n",
            median(kge_values, na.rm = TRUE),
            min(kge_values, na.rm = TRUE),
            max(kge_values, na.rm = TRUE)))

# =============================================================================
# 7. COMPUTE SOBOL INDICES
# =============================================================================

# First-order (Si) and total-effect (STi) indices for NSE
sobol_nse <- calculate_sobol_indices(
  Y = nse_values,
  sobol_samples = samples,
  boot = TRUE,
  R = 500
)

# Same for KGE
sobol_kge <- calculate_sobol_indices(
  Y = kge_values,
  sobol_samples = samples,
  boot = TRUE,
  R = 500
)

# =============================================================================
# 8. DIAGNOSTIC PLOTS
# =============================================================================

# Output directory for plots
plot_dir <- file.path(project_path, "sensitivity_results")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
sb_tag <- paste0("NB", target_subbasin)   # e.g. "NB003" for file names

# --- 8a. Sobol bar plots (using package function) ---

p_sobol_nse <- plot_sobol(sobol_nse, title = paste("Sobol Indices — NSE, Subbasin", target_subbasin))
print(p_sobol_nse)
ggsave(file.path(plot_dir, paste0("sobol_NSE_", sb_tag, ".png")),
       p_sobol_nse, width = 10, height = 6, dpi = 150)

p_sobol_kge <- plot_sobol(sobol_kge, title = paste("Sobol Indices — KGE, Subbasin", target_subbasin))
print(p_sobol_kge)
ggsave(file.path(plot_dir, paste0("sobol_KGE_", sb_tag, ".png")),
       p_sobol_kge, width = 10, height = 6, dpi = 150)

# --- 8b. Dotty plots (parameter vs metric) ---

p_dotty_nse <- plot_dotty(
  parameter_sets = samples$parameter_sets,
  Y = nse_values,
  y_label = "NSE",
  n_col = 3,
  reference_line = baseline_nse,
  y_min = -0.5
)
p_dotty_nse <- p_dotty_nse +
  labs(title = paste("Dotty Plots — NSE, Subbasin", target_subbasin),
       subtitle = "Red dashed line = baseline NSE")
print(p_dotty_nse)
ggsave(file.path(plot_dir, paste0("dotty_NSE_", sb_tag, ".png")),
       p_dotty_nse, width = 12, height = 8, dpi = 150)

p_dotty_kge <- plot_dotty(
  parameter_sets = samples$parameter_sets,
  Y = kge_values,
  y_label = "KGE",
  n_col = 3,
  reference_line = baseline_kge,
  y_min = -0.5
)
p_dotty_kge <- p_dotty_kge +
  labs(title = paste("Dotty Plots — KGE, Subbasin", target_subbasin),
       subtitle = "Red dashed line = baseline KGE")
print(p_dotty_kge)
ggsave(file.path(plot_dir, paste0("dotty_KGE_", sb_tag, ".png")),
       p_dotty_kge, width = 12, height = 8, dpi = 150)

# --- 8c. Ensemble uncertainty band ---

p_uncertainty <- plot_ensemble_uncertainty(
  ensemble,
  subbasin_id = sprintf("%04d", as.numeric(target_subbasin)),
  variable = "QSIM"
)
p_uncertainty <- p_uncertainty +
  labs(title = paste("Ensemble Discharge Uncertainty — Subbasin", target_subbasin),
       subtitle = sprintf("n = %d runs | Grey bands = 90%% / 50%% CI | Blue = median",
                           nrow(samples$parameter_sets)))
print(p_uncertainty)
ggsave(file.path(plot_dir, paste0("ensemble_uncertainty_", sb_tag, ".png")),
       p_uncertainty, width = 14, height = 6, dpi = 150)

# --- 8d. Metric distributions ---

metric_df <- data.frame(NSE = nse_values, KGE = kge_values) %>%
  pivot_longer(everything(), names_to = "metric", values_to = "value")

p_dist <- ggplot(metric_df, aes(x = value, fill = metric)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  facet_wrap(~metric, scales = "free_x") +
  geom_vline(data = data.frame(metric = c("NSE", "KGE"),
                                baseline = c(baseline_nse, baseline_kge)),
             aes(xintercept = baseline), linetype = "dashed", colour = "red") +
  scale_fill_manual(values = c(NSE = "#2166ac", KGE = "#b2182b")) +
  labs(title = "Distribution of Performance Metrics Across Ensemble",
       x = "Metric Value", y = "Count") +
  theme_bw() +
  theme(legend.position = "none")
print(p_dist)
ggsave(file.path(plot_dir, paste0("metric_distribution_", sb_tag, ".png")),
       p_dist, width = 10, height = 5, dpi = 150)

# --- 8e. Behavioural parameter ranges ---

# Identify behavioural runs (e.g., NSE > 0.6)
threshold <- 0.6
behavioural_idx <- which(nse_values > threshold)
cat(sprintf("\nBehavioural runs (NSE > %.1f): %d / %d (%.0f%%)\n",
            threshold, length(behavioural_idx), length(nse_values),
            100 * length(behavioural_idx) / length(nse_values)))

if (length(behavioural_idx) > 5) {
  behav_params <- samples$parameter_sets[behavioural_idx, ]
  all_params   <- samples$parameter_sets

  # Normalise to [0,1] for comparability
  param_ranges <- data.frame()
  for (p in param_names) {
    p_min <- par_bounds$min[par_bounds$parameter == p]
    p_max <- par_bounds$max[par_bounds$parameter == p]
    param_ranges <- rbind(param_ranges, data.frame(
      parameter   = p,
      type        = c("All", "Behavioural"),
      q25         = c(quantile((all_params[[p]] - p_min) / (p_max - p_min), 0.25),
                       quantile((behav_params[[p]] - p_min) / (p_max - p_min), 0.25)),
      q50         = c(quantile((all_params[[p]] - p_min) / (p_max - p_min), 0.50),
                       quantile((behav_params[[p]] - p_min) / (p_max - p_min), 0.50)),
      q75         = c(quantile((all_params[[p]] - p_min) / (p_max - p_min), 0.75),
                       quantile((behav_params[[p]] - p_min) / (p_max - p_min), 0.75))
    ))
  }

  p_ranges <- ggplot(param_ranges, aes(x = parameter, y = q50, colour = type)) +
    geom_pointrange(aes(ymin = q25, ymax = q75),
                    position = position_dodge(width = 0.5), linewidth = 0.8) +
    geom_hline(yintercept = c(0, 1), linetype = "dotted", colour = "grey60") +
    scale_colour_manual(values = c(All = "grey50", Behavioural = "#e74c3c")) +
    labs(title = sprintf("Parameter Ranges: All vs Behavioural (NSE > %.1f)", threshold),
         subtitle = "Normalised to parameter bounds [0, 1]; points = median, bars = IQR",
         x = NULL, y = "Normalised Value", colour = NULL) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
  print(p_ranges)
  ggsave(file.path(plot_dir, paste0("behavioural_ranges_", sb_tag, ".png")),
         p_ranges, width = 10, height = 6, dpi = 150)
}

cat("\nPlots saved to:", plot_dir, "\n")

# =============================================================================
# 9. EXPORT RESULTS
# =============================================================================

output_dir <- file.path(project_path, "sensitivity_results")

export_sensitivity_results(
  output_dir     = output_dir,
  sobol_indices  = sobol_nse,
  parameter_sets = samples$parameter_sets,
  metrics        = nse_values,
  prefix         = paste0("sobol_NSE_NB", target_subbasin)
)

export_sensitivity_results(
  output_dir     = output_dir,
  sobol_indices  = sobol_kge,
  parameter_sets = samples$parameter_sets,
  metrics        = kge_values,
  prefix         = paste0("sobol_KGE_NB", target_subbasin)
)

# Best parameter set
best_idx <- which.max(nse_values)
cat("\nBest parameter set (NSE =", round(nse_values[best_idx], 4), "):\n")
print(samples$parameter_sets[best_idx, ])

cat("\n--- Script 2 complete ---\n")
