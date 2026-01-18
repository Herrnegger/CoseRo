# =============================================================================
# PART 2: COMBINE BATCHES AND GENERATE RESULTS
# =============================================================================
# This workflow combines simulation batches from Part 1 and generates:
# - Performance metrics (NSE, KGE)
# - Sobol sensitivity analysis
# - Publication-ready plots (11-12 plots at 600 DPI)
# - Best parameter files
# - Comprehensive analysis report
# =============================================================================

# Load required libraries
devtools::load_all()
library(ggplot2)
library(dplyr)
library(tidyr)

# Memory optimization settings
options(expressions = 500000)  # Increase expression limit
gc(full = TRUE, verbose = FALSE)  # Initial cleanup

cat("Initial memory status:\n")
print(gc())

# =============================================================================
# CONFIGURATION
# =============================================================================

cat("=============================================================================\n")
cat("PART 2: RESULTS GENERATION - COSERO SENSITIVITY ANALYSIS\n")
cat("=============================================================================\n\n")

# Project path (must match Part 1)
project_path <- "D:/Structural Exercise/COSERO_MonteCarlo_Optimierung_SH/COSERO_MonteCarlo_Optimierung_SH"
param_file <- file.path(project_path, "input", "para_run_opt6_final.txt")

# Results directory
results_base_dir <- file.path(project_path, "sensitivity_analysis_results")

if (!dir.exists(results_base_dir)) {
  stop("❌ Results directory not found! Please run workflow_1_run_simulations.R first.")
}

# Visualization settings
viz_dpi <- 600
viz_width <- 16
viz_height <- 12
viz_sobol_width <- 14
viz_sobol_height <- 8

cat("Configuration:\n")
cat("  Results directory:", results_base_dir, "\n")
cat("  Visualization: Publication-ready (", viz_dpi, " DPI)\n\n")

# =============================================================================
# STEP 1: LOAD CONFIGURATION AND SAMPLES
# =============================================================================

cat("=============================================================================\n")
cat("STEP 1: LOAD CONFIGURATION\n")
cat("=============================================================================\n\n")

config_file <- file.path(results_base_dir, "analysis_configuration.rds")

if (!file.exists(config_file)) {
  stop("❌ Configuration file not found! Please run workflow_1_run_simulations.R first.")
}

config <- readRDS(config_file)

# Extract configuration
n_samples <- config$n_samples
param_names <- config$param_names
bounds <- config$bounds
sobol_bounds <- config$sobol_bounds
samples <- config$samples
plot_subbasin <- config$plot_subbasin
n_total <- config$total_simulations

cat("✓ Configuration loaded successfully\n")
cat("  Generated:", format(config$generated_date, "%Y-%m-%d %H:%M:%S"), "\n")
cat("  Total simulations:", n_total, "\n")
cat("  Parameters:", paste(param_names, collapse=", "), "\n")
cat("  Target subbasin:", plot_subbasin, "\n\n")

# =============================================================================
# STEP 2: COMBINE SIMULATION BATCHES
# =============================================================================

cat("=============================================================================\n")
cat("STEP 2: COMBINE SIMULATION BATCHES\n")
cat("=============================================================================\n\n")

# Check for batch directories (try both locations)
batch_dir_new <- file.path(results_base_dir, "simulation_batches")
batch_dir_old <- file.path(project_path, "temp_sequential_batches")

if (dir.exists(batch_dir_new)) {
  batch_dir <- batch_dir_new
  cat("Found batch directory (new location):", batch_dir, "\n")
} else if (dir.exists(batch_dir_old)) {
  batch_dir <- batch_dir_old
  cat("Found batch directory (old location):", batch_dir, "\n")
} else {
  stop("❌ Batch directory not found! Searched:\n  - ", batch_dir_new, "\n  - ", batch_dir_old)
}

cat("Combining batch results...\n")
batch_files <- list.files(batch_dir, pattern = "^batch_.*\\.rds$", full.names = TRUE)

if (length(batch_files) == 0) {
  stop("❌ No batch files found in:", batch_dir)
}

cat("  Found", length(batch_files), "batch files\n")

# Load first batch
results_seq <- readRDS(batch_files[1])
cat("  Loaded batch 1/", length(batch_files), "\n", sep="")

# Combine remaining batches with aggressive memory management after EVERY batch
if (length(batch_files) > 1) {
  for (i in 2:length(batch_files)) {
    batch_i <- readRDS(batch_files[i])
    results_seq$results <- c(results_seq$results, batch_i$results)
    results_seq$parameter_sets <- rbind(results_seq$parameter_sets, batch_i$parameter_sets)
    rm(batch_i)

    # Full garbage collection after EVERY batch
    gc(full = TRUE)

    # Show memory usage every 10 batches
    if (i %% 10 == 0) {
      cat("  Loaded batch", i, "/", length(batch_files),
          sprintf(" [Memory: %.1f MB]\n", sum(gc()[, 2])))
    } else {
      cat("  Loaded batch", i, "/", length(batch_files), "\n")
    }
  }
}

cat("\n✓ All batches combined successfully\n")
cat("  Total simulations:", length(results_seq$results), "\n\n")

# Load timing information if available
timing_file <- file.path(results_base_dir, "simulation_timing.rds")
if (file.exists(timing_file)) {
  timing_info <- readRDS(timing_file)
  results_seq$runtime_minutes <- timing_info$total_runtime_minutes
  cat("✓ Timing information loaded\n")
  cat("  Total runtime:", round(timing_info$total_runtime_minutes, 2), "minutes\n")
  cat("  Avg per run:", round(timing_info$avg_time_per_run, 2), "seconds\n\n")
} else {
  results_seq$runtime_minutes <- NA
}

# Save complete results
results_file <- file.path(results_base_dir, "ensemble_results_complete.rds")
cat("Saving complete results...\n")
saveRDS(results_seq, file = results_file)
cat("✓ Complete results saved:", results_file, "\n")
cat("  File size:", round(file.info(results_file)$size / 1024^2, 2), "MB\n\n")

# =============================================================================
# STEP 3: EXTRACT PERFORMANCE METRICS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 3: PERFORMANCE METRICS EXTRACTION\n")
cat("=============================================================================\n\n")

cat("Extracting metrics for subbasin", plot_subbasin, "...\n")

# Extract NSE and KGE from COSERO statistics
nse_seq <- extract_cosero_metrics(results_seq, subbasin_id = plot_subbasin,
                                   metric = "NSE", warn_nan = TRUE)
kge_seq <- extract_cosero_metrics(results_seq, subbasin_id = plot_subbasin,
                                   metric = "KGE", warn_nan = FALSE)

# Calculate metrics with spinup (alternative method for verification)
nse_calc <- calculate_ensemble_metrics(results_seq, subbasin_id = plot_subbasin,
                                        metric = "NSE", spinup = 365)
kge_calc <- calculate_ensemble_metrics(results_seq, subbasin_id = plot_subbasin,
                                        metric = "KGE", spinup = 365)

cat("\nMetrics summary:\n")
cat(sprintf("  NSE: %d/%d valid (mean=%.4f, median=%.4f, sd=%.4f)\n",
            sum(!is.na(nse_seq)), length(nse_seq),
            mean(nse_seq, na.rm=TRUE), median(nse_seq, na.rm=TRUE), sd(nse_seq, na.rm=TRUE)))
cat(sprintf("  KGE: %d/%d valid (mean=%.4f, median=%.4f, sd=%.4f)\n\n",
            sum(!is.na(kge_seq)), length(kge_seq),
            mean(kge_seq, na.rm=TRUE), median(kge_seq, na.rm=TRUE), sd(kge_seq, na.rm=TRUE)))

# =============================================================================
# STEP 4: CONVERT TO ABSOLUTE PARAMETER VALUES
# =============================================================================

cat("=============================================================================\n")
cat("STEP 4: PARAMETER VALUE CONVERSION\n")
cat("=============================================================================\n\n")

# Read original parameter values
original_all <- read_parameter_table(param_file, param_names, zone_id = "all")

# Convert relative changes to absolute values
cat("Converting sampled multipliers to absolute parameter values...\n")
absolute_params <- as.data.frame(samples$parameter_sets)

for (param in param_names) {
  original_vals <- original_all[[param]]
  param_bounds <- bounds[bounds$parameter == param, ]
  reference_val <- median(original_vals, na.rm = TRUE)

  if (param_bounds$modification_type == "relchg") {
    # Relative change: absolute = reference × sampled_multiplier
    absolute_params[[param]] <- reference_val * samples$parameter_sets[[param]]
  } else if (param_bounds$modification_type == "abschg") {
    # Absolute change: absolute = reference + sampled_value
    absolute_params[[param]] <- reference_val + samples$parameter_sets[[param]]
  } else {
    # Direct value
    absolute_params[[param]] <- samples$parameter_sets[[param]]
  }

  # Clip to physical bounds
  absolute_params[[param]] <- pmax(param_bounds$min,
                                   pmin(param_bounds$max,
                                        absolute_params[[param]]))
}

cat("✓ Conversion complete\n\n")
cat("Absolute parameter ranges:\n")
for (param in param_names) {
  cat(sprintf("  %-8s: [%.2f, %.2f]\n", param,
              min(absolute_params[[param]]), max(absolute_params[[param]])))
}
cat("\n")

# =============================================================================
# STEP 5: SOBOL SENSITIVITY ANALYSIS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 5: SOBOL SENSITIVITY INDICES\n")
cat("=============================================================================\n\n")

# Calculate Sobol indices for NSE
cat("Calculating Sobol sensitivity indices for NSE...\n")
sobol_indices_nse <- calculate_sobol_indices(nse_seq, samples, boot = TRUE, R = 100)
cat("✓ NSE Sobol indices calculated\n\n")
print(sobol_indices_nse)

# Calculate Sobol indices for KGE
cat("\nCalculating Sobol sensitivity indices for KGE...\n")
sobol_indices_kge <- calculate_sobol_indices(kge_calc, samples, boot = TRUE, R = 100)
cat("✓ KGE Sobol indices calculated\n\n")
print(sobol_indices_kge)

# =============================================================================
# STEP 6: PUBLICATION-READY VISUALIZATIONS
# =============================================================================

cat("\n=============================================================================\n")
cat("STEP 6: PUBLICATION-READY VISUALIZATIONS\n")
cat("=============================================================================\n\n")

plots_dir <- file.path(results_base_dir, "plots_publication")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------
# 6.1: NSE Sobol Indices
# ------------------------
cat("Creating NSE Sobol indices plot...\n")
p_sobol_nse <- plot_sobol(sobol_indices_nse,
                           title = "Sobol Sensitivity Indices (NSE)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

ggsave(
  filename = file.path(plots_dir, "01_sobol_indices_nse.png"),
  plot = p_sobol_nse,
  width = viz_sobol_width, height = viz_sobol_height, dpi = viz_dpi
)
cat("  ✓ Saved: 01_sobol_indices_nse.png\n")

# ------------------------
# 6.2: KGE Sobol Indices
# ------------------------
cat("Creating KGE Sobol indices plot...\n")
p_sobol_kge <- plot_sobol(sobol_indices_kge,
                           title = "Sobol Sensitivity Indices (KGE)") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 13, face = "bold")
  )

ggsave(
  filename = file.path(plots_dir, "02_sobol_indices_kge.png"),
  plot = p_sobol_kge,
  width = viz_sobol_width, height = viz_sobol_height, dpi = viz_dpi
)
cat("  ✓ Saved: 02_sobol_indices_kge.png\n")

# ------------------------
# 6.3: NSE Dotty Plots
# ------------------------
cat("Creating NSE dotty plots...\n")
p_dotty_nse <- plot_dotty(
  parameter_sets = absolute_params,
  Y = nse_seq,
  y_label = "NSE",
  n_col = 3,
  y_min = 0
) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold")
  ) +
  expand_limits(y = 0)  # Ensure y-axis starts at 0

ggsave(
  filename = file.path(plots_dir, "03_dotty_plots_nse.png"),
  plot = p_dotty_nse,
  width = viz_width, height = viz_height, dpi = viz_dpi
)
cat("  ✓ Saved: 03_dotty_plots_nse.png\n")

# ------------------------
# 6.4: KGE Dotty Plots
# ------------------------
cat("Creating KGE dotty plots...\n")
p_dotty_kge <- plot_dotty(
  parameter_sets = absolute_params,
  Y = kge_calc,
  y_label = "KGE",
  n_col = 3,
  y_min = 0
) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(size = 13, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold")
  ) +
  expand_limits(y = 0)  # Ensure y-axis starts at 0

ggsave(
  filename = file.path(plots_dir, "04_dotty_plots_kge.png"),
  plot = p_dotty_kge,
  width = viz_width, height = viz_height, dpi = viz_dpi
)
cat("  ✓ Saved: 04_dotty_plots_kge.png\n")

# ------------------------
# 6.5: Ensemble Uncertainty
# ------------------------
cat("Creating ensemble uncertainty plot...\n")
tryCatch({
  p_uncertainty <- plot_ensemble_uncertainty(
    ensemble_output = results_seq,
    observed = NULL,
    subbasin_id = plot_subbasin,
    variable = "QSIM"
  ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 16, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold")
    )

  ggsave(
    filename = file.path(plots_dir, "05_ensemble_uncertainty.png"),
    plot = p_uncertainty,
    width = 14, height = 8, dpi = viz_dpi
  )
  cat("  ✓ Saved: 05_ensemble_uncertainty.png\n")
}, error = function(e) {
  cat("  ⚠ Could not create uncertainty plot:", e$message, "\n")
})

# ------------------------
# 6.6: Best NSE Simulation Dotty Plot
# ------------------------
cat("Creating best NSE simulation dotty plot...\n")

# Find best NSE simulation
best_nse_idx_plot <- which.max(nse_seq)
best_nse_value_plot <- nse_seq[best_nse_idx_plot]

# Create dotty plot data with highlighting
dotty_nse_data <- absolute_params %>%
  mutate(
    NSE = nse_seq,
    is_best = row_number() == best_nse_idx_plot
  ) %>%
  pivot_longer(cols = all_of(param_names), names_to = "parameter", values_to = "value")

p_best_nse <- ggplot(dotty_nse_data, aes(x = value, y = NSE)) +
  geom_point(aes(color = is_best, size = is_best, alpha = is_best)) +
  scale_color_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#3498DB"),
                     labels = c("TRUE" = sprintf("Best (NSE=%.4f)", best_nse_value_plot),
                               "FALSE" = "Other simulations")) +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 1.5),
                    labels = c("TRUE" = sprintf("Best (NSE=%.4f)", best_nse_value_plot),
                              "FALSE" = "Other simulations")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4),
                     labels = c("TRUE" = sprintf("Best (NSE=%.4f)", best_nse_value_plot),
                               "FALSE" = "Other simulations")) +
  facet_wrap(~ parameter, scales = "free_x", ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Best NSE Simulation - Parameter Sensitivity",
    subtitle = sprintf("Best simulation: #%d (NSE = %.4f, KGE = %.4f)",
                      best_nse_idx_plot, best_nse_value_plot, kge_calc[best_nse_idx_plot]),
    x = "Parameter Value",
    y = "NSE",
    color = NULL,
    size = NULL,
    alpha = NULL
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 13, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  ) +
  expand_limits(y = 0) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

ggsave(
  filename = file.path(plots_dir, "05b_best_nse_dotty.png"),
  plot = p_best_nse,
  width = viz_width, height = viz_height, dpi = viz_dpi
)
cat("  ✓ Saved: 05b_best_nse_dotty.png\n")

# ------------------------
# 6.7: Best KGE Simulation Dotty Plot
# ------------------------
cat("Creating best KGE simulation dotty plot...\n")

# Find best KGE simulation
best_kge_idx_plot <- which.max(kge_calc)
best_kge_value_plot <- kge_calc[best_kge_idx_plot]

# Create dotty plot data with highlighting
dotty_kge_data <- absolute_params %>%
  mutate(
    KGE = kge_calc,
    is_best = row_number() == best_kge_idx_plot
  ) %>%
  pivot_longer(cols = all_of(param_names), names_to = "parameter", values_to = "value")

p_best_kge <- ggplot(dotty_kge_data, aes(x = value, y = KGE)) +
  geom_point(aes(color = is_best, size = is_best, alpha = is_best)) +
  scale_color_manual(values = c("TRUE" = "#E74C3C", "FALSE" = "#27AE60"),
                     labels = c("TRUE" = sprintf("Best (KGE=%.4f)", best_kge_value_plot),
                               "FALSE" = "Other simulations")) +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 1.5),
                    labels = c("TRUE" = sprintf("Best (KGE=%.4f)", best_kge_value_plot),
                              "FALSE" = "Other simulations")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.4),
                     labels = c("TRUE" = sprintf("Best (KGE=%.4f)", best_kge_value_plot),
                               "FALSE" = "Other simulations")) +
  facet_wrap(~ parameter, scales = "free_x", ncol = 3) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Best KGE Simulation - Parameter Sensitivity",
    subtitle = sprintf("Best simulation: #%d (KGE = %.4f, NSE = %.4f)",
                      best_kge_idx_plot, best_kge_value_plot, nse_seq[best_kge_idx_plot]),
    x = "Parameter Value",
    y = "KGE",
    color = NULL,
    size = NULL,
    alpha = NULL
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = 13, face = "bold"),
    strip.background = element_rect(fill = "lightgray", color = "black"),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 11)
  ) +
  expand_limits(y = 0) +
  guides(color = guide_legend(override.aes = list(size = 4, alpha = 1)))

ggsave(
  filename = file.path(plots_dir, "05c_best_kge_dotty.png"),
  plot = p_best_kge,
  width = viz_width, height = viz_height, dpi = viz_dpi
)
cat("  ✓ Saved: 05c_best_kge_dotty.png\n")

# ------------------------
# 6.8: NSE vs KGE Comparison
# ------------------------
cat("Creating NSE vs KGE comparison plot...\n")
comparison_data <- data.frame(
  NSE = nse_seq,
  KGE = kge_calc
) %>% filter(!is.na(NSE) & !is.na(KGE))

p_comparison <- ggplot(comparison_data, aes(x = NSE, y = KGE)) +
  geom_point(alpha = 0.5, size = 2, color = "#2E86AB") +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72", linewidth = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray40", linewidth = 1) +
  theme_minimal(base_size = 14) +
  labs(
    title = "NSE vs KGE Performance Comparison",
    subtitle = sprintf("Correlation: r = %.3f, n = %d simulations",
                      cor(comparison_data$NSE, comparison_data$KGE),
                      nrow(comparison_data)),
    x = "Nash-Sutcliffe Efficiency (NSE)",
    y = "Kling-Gupta Efficiency (KGE)"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 13, hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold")
  ) +
  expand_limits(x = 0, y = 0) +  # Ensure both axes start at 0
  coord_cartesian(xlim = c(0, NA), ylim = c(0, NA))  # Force origin at (0,0)

ggsave(
  filename = file.path(plots_dir, "06_nse_vs_kge_comparison.png"),
  plot = p_comparison,
  width = 10, height = 8, dpi = viz_dpi
)
cat("  ✓ Saved: 06_nse_vs_kge_comparison.png\n")

# ------------------------
# 6.9: Parameter Correlations
# ------------------------
cat("Creating parameter correlation heatmaps...\n")

# NSE correlations
nse_cors <- sapply(param_names, function(p) {
  cor(absolute_params[[p]], nse_seq, use = "complete.obs")
})
nse_cor_data <- data.frame(
  Parameter = param_names,
  Correlation = nse_cors
) %>% arrange(desc(abs(Correlation)))

p_cor_nse <- ggplot(nse_cor_data, aes(x = reorder(Parameter, abs(Correlation)),
                                        y = Correlation)) +
  geom_col(aes(fill = Correlation), width = 0.7) +
  scale_fill_gradient2(low = "#D7263D", mid = "white", high = "#2E86AB",
                       midpoint = 0, limits = c(-1, 1)) +
  coord_flip() +
  theme_minimal(base_size = 14) +
  labs(
    title = "Parameter Correlations with NSE",
    x = "Parameter",
    y = "Pearson Correlation Coefficient"
  ) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    legend.position = "right"
  ) +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30")

ggsave(
  filename = file.path(plots_dir, "07_parameter_correlations_nse.png"),
  plot = p_cor_nse,
  width = 10, height = 7, dpi = viz_dpi
)
cat("  ✓ Saved: 07_parameter_correlations_nse.png\n")

# ------------------------
# 6.10: NSE vs KGE Sobol Comparison
# ------------------------
cat("Creating NSE vs KGE Sobol indices comparison...\n")

# Combine NSE and KGE Sobol indices
sobol_nse_df <- sobol_indices_nse$results %>%
  filter(sensitivity == "Ti") %>%
  select(parameters, original) %>%
  rename(NSE_Total = original)

sobol_kge_df <- sobol_indices_kge$results %>%
  filter(sensitivity == "Ti") %>%
  select(parameters, original) %>%
  rename(KGE_Total = original)

sobol_comparison <- sobol_nse_df %>%
  left_join(sobol_kge_df, by = "parameters") %>%
  pivot_longer(cols = c(NSE_Total, KGE_Total),
               names_to = "Metric",
               values_to = "Sensitivity") %>%
  mutate(Metric = gsub("_Total", "", Metric))

# Calculate average for ordering
param_order <- sobol_comparison %>%
  group_by(parameters) %>%
  summarise(avg_sens = mean(Sensitivity, na.rm = TRUE)) %>%
  arrange(desc(avg_sens)) %>%
  pull(parameters)

sobol_comparison$parameters <- factor(sobol_comparison$parameters, levels = param_order)

p_sobol_comparison <- ggplot(sobol_comparison, aes(x = parameters, y = Sensitivity, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("NSE" = "#3498DB", "KGE" = "#27AE60"),
                    labels = c("NSE" = "NSE (Total-order)", "KGE" = "KGE (Total-order)")) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Sobol Sensitivity Indices Comparison: NSE vs KGE",
    subtitle = "Total-order indices showing parameter importance for each metric",
    x = "Parameter",
    y = "Total-order Sensitivity Index",
    fill = "Metric"
  ) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12)
  ) +
  expand_limits(y = 0) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))

ggsave(
  filename = file.path(plots_dir, "08_sobol_nse_kge_comparison.png"),
  plot = p_sobol_comparison,
  width = 12, height = 8, dpi = viz_dpi
)
cat("  ✓ Saved: 08_sobol_nse_kge_comparison.png\n")

# ------------------------
# 6.11: Best Simulations Hydrographs
# ------------------------
cat("Creating best simulations hydrograph comparison...\n")

tryCatch({
  # Get runoff data for best NSE and best KGE simulations
  best_nse_runoff <- results_seq$results[[best_nse_idx_plot]]$output_data$runoff
  best_kge_runoff <- results_seq$results[[best_kge_idx_plot]]$output_data$runoff

  if (!is.null(best_nse_runoff) && !is.null(best_kge_runoff)) {
    # Construct column names
    qsim_col <- paste0("QSIM_", plot_subbasin)
    qobs_col <- paste0("QOBS_", plot_subbasin)

    # Extract dates
    if ("date" %in% colnames(best_nse_runoff)) {
      dates_nse <- best_nse_runoff$date
    } else {
      dates_nse <- as.Date(paste(best_nse_runoff$yyyy, best_nse_runoff$mm, best_nse_runoff$dd, sep = "-"))
    }

    # Prepare hydrograph data
    hydro_data <- data.frame(
      Date = dates_nse,
      Observed = best_nse_runoff[[qobs_col]],
      Best_NSE = best_nse_runoff[[qsim_col]],
      Best_KGE = best_kge_runoff[[qsim_col]]
    ) %>%
      pivot_longer(cols = c(Observed, Best_NSE, Best_KGE),
                   names_to = "Series",
                   values_to = "Discharge")

    # Create hydrograph plot
    p_hydro <- ggplot(hydro_data, aes(x = Date, y = Discharge, color = Series, linetype = Series)) +
      geom_line(linewidth = 0.8) +
      scale_color_manual(
        values = c("Observed" = "#2C3E50", "Best_NSE" = "#3498DB", "Best_KGE" = "#27AE60"),
        labels = c("Observed" = "Observed",
                   "Best_NSE" = sprintf("Best NSE (%.4f)", best_nse_value_plot),
                   "Best_KGE" = sprintf("Best KGE (%.4f)", best_kge_value_plot))
      ) +
      scale_linetype_manual(
        values = c("Observed" = "solid", "Best_NSE" = "dashed", "Best_KGE" = "dotted"),
        labels = c("Observed" = "Observed",
                   "Best_NSE" = sprintf("Best NSE (%.4f)", best_nse_value_plot),
                   "Best_KGE" = sprintf("Best KGE (%.4f)", best_kge_value_plot))
      ) +
      theme_minimal(base_size = 12) +
      labs(
        title = "Best Simulations Hydrograph Comparison",
        subtitle = sprintf("Subbasin %s: NSE sim #%d vs KGE sim #%d",
                          plot_subbasin, best_nse_idx_plot, best_kge_idx_plot),
        x = "Date",
        y = "Discharge (m³/s)",
        color = "Simulation",
        linetype = "Simulation"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 13, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 11)
      )

    ggsave(
      filename = file.path(plots_dir, "09_best_simulations_hydrographs.png"),
      plot = p_hydro,
      width = 14, height = 8, dpi = viz_dpi
    )
    cat("  ✓ Saved: 09_best_simulations_hydrographs.png\n")
  } else {
    cat("  ⚠ Could not create hydrograph plot: Missing runoff data\n")
  }
}, error = function(e) {
  cat("  ⚠ Could not create hydrograph plot:", e$message, "\n")
})

# ------------------------
# 6.12: Behavioral Parameter Ranges
# ------------------------
cat("Creating behavioral parameter ranges plot...\n")

# Define behavioral threshold (NSE > 0.5 is commonly used)
nse_threshold <- 0.5

# Identify behavioral simulations
behavioral_mask <- nse_seq > nse_threshold & !is.na(nse_seq)
n_behavioral <- sum(behavioral_mask)

cat(sprintf("  Behavioral simulations: %d / %d (NSE > %.2f)\n",
            n_behavioral, length(nse_seq), nse_threshold))

if (n_behavioral > 10) {  # Only create plot if enough behavioral simulations
  behavioral_data <- absolute_params %>%
    mutate(
      NSE = nse_seq,
      Behavioral = ifelse(behavioral_mask, "Behavioral", "Non-behavioral")
    ) %>%
    pivot_longer(cols = all_of(param_names), names_to = "parameter", values_to = "value")

  p_behavioral <- ggplot(behavioral_data, aes(x = value, y = NSE)) +
    geom_point(aes(color = Behavioral, alpha = Behavioral, size = Behavioral)) +
    scale_color_manual(values = c("Behavioral" = "#27AE60", "Non-behavioral" = "#95A5A6")) +
    scale_alpha_manual(values = c("Behavioral" = 0.7, "Non-behavioral" = 0.3)) +
    scale_size_manual(values = c("Behavioral" = 2, "Non-behavioral" = 1)) +
    geom_hline(yintercept = nse_threshold, linetype = "dashed", color = "#E74C3C", linewidth = 1) +
    facet_wrap(~ parameter, scales = "free_x", ncol = 3) +
    theme_minimal(base_size = 12) +
    labs(
      title = "Behavioral vs Non-Behavioral Parameter Ranges",
      subtitle = sprintf("Behavioral: NSE > %.2f (%d / %d simulations)",
                        nse_threshold, n_behavioral, length(nse_seq)),
      x = "Parameter Value",
      y = "NSE",
      color = NULL,
      alpha = NULL,
      size = NULL
    ) +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(size = 13, face = "bold"),
      strip.background = element_rect(fill = "lightgray", color = "black"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 13, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 11)
    ) +
    expand_limits(y = 0)

  ggsave(
    filename = file.path(plots_dir, "10_behavioral_parameter_ranges.png"),
    plot = p_behavioral,
    width = viz_width, height = viz_height, dpi = viz_dpi
  )
  cat("  ✓ Saved: 10_behavioral_parameter_ranges.png\n")
} else {
  cat("  ⚠ Skipped behavioral plot: Not enough behavioral simulations\n")
}

cat("\n✓ All publication-ready visualizations created\n")
cat("  Location:", plots_dir, "\n")
cat("  Total plots: 11-12 (depending on data availability)\n")
cat("  Resolution:", viz_dpi, "DPI\n\n")

# =============================================================================
# STEP 7: IDENTIFY AND EXPORT BEST PARAMETER SETS
# =============================================================================

cat("=============================================================================\n")
cat("STEP 7: BEST PARAMETER SETS IDENTIFICATION AND EXPORT\n")
cat("=============================================================================\n\n")

best_params_dir <- file.path(results_base_dir, "best_parameter_sets")
dir.create(best_params_dir, recursive = TRUE, showWarnings = FALSE)

# Find best NSE simulation
best_nse_idx <- which.max(nse_seq)
best_nse_value <- nse_seq[best_nse_idx]
best_nse_params <- as.list(absolute_params[best_nse_idx, ])

cat("Best NSE simulation:\n")
cat(sprintf("  Index: %d / %d\n", best_nse_idx, length(nse_seq)))
cat(sprintf("  NSE value: %.4f\n", best_nse_value))
cat(sprintf("  KGE value: %.4f\n", kge_calc[best_nse_idx]))
cat("  Parameter values:\n")
for (p in param_names) {
  cat(sprintf("    %-8s = %.4f\n", p, best_nse_params[[p]]))
}
cat("\n")

# Find best KGE simulation
best_kge_idx <- which.max(kge_calc)
best_kge_value <- kge_calc[best_kge_idx]
best_kge_params <- as.list(absolute_params[best_kge_idx, ])

cat("Best KGE simulation:\n")
cat(sprintf("  Index: %d / %d\n", best_kge_idx, length(kge_calc)))
cat(sprintf("  KGE value: %.4f\n", best_kge_value))
cat(sprintf("  NSE value: %.4f\n", nse_seq[best_kge_idx]))
cat("  Parameter values:\n")
for (p in param_names) {
  cat(sprintf("    %-8s = %.4f\n", p, best_kge_params[[p]]))
}
cat("\n")

# Export best NSE parameter file
cat("Exporting best NSE parameter file...\n")
best_nse_file <- file.path(best_params_dir, "para_BEST_NSE.txt")
file.copy(param_file, best_nse_file, overwrite = TRUE)
original_values <- read_parameter_table(param_file, param_names, zone_id = 1)
modify_parameter_table(
  par_file = best_nse_file,
  params = best_nse_params,
  par_bounds = bounds,
  original_values = original_values,
  zones = NULL
)
cat("  ✓ Saved:", best_nse_file, "\n")

# Export best KGE parameter file
cat("Exporting best KGE parameter file...\n")
best_kge_file <- file.path(best_params_dir, "para_BEST_KGE.txt")
file.copy(param_file, best_kge_file, overwrite = TRUE)
original_values <- read_parameter_table(param_file, param_names, zone_id = 1)
modify_parameter_table(
  par_file = best_kge_file,
  params = best_kge_params,
  par_bounds = bounds,
  original_values = original_values,
  zones = NULL
)
cat("  ✓ Saved:", best_kge_file, "\n\n")

# Export top 10 parameter sets
cat("Exporting top 10 parameter sets...\n")
top10_nse_idx <- order(nse_seq, decreasing = TRUE)[1:min(10, length(nse_seq))]
top10_data <- data.frame(
  Rank = 1:length(top10_nse_idx),
  SimulationID = top10_nse_idx,
  NSE = nse_seq[top10_nse_idx],
  KGE = kge_calc[top10_nse_idx],
  absolute_params[top10_nse_idx, ]
)
write.csv(top10_data,
          file.path(best_params_dir, "top10_parameter_sets.csv"),
          row.names = FALSE)
cat("  ✓ Saved: top10_parameter_sets.csv\n\n")

# ------------------------
# 7.1: Parameter Constraints Table
# ------------------------
cat("Creating parameter constraints based on behavioral simulations...\n")

if (n_behavioral > 10) {
  # Calculate parameter ranges for behavioral simulations
  behavioral_params <- absolute_params[behavioral_mask, ]

  param_constraints <- data.frame(
    Parameter = param_names,
    Original_Min = sapply(param_names, function(p) min(absolute_params[[p]], na.rm = TRUE)),
    Original_Max = sapply(param_names, function(p) max(absolute_params[[p]], na.rm = TRUE)),
    Behavioral_Min = sapply(param_names, function(p) min(behavioral_params[[p]], na.rm = TRUE)),
    Behavioral_Max = sapply(param_names, function(p) max(behavioral_params[[p]], na.rm = TRUE)),
    Behavioral_Median = sapply(param_names, function(p) median(behavioral_params[[p]], na.rm = TRUE)),
    Behavioral_Mean = sapply(param_names, function(p) mean(behavioral_params[[p]], na.rm = TRUE)),
    Best_NSE_Value = sapply(param_names, function(p) absolute_params[[p]][best_nse_idx]),
    Best_KGE_Value = sapply(param_names, function(p) absolute_params[[p]][best_kge_idx])
  )

  # Calculate range reduction percentage
  param_constraints$Range_Reduction_Pct <-
    100 * (1 - (param_constraints$Behavioral_Max - param_constraints$Behavioral_Min) /
           (param_constraints$Original_Max - param_constraints$Original_Min))

  # Save to best_parameter_sets directory
  write.csv(param_constraints,
            file.path(best_params_dir, "parameter_constraints_behavioral.csv"),
            row.names = FALSE)
  cat("  ✓ Saved: parameter_constraints_behavioral.csv\n")

  # Print summary to console
  cat("\n  Parameter Constraints Summary (NSE > ", nse_threshold, "):\n", sep = "")
  cat("  ", paste(rep("-", 70), collapse = ""), "\n", sep = "")
  for (i in 1:nrow(param_constraints)) {
    cat(sprintf("  %-8s: [%.2f, %.2f] → [%.2f, %.2f] (%.1f%% reduction)\n",
                param_constraints$Parameter[i],
                param_constraints$Original_Min[i],
                param_constraints$Original_Max[i],
                param_constraints$Behavioral_Min[i],
                param_constraints$Behavioral_Max[i],
                param_constraints$Range_Reduction_Pct[i]))
  }
  cat("\n")
} else {
  cat("  ⚠ Skipped constraints: Not enough behavioral simulations\n")
}

# =============================================================================
# STEP 8: GENERATE ANALYSIS REPORT
# =============================================================================

cat("=============================================================================\n")
cat("STEP 8: ANALYSIS REPORT GENERATION\n")
cat("=============================================================================\n\n")

report_file <- file.path(results_base_dir, "SENSITIVITY_ANALYSIS_REPORT.txt")
sink(report_file)

cat("=============================================================================\n")
cat("COSERO SENSITIVITY ANALYSIS - COMPREHENSIVE REPORT\n")
cat("=============================================================================\n\n")

cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Project:", basename(project_path), "\n\n")

cat("CONFIGURATION\n")
cat("-------------\n")
cat("Base sample size:", n_samples, "\n")
cat("Total simulations:", n_total, "\n")
cat("Parameters analyzed:", paste(param_names, collapse = ", "), "\n")
cat("Target subbasin:", plot_subbasin, "\n")
if (!is.na(results_seq$runtime_minutes)) {
  cat("Runtime:", round(results_seq$runtime_minutes, 2), "minutes\n")
  cat("Avg per simulation:", round(results_seq$runtime_minutes * 60 / n_total, 2), "seconds\n\n")
}

cat("PERFORMANCE METRICS SUMMARY\n")
cat("---------------------------\n")
cat(sprintf("NSE: Mean=%.4f, Median=%.4f, SD=%.4f, Range=[%.4f, %.4f]\n",
            mean(nse_seq, na.rm=TRUE), median(nse_seq, na.rm=TRUE),
            sd(nse_seq, na.rm=TRUE), min(nse_seq, na.rm=TRUE), max(nse_seq, na.rm=TRUE)))
cat(sprintf("KGE: Mean=%.4f, Median=%.4f, SD=%.4f, Range=[%.4f, %.4f]\n",
            mean(kge_calc, na.rm=TRUE), median(kge_calc, na.rm=TRUE),
            sd(kge_calc, na.rm=TRUE), min(kge_calc, na.rm=TRUE), max(kge_calc, na.rm=TRUE)))
cat(sprintf("Correlation (NSE vs KGE): r = %.4f\n\n",
            cor(nse_seq, kge_calc, use = "complete.obs")))

cat("TOP 5 MOST SENSITIVE PARAMETERS (NSE)\n")
cat("--------------------------------------\n")
sobol_nse_sorted <- sobol_indices_nse$results %>%
  filter(sensitivity == "Ti") %>%
  arrange(desc(original)) %>%
  head(5)
for (i in 1:nrow(sobol_nse_sorted)) {
  cat(sprintf("%d. %s: Total-order index = %.4f\n",
              i, sobol_nse_sorted$parameters[i], sobol_nse_sorted$original[i]))
}
cat("\n")

cat("TOP 5 MOST SENSITIVE PARAMETERS (KGE)\n")
cat("--------------------------------------\n")
sobol_kge_sorted <- sobol_indices_kge$results %>%
  filter(sensitivity == "Ti") %>%
  arrange(desc(original)) %>%
  head(5)
for (i in 1:nrow(sobol_kge_sorted)) {
  cat(sprintf("%d. %s: Total-order index = %.4f\n",
              i, sobol_kge_sorted$parameters[i], sobol_kge_sorted$original[i]))
}
cat("\n")

cat("BEST SIMULATIONS\n")
cat("----------------\n")
cat(sprintf("Best NSE: Simulation #%d, NSE=%.4f, KGE=%.4f\n",
            best_nse_idx, best_nse_value, kge_calc[best_nse_idx]))
cat("  Parameter values:\n")
for (p in param_names) {
  cat(sprintf("    %-8s = %.4f\n", p, best_nse_params[[p]]))
}
cat("\n")
cat(sprintf("Best KGE: Simulation #%d, KGE=%.4f, NSE=%.4f\n",
            best_kge_idx, best_kge_value, nse_seq[best_kge_idx]))
cat("  Parameter values:\n")
for (p in param_names) {
  cat(sprintf("    %-8s = %.4f\n", p, best_kge_params[[p]]))
}
cat("\n")

cat("OUTPUT FILES\n")
cat("------------\n")
cat("Results directory:", results_base_dir, "\n")
cat("  - ensemble_results_complete.rds (",
    round(file.info(results_file)$size / 1024^2, 2), " MB)\n", sep="")
cat("  - plots_publication/ (11-12 publication-ready plots,", viz_dpi, "DPI)\n")
cat("  - best_parameter_sets/ (COSERO-ready parameter files + constraints)\n\n")

cat("=============================================================================\n")
cat("Analysis complete. All results saved successfully.\n")
cat("=============================================================================\n")

sink()

cat("✓ Analysis report generated:", report_file, "\n\n")

# =============================================================================
# FINAL SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("WORKFLOW COMPLETE - SUMMARY\n")
cat("=============================================================================\n\n")

cat("✓ Sensitivity analysis completed successfully\n\n")

cat("Key Results:\n")
cat("  • Total simulations:", n_total, "\n")
if (!is.na(results_seq$runtime_minutes)) {
  cat("  • Runtime:", round(results_seq$runtime_minutes, 2), "minutes\n")
}
cat("  • Best NSE:", round(best_nse_value, 4), "(simulation #", best_nse_idx, ")\n")
cat("  • Best KGE:", round(best_kge_value, 4), "(simulation #", best_kge_idx, ")\n\n")

cat("Output Directories:\n")
cat("  📁", results_base_dir, "\n")
cat("     ├── 📊 plots_publication/ (11-12 high-res plots)\n")
cat("     ├── 📄 best_parameter_sets/ (COSERO files + constraints CSV)\n")
cat("     ├── 📄 ensemble_results_complete.rds\n")
cat("     ├── 📄 analysis_configuration.rds\n")
cat("     └── 📄 SENSITIVITY_ANALYSIS_REPORT.txt\n\n")

cat("Next Steps:\n")
cat("  1. Review plots in: plots_publication/\n")
cat("  2. Check analysis report:", basename(report_file), "\n")
cat("  3. Use best parameter files for production runs\n\n")

cat("=============================================================================\n\n")

# Memory cleanup
gc(full = TRUE)
closeAllConnections()

cat("✓ Workflow completed at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
