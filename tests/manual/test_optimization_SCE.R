# =============================================================================
# COSERO PARAMETER OPTIMIZATION TEST - SCE-UA
# =============================================================================
# Tests SCE-UA (Shuffled Complex Evolution) optimization with the Wildalpen
# example catchment. Includes extended visualization options.
#
# NOTE: SCE-UA does not provide iteration history like DDS does.
# This test includes alternative visualizations suitable for SCE-UA results.
# =============================================================================
devtools::document()
devtools::load_all()

library(ggplot2)
library(patchwork)  # For combining plots
library(dplyr)
library(tidyr)

# =============================================================================
# SETUP PROJECT
# =============================================================================

project_path <- "D:/temp/COSERO_Optim_SCE"

# Create fresh project from example
if (dir.exists(project_path)) {
  unlink(project_path, recursive = TRUE)
}
setup_cosero_project_example(project_path)

cat("Project created at:", project_path, "\n")

# Verify setup
stopifnot(
  file.exists(file.path(project_path, "COSERO.exe")),
  file.exists(file.path(project_path, "input", "para_ini.txt")),
  file.exists(file.path(project_path, "input", "defaults.txt"))
)

# =============================================================================
# CONFIGURATION
# =============================================================================

# Parameters to optimize (runoff-related)
#param_names <- c("BETA", "CTMAX", "M", "TAB1")
# Extended parameter set (uncomment for more comprehensive calibration)
param_names <- c("M", "KBF", "TAB1", "H1", "TVS1", "TAB2", "H2", "TVS2", "TAB3")

# Optimization settings
max_evaluations <- 50  # Increase to 2000-5000 for production
target_subbasin <- c("001", "002", "003")

# Simulation period
start_date <- "2014 1 1 0 0"
end_date <- "2017 12 31 0 0"
spinup <- 365

# =============================================================================
# INITIAL RUN
# =============================================================================

cat("\nRunning initial model...\n")

initial_run <- run_cosero(
  project_path = project_path,
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  quiet = FALSE
)

initial_nse <- extract_run_metrics(initial_run, target_subbasin, "NSE")
initial_kge <- extract_run_metrics(initial_run, target_subbasin, "KGE")

cat(sprintf("  Initial NSE: %.4f\n", initial_nse))
cat(sprintf("  Initial KGE: %.4f\n\n", initial_kge))

# Store initial runoff for comparison
initial_runoff <- initial_run$output_data$runoff

# =============================================================================
# DEFINE PARAMETER BOUNDS (from package CSV)
# =============================================================================

par_bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameter bounds:\n")
print(par_bounds[, c("parameter", "description", "min", "max", "default", "modification_type")])
cat("\n")

# =============================================================================
# RUN SCE-UA OPTIMIZATION
# =============================================================================

cat("Starting SCE-UA optimization...\n")
cat("  Max evaluations:", max_evaluations, "\n")
cat("  Target subbasins:", paste(target_subbasin, collapse = ", "), "\n")
cat("  Number of complexes (ngs): 2\n\n")

set.seed(42)

result_sce <- optimize_cosero_sce(
  cosero_path = project_path,
  par_bounds = par_bounds,
  target_subbasins = target_subbasin,
  metric = "NSE",
  subbasin_weights = c(0.2, 0.2, 0.6),
  aggregation = "weighted",
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  maxn = max_evaluations*0.1,
  kstop = 5,       # Check convergence every 5 shuffling loops
  pcento = 0.01,   # 1% improvement threshold for convergence
  ngs = 3,         # Number of complexes
  verbose = TRUE,
  read_final_outputs = TRUE
)

# =============================================================================
# RESULTS
# =============================================================================

cat("\n=============================================================================\n")
cat("OPTIMIZATION RESULTS (SCE-UA)\n")
cat("=============================================================================\n")

cat("\nOptimal parameters:\n")
print(result_sce$par_bounds[, c("parameter", "min", "default", "optimal_value", "max")])

cat(sprintf("\nInitial NSE: %.4f\n", initial_nse))
cat(sprintf("Optimized NSE: %.4f\n", -result_sce$value))
cat(sprintf("Improvement: %.4f (%.1f%%)\n",
            -result_sce$value - initial_nse,
            100 * (-result_sce$value - initial_nse) / abs(initial_nse)))

cat(sprintf("\nRuntime: %.1f seconds\n", result_sce$runtime_seconds))
cat(sprintf("Convergence: %s\n", if (!is.null(result_sce$convergence)) result_sce$convergence else "N/A"))

# =============================================================================
# EXTENDED VISUALIZATION
# =============================================================================

results_dir <- file.path(project_path, "optimization_results")
dir.create(results_dir, showWarnings = FALSE)

# -----------------------------------------------------------------------------
# 1. Parameter Comparison Plot (Initial vs Optimal)
# -----------------------------------------------------------------------------

param_comparison <- data.frame(
  parameter = result_sce$par_bounds$parameter,
  min = result_sce$par_bounds$min,
  max = result_sce$par_bounds$max,
  initial = result_sce$par_bounds$default,
  optimal = result_sce$par_bounds$optimal_value
)

# Normalize to 0-1 range for comparison
param_comparison$initial_norm <- (param_comparison$initial - param_comparison$min) /
                                  (param_comparison$max - param_comparison$min)
param_comparison$optimal_norm <- (param_comparison$optimal - param_comparison$min) /
                                  (param_comparison$max - param_comparison$min)

# Reshape for plotting
param_long <- data.frame(
  parameter = rep(param_comparison$parameter, 2),
  type = rep(c("Initial", "Optimal"), each = nrow(param_comparison)),
  value_norm = c(param_comparison$initial_norm, param_comparison$optimal_norm),
  value_abs = c(param_comparison$initial, param_comparison$optimal)
)

p_params <- ggplot(param_long, aes(x = parameter, y = value_norm, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_fill_manual(values = c("Initial" = "#3498db", "Optimal" = "#e74c3c")) +
  labs(
    title = "Parameter Comparison: Initial vs Optimal",
    subtitle = "Values normalized to parameter bounds [0, 1]",
    x = "Parameter",
    y = "Normalized Value",
    fill = "State"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

ggsave(file.path(results_dir, "parameter_comparison.png"), p_params, width = 10, height = 6, dpi = 150)
cat("\nParameter comparison plot saved\n")

# -----------------------------------------------------------------------------
# 2. Parameter Table Plot
# -----------------------------------------------------------------------------

# Create a visual table of parameter values
param_table_df <- result_sce$par_bounds[, c("parameter", "min", "default", "optimal_value", "max")]
param_table_df$change <- param_table_df$optimal_value - param_table_df$default
param_table_df$change_pct <- 100 * param_table_df$change / param_table_df$default

# Format for display
param_display <- data.frame(
  Parameter = param_table_df$parameter,
  Min = sprintf("%.2f", param_table_df$min),
  Default = sprintf("%.2f", param_table_df$default),
  Optimal = sprintf("%.2f", param_table_df$optimal_value),
  Max = sprintf("%.2f", param_table_df$max),
  Change = sprintf("%+.2f (%.1f%%)", param_table_df$change, param_table_df$change_pct)
)

# Plot as table using ggplot
p_table <- ggplot() +
  annotate("text", x = 0.5, y = 0.9, label = "SCE-UA Optimization Results",
           size = 6, fontface = "bold") +
  annotate("text", x = 0.5, y = 0.8,
           label = sprintf("Final NSE: %.4f | Runtime: %.1f sec",
                          -result_sce$value, result_sce$runtime_seconds),
           size = 4) +
  xlim(0, 1) + ylim(0, 1) +
  theme_void()

# -----------------------------------------------------------------------------
# 3. Hydrograph Comparison (if final run data available)
# -----------------------------------------------------------------------------

if (!is.null(result_sce$final_run) && !is.null(result_sce$final_run$output_data$runoff)) {

  optimized_runoff <- result_sce$final_run$output_data$runoff

  # Get column names for the first target subbasin
  sb_id <- sprintf("%04d", as.numeric(target_subbasin[length(target_subbasin)]))  # Use last (highest weight)
  qobs_col <- paste0("QOBS_", sb_id)
  qsim_col <- paste0("QSIM_", sb_id)

  if (qobs_col %in% colnames(optimized_runoff) && qsim_col %in% colnames(optimized_runoff)) {

    # Prepare data for plotting (skip spinup period)
    n_total <- nrow(optimized_runoff)
    idx_plot <- (spinup + 1):n_total

    hydro_data <- data.frame(
      datetime = optimized_runoff$DateTime[idx_plot],
      observed = optimized_runoff[[qobs_col]][idx_plot],
      initial = initial_runoff[[qsim_col]][idx_plot],
      optimized = optimized_runoff[[qsim_col]][idx_plot]
    )

    # Reshape for plotting
    hydro_long <- tidyr::pivot_longer(
      hydro_data,
      cols = c("observed", "initial", "optimized"),
      names_to = "series",
      values_to = "discharge"
    )
    hydro_long$series <- factor(hydro_long$series,
                                 levels = c("observed", "initial", "optimized"),
                                 labels = c("Observed", "Initial", "Optimized"))

    p_hydro <- ggplot(hydro_long, aes(x = datetime, y = discharge, color = series)) +
      geom_line(aes(linewidth = series, alpha = series)) +
      scale_color_manual(values = c("Observed" = "black", "Initial" = "#3498db", "Optimized" = "#e74c3c")) +
      scale_linewidth_manual(values = c("Observed" = 0.8, "Initial" = 0.6, "Optimized" = 0.6)) +
      scale_alpha_manual(values = c("Observed" = 1, "Initial" = 0.7, "Optimized" = 0.9)) +
      labs(
        title = paste("Hydrograph Comparison - Subbasin", sb_id),
        subtitle = sprintf("Initial NSE: %.3f | Optimized NSE: %.3f", initial_nse, -result_sce$value),
        x = "Date",
        y = expression("Discharge [m"^3*"/s]"),
        color = "Series",
        linewidth = "Series",
        alpha = "Series"
      ) +
      theme_bw() +
      theme(legend.position = "top") +
      guides(linewidth = "none", alpha = "none")

    ggsave(file.path(results_dir, "hydrograph_comparison.png"), p_hydro, width = 14, height = 6, dpi = 150)
    cat("Hydrograph comparison plot saved\n")

    # -----------------------------------------------------------------------------
    # 4. Scatter Plot (Observed vs Simulated)
    # -----------------------------------------------------------------------------

    scatter_data <- data.frame(
      observed = hydro_data$observed,
      initial = hydro_data$initial,
      optimized = hydro_data$optimized
    )

    # Calculate metrics for both
    nse_initial <- hydroGOF::NSE(scatter_data$initial, scatter_data$observed)
    nse_optimized <- hydroGOF::NSE(scatter_data$optimized, scatter_data$observed)

    p_scatter_init <- ggplot(scatter_data, aes(x = observed, y = initial)) +
      geom_point(alpha = 0.3, color = "#3498db", size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Initial Parameters",
        subtitle = sprintf("NSE = %.3f", nse_initial),
        x = expression("Observed [m"^3*"/s]"),
        y = expression("Simulated [m"^3*"/s]")
      ) +
      coord_equal() +
      theme_bw()

    p_scatter_opt <- ggplot(scatter_data, aes(x = observed, y = optimized)) +
      geom_point(alpha = 0.3, color = "#e74c3c", size = 1) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Optimized Parameters",
        subtitle = sprintf("NSE = %.3f", nse_optimized),
        x = expression("Observed [m"^3*"/s]"),
        y = expression("Simulated [m"^3*"/s]")
      ) +
      coord_equal() +
      theme_bw()

    p_scatter_combined <- patchwork::wrap_plots(p_scatter_init, p_scatter_opt, ncol = 2) +
      patchwork::plot_annotation(title = "Observed vs Simulated Discharge",
                                 theme = theme(plot.title = element_text(size = 14, face = "bold")))

    ggsave(file.path(results_dir, "scatter_comparison.png"), p_scatter_combined, width = 12, height = 6, dpi = 150)
    cat("Scatter comparison plot saved\n")

    # -----------------------------------------------------------------------------
    # 5. Flow Duration Curve
    # -----------------------------------------------------------------------------

    # Calculate exceedance probabilities
    calc_fdc <- function(q) {
      q_sorted <- sort(q, decreasing = TRUE)
      n <- length(q_sorted)
      prob <- (1:n) / (n + 1) * 100
      data.frame(exceedance = prob, discharge = q_sorted)
    }

    fdc_obs <- calc_fdc(hydro_data$observed)
    fdc_obs$series <- "Observed"
    fdc_init <- calc_fdc(hydro_data$initial)
    fdc_init$series <- "Initial"
    fdc_opt <- calc_fdc(hydro_data$optimized)
    fdc_opt$series <- "Optimized"

    fdc_all <- rbind(fdc_obs, fdc_init, fdc_opt)
    fdc_all$series <- factor(fdc_all$series, levels = c("Observed", "Initial", "Optimized"))

    p_fdc <- ggplot(fdc_all, aes(x = exceedance, y = discharge, color = series)) +
      geom_line(aes(linewidth = series)) +
      scale_y_log10() +
      scale_color_manual(values = c("Observed" = "black", "Initial" = "#3498db", "Optimized" = "#e74c3c")) +
      scale_linewidth_manual(values = c("Observed" = 1.2, "Initial" = 0.8, "Optimized" = 0.8)) +
      labs(
        title = "Flow Duration Curve",
        subtitle = paste("Subbasin", sb_id),
        x = "Exceedance Probability [%]",
        y = expression("Discharge [m"^3*"/s]"),
        color = "Series"
      ) +
      theme_bw() +
      theme(legend.position = "top") +
      guides(linewidth = "none")

    ggsave(file.path(results_dir, "flow_duration_curve.png"), p_fdc, width = 10, height = 6, dpi = 150)
    cat("Flow duration curve saved\n")

    # -----------------------------------------------------------------------------
    # 6. Monthly Performance Comparison
    # -----------------------------------------------------------------------------

    hydro_data$month <- format(hydro_data$datetime, "%m")
    hydro_data$month_name <- format(hydro_data$datetime, "%b")

    # Calculate monthly NSE
    monthly_nse <- hydro_data %>%
      dplyr::group_by(month, month_name) %>%
      dplyr::summarize(
        nse_initial = tryCatch(hydroGOF::NSE(initial, observed), error = function(e) NA),
        nse_optimized = tryCatch(hydroGOF::NSE(optimized, observed), error = function(e) NA),
        .groups = "drop"
      )

    monthly_long <- tidyr::pivot_longer(
      monthly_nse,
      cols = c("nse_initial", "nse_optimized"),
      names_to = "type",
      values_to = "nse"
    )
    monthly_long$type <- factor(monthly_long$type,
                                 levels = c("nse_initial", "nse_optimized"),
                                 labels = c("Initial", "Optimized"))
    monthly_long$month_name <- factor(monthly_long$month_name,
                                       levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

    p_monthly <- ggplot(monthly_long, aes(x = month_name, y = nse, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      geom_hline(yintercept = 0, linetype = "solid", color = "gray30") +
      scale_fill_manual(values = c("Initial" = "#3498db", "Optimized" = "#e74c3c")) +
      labs(
        title = "Monthly NSE Performance",
        subtitle = paste("Subbasin", sb_id),
        x = "Month",
        y = "NSE",
        fill = "Parameters"
      ) +
      theme_bw() +
      theme(legend.position = "top")

    ggsave(file.path(results_dir, "monthly_performance.png"), p_monthly, width = 10, height = 6, dpi = 150)
    cat("Monthly performance plot saved\n")

  } else {
    cat("Warning: Could not find runoff columns for subbasin", sb_id, "\n")
  }
} else {
  cat("Warning: No final run data available for hydrograph plots\n")
}

# -----------------------------------------------------------------------------
# 7. Combined Summary Plot
# -----------------------------------------------------------------------------

# Create a summary combining key plots
if (exists("p_hydro") && exists("p_params")) {
  p_summary <- patchwork::wrap_plots(
    patchwork::wrap_plots(p_params, p_fdc, ncol = 2),
    p_hydro,
    nrow = 2
  ) +
    patchwork::plot_annotation(
      title = "SCE-UA Optimization Summary",
      subtitle = sprintf("Subbasins: %s | NSE improvement: %.3f -> %.3f",
                        paste(target_subbasin, collapse = ", "),
                        initial_nse, -result_sce$value),
      theme = theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12)
      )
    )

  ggsave(file.path(results_dir, "optimization_summary.png"), p_summary, width = 16, height = 12, dpi = 150)
  cat("Combined summary plot saved\n")
}

# Export results
export_cosero_optimization(result_sce, results_dir)

# Save optimal parameter file

# =============================================================================
# MULTI-OBJECTIVE TEST (NSE + KGE)
# =============================================================================

cat("\n=============================================================================\n")
cat("MULTI-OBJECTIVE OPTIMIZATION (NSE 60% + KGE 40%)\n")
cat("=============================================================================\n")

set.seed(123)

result_multi <- optimize_cosero_sce(
  cosero_path = project_path,
  par_bounds = par_bounds,
  target_subbasins = target_subbasin,
  metric = c("NSE", "KGE"),
  metric_weights = c(0.6, 0.4),
  aggregation = "weighted",
  subbasin_weights = c(0.2, 0.2, 0.6),
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  maxn = max_evaluations,
  ngs = 2,
  verbose = TRUE
)

cat("\nMulti-objective optimal parameters:\n")
print(result_multi$par_bounds[, c("parameter", "optimal_value")])
cat(sprintf("\nCombined metric: %.4f\n", -result_multi$value))

# =============================================================================
# COMPARISON: SCE-UA vs DDS
# =============================================================================

cat("\n=============================================================================\n")
cat("ALGORITHM COMPARISON: SCE-UA vs DDS\n")
cat("=============================================================================\n")

# Run DDS with similar computational budget
# DDS iterations roughly equal to SCE evaluations / (2 * ngs * n_params)
dds_iter <- round(max_evaluations / 2)

set.seed(42)

result_dds <- optimize_cosero_dds(
  cosero_path = project_path,
  par_bounds = par_bounds,
  target_subbasins = target_subbasin,
  metric = "NSE",
  aggregation = "weighted",
  subbasin_weights = c(0.2, 0.2, 0.6),
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  max_iter = dds_iter,
  verbose = TRUE,
  read_final_outputs = FALSE
)

cat("\n--- Algorithm Comparison ---\n")
cat(sprintf("SCE-UA:  NSE = %.4f  (%.1f sec, %d evaluations)\n",
            -result_sce$value, result_sce$runtime_seconds, max_evaluations))
cat(sprintf("DDS:     NSE = %.4f  (%.1f sec, %d iterations)\n",
            -result_dds$value, result_dds$runtime_seconds, dds_iter))

# Comparison plot
comparison_df <- data.frame(
  algorithm = c("SCE-UA", "DDS"),
  nse = c(-result_sce$value, -result_dds$value),
  runtime = c(result_sce$runtime_seconds, result_dds$runtime_seconds)
)

p_algo_compare <- ggplot(comparison_df, aes(x = algorithm, y = nse, fill = algorithm)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = sprintf("%.4f", nse)), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("SCE-UA" = "#9b59b6", "DDS" = "#2ecc71")) +
  labs(
    title = "Algorithm Comparison: SCE-UA vs DDS",
    subtitle = sprintf("SCE: %d evals | DDS: %d iters", max_evaluations, dds_iter),
    x = "Algorithm",
    y = "NSE"
  ) +
  ylim(0, 1) +
  theme_bw() +
  theme(legend.position = "none")

ggsave(file.path(results_dir, "algorithm_comparison.png"), p_algo_compare, width = 8, height = 6, dpi = 150)

# DDS convergence plot (SCE doesn't have history)
if (!is.null(result_dds$history)) {
  p_dds_conv <- plot_cosero_optimization(result_dds) +
    labs(title = "DDS Convergence History")
  ggsave(file.path(results_dir, "dds_convergence.png"), p_dds_conv, width = 10, height = 6, dpi = 150)
}

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST COMPLETE\n")
cat("=============================================================================\n")
cat("Project:", project_path, "\n")
cat("Results:", results_dir, "\n")
cat("\nGenerated files:\n")
cat("  - optimal_parameters.csv\n")
cat("  - optimization_summary.csv\n")
cat("  - parameter_comparison.png\n")
cat("  - hydrograph_comparison.png\n")
cat("  - scatter_comparison.png\n")
cat("  - flow_duration_curve.png\n")
cat("  - monthly_performance.png\n")
cat("  - optimization_summary.png\n")
cat("  - algorithm_comparison.png\n")
cat("  - dds_convergence.png\n")
cat("  - para_optimized_SCE_NB1_2_3_NSE.txt\n")

gc(full = TRUE)
