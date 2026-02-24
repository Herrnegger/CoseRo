# SCE-UA Parameter Optimisation for COSERO
# Uses Shuffled Complex Evolution (SCE-UA) via optimize_cosero_sce()
# Adapt paths, parameters, and settings in the CONFIGURATION section below.

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)
devtools::load_all()

# ── Paths ──────────────────────────────────────────────────────────────────────
cosero_path <- "D:/temp/COSERO_Optim_SCE"   # Project directory (COSERO.exe here)
results_dir <- file.path(cosero_path, "output", "sce_optimisation")

# ── Simulation period ──────────────────────────────────────────────────────────
start_date <- "2014 1 1 0 0"
end_date   <- "2017 12 31 0 0"
spinup     <- 365              # Days to exclude from metric calculation

# ── Target subbasins ───────────────────────────────────────────────────────────
# Zones are mapped automatically from topology.txt (requires prior COSERO run)
target_subbasins  <- c("001", "002", "003")
subbasin_weights  <- c(0.2, 0.2, 0.6)  # Used only with aggregation = "weighted"

# ── Parameters to optimise ─────────────────────────────────────────────────────
# Option A: load from bundled CSV (30 pre-defined parameters)
param_names <- c("CTMIN", "CTMAX", "M", "BETA", "KBF", "TAB1", "H1", "TVS1", "TAB2", "H2", "TVS2", "TAB3")
par_bounds  <- load_parameter_bounds(parameters = param_names)

# Option B: custom bounds (uncomment to use instead)
# par_bounds <- create_optimization_bounds(
#   parameters        = c("BETA", "CTMAX", "M", "TAB1"),
#   lower             = c(1,   2,  20,  1),
#   upper             = c(6,   8, 600, 50),
#   modification_type = rep("relchg", 4)
# )

cat("Parameter bounds:\n")
print(par_bounds[, c("parameter", "description", "min", "default", "max", "modification_type")])

# ── SCE-UA settings ────────────────────────────────────────────────────────────
# optimize_cosero_sce(
#   cosero_path        Path to project directory
#   par_bounds         Parameter bounds data frame (from load_parameter_bounds or create_optimization_bounds)
#   target_subbasins   Subbasin ID(s) to evaluate; zones mapped via topology.txt
#   zones_to_modify    Override automatic zone mapping (NULL = auto from topology.txt)
#   metric             "NSE", "KGE", "RMSE", "PBIAS", or c("NSE","KGE") for multi-objective
#   metric_weights     Weights for multi-objective (e.g. c(0.6, 0.4)); must sum to 1
#   subbasin_weights   Per-subbasin weights; only used with aggregation = "weighted"
#   aggregation        "mean" | "weighted" | "min" | "product"
#   defaults_settings  Named list of COSERO defaults to override (STARTDATE, ENDDATE, SPINUP, ...)
#   maxn               Approximate max function evaluations (soft limit — see gotcha #18)
#   kstop              Number of shuffling loops between convergence checks (default: 10)
#   pcento             Min relative improvement to continue (default: 0.01 = 1%)
#   ngs                Number of complexes; more = better exploration, more evaluations
#   verbose            Print progress (default: TRUE)
#   read_final_outputs Run and read COSERO output after optimisation (default: TRUE)
#   use_minimal_reading Read only runoff file during optimisation for speed (default: TRUE)
# )
#
# Return value:
#   $value             Best objective value (negated metric — negate to get NSE/KGE)
#   $par               Optimal parameter vector
#   $par_bounds        par_bounds with added optimal_value column
#   $runtime_seconds   Wall-clock time
#   $convergence       Convergence message from rtop::sceua
#   $final_run         Full run_cosero() result with optimal parameters (if read_final_outputs)
#   $optimized_par_file Path to saved optimised parameter file
#   $report_file       Path to auto-saved report .txt file

max_evaluations <- 500          # Increase to 2000-10000 for production
n_complexes     <- 3            # ngs: 2-3 typical; more params → more complexes
kstop           <- 10           # Convergence check every N shuffling loops
pcento          <- 0.01         # 1% improvement threshold

# ── defaults_settings passed to run_cosero() ──────────────────────────────────
cosero_defaults <- list(
  STARTDATE  = start_date,
  ENDDATE    = end_date,
  SPINUP     = spinup,
  OUTPUTTYPE = 1        # 1 = basic (fastest); use 2 or 3 for full output
)

# ── Initial run (baseline) ─────────────────────────────────────────────────────
cat("\n--- Initial model run ---\n")
initial_run <- run_cosero(
  project_path      = cosero_path,
  defaults_settings = cosero_defaults,
  statevar_source   = 2,
  quiet             = FALSE
)
initial_nse <- extract_run_metrics(initial_run, target_subbasins, "NSE")
initial_kge <- extract_run_metrics(initial_run, target_subbasins, "KGE")
cat(sprintf("  Initial NSE: %.4f\n", mean(initial_nse)))
cat(sprintf("  Initial KGE: %.4f\n", mean(initial_kge)))
initial_runoff <- initial_run$output_data$runoff

# ── Run SCE-UA optimisation ────────────────────────────────────────────────────
cat("\n--- SCE-UA optimisation ---\n")
set.seed(42)

result_sce <- optimize_cosero_sce(
  cosero_path       = cosero_path,
  par_bounds        = par_bounds,
  target_subbasins  = target_subbasins,
  zones_to_modify   = NULL,          # NULL = auto-mapped from topology.txt
  metric            = "NSE",         # Single objective
  metric_weights    = NULL,          # Only needed for multi-objective
  subbasin_weights  = subbasin_weights,
  aggregation       = "weighted",
  defaults_settings = cosero_defaults,
  maxn              = max_evaluations,
  kstop             = kstop,
  pcento            = pcento,
  ngs               = n_complexes,
  verbose           = TRUE,
  read_final_outputs = TRUE,
  use_minimal_reading = TRUE
)

# ── Print report ───────────────────────────────────────────────────────────────
# Report is always saved to output/ alongside the optimised parameter file.
# Set verbose = TRUE above to also print it to console during optimisation.
cat("\n--- Results summary ---\n")
cat(sprintf("  Initial  NSE : %.4f\n", mean(initial_nse)))
cat(sprintf("  Optimised NSE: %.4f\n", -result_sce$value))
cat(sprintf("  Runtime      : %.1f sec\n", result_sce$runtime_seconds))
cat(sprintf("  Convergence  : %s\n", result_sce$convergence))
cat("  Optimised param file:", result_sce$optimized_par_file, "\n")
cat("  Report file         :", result_sce$report_file, "\n")

cat("\nOptimal parameters:\n")
print(result_sce$par_bounds[, c("parameter", "min", "default", "optimal_value", "max")])

# ── Export CSVs ────────────────────────────────────────────────────────────────
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
export_cosero_optimization(result_sce, results_dir)

# ── Plots ──────────────────────────────────────────────────────────────────────
cat("\n--- Generating plots ---\n")

# -- Parameter comparison (normalised to [0,1]) ---------------------------------
param_df <- result_sce$par_bounds
param_df$initial_norm <- (param_df$default       - param_df$min) / (param_df$max - param_df$min)
param_df$optimal_norm <- (param_df$optimal_value - param_df$min) / (param_df$max - param_df$min)

param_long <- data.frame(
  parameter  = rep(param_df$parameter, 2),
  type       = rep(c("Initial", "Optimal"), each = nrow(param_df)),
  value_norm = c(param_df$initial_norm, param_df$optimal_norm)
)

p_params <- ggplot(param_long, aes(x = parameter, y = value_norm, fill = type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "gray50", alpha = 0.5) +
  scale_fill_manual(values = c("Initial" = "#3498db", "Optimal" = "#e74c3c")) +
  labs(title = "Parameter Comparison: Initial vs Optimal",
       subtitle = "Values normalised to parameter bounds [0, 1]",
       x = NULL, y = "Normalised value", fill = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

ggsave(file.path(results_dir, "parameter_comparison.png"), p_params, width = 10, height = 6, dpi = 150)

# -- Hydrograph, scatter, FDC, monthly NSE (only if final run available) --------
if (!is.null(result_sce$final_run) && !is.null(result_sce$final_run$output_data$runoff)) {

  opt_runoff <- result_sce$final_run$output_data$runoff

  # Use last (highest-weight) subbasin for single-subbasin plots
  sb_id    <- sprintf("%04d", as.numeric(target_subbasins[length(target_subbasins)]))
  qobs_col <- paste0("QOBS_", sb_id)
  qsim_col <- paste0("QSIM_", sb_id)

  if (all(c(qobs_col, qsim_col) %in% colnames(opt_runoff))) {

    idx <- (spinup + 1):nrow(opt_runoff)
    hydro <- data.frame(
      datetime  = opt_runoff$DateTime[idx],
      observed  = opt_runoff[[qobs_col]][idx],
      initial   = initial_runoff[[qsim_col]][idx],
      optimized = opt_runoff[[qsim_col]][idx]
    )

    # Hydrograph
    hydro_long <- tidyr::pivot_longer(hydro, c("observed", "initial", "optimized"),
                                       names_to = "series", values_to = "Q")
    hydro_long$series <- factor(hydro_long$series,
                                 levels = c("observed", "initial", "optimized"),
                                 labels = c("Observed", "Initial", "Optimised"))

    p_hydro <- ggplot(hydro_long, aes(x = datetime, y = Q, color = series)) +
      geom_line(aes(linewidth = series, alpha = series)) +
      scale_color_manual(values = c("Observed" = "black", "Initial" = "#3498db", "Optimised" = "#e74c3c")) +
      scale_linewidth_manual(values = c("Observed" = 0.8, "Initial" = 0.6, "Optimised" = 0.6)) +
      scale_alpha_manual(values = c("Observed" = 1.0, "Initial" = 0.7, "Optimised" = 0.9)) +
      labs(title = sprintf("Hydrograph — Subbasin %s", sb_id),
           subtitle = sprintf("Initial NSE: %.3f  |  Optimised NSE: %.3f",
                              mean(initial_nse), -result_sce$value),
           x = NULL, y = expression("Q [m"^3*"/s]"), color = NULL) +
      theme_bw() + theme(legend.position = "top") +
      guides(linewidth = "none", alpha = "none")

    ggsave(file.path(results_dir, "hydrograph_comparison.png"), p_hydro, width = 14, height = 6, dpi = 150)

    # Scatter: observed vs simulated
    nse_init <- hydroGOF::NSE(hydro$initial,   hydro$observed)
    nse_opt  <- hydroGOF::NSE(hydro$optimized, hydro$observed)

    make_scatter <- function(x, y, title, subtitle, color) {
      ggplot(data.frame(obs = x, sim = y), aes(x = obs, y = sim)) +
        geom_point(alpha = 0.3, color = color, size = 1) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
        labs(title = title, subtitle = subtitle,
             x = expression("Observed [m"^3*"/s]"),
             y = expression("Simulated [m"^3*"/s]")) +
        coord_equal() + theme_bw()
    }

    p_scatter <- patchwork::wrap_plots(
      make_scatter(hydro$observed, hydro$initial,   "Initial",   sprintf("NSE = %.3f", nse_init), "#3498db"),
      make_scatter(hydro$observed, hydro$optimized, "Optimised", sprintf("NSE = %.3f", nse_opt),  "#e74c3c"),
      ncol = 2
    ) + patchwork::plot_annotation(title = "Observed vs Simulated Discharge")

    ggsave(file.path(results_dir, "scatter_comparison.png"), p_scatter, width = 12, height = 6, dpi = 150)

    # Flow duration curve
    calc_fdc <- function(q, label) {
      q_s  <- sort(q, decreasing = TRUE)
      data.frame(exceedance = seq_along(q_s) / (length(q_s) + 1) * 100,
                 Q = q_s, series = label)
    }

    fdc_all <- rbind(calc_fdc(hydro$observed,  "Observed"),
                     calc_fdc(hydro$initial,    "Initial"),
                     calc_fdc(hydro$optimized,  "Optimised"))
    fdc_all$series <- factor(fdc_all$series, levels = c("Observed", "Initial", "Optimised"))

    p_fdc <- ggplot(fdc_all, aes(x = exceedance, y = Q, color = series, linewidth = series)) +
      geom_line() +
      scale_y_log10() +
      scale_color_manual(values = c("Observed" = "black", "Initial" = "#3498db", "Optimised" = "#e74c3c")) +
      scale_linewidth_manual(values = c("Observed" = 1.2, "Initial" = 0.8, "Optimised" = 0.8)) +
      labs(title = "Flow Duration Curve", subtitle = sprintf("Subbasin %s", sb_id),
           x = "Exceedance probability [%]", y = expression("Q [m"^3*"/s]"), color = NULL) +
      theme_bw() + theme(legend.position = "top") + guides(linewidth = "none")

    ggsave(file.path(results_dir, "flow_duration_curve.png"), p_fdc, width = 10, height = 6, dpi = 150)

    # Monthly NSE
    hydro$month <- format(hydro$datetime, "%m")
    hydro$month_name <- factor(format(hydro$datetime, "%b"), levels = month.abb)

    monthly_nse <- hydro %>%
      dplyr::group_by(month, month_name) %>%
      dplyr::summarise(
        Initial   = tryCatch(hydroGOF::NSE(initial,   observed), error = function(e) NA_real_),
        Optimised = tryCatch(hydroGOF::NSE(optimized, observed), error = function(e) NA_real_),
        .groups = "drop"
      ) %>%
      tidyr::pivot_longer(c("Initial", "Optimised"), names_to = "type", values_to = "NSE")
    monthly_nse$type <- factor(monthly_nse$type, levels = c("Initial", "Optimised"))

    p_monthly <- ggplot(monthly_nse, aes(x = month_name, y = NSE, fill = type)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
      geom_hline(yintercept = 0, color = "gray30") +
      scale_fill_manual(values = c("Initial" = "#3498db", "Optimised" = "#e74c3c")) +
      labs(title = "Monthly NSE", subtitle = sprintf("Subbasin %s", sb_id),
           x = NULL, y = "NSE", fill = NULL) +
      theme_bw() + theme(legend.position = "top")

    ggsave(file.path(results_dir, "monthly_nse.png"), p_monthly, width = 10, height = 6, dpi = 150)

    # Combined summary (parameters + FDC top row, hydrograph bottom)
    p_summary <- patchwork::wrap_plots(
      patchwork::wrap_plots(p_params, p_fdc, ncol = 2),
      p_hydro,
      nrow = 2
    ) + patchwork::plot_annotation(
      title    = "SCE-UA Optimisation Summary",
      subtitle = sprintf("Subbasins: %s  |  NSE: %.3f → %.3f",
                         paste(target_subbasins, collapse = ", "),
                         mean(initial_nse), -result_sce$value)
    )

    ggsave(file.path(results_dir, "optimisation_summary.png"), p_summary,
           width = 16, height = 12, dpi = 150)

  } else {
    cat("Warning: runoff columns for subbasin", sb_id, "not found — skipping discharge plots\n")
  }
} else {
  cat("Warning: no final run data — skipping discharge plots\n")
}

cat("\nDone. Results saved to:", results_dir, "\n")
