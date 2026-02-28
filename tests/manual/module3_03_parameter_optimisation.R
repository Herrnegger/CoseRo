# =============================================================================
# Module 3, Script 3: Parameter Optimisation (DDS & SCE-UA)
# =============================================================================
# Demonstrates automatic calibration with DDS and SCE-UA, including
# single- and multi-objective settings, hydrograph diagnostics,
# and algorithm comparison.
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

start_date   <- "2010 1 1 0 0"
end_date     <- "2020 12 31 0 0"
spinup       <- 365

target_subbasins <- c("001", "002", "003")  # subbasins to optimise

# DDS settings
dds_max_iter <- 200     # use 1000–2000 for production

# SCE-UA settings
sce_maxn     <- 500     # max function evaluations (use 2000–5000 for production)
sce_ngs      <- 3       # number of complexes

# Parameters to calibrate (informed by sensitivity analysis, Script 2)
param_names <- c("M", "KBF", "TAB1", "TAB2", "TAB3", "H1", "H2", "TVS1", "TVS2")

# =============================================================================
# 1. SETUP EXAMPLE PROJECT (optional)
# =============================================================================

# Uncomment to create a fresh Wildalpen example project:
# project_path <- "D:/temp/COSERO_Optimisation"
# if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
# setup_cosero_project_example(project_path)

# =============================================================================
# 2. LOAD PARAMETER BOUNDS & INITIAL RUN
# =============================================================================

par_bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameters to optimise:\n")
print(par_bounds[, c("parameter", "min", "max", "default", "modification_type")])

base_settings <- list(
  STARTDATE  = start_date,
  ENDDATE    = end_date,
  SPINUP     = spinup,
  OUTPUTTYPE = 1
)

# Run with initial parameters for baseline comparison
cat("\n--- Initial run ---\n")
result_initial <- run_cosero(
  project_path      = project_path,
  defaults_settings = base_settings,
  statevar_source   = 1,
  quiet             = FALSE
)

# Store initial metrics
initial_metrics <- data.frame()
for (sb in target_subbasins) {
  nse <- extract_run_metrics(result_initial, sb, "NSE")
  kge <- extract_run_metrics(result_initial, sb, "KGE")
  initial_metrics <- rbind(initial_metrics,
                           data.frame(subbasin = sb, NSE = nse, KGE = kge))
}
cat("\nInitial performance:\n")
print(initial_metrics)

# =============================================================================
# 3. DDS OPTIMISATION — SINGLE OBJECTIVE (NSE)
# =============================================================================

cat("\n=== DDS Optimisation (NSE) ===\n")
set.seed(42)

result_dds <- optimize_cosero_dds(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = target_subbasins,
  metric            = "NSE",
  aggregation       = "mean",
  defaults_settings = base_settings,
  max_iter          = dds_max_iter,
  r                 = 0.2,
  verbose           = TRUE,
  read_final_outputs = TRUE
)

cat(sprintf("\nDDS best NSE: %.4f (runtime: %.1f s)\n",
            -result_dds$value, result_dds$runtime_seconds))

# =============================================================================
# 4. DDS — MULTI-OBJECTIVE (NSE + KGE)
# =============================================================================

cat("\n=== DDS Multi-Objective (60%% NSE + 40%% KGE) ===\n")
set.seed(42)

result_dds_multi <- optimize_cosero_dds(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = target_subbasins,
  metric            = c("NSE", "KGE"),
  metric_weights    = c(0.6, 0.4),
  aggregation       = "mean",
  defaults_settings = base_settings,
  max_iter          = dds_max_iter,
  verbose           = TRUE,
  read_final_outputs = FALSE
)

cat(sprintf("DDS multi-objective best: %.4f (runtime: %.1f s)\n",
            -result_dds_multi$value, result_dds_multi$runtime_seconds))

# =============================================================================
# 5. SCE-UA OPTIMISATION
# =============================================================================

cat("\n=== SCE-UA Optimisation (NSE) ===\n")
set.seed(42)

result_sce <- optimize_cosero_sce(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = target_subbasins,
  metric            = "NSE",
  aggregation       = "mean",
  defaults_settings = base_settings,
  maxn              = sce_maxn,
  ngs               = sce_ngs,
  verbose           = TRUE,
  read_final_outputs = TRUE
)

cat(sprintf("SCE-UA best NSE: %.4f (runtime: %.1f s)\n",
            -result_sce$value, result_sce$runtime_seconds))

# =============================================================================
# 6. COMPARE OPTIMAL PARAMETERS
# =============================================================================

cat("\n=== Parameter Comparison ===\n")
param_comparison <- data.frame(
  parameter = par_bounds$parameter,
  min       = par_bounds$min,
  max       = par_bounds$max,
  default   = par_bounds$default,
  DDS       = result_dds$par_bounds$optimal_value,
  DDS_multi = result_dds_multi$par_bounds$optimal_value,
  SCE       = result_sce$par_bounds$optimal_value
)
print(param_comparison, digits = 3)

# =============================================================================
# 7. DIAGNOSTIC PLOTS
# =============================================================================

# Output directory for plots (same as export directory)
plot_dir <- file.path(project_path, "optimisation_results")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
sb_tag <- paste0("NB", paste(target_subbasins, collapse = "_"))  # e.g. "NB001_002_003"

# --- 7a. DDS convergence ---

p_conv <- plot_cosero_optimization(result_dds) +
  labs(title = "DDS Convergence History",
       subtitle = sprintf("Best NSE = %.4f after %d iterations",
                           -result_dds$value, dds_max_iter))
print(p_conv)
ggsave(file.path(plot_dir, paste0("convergence_DDS_", sb_tag, ".png")),
       p_conv, width = 10, height = 6, dpi = 150)

# --- 7b. Parameter comparison (normalised bar chart) ---

param_long <- param_comparison %>%
  mutate(across(c(default, DDS, DDS_multi, SCE),
                ~ (. - min) / (max - min),
                .names = "{.col}_norm")) %>%
  select(parameter, default_norm, DDS_norm, DDS_multi_norm, SCE_norm) %>%
  pivot_longer(-parameter, names_to = "source", values_to = "value_norm") %>%
  mutate(source = gsub("_norm$", "", source),
         source = factor(source, levels = c("default", "DDS", "DDS_multi", "SCE")))

p_params <- ggplot(param_long, aes(x = parameter, y = value_norm, fill = source)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_hline(yintercept = c(0, 1), linetype = "dotted", colour = "grey60") +
  scale_fill_manual(values = c(default = "grey60", DDS = "#2166ac",
                                DDS_multi = "#5aae61", SCE = "#9b59b6"),
                    labels = c("Default", "DDS (NSE)", "DDS (NSE+KGE)", "SCE-UA")) +
  labs(title = "Optimal Parameters — Normalised to Bounds [0, 1]",
       x = NULL, y = "Normalised Value", fill = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")
print(p_params)
ggsave(file.path(plot_dir, paste0("parameter_comparison_", sb_tag, ".png")),
       p_params, width = 10, height = 6, dpi = 150)

# --- 7c. Hydrograph comparison (initial vs DDS vs SCE-UA) ---

# Pick the main subbasin for visualisation
sb_plot <- target_subbasins[length(target_subbasins)]
sb_id   <- sprintf("%04d", as.numeric(sb_plot))
qobs_col <- paste0("QOBS_", sb_id)
qsim_col <- paste0("QSIM_", sb_id)

runoff_ini <- result_initial$output_data$runoff
runoff_dds <- result_dds$final_run$output_data$runoff
runoff_sce <- result_sce$final_run$output_data$runoff

idx <- (spinup + 1):nrow(runoff_ini)

hydro_df <- data.frame(
  date     = runoff_ini$DateTime[idx],
  Observed = runoff_ini[[qobs_col]][idx],
  Initial  = runoff_ini[[qsim_col]][idx],
  DDS      = runoff_dds[[qsim_col]][idx],
  SCE_UA   = runoff_sce[[qsim_col]][idx]
) %>%
  pivot_longer(-date, names_to = "series", values_to = "Q")

hydro_df$series <- factor(hydro_df$series,
                           levels = c("Observed", "Initial", "DDS", "SCE_UA"),
                           labels = c("Observed", "Initial", "DDS", "SCE-UA"))

nse_ini <- extract_run_metrics(result_initial, sb_plot, "NSE")

p_hydro <- ggplot(hydro_df, aes(x = date, y = Q, colour = series)) +
  geom_line(aes(linewidth = series, alpha = series)) +
  scale_colour_manual(values = c(Observed = "black", Initial = "grey60",
                                  DDS = "#2166ac", `SCE-UA` = "#9b59b6")) +
  scale_linewidth_manual(values = c(Observed = 0.8, Initial = 0.4, DDS = 0.5, `SCE-UA` = 0.5)) +
  scale_alpha_manual(values = c(Observed = 1, Initial = 0.6, DDS = 0.85, `SCE-UA` = 0.85)) +
  labs(title = paste("Hydrograph — Subbasin", sb_plot),
       subtitle = sprintf("NSE:  Initial=%.3f  |  DDS=%.3f  |  SCE-UA=%.3f",
                           nse_ini, -result_dds$value, -result_sce$value),
       x = NULL, y = expression(Q~"["*m^3/s*"]"), colour = NULL) +
  theme_bw() +
  theme(legend.position = "top") +
  guides(linewidth = "none", alpha = "none")
print(p_hydro)
ggsave(file.path(plot_dir, paste0("hydrograph_", sb_tag, ".png")),
       p_hydro, width = 14, height = 6, dpi = 150)

# --- 7d. Scatter plots (DDS) ---

scatter_df <- data.frame(
  Observed = runoff_ini[[qobs_col]][idx],
  Initial  = runoff_ini[[qsim_col]][idx],
  DDS      = runoff_dds[[qsim_col]][idx]
)

max_q <- max(scatter_df, na.rm = TRUE)

p_scat_ini <- ggplot(scatter_df, aes(x = Observed, y = Initial)) +
  geom_point(alpha = 0.2, size = 0.6, colour = "grey50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  coord_equal(xlim = c(0, max_q), ylim = c(0, max_q)) +
  labs(title = "Initial", subtitle = sprintf("NSE = %.3f", nse_ini),
       x = expression(Q[obs]), y = expression(Q[sim])) +
  theme_bw()

p_scat_dds <- ggplot(scatter_df, aes(x = Observed, y = DDS)) +
  geom_point(alpha = 0.2, size = 0.6, colour = "#2166ac") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  coord_equal(xlim = c(0, max_q), ylim = c(0, max_q)) +
  labs(title = "DDS Optimised", subtitle = sprintf("NSE = %.3f", -result_dds$value),
       x = expression(Q[obs]), y = expression(Q[sim])) +
  theme_bw()

p_scatter <- patchwork::wrap_plots(p_scat_ini, p_scat_dds, ncol = 2) +
  patchwork::plot_annotation(
    title = paste("Observed vs Simulated — Subbasin", sb_plot)
  )
print(p_scatter)
ggsave(file.path(plot_dir, paste0("scatter_", sb_tag, ".png")),
       p_scatter, width = 12, height = 6, dpi = 150)

# --- 7e. Flow duration curves ---

calc_fdc <- function(q) {
  q_sorted <- sort(q, decreasing = TRUE)
  data.frame(
    exceedance = seq_along(q_sorted) / (length(q_sorted) + 1) * 100,
    Q = q_sorted
  )
}

fdc_all <- bind_rows(
  calc_fdc(scatter_df$Observed) %>% mutate(series = "Observed"),
  calc_fdc(scatter_df$Initial)  %>% mutate(series = "Initial"),
  calc_fdc(scatter_df$DDS)      %>% mutate(series = "DDS"),
  calc_fdc(runoff_sce[[qsim_col]][idx]) %>% mutate(series = "SCE-UA")
)
fdc_all$series <- factor(fdc_all$series,
                          levels = c("Observed", "Initial", "DDS", "SCE-UA"))

p_fdc <- ggplot(fdc_all, aes(x = exceedance, y = Q, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10() +
  scale_colour_manual(values = c(Observed = "black", Initial = "grey60",
                                  DDS = "#2166ac", `SCE-UA` = "#9b59b6")) +
  labs(title = paste("Flow Duration Curve — Subbasin", sb_plot),
       x = "Exceedance Probability [%]",
       y = expression(Q~"["*m^3/s*"]"), colour = NULL) +
  theme_bw() +
  theme(legend.position = "top")
print(p_fdc)
ggsave(file.path(plot_dir, paste0("fdc_", sb_tag, ".png")),
       p_fdc, width = 10, height = 6, dpi = 150)

# --- 7f. Monthly NSE comparison ---

runoff_monthly <- data.frame(
  month    = as.integer(format(runoff_ini$DateTime[idx], "%m")),
  Observed = runoff_ini[[qobs_col]][idx],
  Initial  = runoff_ini[[qsim_col]][idx],
  DDS      = runoff_dds[[qsim_col]][idx],
  SCE_UA   = runoff_sce[[qsim_col]][idx]
)

monthly_nse <- runoff_monthly %>%
  group_by(month) %>%
  summarise(
    Initial = tryCatch(hydroGOF::NSE(Initial, Observed), error = function(e) NA),
    DDS     = tryCatch(hydroGOF::NSE(DDS, Observed), error = function(e) NA),
    SCE_UA  = tryCatch(hydroGOF::NSE(SCE_UA, Observed), error = function(e) NA),
    .groups = "drop"
  ) %>%
  pivot_longer(-month, names_to = "source", values_to = "NSE") %>%
  mutate(source = factor(source, levels = c("Initial", "DDS", "SCE_UA"),
                          labels = c("Initial", "DDS", "SCE-UA")))

p_monthly <- ggplot(monthly_nse, aes(x = factor(month), y = NSE, fill = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, colour = "grey40") +
  scale_fill_manual(values = c(Initial = "grey60", DDS = "#2166ac", `SCE-UA` = "#9b59b6")) +
  labs(title = paste("Monthly NSE — Subbasin", sb_plot),
       x = "Month", y = "NSE", fill = NULL) +
  theme_bw() +
  theme(legend.position = "top")
print(p_monthly)
ggsave(file.path(plot_dir, paste0("monthly_NSE_", sb_tag, ".png")),
       p_monthly, width = 10, height = 6, dpi = 150)

# --- 7g. Algorithm comparison summary ---

algo_summary <- data.frame(
  Algorithm    = c("Initial", "DDS (NSE)", "DDS (NSE+KGE)", "SCE-UA"),
  Objective    = c(NA, -result_dds$value, -result_dds_multi$value, -result_sce$value),
  Runtime_sec  = c(result_initial$runtime_seconds, result_dds$runtime_seconds,
                   result_dds_multi$runtime_seconds, result_sce$runtime_seconds)
)

cat("\n=== Algorithm Comparison ===\n")
print(algo_summary, digits = 4)

p_algo <- ggplot(algo_summary %>% filter(!is.na(Objective)),
                  aes(x = Algorithm, y = Objective, fill = Algorithm)) +
  geom_col(width = 0.5) +
  geom_text(aes(label = sprintf("%.4f", Objective)), vjust = -0.4, size = 4) +
  scale_fill_manual(values = c(`DDS (NSE)` = "#2166ac", `DDS (NSE+KGE)` = "#5aae61",
                                `SCE-UA` = "#9b59b6")) +
  labs(title = "Algorithm Performance Comparison",
       subtitle = sprintf("DDS: %d iter  |  SCE-UA: %d max evals",
                           dds_max_iter, sce_maxn),
       y = "Best Metric Value", x = NULL) +
  theme_bw() +
  theme(legend.position = "none")
print(p_algo)
ggsave(file.path(plot_dir, paste0("algorithm_comparison_", sb_tag, ".png")),
       p_algo, width = 8, height = 6, dpi = 150)

cat("\nPlots saved to:", plot_dir, "\n")

# =============================================================================
# 8. EXPORT RESULTS
# =============================================================================

results_dir <- file.path(project_path, "optimisation_results")

# DDS
export_cosero_optimization(result_dds, file.path(results_dir, "DDS_NSE"))
cat("DDS optimised parameters saved to:", result_dds$optimized_par_file, "\n")

# SCE-UA
export_cosero_optimization(result_sce, file.path(results_dir, "SCE_NSE"))
cat("SCE-UA optimised parameters saved to:", result_sce$optimized_par_file, "\n")

# Summary table
write.csv(param_comparison, file.path(results_dir, "parameter_comparison.csv"),
          row.names = FALSE)

cat("\n--- Script 3 complete ---\n")
cat("Tip: Use launch_cosero_app(project_path) to inspect results interactively.\n")
