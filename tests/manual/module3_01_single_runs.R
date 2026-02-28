# =============================================================================
# Module 3, Script 1: Single COSERO Runs & Manual Parameter Exploration
# =============================================================================
# Demonstrates running COSERO with different configurations, manually changing
# parameters, comparing results, and visualising model performance.
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

project_path <- "C:/COSERO/MyCatchment"   # <-- adjust to your project

start_date <- "2010 1 1 0 0"
end_date   <- "2020 12 31 0 0"
spinup     <- 365
subbasins  <- c("001", "002", "003")       # <-- adjust to your catchment

# =============================================================================
# 1. SETUP EXAMPLE PROJECT (optional — skip if you have your own)
# =============================================================================

# Uncomment to create a ready-to-run Wildalpen example:
# project_path <- "D:/temp/COSERO_Module3"
# if (dir.exists(project_path)) unlink(project_path, recursive = TRUE)
# setup_cosero_project_example(project_path)

# =============================================================================
# 2. INSPECT CONFIGURATION
# =============================================================================

# Show all configurable COSERO defaults
show_cosero_defaults()

# Read current defaults.txt
defaults <- read_defaults(file.path(project_path, "input", "defaults.txt"))
cat("Current STARTDATE:", defaults$STARTDATE, "\n")
cat("Current PARAFILE :", defaults$PARAFILE, "\n")

# Read current parameter values for selected parameters
par_file <- file.path(project_path, "input", defaults$PARAFILE)
params_of_interest <- c("BETA", "M", "KBF", "TAB1", "H1", "TVS1")
current_params <- read_parameter_table(par_file, params_of_interest, zone_id = "all")
print(current_params)

# =============================================================================
# 3. BASELINE RUN
# =============================================================================

base_settings <- list(
  STARTDATE  = start_date,
  ENDDATE    = end_date,
  SPINUP     = spinup,
  OUTPUTTYPE = 1
)

cat("\n--- Running baseline ---\n")
result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = base_settings,
  statevar_source   = 1,   # cold start
  quiet             = FALSE
)

# Extract metrics for all subbasins
for (sb in subbasins) {
  nse <- extract_run_metrics(result_base, sb, "NSE")
  kge <- extract_run_metrics(result_base, sb, "KGE")
  cat(sprintf("  Subbasin %s:  NSE = %.4f  |  KGE = %.4f\n", sb, nse, kge))
}

# =============================================================================
# 4. MODIFY PARAMETERS AND RE-RUN
# =============================================================================

# modify_parameter_table() uses "relchg" or "abschg" (set in parameter_bounds.csv):
#
# relchg (relative change) — preserves spatial patterns:
#   factor = target_value / spatial_mean(original)
#   new_zone_i = original_zone_i × factor
#   Example: 3 zones with BETA = [3, 4.5, 6], spatial mean = 4.5
#            target = 6 → factor = 6/4.5 = 1.333
#            result: [4.0, 6.0, 8.0] — ratios between zones are preserved
#
# abschg (absolute change) — preserves spatial differences:
#   offset = target_value − spatial_mean(original)
#   new_zone_i = original_zone_i + offset
#   Example: TCOR = [0, 0.5, 1.0], spatial mean = 0.5
#            target = 2.0 → offset = 1.5
#            result: [1.5, 2.0, 2.5] — absolute differences preserved
#
# All 6 parameters use "relchg" (see parameter_bounds.csv).

par_bounds_mod <- load_parameter_bounds(parameters = params_of_interest)

# Read original spatial values (all zones)
original_values <- read_parameter_table(par_file, params_of_interest, zone_id = "all")
cat("\nOriginal parameter values per zone:\n")
print(original_values)
cat("\nSpatial means:\n")
for (p in params_of_interest) {
  cat(sprintf("  %s = %.2f\n", p, mean(original_values[[p]])))
}

# Copy original file, then modify
par_file_mod <- file.path(project_path, "input", "para_mod.txt")
file.copy(par_file, par_file_mod, overwrite = TRUE)

# Target: shift spatial means (spatial pattern preserved via relchg)
# This modifies ALL zones:
new_params <- list(BETA = 6.0, M = 150, KBF = 5000, TAB1 = 10, H1 = 5, TVS1 = 50)
modify_parameter_table(par_file_mod, new_params, par_bounds_mod, original_values,
                       add_timestamp = TRUE)

# To modify only zones belonging to specific subbasins, use the zones argument.
# get_zones_for_subbasins() reads the parameter file (NB_ and NZ_ columns) to
# map subbasin IDs → zone IDs. No prior COSERO run is needed.
#
# zone_map <- get_zones_for_subbasins(project_path, subbasins = "003")
# cat("Zones in subbasin 003:", paste(zone_map$zones, collapse = ", "), "\n")
#
# modify_parameter_table(par_file_mod, new_params, par_bounds_mod, original_values,
#                        zones = zone_map$zones, add_timestamp = TRUE)

# Verify: read back and compare
modified_values <- read_parameter_table(par_file_mod, params_of_interest, zone_id = "all")
cat("\nModified parameter values per zone:\n")
print(modified_values)
cat("\nNew spatial means:\n")
for (p in params_of_interest) {
  cat(sprintf("  %s = %.2f\n", p, mean(modified_values[[p]])))
}

# Run with modified parameters
cat("\n--- Running with modified parameters ---\n")
result_mod <- run_cosero(
  project_path      = project_path,
  defaults_settings = c(base_settings, list(PARAFILE = "para_mod.txt")),
  statevar_source   = 1,
  quiet             = FALSE
)

for (sb in subbasins) {
  nse <- extract_run_metrics(result_mod, sb, "NSE")
  kge <- extract_run_metrics(result_mod, sb, "KGE")
  cat(sprintf("  Subbasin %s:  NSE = %.4f  |  KGE = %.4f\n", sb, nse, kge))
}

# =============================================================================
# 5. COMPARE MULTIPLE PARAMETER SCENARIOS
# =============================================================================

# Define scenarios as named lists of target spatial-mean values.
# Only the parameters listed are changed; all others stay at their original values.
scenarios <- list(
  "base"           = NULL,                                    # no modification
  "low_storage"    = list(M = 100),                           # less soil storage
  "wet_buffered"   = list(M = 500, TAB3 = 5000),             # more storage + slow baseflow
  "fast_response"  = list(TAB1 = 5, H1 = 1, TVS1 = 20),     # faster surface flow
  "precip_plus20"  = list(PCOR = 1.2),                        # 20% precipitation
  "flashy"         = list(BETA = 8, M = 100, TAB1 = 5)       # combined: flashy runoff
)

scenario_params <- c("BETA", "M", "TAB3", "TAB1", "H1", "TVS1", "PCOR")
par_bounds_all  <- load_parameter_bounds(parameters = scenario_params)
orig_vals       <- read_parameter_table(par_file, scenario_params, zone_id = "all")

# Run each scenario and store results
scenario_metrics <- data.frame()
scenario_results <- list()          # keep run results for detailed plots

for (name in names(scenarios)) {
  cat("\n--- Scenario:", name, "---\n")

  if (is.null(scenarios[[name]])) {
    # Baseline — use original parameter file
    run_settings <- base_settings
  } else {
    # Copy original, modify, set PARAFILE
    tmp_file <- file.path(project_path, "input", paste0("para_", name, ".txt"))
    file.copy(par_file, tmp_file, overwrite = TRUE)
    modify_parameter_table(tmp_file, scenarios[[name]], par_bounds_all, orig_vals)
    run_settings <- c(base_settings, list(PARAFILE = paste0("para_", name, ".txt")))
  }

  res <- run_cosero(
    project_path      = project_path,
    defaults_settings = run_settings,
    statevar_source   = 1,
    quiet             = TRUE
  )

  scenario_results[[name]] <- res

  # Collect metrics per subbasin
  for (sb in subbasins) {
    nse <- extract_run_metrics(res, sb, "NSE")
    kge <- extract_run_metrics(res, sb, "KGE")
    scenario_metrics <- rbind(scenario_metrics, data.frame(
      scenario = name, subbasin = sb, NSE = nse, KGE = kge, stringsAsFactors = FALSE
    ))
  }
}

# Restore defaults.txt to the original parameter file
# (the loop may have overwritten PARAFILE with a scenario-specific file)
modify_defaults(
  file.path(project_path, "input", "defaults.txt"),
  list(PARAFILE = defaults$PARAFILE),
  quiet = TRUE
)
cat("\nRestored defaults.txt PARAFILE to:", defaults$PARAFILE, "\n")

print(scenario_metrics)

# =============================================================================
# 6. DIAGNOSTIC PLOTS
# =============================================================================

# Output directory for plots
plot_dir <- file.path(project_path, "single_run_results")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# --- 6a. Scenario performance comparison (bar chart) ---

metrics_long <- scenario_metrics %>%
  pivot_longer(cols = c(NSE, KGE), names_to = "metric", values_to = "value")

p_scenario <- ggplot(metrics_long, aes(x = scenario, y = value, fill = metric)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  facet_wrap(~subbasin, labeller = labeller(subbasin = function(x) paste("Subbasin", x))) +
  geom_hline(yintercept = 0, color = "grey40") +
  scale_fill_manual(values = c(NSE = "#2166ac", KGE = "#b2182b")) +
  labs(title = "Model Performance Across Parameter Scenarios",
       x = NULL, y = "Metric Value", fill = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 35, hjust = 1),
        legend.position = "top")
print(p_scenario)
ggsave(file.path(plot_dir, "scenario_performance.png"),
       p_scenario, width = 12, height = 6, dpi = 150)

# --- 6b–6e: Detailed comparison of baseline vs one selected scenario ---
# Pick the scenario to zoom into (change this to explore different ones)
compare_scenario <- "flashy"

cat(sprintf("\n--- Detailed plots: 'base' vs '%s' ---\n", compare_scenario))
cat("Modified parameters:",
    paste(names(scenarios[[compare_scenario]]), "=",
          scenarios[[compare_scenario]], collapse = ", "), "\n")

# Subbasin for plotting
sb_plot <- subbasins[length(subbasins)]
sb_id   <- sprintf("%04d", as.numeric(sb_plot))
qobs_col <- paste0("QOBS_", sb_id)
qsim_col <- paste0("QSIM_", sb_id)

runoff_base <- scenario_results[["base"]]$output_data$runoff
runoff_scen <- scenario_results[[compare_scenario]]$output_data$runoff

nse_base  <- extract_run_metrics(scenario_results[["base"]], sb_plot, "NSE")
nse_scen  <- extract_run_metrics(scenario_results[[compare_scenario]], sb_plot, "NSE")

# Skip spinup
idx <- (spinup + 1):nrow(runoff_base)

# --- 6b. Hydrograph ---

hydro_df <- data.frame(
  date     = runoff_base$DateTime[idx],
  Observed = runoff_base[[qobs_col]][idx],
  Baseline = runoff_base[[qsim_col]][idx],
  Scenario = runoff_scen[[qsim_col]][idx]
) %>%
  pivot_longer(-date, names_to = "series", values_to = "Q")

hydro_df$series <- factor(hydro_df$series,
                           levels = c("Observed", "Baseline", "Scenario"),
                           labels = c("Observed", "Baseline", compare_scenario))

p_hydro <- ggplot(hydro_df, aes(x = date, y = Q, colour = series)) +
  geom_line(aes(linewidth = series, alpha = series)) +
  scale_colour_manual(values = setNames(c("black", "#2166ac", "#d6604d"),
                                         c("Observed", "Baseline", compare_scenario))) +
  scale_linewidth_manual(values = setNames(c(0.8, 0.5, 0.5),
                                            c("Observed", "Baseline", compare_scenario))) +
  scale_alpha_manual(values = setNames(c(1, 0.8, 0.8),
                                        c("Observed", "Baseline", compare_scenario))) +
  labs(title = paste("Hydrograph — Subbasin", sb_plot),
       subtitle = sprintf("Baseline NSE=%.3f  |  '%s' NSE=%.3f",
                           nse_base, compare_scenario, nse_scen),
       x = NULL, y = expression(Q~"["*m^3/s*"]"), colour = NULL) +
  theme_bw() +
  theme(legend.position = "top") +
  guides(linewidth = "none", alpha = "none")
print(p_hydro)
ggsave(file.path(plot_dir, paste0("hydrograph_NB", sb_plot, "_", compare_scenario, ".png")),
       p_hydro, width = 14, height = 6, dpi = 150)

# --- 6c. Scatter plots ---

scatter_df <- data.frame(
  Observed = runoff_base[[qobs_col]][idx],
  Baseline = runoff_base[[qsim_col]][idx],
  Scenario = runoff_scen[[qsim_col]][idx]
)

max_q <- max(scatter_df, na.rm = TRUE)

p_scat1 <- ggplot(scatter_df, aes(x = Observed, y = Baseline)) +
  geom_point(alpha = 0.25, size = 0.8, colour = "#2166ac") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  coord_equal(xlim = c(0, max_q), ylim = c(0, max_q)) +
  labs(title = "Baseline",
       subtitle = sprintf("NSE = %.3f", nse_base),
       x = expression(Q[obs]~"["*m^3/s*"]"), y = expression(Q[sim]~"["*m^3/s*"]")) +
  theme_bw()

p_scat2 <- ggplot(scatter_df, aes(x = Observed, y = Scenario)) +
  geom_point(alpha = 0.25, size = 0.8, colour = "#d6604d") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
  coord_equal(xlim = c(0, max_q), ylim = c(0, max_q)) +
  labs(title = paste0("Scenario: ", compare_scenario),
       subtitle = sprintf("NSE = %.3f", nse_scen),
       x = expression(Q[obs]~"["*m^3/s*"]"), y = expression(Q[sim]~"["*m^3/s*"]")) +
  theme_bw()

p_scatter <- patchwork::wrap_plots(p_scat1, p_scat2, ncol = 2)
print(p_scatter)
ggsave(file.path(plot_dir, paste0("scatter_NB", sb_plot, "_", compare_scenario, ".png")),
       p_scatter, width = 12, height = 6, dpi = 150)

# --- 6d. Flow duration curve ---

calc_fdc <- function(q) {
  q_sorted <- sort(q, decreasing = TRUE)
  data.frame(
    exceedance = seq_along(q_sorted) / (length(q_sorted) + 1) * 100,
    Q          = q_sorted
  )
}

fdc_obs  <- calc_fdc(scatter_df$Observed)  %>% mutate(series = "Observed")
fdc_base <- calc_fdc(scatter_df$Baseline)  %>% mutate(series = "Baseline")
fdc_scen <- calc_fdc(scatter_df$Scenario)  %>% mutate(series = compare_scenario)
fdc_all  <- bind_rows(fdc_obs, fdc_base, fdc_scen)
fdc_all$series <- factor(fdc_all$series,
                          levels = c("Observed", "Baseline", compare_scenario))

p_fdc <- ggplot(fdc_all, aes(x = exceedance, y = Q, colour = series)) +
  geom_line(linewidth = 0.7) +
  scale_y_log10() +
  scale_colour_manual(values = setNames(c("black", "#2166ac", "#d6604d"),
                                         c("Observed", "Baseline", compare_scenario))) +
  labs(title = paste("Flow Duration Curve — Subbasin", sb_plot),
       x = "Exceedance Probability [%]",
       y = expression(Q~"["*m^3/s*"]"), colour = NULL) +
  theme_bw() +
  theme(legend.position = "top")
print(p_fdc)
ggsave(file.path(plot_dir, paste0("fdc_NB", sb_plot, "_", compare_scenario, ".png")),
       p_fdc, width = 10, height = 6, dpi = 150)

# --- 6e. Mean monthly discharge ---

monthly_df <- data.frame(
  month    = as.integer(format(runoff_base$DateTime[idx], "%m")),
  Observed = runoff_base[[qobs_col]][idx],
  Baseline = runoff_base[[qsim_col]][idx],
  Scenario = runoff_scen[[qsim_col]][idx]
) %>%
  group_by(month) %>%
  summarise(
    Obs  = mean(Observed, na.rm = TRUE),
    Base = mean(Baseline, na.rm = TRUE),
    Scen = mean(Scenario, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(c(Obs, Base, Scen), names_to = "series", values_to = "Q_mean")

monthly_df$series <- factor(monthly_df$series,
                             levels = c("Obs", "Base", "Scen"),
                             labels = c("Observed", "Baseline", compare_scenario))

p_monthly <- ggplot(monthly_df, aes(x = factor(month), y = Q_mean, fill = series)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  scale_fill_manual(values = setNames(c("grey30", "#2166ac", "#d6604d"),
                                       c("Observed", "Baseline", compare_scenario))) +
  labs(title = paste("Mean Monthly Discharge — Subbasin", sb_plot),
       x = "Month", y = expression(bar(Q)~"["*m^3/s*"]"), fill = NULL) +
  theme_bw() +
  theme(legend.position = "top")
print(p_monthly)
ggsave(file.path(plot_dir, paste0("monthly_Q_NB", sb_plot, "_", compare_scenario, ".png")),
       p_monthly, width = 10, height = 6, dpi = 150)

cat("\nPlots saved to:", plot_dir, "\n")

# =============================================================================
# 7. LAUNCH INTERACTIVE SHINY APP (optional)
# =============================================================================

# Explore outputs interactively — auto-loads the project data
# launch_cosero_app(project_path)

cat("\n--- Script 1 complete ---\n")
