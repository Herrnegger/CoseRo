# =============================================================================
# Wildalpen NDC — Script 1: Single Runs
# =============================================================================
# Explores the NDC disaggregation feature using the Wildalpen aggregated
# example project (3 subbasins, 6 zones, NDC = 5 in para_ini_agg.txt).
#
# Workflow:
#   1. Setup aggregated example project
#   2. Baseline run with default parameters (NDC = 5 already in para file)
#   3. Compare full and short simulation periods
#   4. Scenario comparison — vary lapse rates and CV parameters
#   5. Compare metrics across all subbasins
#   6. Save diagnostic plots to project/single_run_results/
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

project_path <- "D:/temp/Wildalpen_Example"

base_settings <- list(
  STARTDATE  = c(2000, 10, 1, 0, 0),
  ENDDATE    = c(2024, 9, 30, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini_agg.txt"
)

# Short period for quick scenario tests (avoid long runtimes with NDC = 5)
short_settings <- modifyList(base_settings, list(
  STARTDATE = c(2010, 10, 1, 0, 0),
  ENDDATE   = c(2015, 9, 30, 0, 0)
))

# =============================================================================
# 1. SETUP EXAMPLE PROJECT
# =============================================================================

cat("=== Setting up aggregated example project ===\n")

if (!dir.exists(project_path)) {
  setup_cosero_project_example_aggregated(project_path)
  cat("Project created at:", project_path, "\n")
} else {
  cat("Using existing project at:", project_path, "\n")
}

stopifnot(dir.exists(project_path))
stopifnot(file.exists(file.path(project_path, "COSERO.exe")))
stopifnot(file.exists(file.path(project_path, "input", "defaults.txt")))
stopifnot(file.exists(file.path(project_path, "input", "para_ini_agg.txt")))

par_file <- file.path(project_path, "input", "para_ini_agg.txt")

# Verify parameter file has NDC and disaggregation columns
params <- read_cosero_parameters(par_file)
cat(sprintf("Parameter file: %d rows (zones), %d columns\n", nrow(params), ncol(params)))
disag_cols <- grep("LAPSE|SOILVAR|HYDROVAR|CTVAR|NDC|HYPSO", names(params), value = TRUE)
cat("Disaggregation columns present:", paste(disag_cols[!grepl("HYPSO", disag_cols)], collapse = ", "), "\n")
cat("Hypsometric columns (HYPSO): ", sum(grepl("HYPSO", names(params))), "\n\n")

# =============================================================================
# 2. BASELINE RUN — full period, all subbasins
# =============================================================================

cat("=== Baseline run (full period 2000–2024) ===\n")

result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = base_settings,
  statevar_source   = 1,
  quiet             = FALSE,
  read_outputs      = TRUE
)

# Print metrics for all three subbasins
for (sb in c("001", "002", "003")) {
  nse <- tryCatch(extract_run_metrics(result_base, sb, "NSE"), error = function(e) NA_real_)
  kge <- tryCatch(extract_run_metrics(result_base, sb, "KGE"), error = function(e) NA_real_)
  cat(sprintf("  Subbasin %s — NSE = %.4f  |  KGE = %.4f\n", sb, nse, kge))
}
cat("\n")

# =============================================================================
# 3. SHORT-PERIOD VERIFICATION RUN
# =============================================================================

cat("=== Short-period run (2010–2015) ===\n")

result_short <- run_cosero(
  project_path      = project_path,
  defaults_settings = short_settings,
  statevar_source   = 1,
  quiet             = FALSE,
  read_outputs      = TRUE
)

for (sb in c("001", "002", "003")) {
  nse <- tryCatch(extract_run_metrics(result_short, sb, "NSE"), error = function(e) NA_real_)
  kge <- tryCatch(extract_run_metrics(result_short, sb, "KGE"), error = function(e) NA_real_)
  cat(sprintf("  Subbasin %s — NSE = %.4f  |  KGE = %.4f\n", sb, nse, kge))
}
cat("\n")

# =============================================================================
# 4. SCENARIO COMPARISON — vary disaggregation parameters
# =============================================================================
# Each scenario modifies disaggregation parameters and re-runs the model.
# The original para_ini_agg.txt is restored after the loop.
# =============================================================================

cat("=== Scenario comparison (short period) ===\n")

scenarios <- list(
  default = list(
    label  = "Default (NDC=5, default lapse rates)",
    params = list()    # no changes — use existing file as-is
  ),
  strong_lapse = list(
    label  = "Strong temperature lapse (-1.0°C/100m)",
    params = list(LAPSE_T = -1.0)
  ),
  weak_lapse = list(
    label  = "Weak temperature lapse (-0.3°C/100m)",
    params = list(LAPSE_T = -0.3)
  ),
  high_cv = list(
    label  = "High CV (SOILVAR = HYDROVAR = CTVAR = 1.5)",
    params = list(SOILVAR = 1.5, HYDROVAR = 1.5, CTVAR = 1.5)
  ),
  low_cv = list(
    label  = "Low CV (SOILVAR = HYDROVAR = CTVAR = 0.1)",
    params = list(SOILVAR = 0.1, HYDROVAR = 0.1, CTVAR = 0.1)
  )
)

disag_bounds <- load_parameter_bounds(
  parameters = c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR")
)

orig_params <- read_cosero_parameters(par_file)

scenario_metrics <- data.frame()

for (sc_name in names(scenarios)) {
  sc <- scenarios[[sc_name]]
  cat(sprintf("  Running: %s\n", sc$label))

  if (length(sc$params) > 0) {
    # Read current values and apply modification
    orig_vals <- lapply(names(sc$params), function(p) {
      orig_params[[paste0(p, "_")]]
    })
    names(orig_vals) <- names(sc$params)

    sc_bounds <- disag_bounds[disag_bounds$parameter %in% names(sc$params), ]
    modify_parameter_table(par_file, sc$params, sc_bounds, orig_params)
  }

  result_sc <- run_cosero(
    project_path      = project_path,
    defaults_settings = short_settings,
    statevar_source   = 1,
    quiet             = TRUE
  )

  for (sb in c("001", "002", "003")) {
    nse <- tryCatch(extract_run_metrics(result_sc, sb, "NSE"), error = function(e) NA_real_)
    kge <- tryCatch(extract_run_metrics(result_sc, sb, "KGE"), error = function(e) NA_real_)
    scenario_metrics <- rbind(scenario_metrics, data.frame(
      scenario  = sc_name,
      label     = sc$label,
      subbasin  = sb,
      NSE       = nse,
      KGE       = kge,
      stringsAsFactors = FALSE
    ))
  }

  # Restore original parameter file after each non-default scenario
  if (length(sc$params) > 0) {
    write_cosero_parameters(par_file, orig_params)
  }
}

cat("\nScenario metrics summary:\n")
print(scenario_metrics %>%
        select(scenario, subbasin, NSE, KGE) %>%
        mutate(across(c(NSE, KGE), \(x) round(x, 4))))

# =============================================================================
# 5. OUTPUT READING — check all output types are readable
# =============================================================================

cat("\n=== Reading model output (OUTPUTTYPE = 1) ===\n")

output <- read_cosero_output(project_path)
cat("Available output tables:", paste(names(output), collapse = ", "), "\n")
stopifnot(!is.null(output$runoff))
stopifnot(nrow(output$runoff) > 0)

cat(sprintf("Runoff table: %d rows, %d columns\n", nrow(output$runoff), ncol(output$runoff)))
cat(sprintf("Date range: %s to %s\n",
            format(min(output$runoff$DateTime)), format(max(output$runoff$DateTime))))

# =============================================================================
# 6. DIAGNOSTIC PLOTS
# =============================================================================

plot_dir <- file.path(project_path, "single_run_results")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
cat("\n=== Saving diagnostic plots to", plot_dir, "===\n")

# --- 6a. Discharge hydrograph (baseline, all subbasins) ---

qsim_cols <- grep("^QSIM_", names(output$runoff), value = TRUE)
qobs_cols  <- grep("^QOBS_", names(output$runoff), value = TRUE)

if (length(qsim_cols) > 0) {
  plot_data <- output$runoff %>%
    select(DateTime, all_of(c(qsim_cols, qobs_cols))) %>%
    pivot_longer(-DateTime, names_to = "variable", values_to = "value") %>%
    mutate(
      subbasin = sub(".*(\\d{4})$", "NB\\1", variable),
      type     = ifelse(grepl("^QSIM", variable), "Simulated", "Observed")
    ) %>%
    filter(!is.na(value), value > 0)

  p_hydro <- ggplot(plot_data, aes(x = DateTime, y = value,
                                    colour = type, linetype = type)) +
    geom_line(linewidth = 0.5) +
    facet_wrap(~subbasin, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(Simulated = "#c0392b", Observed = "#2980b9")) +
    scale_linetype_manual(values = c(Simulated = "solid", Observed = "dashed")) +
    labs(title    = "Simulated vs Observed Discharge — All Subbasins (Baseline)",
         subtitle = paste("Period:", format(min(plot_data$DateTime), "%Y-%m-%d"),
                          "to", format(max(plot_data$DateTime), "%Y-%m-%d")),
         x = NULL, y = "Discharge (m³/s)",
         colour = NULL, linetype = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "top", strip.background = element_blank())

  print(p_hydro)
  ggsave(file.path(plot_dir, "hydro_baseline_all_subbasins.png"),
         p_hydro, width = 14, height = 10, dpi = 150)
  cat("Saved: hydro_baseline_all_subbasins.png\n")
}

# --- 6b. Scenario comparison — bar plot of NSE and KGE ---

p_scenario <- scenario_metrics %>%
  pivot_longer(c(NSE, KGE), names_to = "metric", values_to = "value") %>%
  mutate(
    subbasin_label = paste0("NB", subbasin),
    scenario       = factor(scenario, levels = names(scenarios))
  ) %>%
  ggplot(aes(x = scenario, y = value, fill = subbasin_label)) +
  geom_col(position = position_dodge(0.8), width = 0.7) +
  facet_wrap(~metric, ncol = 1, scales = "fixed") +
  geom_hline(yintercept = c(0, 1), linetype = "dotted", colour = "grey60") +
  scale_fill_manual(values = c(NB001 = "#2980b9", NB002 = "#27ae60", NB003 = "#e74c3c")) +
  labs(title    = "Scenario Comparison — NSE and KGE (short period 2010–2015)",
       subtitle = "Each scenario varies NDC disaggregation parameters",
       x = "Scenario", y = "Metric value", fill = "Subbasin") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        legend.position = "top",
        strip.background = element_blank())

print(p_scenario)
ggsave(file.path(plot_dir, "scenario_comparison_metrics.png"),
       p_scenario, width = 10, height = 8, dpi = 150)
cat("Saved: scenario_comparison_metrics.png\n")

# --- 6c. Lapse rate vs metric scatterplot ---
# Quick visual: how does LAPSE_T affect NSE for each subbasin?

lapse_scan_results <- data.frame()
lapse_vals <- c(-1.0, -0.8, -0.65, -0.4, -0.2)

orig_params_scan <- read_cosero_parameters(par_file)

for (lt in lapse_vals) {
  modify_parameter_table(
    par_file,
    list(LAPSE_T = lt),
    disag_bounds[disag_bounds$parameter == "LAPSE_T", ],
    orig_params_scan
  )

  result_lt <- run_cosero(
    project_path      = project_path,
    defaults_settings = short_settings,
    statevar_source   = 1,
    quiet             = TRUE
  )

  for (sb in c("001", "002", "003")) {
    nse <- tryCatch(extract_run_metrics(result_lt, sb, "NSE"), error = function(e) NA_real_)
    lapse_scan_results <- rbind(lapse_scan_results, data.frame(
      LAPSE_T  = lt,
      subbasin = sb,
      NSE      = nse,
      stringsAsFactors = FALSE
    ))
  }
}

# Restore original parameter file
write_cosero_parameters(par_file, orig_params_scan)

p_lapse <- ggplot(lapse_scan_results,
                   aes(x = LAPSE_T, y = NSE, colour = subbasin, group = subbasin)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = -0.65, linetype = "dashed", colour = "grey50") +
  annotate("text", x = -0.65, y = max(lapse_scan_results$NSE, na.rm = TRUE),
           label = "default\n(-0.65)", hjust = -0.1, size = 3, colour = "grey40") +
  scale_colour_manual(values = c("001" = "#2980b9", "002" = "#27ae60", "003" = "#e74c3c")) +
  labs(title    = "Temperature Lapse Rate Sensitivity — NSE",
       subtitle = "Short period 2010–2015 | Vertical line = default LAPSE_T",
       x = "LAPSE_T (°C / 100 m)", y = "NSE",
       colour = "Subbasin") +
  theme_bw(base_size = 11) +
  theme(legend.position = "top")

print(p_lapse)
ggsave(file.path(plot_dir, "lapse_rate_sensitivity_NSE.png"),
       p_lapse, width = 8, height = 5, dpi = 150)
cat("Saved: lapse_rate_sensitivity_NSE.png\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n============================================================\n")
cat("Script 1 complete.\n")
cat("Results saved to:", plot_dir, "\n")
cat("============================================================\n")
