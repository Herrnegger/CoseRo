# =============================================================================
# Wildalpen NDC — Script 3: Sequential Subbasin Optimization
# =============================================================================
# Calibrates the Wildalpen aggregated catchment (3 subbasins, 6 zones, NDC = 5)
# sequentially from upstream to downstream using DDS:
#
#   Step 1 — Optimize zones of NB=1 (headwater)
#             target_subbasins = "001", starting from para_ini_agg.txt
#             Result saved as para_opt_NB001.txt
#
#   Step 2 — Optimize zones of NB=2 (mid-catchment)
#             target_subbasins = "002", starting from para_opt_NB001.txt
#             Result saved as para_opt_NB002.txt
#
#   Step 3 — Optimize zones of NB=3 (outlet / closure)
#             target_subbasins = "003", starting from para_opt_NB002.txt
#             Result saved as para_opt_NB003.txt
#
# Each step modifies only the zones belonging to the target subbasin
# (target_subbasins auto-maps zones via NB_/NZ_ columns in the parameter file).
# Parameters from previous steps remain fixed, preserving the upstream solution.
#
# Author/Architect: Mathew Herrnegger
# Coding: Claude
# Date: 2026-05-06
# Branch: dev/spatial-disaggregation
# =============================================================================

devtools::load_all()

library(ggplot2)
library(dplyr)
library(patchwork)

# =============================================================================
# USER SETTINGS
# =============================================================================

project_path <- "D:/temp/Wildalpen_Example"

# Calibration period (used for all steps)
cal_settings <- list(
  STARTDATE  = c(2000, 10, 1, 0, 0),
  ENDDATE    = c(2015, 9, 30, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini_agg.txt"   # updated at each step
)

# Validation period
val_settings <- modifyList(cal_settings, list(
  STARTDATE = c(2015, 10, 1, 0, 0),
  ENDDATE   = c(2024, 9, 30, 0, 0)
))

# DDS iterations per step
# Use 50–100 for smoke tests; 500–2000 for production calibration
max_iter_per_step <- 350

# Parameters to calibrate — mix of standard + NDC disaggregation params
# Keep NDC disaggregation params in all steps so each subbasin gets its own
# spatially consistent lapse rates and variability
param_names <- c(
  # NDC disaggregation
  "LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR",
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
# SETUP HELPERS
# =============================================================================

results_dir <- file.path(project_path, "optimisation_results")
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# Named list to collect results from each step
opt_steps <- list()

# Print a section header
section <- function(title) {
  cat(rep("=", 72), "\n", sep = "")
  cat(title, "\n")
  cat(rep("=", 72), "\n", sep = "")
}

# =============================================================================
# 1. VERIFY PROJECT
# =============================================================================

section("Verifying aggregated example project")

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

par_file_initial <- file.path(project_path, "input", "para_ini_agg.txt")
cat("Initial parameter file:", par_file_initial, "\n\n")

# =============================================================================
# 2. INITIAL RUN (before any calibration)
# =============================================================================

section("Initial run — pre-calibration metrics")

result_initial <- run_cosero(
  project_path      = project_path,
  defaults_settings = cal_settings,
  statevar_source   = 1,
  quiet             = FALSE,
  read_outputs      = TRUE
)

initial_metrics <- sapply(c("001", "002", "003"), function(sb) {
  c(NSE = tryCatch(extract_run_metrics(result_initial, sb, "NSE"), error = function(e) NA_real_),
    KGE = tryCatch(extract_run_metrics(result_initial, sb, "KGE"), error = function(e) NA_real_))
}, simplify = FALSE)

cat("\nInitial performance (calibration period):\n")
for (sb in names(initial_metrics)) {
  cat(sprintf("  Subbasin %s — NSE = %.4f  |  KGE = %.4f\n",
              sb, initial_metrics[[sb]]["NSE"], initial_metrics[[sb]]["KGE"]))
}

# =============================================================================
# 3. DEFINE PARAMETER BOUNDS
# =============================================================================
# Custom bounds narrowed around the expected optimum for the Wildalpen
# catchment. modification_type taken from parameter_bounds.csv defaults.

# param_names order:
#   LAPSE_T, LAPSE_P, SOILVAR, HYDROVAR, CTVAR,
#   CTMAX, CTMIN,
#   M, BETA, KBF,
#   H1, H2, TVS1, TVS2, TAB1, TAB2, TAB3,
#   PCOR, TCOR
par_bounds <- create_optimization_bounds(
  parameters        = param_names,
  lower             = c(-0.9, -0.2,  0.0, 0.8, 0.1,   # disaggregation
                         5.0,  0.1,                    # snow
                       200,    3.0, 2500,               # soil + runoff
                         4.0,  8.0,  50, 250, 30, 100, 4000,   # flow recession
                         0.8,  2.0),                   # met corrections
  upper             = c(-0.1,  0.4,  0.4, 1.7, 1.0,   # disaggregation
                         7.0,  1.5,                    # snow
                       400,    7.0, 5500,               # soil + runoff
                         8.0, 20.0, 140, 450, 50, 200, 7000,   # flow recession
                         1.4,  3.5),                   # met corrections
  modification_type = c("abschg", "abschg", "relchg", "relchg", "relchg",  # disaggregation
                        "relchg", "relchg",                                 # snow
                        "relchg", "relchg", "relchg",                       # soil + runoff
                        "relchg", "relchg", "relchg", "relchg",             # flow recession
                        "relchg", "relchg", "relchg",                       # flow recession cont.
                        "relchg", "abschg")                                 # met corrections
)

cat("\nParameter bounds:\n")
print(par_bounds[, c("parameter", "min", "max", "default",
                      "modification_type")])

# =============================================================================
# 4. SEQUENTIAL OPTIMIZATION
# =============================================================================

# Step definitions — (target_subbasin, input_parafile_name, output_parafile_name)
steps <- list(
  list(nb = "001", label = "NB1 (headwater)",     input_para = "para_ini_agg.txt",  output_para = "para_opt_NB001.txt"),
  list(nb = "002", label = "NB2 (mid-catchment)", input_para = "para_opt_NB001.txt", output_para = "para_opt_NB002.txt"),
  list(nb = "003", label = "NB3 (outlet)",        input_para = "para_opt_NB002.txt", output_para = "para_opt_NB003.txt")
)

for (step in steps) {
  section(sprintf("Optimization Step: %s", step$label))
  cat("  Input parameter file : ", step$input_para, "\n")
  cat("  Output parameter file: ", step$output_para, "\n")
  cat("  DDS iterations       : ", max_iter_per_step, "\n")
  cat("  Target subbasin      : NB", step$nb, "\n\n", sep = "")

  step_settings <- modifyList(cal_settings, list(PARAFILE = step$input_para))

  set.seed(42)

  result_opt <- optimize_cosero_dds(
    cosero_path       = project_path,
    par_bounds        = par_bounds,
    target_subbasins  = step$nb,
    metric            = "NSE",
    defaults_settings = step_settings,
    max_iter          = max_iter_per_step,
    r                 = 0.2,
    verbose           = TRUE,
    read_final_outputs = TRUE
  )

  # The optimized file is auto-saved by optimize_cosero_dds with a timestamp name.
  # Copy it to the human-readable step filename so the next step can reference it.
  opt_file_timestamped <- result_opt$optimized_par_file
  opt_file_step        <- file.path(project_path, "input", step$output_para)
  file.copy(opt_file_timestamped, opt_file_step, overwrite = TRUE)
  cat(sprintf("  Saved as: %s\n", opt_file_step))

  # Extract post-optimization metrics for all subbasins using the new parameter file
  result_verify <- run_cosero(
    project_path      = project_path,
    defaults_settings = modifyList(cal_settings, list(PARAFILE = step$output_para)),
    statevar_source   = 1,
    quiet             = TRUE
  )

  step_metrics <- sapply(c("001", "002", "003"), function(sb) {
    c(NSE = tryCatch(extract_run_metrics(result_verify, sb, "NSE"), error = function(e) NA_real_),
      KGE = tryCatch(extract_run_metrics(result_verify, sb, "KGE"), error = function(e) NA_real_))
  }, simplify = FALSE)

  cat(sprintf("\n  Post-step metrics (subbasin NB%s optimized):\n", step$nb))
  for (sb in names(step_metrics)) {
    tag <- if (sb == step$nb) " <-- optimized" else ""
    cat(sprintf("    Subbasin %s — NSE = %.4f  |  KGE = %.4f%s\n",
                sb, step_metrics[[sb]]["NSE"], step_metrics[[sb]]["KGE"], tag))
  }

  # Convergence plot
  p_conv <- plot_cosero_optimization(result_opt) +
    labs(title    = sprintf("DDS Convergence — %s", step$label),
         subtitle = sprintf("Optimizing NB%s | %d iterations", step$nb, max_iter_per_step))
  ggsave(file.path(results_dir, sprintf("convergence_NB%s.png", step$nb)),
         p_conv, width = 10, height = 6, dpi = 150)
  cat(sprintf("  Saved: convergence_NB%s.png\n\n", step$nb))

  # Store for summary
  opt_steps[[step$nb]] <- list(
    result      = result_opt,
    metrics     = step_metrics,
    output_para = step$output_para
  )

  # Export CSVs for this step
  export_cosero_optimization(
    result_opt,
    file.path(results_dir, paste0("step_NB", step$nb))
  )
}

# =============================================================================
# 5. VALIDATION RUN (final parameter file on held-out period)
# =============================================================================

section("Validation run — held-out period 2015–2024")

final_para <- steps[[length(steps)]]$output_para

result_val <- run_cosero(
  project_path      = project_path,
  defaults_settings = modifyList(val_settings, list(PARAFILE = final_para)),
  statevar_source   = 1,
  quiet             = FALSE,
  read_outputs      = TRUE
)

val_metrics <- sapply(c("001", "002", "003"), function(sb) {
  c(NSE = tryCatch(extract_run_metrics(result_val, sb, "NSE"), error = function(e) NA_real_),
    KGE = tryCatch(extract_run_metrics(result_val, sb, "KGE"), error = function(e) NA_real_))
}, simplify = FALSE)

cat("\nValidation performance (2015–2024):\n")
for (sb in names(val_metrics)) {
  cat(sprintf("  Subbasin %s — NSE = %.4f  |  KGE = %.4f\n",
              sb, val_metrics[[sb]]["NSE"], val_metrics[[sb]]["KGE"]))
}

# =============================================================================
# 6. SUMMARY TABLE — progress across steps
# =============================================================================

section("Calibration progress summary")

all_subbasins <- c("001", "002", "003")
periods       <- c("Initial", "After NB1", "After NB2", "After NB3", "Validation")

progress <- data.frame(
  period   = periods,
  NB001_NSE = NA_real_,
  NB002_NSE = NA_real_,
  NB003_NSE = NA_real_,
  NB001_KGE = NA_real_,
  NB002_KGE = NA_real_,
  NB003_KGE = NA_real_
)

# Fill initial row
for (sb in all_subbasins) {
  progress[progress$period == "Initial", paste0("NB", sb, "_NSE")] <- initial_metrics[[sb]]["NSE"]
  progress[progress$period == "Initial", paste0("NB", sb, "_KGE")] <- initial_metrics[[sb]]["KGE"]
}

# Fill post-step rows
step_labels <- c("001" = "After NB1", "002" = "After NB2", "003" = "After NB3")
for (nb in names(opt_steps)) {
  lbl <- step_labels[nb]
  for (sb in all_subbasins) {
    progress[progress$period == lbl, paste0("NB", sb, "_NSE")] <- opt_steps[[nb]]$metrics[[sb]]["NSE"]
    progress[progress$period == lbl, paste0("NB", sb, "_KGE")] <- opt_steps[[nb]]$metrics[[sb]]["KGE"]
  }
}

# Fill validation row
for (sb in all_subbasins) {
  progress[progress$period == "Validation", paste0("NB", sb, "_NSE")] <- val_metrics[[sb]]["NSE"]
  progress[progress$period == "Validation", paste0("NB", sb, "_KGE")] <- val_metrics[[sb]]["KGE"]
}

cat("\nNSE progress:\n")
print(progress[, c("period", "NB001_NSE", "NB002_NSE", "NB003_NSE")], row.names = FALSE)
cat("\nKGE progress:\n")
print(progress[, c("period", "NB001_KGE", "NB002_KGE", "NB003_KGE")], row.names = FALSE)

write.csv(progress, file.path(results_dir, "sequential_opt_progress.csv"), row.names = FALSE)
cat("\nProgress table saved to: sequential_opt_progress.csv\n")

# =============================================================================
# 7. DIAGNOSTIC PLOTS
# =============================================================================

# --- 7a. NSE / KGE progress across steps (all subbasins) ---

progress_long <- progress %>%
  pivot_longer(-period,
               names_to  = c("subbasin", "metric"),
               names_pattern = "NB(\\d+)_(\\w+)") %>%
  mutate(
    period   = factor(period, levels = periods),
    subbasin = paste0("NB", subbasin)
  )

p_progress_nse <- progress_long %>%
  filter(metric == "NSE") %>%
  ggplot(aes(x = period, y = value, colour = subbasin, group = subbasin)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4.5, linetype = "dashed", colour = "grey60") +
  annotate("text", x = 4.6, y = max(progress_long$value[progress_long$metric == "NSE"],
                                      na.rm = TRUE),
           label = "Validation", hjust = 0, size = 3, colour = "grey50") +
  scale_colour_manual(values = c(NB001 = "#2980b9", NB002 = "#27ae60", NB003 = "#e74c3c")) +
  labs(title    = "NSE Progress — Sequential Subbasin Optimization",
       subtitle = "Each step optimizes zones of the labeled subbasin only",
       x = NULL, y = "NSE", colour = "Subbasin") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "top")

p_progress_kge <- progress_long %>%
  filter(metric == "KGE") %>%
  ggplot(aes(x = period, y = value, colour = subbasin, group = subbasin)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4.5, linetype = "dashed", colour = "grey60") +
  scale_colour_manual(values = c(NB001 = "#2980b9", NB002 = "#27ae60", NB003 = "#e74c3c")) +
  labs(title    = "KGE Progress — Sequential Subbasin Optimization",
       x = NULL, y = "KGE", colour = "Subbasin") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1), legend.position = "top")

p_progress_combined <- patchwork::wrap_plots(p_progress_nse, p_progress_kge, ncol = 1)
print(p_progress_combined)
ggsave(file.path(results_dir, "sequential_opt_progress.png"),
       p_progress_combined, width = 10, height = 10, dpi = 150)
cat("Saved: sequential_opt_progress.png\n")

# --- 7b. Discharge hydrograph — initial vs final, all subbasins ---

output_initial <- read_cosero_output(project_path)   # last run was validation; re-read

result_cal_final <- run_cosero(
  project_path      = project_path,
  defaults_settings = modifyList(cal_settings, list(PARAFILE = final_para)),
  statevar_source   = 1,
  quiet             = TRUE,
  read_outputs      = TRUE
)

output_final <- result_cal_final$output_data

for (sb in c("001", "002", "003")) {
  sb_pad <- sprintf("%04d", as.integer(sb))

  qsim_col_init  <- paste0("QSIM_", sb_pad)
  qsim_col_final <- paste0("QSIM_", sb_pad)
  qobs_col       <- paste0("QOBS_", sb_pad)

  if (!qsim_col_init %in% names(output_initial$runoff)) next

  plot_df <- bind_rows(
    output_initial$runoff %>%
      select(DateTime, QSIM = all_of(qsim_col_init),
             QOBS = any_of(qobs_col)) %>%
      mutate(run = "Initial"),
    output_final$runoff %>%
      select(DateTime, QSIM = all_of(qsim_col_final),
             QOBS = any_of(qobs_col)) %>%
      mutate(run = "Optimized")
  ) %>%
    pivot_longer(c(QSIM, QOBS), names_to = "variable", values_to = "Q") %>%
    filter(!is.na(Q), Q >= 0)

  p_hydr <- ggplot(plot_df, aes(x = DateTime, y = Q, colour = interaction(run, variable),
                                  linetype = interaction(run, variable))) +
    geom_line(linewidth = 0.5) +
    scale_colour_manual(values = c(
      "Initial.QSIM"   = "#7f8c8d",
      "Optimized.QSIM" = "#c0392b",
      "Initial.QOBS"   = "#95a5a6",
      "Optimized.QOBS" = "#2980b9"
    )) +
    scale_linetype_manual(values = c(
      "Initial.QSIM"   = "dashed",
      "Optimized.QSIM" = "solid",
      "Initial.QOBS"   = "dashed",
      "Optimized.QOBS" = "solid"
    )) +
    labs(title    = sprintf("Initial vs Optimized Discharge — Subbasin %s (calibration period)", sb),
         x = NULL, y = "Discharge (m³/s)", colour = NULL, linetype = NULL) +
    theme_bw(base_size = 11) +
    theme(legend.position = "top")

  fname <- sprintf("hydro_initial_vs_optimized_NB%s.png", sb)
  ggsave(file.path(results_dir, fname), p_hydr, width = 14, height = 5, dpi = 150)
  cat("Saved:", fname, "\n")
}

# --- 7c. Optimal parameter comparison across steps ---

opt_param_df <- data.frame()
for (nb in names(opt_steps)) {
  res <- opt_steps[[nb]]$result
  vals <- res$par_bounds[, c("parameter", "default", "optimal_value")]
  vals$step <- paste0("After NB", nb)
  opt_param_df <- rbind(opt_param_df, vals)
}

p_params <- opt_param_df %>%
  mutate(
    norm_default = 0.5,   # default is always midpoint by construction
    norm_optimal = (optimal_value - par_bounds$min[match(parameter, par_bounds$parameter)]) /
                   (par_bounds$max[match(parameter, par_bounds$parameter)] -
                      par_bounds$min[match(parameter, par_bounds$parameter)])
  ) %>%
  ggplot(aes(x = parameter, y = norm_optimal, colour = step, shape = step, group = step)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", colour = "grey60") +
  annotate("text", x = 1, y = 0.52, label = "default", hjust = 0, size = 3, colour = "grey50") +
  geom_hline(yintercept = c(0, 1), linetype = "dotted", colour = "grey80") +
  scale_colour_manual(values = c(
    "After NB1" = "#2980b9", "After NB2" = "#27ae60", "After NB3" = "#e74c3c"
  )) +
  labs(title    = "Optimal Parameter Values After Each Calibration Step",
       subtitle = "Normalised to parameter bounds [0, 1] | Dashed = default (0.5)",
       x = NULL, y = "Normalised value", colour = "Step", shape = "Step") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

print(p_params)
ggsave(file.path(results_dir, "optimal_params_by_step.png"),
       p_params, width = 12, height = 6, dpi = 150)
cat("Saved: optimal_params_by_step.png\n")

# =============================================================================
# SUMMARY
# =============================================================================

section("Script 3 complete — summary")

cat("Final parameter file (after all 3 steps):\n")
cat(" ", file.path(project_path, "input", final_para), "\n\n")

cat("Calibration NSE improvement:\n")
for (sb in all_subbasins) {
  nse_init <- initial_metrics[[sb]]["NSE"]
  nse_final <- opt_steps[["003"]]$metrics[[sb]]["NSE"]
  cat(sprintf("  NB%s: %.4f → %.4f  (Δ = %+.4f)\n",
              sb, nse_init, nse_final, nse_final - nse_init))
}

cat("\nResults saved to:", results_dir, "\n")
cat("============================================================\n")
