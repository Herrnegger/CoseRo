# COSERO Sensitivity Analysis - Example Workflow
# Demonstrates sensitivity analysis with different parameter sets
# Author: COSERO R Interface
# Date: 2025-11-04

# Load required libraries
library(dplyr)
library(ggplot2)
library(COSERO)  # Load COSERO package functions

# ==============================================================================
# EXAMPLE 1: Snow and Runoff Parameters
# ==============================================================================

cat("\n=== EXAMPLE 1: Snow and Runoff Parameters ===\n\n")

# 1.1 Define parameters to analyze
snow_runoff_params <- c("BETA", "CTMAX", "CTMIN", "SNOWTRT", "RAINTRT",
                        "TAB1", "TAB2", "H1", "H2")

# 1.2 Load parameter bounds
par_bounds_1 <- load_parameter_bounds(parameters = snow_runoff_params)
cat("Selected parameters:\n")
print(par_bounds_1[, c("parameter", "min", "max", "modification_type", "category")])

# 1.3 Create Sobol bounds
sobol_bounds_1 <- create_sobol_bounds(par_bounds_1)

# 1.4 Generate Sobol samples (n=50 for quick test, use n=100+ for real analysis)
sobol_samples_1 <- generate_sobol_samples(sobol_bounds_1, n = 50)
cat("\nGenerated", nrow(sobol_samples_1$parameter_sets), "parameter combinations\n")
cat("Total COSERO runs needed:", nrow(sobol_samples_1$parameter_sets), "\n")

# 1.5 Run COSERO ensemble
# NOTE: Adjust project_path to your COSERO project location
project_path <- "D:/OneDrive - Universität für Bodenkultur Wien/COSERO_Run_R/COSERO_MonteCarlo_Optimierung_SH"

# Base settings for COSERO run
base_settings <- list(
  STARTDATE = "2015 1 1 0 0",
  ENDDATE = "2020 12 31 23 59",
  SPINUP = 365,
  OUTPUTTYPE = 1  
)

# Run ensemble (uncomment when ready)
 ensemble_results_1 <- run_cosero_ensemble(
   project_path = project_path,
   parameter_sets = sobol_samples_1$parameter_sets,
   par_bounds = par_bounds_1,
   base_settings = base_settings,
   quiet = FALSE
 )
#
# # Save results
# saveRDS(ensemble_results_1, "output/sensitivity_snow_runoff.rds")

# 1.6 Load results (if previously saved)
# ensemble_results_1 <- readRDS("output/sensitivity_snow_runoff.rds")

# 1.7 Extract runoff output
# runoff_monthly_1 <- extract_ensemble_output(
#   ensemble_results_1,
#   variable = "runoff",
#   aggregation = "monthly"
# )

# 1.8 Calculate mean monthly runoff for each run
# runoff_means_1 <- sapply(runoff_monthly_1, function(x) {
#   if (!is.na(x)[1]) mean(x$value, na.rm = TRUE) else NA
# })

# 1.9 Calculate Sobol indices for runoff
# sobol_runoff_1 <- calculate_sobol_indices(
#   Y = runoff_means_1,
#   sobol_samples = sobol_samples_1,
#   boot = TRUE,
#   R = 100
# )

# 1.10 Plot Sobol indices
# p1 <- plot_sobol(sobol_runoff_1,
#                  title = "Sensitivity: Snow & Runoff Parameters → Mean Monthly Runoff")
# print(p1)
# ggsave("output/sobol_snow_runoff.png", p1, width = 10, height = 6, dpi = 300)

# 1.11 Create dotty plots
# p2 <- plot_dotty(
#   parameter_sets = sobol_samples_1$parameter_sets,
#   Y = runoff_means_1,
#   y_label = "Mean Monthly Runoff [mm]",
#   n_col = 3
# )
# print(p2)
# ggsave("output/dotty_snow_runoff.png", p2, width = 12, height = 10, dpi = 300)

# 1.12 If observed data available, calculate performance metrics
# obs_data <- read.csv("path/to/observed_runoff.csv")
# kge_values_1 <- calculate_ensemble_metrics(
#   ensemble_results_1,
#   observed = obs_data$runoff,
#   metric = "KGE"
# )
#
# sobol_kge_1 <- calculate_sobol_indices(
#   Y = kge_values_1,
#   sobol_samples = sobol_samples_1
# )
#
# p3 <- plot_sobol(sobol_kge_1,
#                  title = "Sensitivity: Snow & Runoff Parameters → KGE Performance")
# print(p3)

# 1.13 Export results
# export_sensitivity_results(
#   output_dir = "output/example1",
#   sobol_indices = sobol_runoff_1,
#   parameter_sets = sobol_samples_1$parameter_sets,
#   metrics = kge_values_1,
#   prefix = "snow_runoff"
# )

cat("\nExample 1 setup complete. Uncomment code blocks to run analysis.\n")

# ==============================================================================
# EXAMPLE 2: Evapotranspiration and Soil Parameters
# ==============================================================================

cat("\n=== EXAMPLE 2: Evapotranspiration and Soil Parameters ===\n\n")

# 2.1 Define parameters to analyze
et_soil_params <- c("ETSLPCOR", "ETSYSCOR", "ETVEGCOR", "FKFAK",
                    "M", "FK", "PWP", "PCOR", "TVS1", "TVS2")

# 2.2 Load parameter bounds
par_bounds_2 <- load_parameter_bounds(parameters = et_soil_params)
cat("Selected parameters:\n")
print(par_bounds_2[, c("parameter", "min", "max", "modification_type", "category")])

# 2.3 Create Sobol bounds
sobol_bounds_2 <- create_sobol_bounds(par_bounds_2)

# 2.4 Generate Sobol samples
sobol_samples_2 <- generate_sobol_samples(sobol_bounds_2, n = 50)
cat("\nGenerated", nrow(sobol_samples_2$parameter_sets), "parameter combinations\n")

# 2.5 Run COSERO ensemble
# ensemble_results_2 <- run_cosero_ensemble(
#   project_path = project_path,
#   parameter_sets = sobol_samples_2$parameter_sets,
#   par_bounds = par_bounds_2,
#   base_settings = base_settings,
#   quiet = FALSE
# )
#
# saveRDS(ensemble_results_2, "output/sensitivity_et_soil.rds")

# 2.6 Load results
# ensemble_results_2 <- readRDS("output/sensitivity_et_soil.rds")

# 2.7 Extract ET output
# et_annual_2 <- extract_ensemble_output(
#   ensemble_results_2,
#   variable = "et",
#   aggregation = "annual"
# )

# 2.8 Calculate mean annual ET for each run
# et_means_2 <- sapply(et_annual_2, function(x) {
#   if (!is.na(x)[1]) mean(x$value, na.rm = TRUE) else NA
# })

# 2.9 Calculate Sobol indices for ET
# sobol_et_2 <- calculate_sobol_indices(
#   Y = et_means_2,
#   sobol_samples = sobol_samples_2,
#   boot = TRUE,
#   R = 100
# )

# 2.10 Plot Sobol indices
# p4 <- plot_sobol(sobol_et_2,
#                  title = "Sensitivity: ET & Soil Parameters → Mean Annual ET")
# print(p4)
# ggsave("output/sobol_et_soil.png", p4, width = 10, height = 6, dpi = 300)

# 2.11 Create dotty plots with reference line
# catchment_precip <- 1200  # Example: mean annual precipitation [mm]
# expected_et <- 800        # Example: expected ET based on climate/landcover
#
# p5 <- plot_dotty(
#   parameter_sets = sobol_samples_2$parameter_sets,
#   Y = et_means_2,
#   y_label = "Mean Annual ET [mm]",
#   n_col = 3,
#   reference_line = expected_et
# )
# print(p5)
# ggsave("output/dotty_et_soil.png", p5, width = 12, height = 10, dpi = 300)

# 2.12 Analyze both runoff and ET together (water balance)
# runoff_annual_2 <- extract_ensemble_output(
#   ensemble_results_2,
#   variable = "runoff",
#   aggregation = "annual"
# )
#
# runoff_means_annual_2 <- sapply(runoff_annual_2, function(x) {
#   if (!is.na(x)[1]) mean(x$value, na.rm = TRUE) else NA
# })
#
# # Water balance check
# water_balance <- data.frame(
#   run = 1:length(et_means_2),
#   ET = et_means_2,
#   Runoff = runoff_means_annual_2,
#   Total = et_means_2 + runoff_means_annual_2,
#   Precipitation = catchment_precip
# )
#
# # Plot water balance
# p6 <- ggplot(water_balance, aes(x = Total, y = Precipitation)) +
#   geom_point(alpha = 0.6, size = 2) +
#   geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
#   theme_bw() +
#   labs(x = "ET + Runoff [mm]", y = "Precipitation [mm]",
#        title = "Water Balance Check Across Parameter Space") +
#   annotate("text", x = max(water_balance$Total) * 0.9,
#            y = catchment_precip * 0.5,
#            label = "Perfect balance", color = "red")
# print(p6)

# 2.13 Export results
# export_sensitivity_results(
#   output_dir = "output/example2",
#   sobol_indices = sobol_et_2,
#   parameter_sets = sobol_samples_2$parameter_sets,
#   prefix = "et_soil"
# )

cat("\nExample 2 setup complete. Uncomment code blocks to run analysis.\n")

# ==============================================================================
# EXAMPLE 3 (BONUS): Full Parameter Set
# ==============================================================================

cat("\n=== EXAMPLE 3 (BONUS): Full Parameter Sensitivity ===\n\n")

# 3.1 Load all available parameters
par_bounds_full <- load_parameter_bounds()
cat("Total parameters available:", nrow(par_bounds_full), "\n")
print(table(par_bounds_full$category))

# 3.2 Generate Sobol samples for all parameters
# NOTE: This requires many runs! n=50 with 30 params = 50*(2+30) = 1600 runs
# sobol_bounds_full <- create_sobol_bounds(par_bounds_full)
# sobol_samples_full <- generate_sobol_samples(sobol_bounds_full, n = 25)  # Use smaller n
# cat("\nTotal runs needed:", nrow(sobol_samples_full$parameter_sets), "\n")

# 3.3 Run analysis (same steps as examples 1 and 2)
# ...

cat("\nFor full parameter analysis, consider using n=25 or running in parallel\n")

# ==============================================================================
# Helper Functions for Results Analysis
# ==============================================================================

#' Compare Sensitivity Between Examples
#'
#' @param sobol_1 Sobol results from example 1
#' @param sobol_2 Sobol results from example 2
compare_sensitivity <- function(sobol_1, sobol_2) {

  # Extract first-order indices
  si_1 <- sobol_1$results %>%
    filter(sensitivity == "Si") %>%
    select(parameters, original) %>%
    rename(Si_example1 = original)

  si_2 <- sobol_2$results %>%
    filter(sensitivity == "Si") %>%
    select(parameters, original) %>%
    rename(Si_example2 = original)

  # Combine
  comparison <- full_join(si_1, si_2, by = "parameters") %>%
    arrange(desc(pmax(Si_example1, Si_example2, na.rm = TRUE)))

  return(comparison)
}

#' Create Summary Table
#'
#' @param sobol_result Sobol sensitivity result
create_summary_table <- function(sobol_result) {

  summary_table <- sobol_result$results %>%
    filter(sensitivity == "Si") %>%
    arrange(desc(original)) %>%
    mutate(
      rank = row_number(),
      variance_explained = paste0(round(original * 100, 1), "%"),
      ci = paste0("[", round(low.ci, 3), ", ", round(high.ci, 3), "]")
    ) %>%
    select(rank, parameters, original, std.error, ci, variance_explained) %>%
    rename(
      Rank = rank,
      Parameter = parameters,
      Si = original,
      StdError = std.error,
      CI_95 = ci,
      Variance = variance_explained
    )

  return(summary_table)
}

cat("\n=== Workflow Complete ===\n")
cat("\nTo run the analysis:\n")
cat("1. Set your project_path\n")
cat("2. Uncomment the code blocks in each example\n")
cat("3. Run the script\n")
cat("4. Results will be saved in 'output/' directory\n\n")
