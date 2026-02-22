# =============================================================================
# COSERO PARAMETER OPTIMIZATION TEST
# =============================================================================
# Tests DDS optimization with the Wildalpen example catchment
# =============================================================================
devtools::document()
#devtools::load_all("D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R")
devtools::load_all()
library(ggplot2)

# =============================================================================
# SETUP PROJECT
# =============================================================================

project_path <- "D:/temp/COSERO_Optim_DDS"

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
#param_names <- c("BETA", "CTMAX","M", "TAB1")
#param_names <- c("M", "KBF", "TAB1", "H1", "TVS1", "TAB2", "H2", "TVS2", "TAB3")
param_names <- c("CTMIN", "CTMAX", "BETA", "M", "KBF", "TAB1", "H1", "TVS1", "TAB2", "H2", "TVS2", "TAB3")

# Optimization settings
max_iterations <- 2000  # Increase to 500-2000 for production
target_subbasin <- c("001","002","003")

# Simulation period
start_date <- "2000 1 1 0 0"
end_date <- "2020 12 31 0 0"
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
cat(sprintf("  Initial KGE: %.4f\n", initial_kge))

# =============================================================================
# DEFINE PARAMETER BOUNDS (from package CSV)
# =============================================================================

par_bounds <- load_parameter_bounds(parameters = param_names)

cat("Parameter bounds:\n")
print(par_bounds[, c("parameter", "description", "min", "max", "default", "modification_type")])
cat("\n")

# =============================================================================
# RUN DDS OPTIMIZATION
# =============================================================================

cat("Starting DDS optimization...\n")
cat("  Max iterations:", max_iterations, "\n")
cat("  Target subbasin:", target_subbasin, "\n\n")

set.seed(42)

result_dds <- optimize_cosero_dds(
  cosero_path = project_path,
  par_bounds = par_bounds,
  target_subbasins = target_subbasin,
  metric = "NSE",
  subbasin_weights = c(0.1, 0.1, 0.8),
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  max_iter = max_iterations,
  r = 0.2,
  verbose = TRUE,
  read_final_outputs = TRUE
)

#
# =============================================================================
# RESULTS
# =============================================================================

cat("\n=============================================================================\n")
cat("OPTIMIZATION RESULTS\n")
cat("=============================================================================\n")

cat("\nOptimal parameters:\n")
print(result_dds$par_bounds[, c("parameter", "min", "default", "optimal_value", "max")])

cat(sprintf("\nInitial NSE: %.4f\n", initial_nse))
cat(sprintf("Optimized NSE: %.4f\n", -result_dds$value))
cat(sprintf("Improvement: %.4f (%.1f%%)\n",
            -result_dds$value - initial_nse,
            100 * (-result_dds$value - initial_nse) / abs(initial_nse)))

cat(sprintf("\nRuntime: %.1f seconds\n", result_dds$runtime_seconds))

# =============================================================================
# VISUALIZATION
# =============================================================================

results_dir <- file.path(project_path, "optimization_results")
dir.create(results_dir, showWarnings = FALSE)

# Convergence plot
p_conv <- plot_cosero_optimization(result_dds)
ggsave(file.path(results_dir, "convergence.png"), p_conv, width = 10, height = 6, dpi = 150)
cat("\nConvergence plot saved to:", file.path(results_dir, "convergence.png"), "\n")

# Export results
export_cosero_optimization(result_dds, results_dir)

# Save optimal parameter file
param_file_orig <- file.path(project_path, "input", "para_ini.txt")
param_file_opt <- file.path(results_dir, "para_optimized_NB1_2_3_NSE.txt")
file.copy(param_file_orig, param_file_opt, overwrite = TRUE)

# Apply optimal parameters (only to zones that were optimized)
optimal_params <- setNames(
  as.list(result_dds$par_bounds$optimal_value),
  result_dds$par_bounds$parameter
)
original_values <- read_parameter_table(param_file_orig, param_names, zone_id = "all")
modify_parameter_table(param_file_opt, optimal_params, par_bounds, original_values,
                       zones = result_dds$zones_to_modify)

cat("\nOptimal parameter file saved to:", param_file_opt, "\n")
cat("Zones modified:", if (is.null(result_dds$zones_to_modify)) "all" else
    paste(result_dds$zones_to_modify, collapse = ", "), "\n")


result_dds <- run_cosero(
  project_path,
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    PARAFILE = "para_optimized_NB1_2_NSE.txt",
    SPINUP = 365,
    OUTPUTTYPE = 1
  ),
  statevar_source = 2,  # Cold start
  read_outputs = TRUE,
  quiet = FALSE
)

launch_cosero_app(project_path)

# =============================================================================
# MULTI-OBJECTIVE TEST (NSE + KGE)
# =============================================================================

cat("\n=============================================================================\n")
cat("MULTI-OBJECTIVE OPTIMIZATION (NSE 60% + KGE 40%)\n")
cat("=============================================================================\n")

set.seed(123)

result_multi <- optimize_cosero_dds(
  cosero_path = project_path,
  par_bounds = par_bounds,
  target_subbasins = target_subbasin,
  metric = c("NSE", "KGE"),
  metric_weights = c(0.6, 0.4),
  defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  max_iter = max_iterations,
  verbose = TRUE
)

cat("\nMulti-objective optimal parameters:\n")
print(result_multi$par_bounds[, c("parameter", "optimal_value")])
cat(sprintf("\nCombined metric: %.4f\n", -result_multi$value))

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("TEST COMPLETE\n")
cat("=============================================================================\n")
cat("Project:", project_path, "\n")
cat("Results:", results_dir, "\n")
cat("  - optimal_parameters.csv\n")
cat("  - optimization_history.csv\n")
cat("  - optimization_summary.csv\n")
cat("  - convergence.png\n")

gc(full = TRUE)
