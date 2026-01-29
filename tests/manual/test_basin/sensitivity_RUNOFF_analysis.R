# =============================================================================
# COSERO RUNOFF SENSITIVITY ANALYSIS
# =============================================================================
# Parameters: M, KBF, TAB1, H1, TVS1, TAB2, H2, TVS2, TAB3
# Sobol-based sensitivity analysis for runoff/groundwater parameters
# =============================================================================

devtools::load_all("D:/OneDrive - Universität für Bodenkultur Wien/github/COSERO-R")
library(ggplot2)
library(dplyr)
library(tidyr)

options(expressions = 500000)
gc(full = TRUE, verbose = FALSE)

# =============================================================================
# CONFIGURATION
# =============================================================================

project_path <- "D:/temp/COSERO_test"
param_file <- file.path(project_path, "input", "para_ini.txt")
statevar_input <- file.path(project_path, "input", "statevar.dmp")
statevar_output <- file.path(project_path, "output", "statevar.dmp")

# Copy statevar.dmp if needed
if (!file.exists(statevar_input) && file.exists(statevar_output)) {
  file.copy(statevar_output, statevar_input, overwrite = TRUE)
  cat("✓ statevar.dmp copied to input folder\n")
}

# Analysis settings
n_samples <- 50  # Increase to 100-500 for production
param_names <- c("M", "KBF", "TAB1", "H1", "TVS1", "TAB2", "H2", "TVS2", "TAB3")
plot_subbasin <- "0003"

# Simulation period (same for initial run and sensitivity analysis)
start_date <- "2009 1 1 0 0"
end_date <- "2020 12 31 0 0"
spinup <- 730

# Validate paths
stopifnot(
  file.exists(project_path),
  file.exists(param_file),
  file.exists(file.path(project_path, "COSERO.exe"))
)

cat("Configuration:\n")
cat("  Parameters:", paste(param_names, collapse = ", "), "\n")
cat("  Samples:", n_samples, "| Total runs:", n_samples * (length(param_names) + 2), "\n")
cat("  Warm start:", file.exists(statevar_input), "\n\n")

# =============================================================================
# STEP 1: PARAMETER BOUNDS & SOBOL SAMPLING
# =============================================================================

bounds <- load_parameter_bounds(parameters = param_names)
cat("Parameter bounds:\n")
print(bounds[, c("parameter", "min", "max", "modification_type")])
cat("\n")

sobol_bounds <- create_sobol_bounds(bounds)
set.seed(42)
samples <- generate_sobol_samples(sobol_bounds, n = n_samples)
cat("✓ Generated", nrow(samples$parameter_sets), "parameter sets\n\n")

# =============================================================================
# INITIAL RUN WITH ORIGINAL PARAMETERS
# =============================================================================

cat("Running initial model with original parameters...\n")
statevar_source <- if (file.exists(statevar_input)) 2 else 1

initial_run <- run_cosero(
    project_path = project_path,
    defaults_settings = list(
    STARTDATE = start_date,
    ENDDATE = end_date,
    SPINUP = spinup,
    OUTPUTTYPE = 1
  ),
  statevar_source = statevar_source,
  quiet = TRUE
)

# Extract metrics
initial_nse <- initial_run$output_data$statistics[as.numeric(plot_subbasin), 2]
initial_kge <- initial_run$output_data$statistics[as.numeric(plot_subbasin), 3]
# Get original parameter values
initial_params <- read_parameter_table(param_file, param_names, zone_id = "all")

cat(sprintf("  Initial NSE: %.4f\n", initial_nse))
cat(sprintf("  Initial KGE: %.4f\n\n", initial_kge))

# =============================================================================
# STEP 2: RUN ENSEMBLE SIMULATIONS
# =============================================================================

results_dir <- file.path(project_path, "sensitivity_runoff")
dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)

# Save config and initial run
saveRDS(list(
  n_samples = n_samples, param_names = param_names,
  bounds = bounds, samples = samples,
  generated_date = Sys.time(),
  initial_params = initial_params,
  initial_nse = initial_nse,
  initial_kge = initial_kge
), file.path(results_dir, "config.rds"))

n_total <- nrow(samples$parameter_sets)
batch_size <- 10
n_batches <- ceiling(n_total / batch_size)
batch_dir <- file.path(results_dir, "batches")
if (dir.exists(batch_dir)) unlink(batch_dir, recursive = TRUE)
dir.create(batch_dir)

cat("Running", n_total, "simulations in", n_batches, "batches...\n")


# Execute batches
time_start <- Sys.time()
batch_times <- numeric(n_batches)

for (batch_idx in 1:n_batches) {
  batch_start <- Sys.time()

  start_idx <- (batch_idx - 1) * batch_size + 1
  end_idx <- min(batch_idx * batch_size, n_total)
  batch_samples <- samples$parameter_sets[start_idx:end_idx, ]

  cat(sprintf("Batch %d/%d (simulations %d-%d)... ",
              batch_idx, n_batches, start_idx, end_idx))

  batch_results <- run_cosero_ensemble(
    project_path = project_path,
    parameter_sets = samples$parameter_sets[start_idx:end_idx, ],
    par_bounds = bounds,
    base_settings = list(
      STARTDATE = start_date,
      ENDDATE = end_date,
      SPINUP = spinup,
      OUTPUTTYPE = 1
    ),
    statevar_source = statevar_source,
    quiet = TRUE
  )
  
  saveRDS(batch_results, file.path(batch_dir, sprintf("batch_%04d.rds", batch_idx)))

  
  # Calculate time for this batch
  batch_times[batch_idx] <- as.numeric(difftime(Sys.time(), batch_start, units = "secs"))

  # Display progress with time estimation
  if (batch_idx == 1) {
    avg_batch_time <- batch_times[1]
    cat(sprintf("✓ (%.1fs) | Est. total: %.1f min\n",
                batch_times[1], (avg_batch_time * n_batches) / 60))
  } else {
    avg_batch_time <- mean(batch_times[1:batch_idx], na.rm = TRUE)
    est_remaining <- avg_batch_time * (n_batches - batch_idx)

    if (est_remaining < 60) {
      time_str <- sprintf("%.0fs", est_remaining)
    } else {
      time_str <- sprintf("%.1f min", est_remaining / 60)
    }

    cat(sprintf("✓ (%.1fs) | %.0f%% done | Remaining: %s\n",
                batch_times[batch_idx],
                (batch_idx / n_batches) * 100,
                time_str))
  }
  rm(batch_results); gc(full = TRUE); closeAllConnections()
}

time_elapsed <- as.numeric(difftime(Sys.time(), time_start, units = "secs"))
cat(sprintf("\n✓ Complete: %.1f min (%.2f sec/run)\n\n", time_elapsed/60, time_elapsed/n_total))

# Combine batches
batch_files <- list.files(batch_dir, pattern = "batch_.*\\.rds$", full.names = TRUE)
results <- readRDS(batch_files[1])
for (f in batch_files[-1]) {
  b <- readRDS(f)
  results$results <- c(results$results, b$results)
  results$parameter_sets <- rbind(results$parameter_sets, b$parameter_sets)
  rm(b); gc()
}
results$runtime_minutes <- time_elapsed / 60
saveRDS(results, file.path(results_dir, "ensemble_results.rds"))
unlink(batch_dir, recursive = TRUE)

# =============================================================================
# STEP 3: EXTRACT METRICS & SOBOL ANALYSIS
# =============================================================================

nse <- extract_cosero_metrics(results, subbasin_id = plot_subbasin, metric = "NSE")
kge <- extract_cosero_metrics(results, subbasin_id = plot_subbasin, metric = "KGE")
kge_calc <- calculate_ensemble_metrics(results, subbasin_id = plot_subbasin, metric = "KGE", spinup = spinup)

cat("Metrics summary:\n")
cat(sprintf("  NSE: mean=%.4f, sd=%.4f\n", mean(nse, na.rm=TRUE), sd(nse, na.rm=TRUE)))
cat(sprintf("  KGE: mean=%.4f, sd=%.4f\n\n", mean(kge, na.rm=TRUE), sd(kge, na.rm=TRUE)))

sobol_nse <- calculate_sobol_indices(nse, samples, boot = TRUE, R = 100)
sobol_kge <- calculate_sobol_indices(kge, samples, boot = TRUE, R = 100)

cat("Sobol indices (NSE):\n"); print(sobol_nse); cat("\n")
cat("Sobol indices (KGE):\n"); print(sobol_kge); cat("\n")

# =============================================================================
# STEP 4: VISUALIZATIONS
# =============================================================================

plots_dir <- file.path(results_dir, "plots")
dir.create(plots_dir, showWarnings = FALSE)

# Sobol plots
ggsave(file.path(plots_dir, "sobol_nse.png"), 
       plot_sobol(sobol_nse, title = "Sobol Sensitivity (NSE)"),
       width = 12, height = 7, dpi = 300)

ggsave(file.path(plots_dir, "sobol_kge.png"),
       plot_sobol(sobol_kge, title = "Sobol Sensitivity (KGE)"),
       width = 12, height = 7, dpi = 300)

# Dotty plots
absolute_params <- as.data.frame(samples$parameter_sets)
# Calculate spatial mean for each parameter (exclude NZ_ column)                                                                                                                     initial_params_means <- colMeans(initial_params[, -1])  # Remove first column (NZ_)                                                                                                
initial_params_means <- colMeans(initial_params[, -1])  # Remove first column (NZ_)  
  # Create reference data for dotty plot
  ref_data_nse <- data.frame(
    parameter = names(initial_params_means),   
    value = initial_params_means,              
    output = initial_nse                       
  )
  # Create dotty plot with reference point
  p_dotty_nse <- plot_dotty(absolute_params, Y = nse, y_label = "NSE", n_col = 3, y_min = 0)
  p_dotty_nse <- p_dotty_nse +
    geom_point(data = ref_data_nse, aes(x = value, y = output),
               color = "red", size = 3, shape = 16)
  ggsave(file.path(plots_dir, "dotty_nse.png"), p_dotty_nse, width = 14, height = 10, dpi = 300)


  ref_data_kge <- data.frame(
    parameter = names(initial_params_means),   
    value = initial_params_means,              
    output = initial_kge                       
  )

p_dotty_kge <- plot_dotty(absolute_params, Y = kge, y_label = "KGE", n_col = 3, y_min = 0)
p_dotty_kge <- p_dotty_kge +
    geom_point(data = ref_data_kge, aes(x = value, y = output),
               color = "red", size = 3, shape = 16)
ggsave(file.path(plots_dir, "dotty_kge.png"), p_dotty_kge, width = 14, height = 10, dpi = 300)

# Parameter correlation heatmap
cor_matrix <- cor(absolute_params, use = "complete.obs")
cor_df <- as.data.frame(as.table(cor_matrix)) %>% rename(Param1 = Var1, Param2 = Var2, Correlation = Freq)

p_cor <- ggplot(cor_df, aes(Param1, Param2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 3) +
  scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Parameter Correlation Matrix", x = NULL, y = NULL)
ggsave(file.path(plots_dir, "param_correlation.png"), p_cor, width = 10, height = 8, dpi = 300)

# NSE vs KGE scatter
p_compare <- ggplot(data.frame(NSE = nse, KGE = kge) %>% filter(!is.na(NSE) & !is.na(KGE)),
                    aes(NSE, KGE)) +
  geom_point(alpha = 0.5, color = "#2E86AB") +
  geom_smooth(method = "lm", se = TRUE, color = "#A23B72") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_point(data = data.frame(NSE = initial_nse, KGE = initial_kge),
             aes(NSE, KGE), color = "red", size = 3, shape = 16) +
  theme_minimal(base_size = 14) +
  labs(title = "NSE vs KGE Comparison",
       subtitle = sprintf("r = %.3f, n = %d | Red point = initial parameters",
                          cor(nse, kge, use = "complete.obs"), sum(!is.na(nse) & !is.na(kge))))
ggsave(file.path(plots_dir, "nse_vs_kge.png"), p_compare, width = 10, height = 8, dpi = 300)

# Parameter distribution with performance coloring                                                                                                param_perf_long <- absolute_params %>%                                                                                                          
param_perf_long <- absolute_params %>%        
    mutate(NSE = nse, KGE = kge, run_id = row_number()) %>%
    pivot_longer(cols = all_of(param_names), names_to = "Parameter", values_to = "Value") %>%
    filter(!is.na(NSE))

  # Calculate spatial mean for each parameter (exclude NZ_ column)
  initial_params_means <- colMeans(initial_params[, -1])

  # Create reference data in long format
  initial_params_long <- data.frame(
    Parameter = names(initial_params_means),
    Value = initial_params_means,
    NSE = initial_nse
  )

  p_dist <- ggplot(param_perf_long, aes(Value, NSE)) +
    geom_point(alpha = 0.4, size = 1.5, aes(color = NSE > 0.6)) +
    geom_smooth(method = "loess", se = FALSE, color = "#E63946", linewidth = 0.8) +
    geom_point(data = initial_params_long, aes(Value, NSE), color = "red", size = 3, shape = 16) +
    facet_wrap(~Parameter, scales = "free_x", ncol = 3) +
    scale_color_manual(values = c("FALSE" = "gray60", "TRUE" = "#2A9D8F"),
                       labels = c("NSE ≤ 0.6", "NSE > 0.6"), name = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", strip.text = element_text(face = "bold")) +
    labs(title = "Parameter-Performance Relationships (Red = Initial)", x = "Parameter Value", y = "NSE")
  ggsave(file.path(plots_dir, "param_performance.png"), p_dist, width = 14, height = 12, dpi = 300)

cat("✓ Saved", length(list.files(plots_dir)), "plots to:", plots_dir, "\n\n")

# =============================================================================
# STEP 5: BEST PARAMETERS
# =============================================================================

best_dir <- file.path(results_dir, "best_params")
dir.create(best_dir, showWarnings = FALSE)

best_nse_idx <- which.max(nse)
best_kge_idx <- which.max(kge)
original_values <- read_parameter_table(param_file, param_names, zone_id = 1)

cat("Initial NSE:", round(initial_nse, 4), "| Initial KGE:", round(initial_kge, 4), "\n")
cat("Best NSE:", round(nse[best_nse_idx], 4), "(run", best_nse_idx, ")\n")
cat("Best KGE:", round(kge[best_kge_idx], 4), "(run", best_kge_idx, ")\n\n")

# Export best NSE
file.copy(param_file, file.path(best_dir, "para_BEST_NSE.txt"), overwrite = TRUE)
modify_parameter_table(
  file.path(best_dir, "para_BEST_NSE.txt"),
  as.list(absolute_params[best_nse_idx, ]),
  bounds, original_values
)

# Export best KGE
file.copy(param_file, file.path(best_dir, "para_BEST_KGE.txt"), overwrite = TRUE)
modify_parameter_table(
  file.path(best_dir, "para_BEST_KGE.txt"),
  as.list(absolute_params[best_kge_idx, ]),
  bounds, original_values
)

cat("✓ Exported best parameter files to:", best_dir, "\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("=============================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=============================================================================\n")
cat("Results:", results_dir, "\n")
cat("  • ensemble_results.rds (", round(file.info(file.path(results_dir, "ensemble_results.rds"))$size/1024^2, 2), " MB)\n")
cat("  • plots/ (", length(list.files(plots_dir)), " figures)\n")
cat("  • best_params/ (optimized parameter files)\n")
cat("Runtime:", round(results$runtime_minutes, 2), "min\n")
cat("Initial: NSE =", round(initial_nse, 4), "| KGE =", round(initial_kge, 4), "\n")
cat("Best: NSE =", round(nse[best_nse_idx], 4), "| KGE =", round(kge[best_kge_idx], 4), "\n")

gc(full = TRUE); closeAllConnections()
