# =============================================================================
# Multi-basin DDS calibration on the 36-subbasin KLIRES subset
# =============================================================================
# Project : D:/KLIRES/working files/Model_subset  (used in place)
#
# Basins are renumbered 1..36 in the subset (see basin_id_mapping.csv) and ids
# are unpadded, so a plain "17" works as a target.
#
# Everything meant to change between runs is in the CONFIGURATION block.
# Requires package >= 0.9.8 (logNSE/PDIFF metrics, optimizer file-safety fixes).
# =============================================================================

devtools::load_all()

project_path <- "D:/KLIRES/working files/Model_subset_539"

stopifnot(dir.exists(project_path),
          file.exists(file.path(project_path, "COSERO.exe")),
          file.exists(file.path(project_path, "input", "Defaults.txt")))


# =============================================================================
# CONFIGURATION
# =============================================================================

# --- Target basins and their weights -----------------------------------------
# Names are subbasin ids, values relative weights. They need not sum to 1 --
# they are normalised below. Weight 0 keeps a basin in the report but out of
# the objective.
target_weights <- c(
  "9"  = 1.0
)

# Aggregation across basins:
#   "weighted" - weighted mean using target_weights
#   "mean"     - unweighted mean, ignores target_weights
#   "min"      - optimise the worst basin
#   "product"  - product of metrics, penalises any single bad basin hard
aggregation <- "weighted"

# --- Objective ----------------------------------------------------------------
# "KGE" (balances correlation, variability and bias), "NSE" (high-flow driven),
# "logNSE" (low flows), "PDIFF" (flood peaks), also "rNSE", "RMSE", "PBIAS",
# "VE", "r2", "r". Combine several by giving metric_weights as well.
metric         <- c("NSE","KGE")
metric_weights <- c(0.5,0.5)          # e.g. c(0.6, 0.25, 0.15) for three metrics
metric_args    <- list()        # only PDIFF uses this (n_maxima, window_hours)

# Three-way objective across the flow range -- uncomment both lines together:
# metric         <- c("KGE", "logNSE", "PDIFF")
# metric_weights <- c(0.60, 0.25, 0.15)

# --- Search -------------------------------------------------------------------
# DDS wants roughly 20-50 evaluations per parameter, i.e. ~480-1200 runs for
# the 24 parameters below. Start small to time a run, then scale up.
n_iter <- 200
dds_r  <- 0.2      # perturbation size (0.2 = default)
seed   <- 42       # DDS is stochastic; fix for reproducibility

# --- Output naming ------------------------------------------------------------
# The optimizer writes its own timestamped files; these are the stable copies.
out_para_name <- "para_opt"    # -> output/para_opt.txt
out_stat_name <- "stat_opt"    # -> output/stat_opt.txt
out_dir       <- file.path(project_path, "output")

# --- Calibration settings -----------------------------------------------------
# OUTPUTTYPE = 0 is calibration mode (runoff + statistics only), skipping the
# large COSERO.plus writes. Needs the Lhotse build; try 1 if a run fails.
cal_settings <- list(
  STARTDATE  = c(2000, 1, 1, 0, 0),
  ENDDATE    = c(2020, 12, 31, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 0,
  PARAFILE   = "para_13605_apriori_opt_transfer.txt"
)

# --- Parameters to calibrate --------------------------------------------------
# All 24 verified to resolve against a column of the parameter file and to have
# bounds in inst/extdata/parameter_bounds.csv. PCOR/TCOR are not columns of
# this file (it carries monthly PCor1..12 / TCor1..12), so SNOWCOR and RAINCOR
# are the handles on precipitation volume bias.
param_names <- c(
  # Precipitation / snow input volume (4)
  "SNOWCOR", "RAINCOR", "SNOWTRT", "RAINTRT","ETSLPCOR",
  # Snow melt (3)
  "CTMAX", "CTMIN", "TVAR",
  # Soil / runoff generation (5)
  "M", "BETA", "FKFAK", "H1", "H2",
  # Routing and recession (6)
  "KBF", "TAB1", "TAB2", "TAB3", "TVS1", "TVS2","TAB4","TAB5",
  # Disaggregation (5): only active when NDC > 1; this project runs NDC 3-5.
  # The CVs are SOILVAR_B / HYDROVAR_B / CTVAR_B in the file -- give the plain
  # names here, find_parameter_column() maps them. LAPSE_P (not LAPSE_B) is
  # the precipitation lapse rate.
  "LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR",
  # Hydraulic lift (1): 0 = original model unless calibration turns it on.
  "FHL"
)

# =============================================================================
# END OF CONFIGURATION
# =============================================================================


# --- 1  Resolve and validate --------------------------------------------------
target_subbasins <- names(target_weights)
subbasin_weights <- as.numeric(target_weights)

stopifnot(length(target_subbasins) >= 1,
          !any(is.na(subbasin_weights)),
          all(subbasin_weights >= 0))

if (aggregation == "weighted") {
  if (sum(subbasin_weights) <= 0) {
    stop("target_weights must contain at least one positive weight")
  }
  subbasin_weights <- subbasin_weights / sum(subbasin_weights)
}

# Weights only mean something for "weighted"; passing them otherwise makes the
# optimizer warn that they are ignored.
weights_arg <- if (aggregation == "weighted") subbasin_weights else NULL

if (length(metric) > 1 && is.null(metric_weights)) {
  stop("metric_weights must be supplied when combining several metrics")
}
if (!is.null(metric_weights) && length(metric_weights) != length(metric)) {
  stop("metric_weights must have one entry per metric")
}

cat("\n--- configuration ---\n")
print(data.frame(subbasin = target_subbasins,
                 weight   = round(subbasin_weights, 4)))
cat("Aggregation :", aggregation, "\n")
cat("Metric      :", paste(metric, collapse = " + "), "\n")
cat("Iterations  :", n_iter, "\n")

par_bounds <- load_parameter_bounds(parameters = param_names)

# load_parameter_bounds() silently drops names it cannot find, so a typo would
# otherwise just calibrate fewer parameters with nothing saying so.
missing_bounds <- setdiff(param_names, par_bounds$parameter)
if (length(missing_bounds)) {
  stop("No bounds in parameter_bounds.csv for: ",
       paste(missing_bounds, collapse = ", "))
}

cat("\n--- parameter bounds ---\n")
print(as.data.frame(par_bounds[, c("parameter", "min", "max",
                                   "modification_type")]))


# --- 2  Optimize --------------------------------------------------------------
# No separate baseline run: optimize_cosero_dds() runs the a-priori model
# itself and returns those numbers as opt$initial_metrics.
set.seed(seed)
t0 <- Sys.time()

opt <- optimize_cosero_dds(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = target_subbasins,
  metric            = metric,
  metric_weights    = metric_weights,
  metric_args       = metric_args,
  subbasin_weights  = weights_arg,
  aggregation       = aggregation,
  defaults_settings = cal_settings,
  max_iter          = n_iter,
  r                 = dds_r,
  verbose           = TRUE
)

opt_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cat(sprintf("\nDDS finished in %.1f min (%.2f min/run)\n",
            opt_min, opt_min / n_iter))


# --- 3  Results ---------------------------------------------------------------
# DDS minimises internally, so the achieved metric is -opt$value.
cat("\n=============================================================\n")
cat(sprintf("Objective (%s, %s): %.4f\n",
            paste(metric, collapse = "+"), aggregation, -opt$value))
cat("=============================================================\n")

if (!is.null(opt$initial_metrics)) {
  cat("\nPer-basin, a-priori:\n");  print(opt$initial_metrics)
}
if (!is.null(opt$final_metrics)) {
  cat("\nPer-basin, optimized:\n"); print(opt$final_metrics)
}

cat("\n--- parameters: a-priori vs optimized ---\n")
print(as.data.frame(opt$par_bounds[, c("parameter", "default",
                                       "optimal_value")]))

# Parameters pinned to a bound usually mean the bound is too tight.
ob <- as.data.frame(opt$par_bounds)
at_bound <- abs(ob$optimal_value - ob$min) < 1e-6 * pmax(1, abs(ob$min)) |
            abs(ob$optimal_value - ob$max) < 1e-6 * pmax(1, abs(ob$max))
if (any(at_bound, na.rm = TRUE)) {
  cat("\nNOTE - pinned to a bound (consider widening):\n")
  print(ob[which(at_bound), c("parameter", "min", "optimal_value", "max")])
}


# --- 4  Save under the configured names ---------------------------------------
para_out <- file.path(out_dir, paste0(out_para_name, ".txt"))
stat_out <- file.path(out_dir, paste0(out_stat_name, ".txt"))

stopifnot(file.exists(opt$optimized_par_file))
file.copy(opt$optimized_par_file, para_out, overwrite = TRUE)
cat("\nOptimized parameters ->", para_out, "\n")

# statistics.txt is written by COSERO on the final (optimized) run.
stat_src <- file.path(out_dir, "statistics.txt")
if (file.exists(stat_src)) {
  file.copy(stat_src, stat_out, overwrite = TRUE)
  cat("Final-run statistics ->", stat_out, "\n")
} else {
  warning("statistics.txt not found in ", out_dir, " -- ",
          basename(stat_out), " not written")
}

export_cosero_optimization(opt, output_dir = out_dir)

cat("\nOptimizer's timestamped copies:\n  ", opt$optimized_par_file,
    "\n  ", opt$report_file, "\n")


# =============================================================================
# To re-run the calibrated set over another period, copy it into input/ (that
# is where run_cosero() resolves PARAFILE) and point PARAFILE at it:
#
# file.copy(para_out, file.path(project_path, "input", "para_opt.txt"),
#           overwrite = TRUE)
# chk <- run_cosero(project_path,
#                   defaults_settings = modifyList(cal_settings,
#                     list(PARAFILE  = "para_opt.txt",
#                          STARTDATE = c(2013, 1, 1, 0, 0),
#                          ENDDATE   = c(2020, 12, 31, 0, 0))))
# sapply(target_subbasins, function(sb)
#   calculate_run_metrics(chk, subbasin_id = sb, metric = "KGE"))
# =============================================================================
