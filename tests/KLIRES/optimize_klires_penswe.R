# =============================================================================
# DDS calibration with the PenSWE snow-accumulation penalty
# =============================================================================
# Project : D:/KLIRES/working files/Model/Model_glaciers_01  (used in place)
#
# 403 subbasins (one zone each, all glacierized, NDC 5). Most are ungauged, so
# discharge metrics only see the few gauged ones -- PenSWE is what constrains
# snow in the rest. The objective is
#
#   -(NSE, mean over gauged subbasins)
#     + weight * mean over ALL subbasins of mean over years of
#                max(0, SWE on 31.08 - 5 mm)
#
# The a-priori model scores PenSWE = 113.84 mm (2001-2020), driven by ~20
# subbasins that gain 60-370 mm of snow per year (see test_penswe.R).
#
# PenSWE needs output/swwgeb.txt, which only the new COSERO build writes. The
# optimizer always runs COSERO.exe, so that build must be the one named
# COSERO.exe -- the baseline run stops with a clear error otherwise.
#
# Everything meant to change between runs is in the CONFIGURATION block.
# =============================================================================

devtools::load_all()

project_path <- "D:/KLIRES/working files/Model/Model_glaciers_01"

stopifnot(dir.exists(project_path),
          file.exists(file.path(project_path, "COSERO.exe")),
          file.exists(file.path(project_path, "input", "Defaults.txt")))


# =============================================================================
# CONFIGURATION
# =============================================================================

# --- Target basins ------------------------------------------------------------
# "all" = every subbasin in the parameter file: all zones are calibrated,
# discharge metrics use the gauged ones, PenSWE covers every one of them.
target_subbasins <- "4"

# Aggregation across basins. "mean" is the sensible choice for 403 subbasins;
# PenSWE also uses the plain mean unless aggregation is "weighted".
aggregation <- "mean"

# --- Objective ----------------------------------------------------------------
# Discharge metrics only -- PenSWE is NOT a metric and does not go here; it is
# set under Penalty below and added on top of whatever metric is chosen.
metric         <- c("NSE", "logNSE")
metric_weights <- c(0.5, 0.5) # only needed for several metrics, e.g. c("NSE", "KGE")
metric_args    <- list()        # only PDIFF uses this (n_maxima, window_hours)

# --- Penalty ------------------------------------------------------------------
# objective = NSE - weight * PenSWE   (PenSWE in mm, NSE unitless)
#
# weight converts mm of snow into NSE units: "how much NSE would I give up to
# remove 1 mm of mean excess on 31.08?" It is not a share like
# metric_weights -- NSE keeps its full weight, and once every subbasin is
# below the threshold the penalty is 0 and the calibration is pure NSE.
#   weight 0.001 -> the a-priori 113.84 mm cost 0.114 NSE
#   weight 0.005 -> the same snow costs 0.57 NSE (snow matters about as much
#                   as the fit itself)
# Raise it if the optimizer keeps snow towers for small NSE gains.
# threshold: SWE (mm) allowed on 31.08 without penalty.
#
# use_penswe = FALSE calibrates on NSE alone (e.g. for a comparison run);
# swwgeb.txt is then not read at all and any COSERO.exe build works.
use_penswe       <- TRUE
penswe_weight    <- 0.001
penswe_threshold <- 5      # mm

penalty <- if (use_penswe) {
  list(PenSWE = list(weight = penswe_weight, threshold = penswe_threshold))
} else {
  NULL
}

# --- Search -------------------------------------------------------------------
# DDS wants roughly 20-50 evaluations per parameter, i.e. ~540-1350 runs for
# the 27 parameters below. Start small to time a run, then scale up.
n_iter <- 100
dds_r  <- 0.2      # perturbation size (0.2 = default)
seed   <- 42       # DDS is stochastic; fix for reproducibility

# --- Output naming ------------------------------------------------------------
out_para_name <- "para_opt_penswe"    # -> output/para_opt_penswe.txt
out_stat_name <- "stat_opt_penswe"    # -> output/stat_opt_penswe.txt
out_dir       <- file.path(project_path, "output")

# --- Calibration settings -----------------------------------------------------
# OUTPUTTYPE = 0: runoff + statistics (+ swwgeb.txt with the new build) only.
cal_settings <- list(
  STARTDATE  = c(2000, 1, 1, 0, 0),
  ENDDATE    = c(2020, 12, 31, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 0,
  PARAFILE   = "para_13605_level6_explore_opt.txt"
)

# --- Parameters to calibrate --------------------------------------------------
# Same set as optimize_klires_subset.R. The snow handles (SNOWCOR, SNOWTRT,
# CTMAX, CTMIN, TVAR, CTVAR, LAPSE_T) are the ones PenSWE acts on most.
param_names <- c(
  # Precipitation / snow input volume
  "SNOWTRT", "RAINTRT", "ETSLPCOR",
  # Snow melt
  "CTMAX", "CTMIN", "NVAR",
  # Soil / runoff generation
  "M", "BETA", "FKFAK", "H1", "H2",
  # Routing and recession
  "KBF", "TAB1", "TAB2", "TAB3", "TVS1", "TVS2", "TAB4", "TAB5",
  # Disaggregation (active with NDC > 1; this project runs NDC 5)
  "LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR",
  # Hydraulic lift
  "FHL"
)

# =============================================================================
# END OF CONFIGURATION
# =============================================================================


# --- 1  Resolve and validate --------------------------------------------------
if (length(metric) > 1 && is.null(metric_weights)) {
  stop("metric_weights must be supplied when combining several metrics")
}
if (!is.null(metric_weights) && length(metric_weights) != length(metric)) {
  stop("metric_weights must have one entry per metric")
}

cat("\n--- configuration ---\n")
cat("Subbasins   :", paste(target_subbasins, collapse = ", "), "\n")
cat("Aggregation :", aggregation, "\n")
cat("Metric      :", paste(metric, collapse = " + "), "\n")
cat("Penalty     :", if (is.null(penalty)) "none" else
      sprintf("PenSWE (weight %g, threshold %g mm)",
              penalty$PenSWE$weight, penalty$PenSWE$threshold), "\n")
cat("Iterations  :", n_iter, "\n")

par_bounds <- load_parameter_bounds(parameters = param_names)

# load_parameter_bounds() silently drops names it cannot find
missing_bounds <- setdiff(param_names, par_bounds$parameter)
if (length(missing_bounds)) {
  stop("No bounds in parameter_bounds.csv for: ",
       paste(missing_bounds, collapse = ", "))
}

cat("\n--- parameter bounds ---\n")
print(as.data.frame(par_bounds[, c("parameter", "min", "max",
                                   "modification_type")]))


# --- 2  Optimize --------------------------------------------------------------
# The a-priori run happens inside optimize_cosero_dds(): its metrics are
# opt$initial_metrics and its PenSWE opt$initial_penswe.
set.seed(seed)
t0 <- Sys.time()

opt <- optimize_cosero_dds(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = target_subbasins,
  metric            = metric,
  metric_weights    = metric_weights,
  metric_args       = metric_args,
  aggregation       = aggregation,
  penalty           = penalty,
  defaults_settings = cal_settings,
  max_iter          = n_iter,
  r                 = dds_r,
  verbose           = TRUE
)

opt_min <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
cat(sprintf("\nDDS finished in %.1f min (%.2f min/run)\n",
            opt_min, opt_min / n_iter))


# --- 3  Results ---------------------------------------------------------------
# DDS minimises internally: -opt$value = metric - weight * PenSWE.
cat("\n=============================================================\n")
cat(sprintf("Objective (%s%s, %s): %.4f\n",
            paste(metric, collapse = "+"),
            if (is.null(penalty)) "" else " - PenSWE",
            aggregation, -opt$value))
cat("=============================================================\n")

# The two parts of the best evaluation
best <- opt$eval_log[which.min(opt$eval_log$objective), ]
cat(sprintf("Best evaluation #%d: metric %.4f | PenSWE %.2f mm\n",
            best$eval, best$metric, best$penswe))

# Discharge metrics exist only for gauged subbasins
gauged_summary <- function(m) {
  if (is.null(m)) return(NULL)
  m <- m[rowSums(!is.na(m)) > 0, , drop = FALSE]
  cat(sprintf("  %d gauged subbasins, mean: %s\n", nrow(m),
              paste(sprintf("%s = %.4f", colnames(m), colMeans(m, na.rm = TRUE)),
                    collapse = ", ")))
}
cat("\nA-priori:\n");  gauged_summary(opt$initial_metrics)
cat("Optimized:\n");   gauged_summary(opt$final_metrics)

if (!is.null(opt$initial_penswe)) {
  cat(sprintf("\nPenSWE: %.2f mm -> %.2f mm\n", opt$initial_penswe$value,
              if (is.null(opt$final_penswe)) NA else opt$final_penswe$value))
}

if (!is.null(opt$initial_penswe) && !is.null(opt$final_penswe)) {
  swe_cmp <- merge(
    opt$initial_penswe$per_subbasin[, c("subbasin", "mean_excess")],
    opt$final_penswe$per_subbasin[, c("subbasin", "mean_excess", "max_swe")],
    by = "subbasin", suffixes = c("_initial", "_optimized")
  )
  swe_cmp <- swe_cmp[order(-swe_cmp$mean_excess_initial), ]
  cat("\nWorst a-priori subbasins, mean excess on 31.08 (mm):\n")
  print(head(swe_cmp, 20), row.names = FALSE)
}

cat("\n--- parameters: a-priori vs optimized ---\n")
print(as.data.frame(opt$par_bounds[, c("parameter", "default",
                                       "optimal_value")]))

# Parameters pinned to a bound usually mean the bound is too tight
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

stat_src <- file.path(out_dir, "statistics.txt")
if (file.exists(stat_src)) {
  file.copy(stat_src, stat_out, overwrite = TRUE)
  cat("Final-run statistics ->", stat_out, "\n")
} else {
  warning("statistics.txt not found in ", out_dir, " -- ",
          basename(stat_out), " not written")
}

# Also writes penswe_per_subbasin.csv and evaluation_log.csv
export_cosero_optimization(opt, output_dir = out_dir)

cat("\nOptimizer's timestamped copies:\n  ", opt$optimized_par_file,
    "\n  ", opt$report_file, "\n")
