# =============================================================================
# DDS calibration of the lake test models, incl. the lake parameter TABLAKE
# =============================================================================
# Models : D:/KLIRES/working files/Trimmed models/Model_trimmed_headwater_lake20
#          5 single-zone Finnish headwaters, one folder NB_<NB_> each, every one
#          a complete COSERO project (own COSERO.exe + DLLs). Used in place.
#          The subbasin inside each model is always "1".
#
# | NB_  | basin    | km2  | lake share | TABLAKE_ | NDC_ | a-priori NSE / KGE / beta |
# |------|----------|------|------------|----------|------|---------------------------|
# | 8636 | FI000217 |  181 | 0.254      | 104 h    | 1    | -0.62 / 0.16 / 1.10       |
# | 8661 | FI000203 | 1477 | 0.225      | 418 h    | 2    | -0.43 / 0.42 / 0.68       |
# | 8678 | FI000384 |  315 | 0.208      | 113 h    | 2    | -0.24 / -0.17 / 0.57      |
# | 8761 | FI000361 |  955 | 0.278      | 395 h    | 2    | -0.28 / 0.10 / 0.67       |
# | 8839 | FI000526 |  665 | 0.211      | 159 h    | 2    | -1.55 / 0.10 / 0.59       |
#
# The lake module (COSEROgit, run_blend_waterbody_B.f): WATERBODY_ is the lake
# fraction; its share runs through a linear reservoir Q_out = V / TABLAKE_
# (hours, like TAB4_) -- but only where TABLAKE_ > 0 AND AREF_LAKE_ > 0.
# TABLAKE_ is the one calibratable lake parameter; KELAKE_ (open-water ET
# factor), BWLAKEINI_, AREF_LAKE_, VREF_LAKE_ and PLAKE_ stay fixed inputs.
#
# Parameters start from para_13605_apriori_lake.txt as is, with RAINCOR_ =
# SNOWCOR_ = 1. Precipitation correction is deliberately NOT calibrated, so the
# low a-priori beta (0.57-0.68 in 4 of 5) has to be closed on the ET side.
#
# Each model is optimized independently; with n_workers > 1 they run in
# parallel, one model per R worker, each logging to its own output/ folder.
#
# One COSERO run (2000-2025, one zone, OUTPUTTYPE 0) takes ~1 s, so 1000 DDS
# iterations are roughly 20-30 min per model.
#
# Everything meant to change between runs is in the CONFIGURATION block.
# =============================================================================

devtools::load_all()

# Workers load the package from here -- run the script from the package root.
pkg_root <- getwd()
stopifnot(file.exists(file.path(pkg_root, "DESCRIPTION")))

models_root <- "D:/KLIRES/working files/Trimmed models/Model_trimmed_headwater_lake20"
stopifnot(dir.exists(models_root),
          file.exists(file.path(models_root, "basin_index.csv")))


# =============================================================================
# CONFIGURATION
# =============================================================================

# --- Models -------------------------------------------------------------------
# One folder, several (c("NB_8661", "NB_8761")) or "all" NB_* folders.
# NB_8636: NDC_ = 1, so the disaggregation parameters drop out (23 remain),
# and unlike the other four its a-priori beta is too HIGH (1.10).
models <- "NB_8636"

# --- Objective ----------------------------------------------------------------
# Any metrics work; the summary gets an a-priori/optimized pair for each, plus
# COSERO's NSE / KGE / beta of the optimized run. KGE would also carry the
# volume (beta) error the a-priori runs have.
metric         <- c("NSE", "logNSE")
metric_weights <- c(0.7, 0.3)
metric_args    <- list()        # only PDIFF uses this (n_maxima, window_hours)

# --- Search -------------------------------------------------------------------
# DDS wants roughly 20-50 evaluations per parameter, i.e. ~600-1500 runs for
# the ~30 parameters below. Start small to time a run, then scale up.
n_iter <- 1000
dds_r  <- 0.2      # perturbation size (0.2 = default)
seed   <- 42       # DDS is stochastic; fix for reproducibility

# --- Parallel -----------------------------------------------------------------
# Only matters with several models. 1 = sequential, optimizer output on the
# console. > 1 = that many models at once, console output only in each model's
# output/opt_lake_log.txt.
n_workers <- 4

# --- Output naming (per model, in <model>/output/) ----------------------------
out_para_name <- "para_opt_lake"    # -> output/para_opt_lake.txt
out_stat_name <- "stat_opt_lake"    # -> output/stat_opt_lake.txt
summary_csv   <- file.path(models_root, "lake20_optimization_summary.csv")

# --- Calibration settings -----------------------------------------------------
# The full period the models were built for. OUTPUTTYPE = 0 is calibration
# mode (runoff + statistics only).
cal_settings <- list(
  STARTDATE  = c(2000, 1, 1, 0, 0),
  ENDDATE    = c(2025, 12, 31, 0, 0),
  SPINUP     = 365,
  OUTPUTTYPE = 0,
  PARAFILE   = "para_13605_apriori_lake.txt"
)

# --- Parameters to calibrate --------------------------------------------------
# Not here on purpose:
#   RAINCOR, SNOWCOR, PCOR - fixed (precipitation stays as observed)
#   ETVEGCOR               - fixed a-priori lookup; do not calibrate with FHL
#   GLAC_CT                - glacier routine is off (NC_ 5-6, no NC_ = 8)
#   KELAKE & co.           - fixed lake inputs, see header
param_names <- c(
  # Lake reservoir
  "TABLAKE",
  # Evapotranspiration -- the volume handles, since P is not corrected
  "ETSYSCOR", "ETSLPCOR", "FKFAK", "FHL",
  # Snow
  "SNOWTRT", "RAINTRT", "CTMAX", "CTMIN", "NVAR", "TVAR",
  # Soil / runoff generation
  "M", "BETA", "H1", "H2",
  # Routing and recession
  "KBF", "TAB1", "TAB2", "TAB3", "TVS1", "TVS2", "TAB5",
  # Disaggregation -- dropped automatically for a model with NDC_ = 1
  "LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR"
)
disagg_params <- c("LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR")

# The a-priori file holds values outside parameter_bounds.csv (BETA 11, H2 55,
# TAB1 71, TAB5 14-20, TVS2 0.1, SNOWTRT -2.84, ETSLPCOR 1.35). Every run clamps
# to [min, max], so without widening the optimizer could not even reproduce
# the a-priori model. TRUE stretches each bound just far enough to include
# that model's own a-priori values.
widen_to_apriori <- TRUE

# Bounds that replace the parameter_bounds.csv ones for this calibration,
# as c(min, max). Applied before widen_to_apriori, which can still stretch
# them to a model's a-priori value (the lake20 a-priori values fit: LAPSE_T
# -0.72 .. -0.44, LAPSE_P 0.10 .. 0.15).
bound_overrides <- list(
  LAPSE_T = c(-1.2, 0),     # CSV: -1.2 .. 1.0 -- no inversions (T rising with height)
  LAPSE_P = c(-0.2, 0.2)    # CSV: -0.5 .. 0.5
)

# =============================================================================
# END OF CONFIGURATION
# =============================================================================


# --- 1  Resolve models and validate -------------------------------------------
all_models <- sort(basename(list.dirs(models_root, recursive = FALSE)))
all_models <- grep("^NB_[0-9]+$", all_models, value = TRUE)
if (identical(models, "all")) models <- all_models

unknown <- setdiff(models, all_models)
if (length(unknown)) stop("No such model folder: ", paste(unknown, collapse = ", "))

for (m in models) {
  md <- file.path(models_root, m)
  stopifnot(file.exists(file.path(md, "COSERO.exe")),
            file.exists(file.path(md, "input", cal_settings$PARAFILE)))
}

if (length(metric) > 1 && is.null(metric_weights)) {
  stop("metric_weights must be supplied when combining several metrics")
}
if (!is.null(metric_weights) && length(metric_weights) != length(metric)) {
  stop("metric_weights must have one entry per metric")
}

# load_parameter_bounds() silently drops names it cannot find
base_bounds <- load_parameter_bounds(parameters = param_names)
missing_bounds <- setdiff(param_names, base_bounds$parameter)
if (length(missing_bounds)) {
  stop("No bounds in parameter_bounds.csv for: ",
       paste(missing_bounds, collapse = ", "))
}

for (p in names(bound_overrides)) {
  i <- which(base_bounds$parameter == p)
  if (!length(i)) next   # overridden but not calibrated
  lim <- bound_overrides[[p]]
  stopifnot(length(lim) == 2, lim[1] < lim[2])
  base_bounds$min[i] <- lim[1]
  base_bounds$max[i] <- lim[2]
  if ("sample_min" %in% names(base_bounds)) base_bounds$sample_min[i] <- lim[1]
  if ("sample_max" %in% names(base_bounds)) base_bounds$sample_max[i] <- lim[2]
}

basin_index <- utils::read.csv(file.path(models_root, "basin_index.csv"))

cat("\n--- configuration ---\n")
cat("Models      :", paste(models, collapse = ", "), "\n")
cat("Metric      :", paste(metric, collapse = " + "), "\n")
cat("Iterations  :", n_iter, "per model\n")
cat("Workers     :", min(n_workers, length(models)), "\n")


# --- 2  One model -------------------------------------------------------------
# Bounds for one model: drop what is inert there, widen to its a-priori values.
model_bounds <- function(para, base_bounds) {
  pb <- base_bounds

  if (all(para$NDC_ <= 1)) pb <- pb[!pb$parameter %in% disagg_params, ]

  # A parameter without a column would only produce warnings on every run
  has_col <- vapply(pb$parameter, function(p)
    length(find_parameter_column(p, names(para))) > 0, logical(1))
  if (any(!has_col)) {
    message("Not in the parameter file, skipped: ",
            paste(pb$parameter[!has_col], collapse = ", "))
    pb <- pb[has_col, ]
  }

  pb$widened <- ""
  if (widen_to_apriori) {
    for (i in seq_len(nrow(pb))) {
      cols <- find_parameter_column(pb$parameter[i], names(para), return_all = TRUE)
      v <- unlist(para[, cols, drop = FALSE])
      v <- v[is.finite(v)]
      if (!length(v)) next
      if (min(v) < pb$min[i]) { pb$min[i] <- min(v); pb$widened[i] <- "min" }
      if (max(v) > pb$max[i]) { pb$max[i] <- max(v); pb$widened[i] <- "max" }
    }
    # Sensitivity-style bounds carry their own sampling range; keep it in step
    if ("sample_min" %in% names(pb)) pb$sample_min <- pmin(pb$sample_min, pb$min)
    if ("sample_max" %in% names(pb)) pb$sample_max <- pmax(pb$sample_max, pb$max)
  }
  pb
}

optimize_lake_model <- function(model) {
  model_dir <- file.path(models_root, model)
  out_dir   <- file.path(model_dir, "output")
  log_file  <- file.path(out_dir, "opt_lake_log.txt")

  # split = TRUE: console as well when sequential; workers have no console
  sink(log_file, split = TRUE)
  on.exit(sink(), add = TRUE)

  t0  <- Sys.time()
  row <- data.frame(model = model, status = "failed", stringsAsFactors = FALSE)

  tryCatch({
    para <- read_cosero_parameters(
      file.path(model_dir, "input", cal_settings$PARAFILE), quiet = TRUE)

    lake_on <- all(para$TABLAKE_ > 0 & para$AREF_LAKE_ > 0)
    if (!lake_on) {
      warning(model, ": TABLAKE_ or AREF_LAKE_ is 0 -- lake reservoir off, ",
              "TABLAKE has no effect")
    }

    pb <- model_bounds(para, base_bounds)

    cat("\n=============================================================\n")
    cat(model, "-", nrow(pb), "parameters, NDC_ =", max(para$NDC_), "\n")
    cat("=============================================================\n")
    print(as.data.frame(pb[, c("parameter", "min", "max",
                               "modification_type", "widened")]))

    set.seed(seed)
    opt <- optimize_cosero_dds(
      cosero_path       = model_dir,
      par_bounds        = pb[, setdiff(names(pb), "widened")],
      target_subbasins  = "1",
      metric            = metric,
      metric_weights    = metric_weights,
      metric_args       = metric_args,
      aggregation       = "mean",
      defaults_settings = cal_settings,
      max_iter          = n_iter,
      r                 = dds_r,
      verbose           = TRUE
    )

    # Save under the configured names
    para_out <- file.path(out_dir, paste0(out_para_name, ".txt"))
    stat_out <- file.path(out_dir, paste0(out_stat_name, ".txt"))
    stopifnot(file.exists(opt$optimized_par_file))
    file.copy(opt$optimized_par_file, para_out, overwrite = TRUE)

    # statistics.txt is written by COSERO on the final (optimized) run
    stat_src <- file.path(out_dir, "statistics.txt")
    final_stat <- NULL
    if (file.exists(stat_src)) {
      file.copy(stat_src, stat_out, overwrite = TRUE)
      final_stat <- read_cosero_statistics(stat_out)
    }

    export_cosero_optimization(opt, output_dir = out_dir)

    ob <- as.data.frame(opt$par_bounds)
    at_bound <- abs(ob$optimal_value - ob$min) < 1e-6 * pmax(1, abs(ob$min)) |
                abs(ob$optimal_value - ob$max) < 1e-6 * pmax(1, abs(ob$max))

    row <- data.frame(model = model, status = "ok", n_params = nrow(ob),
                      objective = -opt$value, stringsAsFactors = FALSE)

    # One a-priori / optimized pair per calibration metric, whichever are set
    metric_value <- function(m, name) {
      if (is.null(m) || !name %in% colnames(m)) NA else m[1, name]
    }
    for (mt in metric) {
      row[[paste0(mt, "_apriori")]] <- metric_value(opt$initial_metrics, mt)
      row[[paste0(mt, "_opt")]]     <- metric_value(opt$final_metrics, mt)
    }

    # COSERO's own statistics of the optimized run, whatever the objective was
    for (s in c("NSE", "KGE", "BETA")) {
      row[[paste0("stat_", s, "_opt")]] <-
        if (!is.null(final_stat)) final_stat[[s]][1] else NA
    }

    row$TABLAKE_apriori <- para$TABLAKE_[1]
    row$TABLAKE_opt     <- ob$optimal_value[ob$parameter == "TABLAKE"]
    row$at_bound        <- paste(ob$parameter[which(at_bound)], collapse = " ")
    row$minutes         <- as.numeric(difftime(Sys.time(), t0, units = "mins"))

    # a-priori -> optimized means are in the optimizer's report above;
    # "default" here is only the CSV default, not this model's a-priori value
    cat("\n--- bounds and optimized values ---\n")
    print(ob[, c("parameter", "min", "optimal_value", "max")])
    if (any(at_bound, na.rm = TRUE)) {
      cat("\nNOTE - pinned to a bound (consider widening):",
          row$at_bound, "\n")
    }
  }, error = function(e) {
    cat("\nERROR in", model, ":", conditionMessage(e), "\n")
    row$error <<- conditionMessage(e)
  })

  row
}


# --- 3  Optimize --------------------------------------------------------------
t_all <- Sys.time()
n_workers <- min(n_workers, length(models))

if (n_workers > 1) {
  cl <- parallel::makeCluster(n_workers)
  results <- tryCatch({
    parallel::clusterExport(cl, c(
      "pkg_root", "models_root", "cal_settings", "metric", "metric_weights",
      "metric_args", "n_iter", "dds_r", "seed", "base_bounds", "disagg_params",
      "widen_to_apriori", "out_para_name", "out_stat_name", "model_bounds"
    ))
    parallel::clusterEvalQ(cl, suppressMessages(
      devtools::load_all(pkg_root, quiet = TRUE)))

    cat("\nRunning", length(models), "models on", n_workers,
        "workers -- follow progress in <model>/output/opt_lake_log.txt\n")
    parallel::parLapply(cl, models, optimize_lake_model)
  }, finally = parallel::stopCluster(cl))
} else {
  results <- lapply(models, optimize_lake_model)
}

cat(sprintf("\nAll models finished in %.1f min\n",
            as.numeric(difftime(Sys.time(), t_all, units = "mins"))))


# --- 4  Summary ---------------------------------------------------------------
all_cols <- unique(unlist(lapply(results, names)))
summary_df <- do.call(rbind, lapply(results, function(r) {
  r[setdiff(all_cols, names(r))] <- NA
  r[all_cols]
}))

# Attach the gauge id and lake share from the build list
summary_df <- merge(
  basin_index[, c("folder", "basin_id", "WATERBODY_", "DFZON_")],
  summary_df, by.x = "folder", by.y = "model", all.y = TRUE
)
names(summary_df)[names(summary_df) == "folder"] <- "model"

cat("\n=============================================================\n")
cat("Lake20 calibration summary\n")
cat("=============================================================\n")
num <- vapply(summary_df, is.numeric, logical(1))
print_df <- summary_df
print_df[num] <- lapply(print_df[num], round, 3)
print(print_df, row.names = FALSE)

utils::write.csv(summary_df, summary_csv, row.names = FALSE)
cat("\nSummary ->", summary_csv, "\n")
cat("Per model: output/", out_para_name, ".txt, output/", out_stat_name,
    ".txt, output/opt_lake_log.txt and the optimizer's report\n", sep = "")


# =============================================================================
# To check a calibrated model by hand, copy its set into input/ (that is where
# run_cosero() resolves PARAFILE) and run it:
#
# md <- file.path(models_root, "NB_8661")
# file.copy(file.path(md, "output", "para_opt_lake.txt"),
#           file.path(md, "input", "para_opt_lake.txt"), overwrite = TRUE)
# chk <- run_cosero(md, defaults_settings = modifyList(cal_settings,
#                     list(PARAFILE = "para_opt_lake.txt", OUTPUTTYPE = 3)))
# calculate_run_metrics(chk, subbasin_id = "1", metric = "KGE")
# =============================================================================
