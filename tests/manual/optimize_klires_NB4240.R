# =============================================================================
# DDS calibration of a single headwater subbasin (NB 4240) -- KLIRES Europe
# =============================================================================
# Project : D:/KLIRES/working files/Model
# Target  : NB 4240 -- verified headwater (nothing drains in, FACC = 1),
#           drains to NB 4283, has exactly ONE zone (NZ 4240, IZ 1), so the
#           25 calibrated parameters map to 25 values with no zone averaging.
#           Area 776 km2, mean elevation 208 m, NDC = 2.
#
# A-priori performance (from output/.cache/station_metrics.csv, spinup 365):
#           NSE -0.15 | KGE 0.16 | r 0.62 | alpha 1.16 | BETA 1.73 | n 3328
#           -> the dominant error is a 73 % over-estimation of mean flow,
#              which is why the objective below is KGE and not NSE.
#
# PREREQUISITE -- the model executable must be named COSERO.exe in the project
# directory. optimize_cosero_dds() does not expose run_cosero()'s exe_name
# argument, so the default "COSERO.exe" is always what gets run; with only
# COSERO_Lhotse_release.exe present every iteration fails with
# "COSERO executable not found" (or, once found by another name, with
# "is not recognized as an internal or external command", because the
# generated .bat calls the bare name and cmd.exe searches PATH, not ".").
#
# The errors previously seen in errorfile.log / run_log.txt were STALE
# (Aug 27 and earlier): MetDefaults.txt already points at the correct
# precipitation file, and every NDC value in para_13605_apriori.txt is a valid
# 1-5, NB 6339 included. A direct run_cosero() call completed in 1.3 min for
# 2 years with OUTPUTTYPE = 1.
# =============================================================================

devtools::load_all()

project_path <- "D:/KLIRES/working files/Model"

# The package runs "COSERO.exe" (the default of run_cosero()'s exe_name, which
# optimize_cosero_dds() does not expose). The Lhotse build must therefore be
# present under that exact name -- copy it once:
#   Copy-Item ".../COSERO_Lhotse_release.exe" ".../COSERO.exe"
# A bare name is also what the generated .bat uses, and cmd.exe resolves those
# against PATH, so keeping the default name is the path of least resistance.
stopifnot(dir.exists(project_path),
          file.exists(file.path(project_path, "COSERO.exe")))


# --- 1  Parameters to calibrate ----------------------------------------------
# 25 parameters. Checked against the header of para_13605_apriori.txt: five
# entries of the bounds table are NOT columns of this parameter file --
# ETSYSCOR, ETVEGCOR, INTMAX, and (importantly) PCOR and TCOR. PCOR/TCOR are
# the usual precipitation/temperature bias correctors; without them SNOWCOR
# and RAINCOR are the available handles on the +73 % volume bias.
param_names <- c(
  # NDC hypsometric disaggregation (5)
  "LAPSE_T", "LAPSE_P", "SOILVAR", "HYDROVAR", "CTVAR",
  # Snow (7)
  "CTMAX", "CTMIN", "SNOWCOR", "RAINCOR", "SNOWTRT", "RAINTRT", "TVAR",
  # Soil (7)
  "M", "BETA", "FK", "PWP", "FKFAK", "H1", "H2",
  # Routing / recession (6)
  "KBF", "TAB1", "TAB2", "TAB3", "TVS1", "TVS2"
)

# Use load_parameter_bounds(), NOT create_optimization_bounds():
# create_optimization_bounds() requires explicit lower/upper vectors and does
# not read inst/extdata/parameter_bounds.csv -- calling it with only the
# parameter names fails with 'argument "lower" is missing'.
par_bounds <- load_parameter_bounds(parameters = param_names)
stopifnot(nrow(par_bounds) == length(param_names))

print(as.data.frame(par_bounds[, c("parameter", "min", "max",
                                   "modification_type")]))
# Note: BETA's upper bound is 10 but the a-priori value for 4240 is 11, so it
# is pulled into range on the first iteration. The five abschg parameters
# (RAINTRT, SNOWTRT, TVAR, LAPSE_T, LAPSE_P) are thresholds / lapse rates,
# where a relative change around zero would be meaningless.


# --- 2  Simulation settings ---------------------------------------------------
# OUTPUTTYPE = 0 is calibration mode: runoff + statistics only, skipping the
# COSERO.plus / .plus1 writes (~8 GB per run). The full-output run of
# 2026-08-31 took ~7 min; measure what type 0 gives before scaling max_iter.
cal_settings <- list(
  STARTDATE  = c(2013, 1, 1, 0, 0),
  ENDDATE    = c(2019, 12, 31, 0, 0),   # calibration window
  SPINUP     = 365,
  OUTPUTTYPE = 0,
  PARAFILE   = "para_13605_apriori.txt"
)

# Held-out validation window (same parameters, later period)
val_settings <- modifyList(cal_settings, list(
  STARTDATE = c(2019, 1, 1, 0, 0),
  ENDDATE   = c(2020, 12, 31, 0, 0)
))


# --- 3  Run DDS ---------------------------------------------------------------
# target_subbasins takes the plain integer ID as it appears in the NB_ column
# ("4240", not "004240"); get_zones_for_subbasins() maps it to zone NZ 4240.
#
# Start at max_iter = 50 purely to time one iteration. DDS normally wants
# ~20-50 evaluations per parameter, i.e. 500-1250 runs for 25 parameters --
# decide that only once the per-run cost is known.
n_iter  <- 50
t_start <- Sys.time()

opt <- optimize_cosero_dds(
  cosero_path       = project_path,
  par_bounds        = par_bounds,
  target_subbasins  = "4240",
  metric            = "KGE",       # bias-sensitive; NSE would ignore the +73 %
  defaults_settings = cal_settings,
  max_iter          = n_iter,
  r                 = 0.2,         # DDS perturbation size (0.2 = default)
  verbose           = TRUE
)

elapsed_min <- as.numeric(difftime(Sys.time(), t_start, units = "mins"))
cat(sprintf("\nElapsed: %.1f min for %d iterations (%.2f min per run)\n",
            elapsed_min, n_iter, elapsed_min / n_iter))


# --- 4  Inspect the result ----------------------------------------------------
print(opt$best_metric)
print(as.data.frame(opt$par_bounds[, c("parameter", "default",
                                       "optimal_value")]))

# Convergence trace and a parameter-value summary
plot_cosero_optimization(opt)

# Write the optimised parameter set + logs to disk
export_cosero_optimization(opt, output_dir = file.path(project_path, "opt_4240"))


# --- 5  Validation ------------------------------------------------------------
# Re-run the optimised parameter file over the held-out window and compare.
# save_optimized_params() writes the calibrated values into a new para file;
# point PARAFILE at it, then run and read the metrics.
#
# val <- run_cosero(project_path,
#                   defaults_settings = modifyList(val_settings,
#                     list(PARAFILE = "para_opt_NB4240.txt")))
# print(calculate_single_metric(read_cosero_minimal(file.path(project_path,
#       "output")), subbasin = "4240", metric = "KGE", spinup_value = 365))
