# Simple COSERO run with performance metrics and annual rainfall summary
# Project: D:/temp/COSERO_Optim_DDS/

devtools::load_all()

# =============================================================================
# SETTINGS
# =============================================================================

project_path <- "D:/temp/COSERO_Optim_DDS"

start_date   <- "2000 1 1 0 0"
end_date     <- "2020 12 31 0 0"
spinup       <- 365

# Parameter file (PARAFILE in defaults.txt): filename relative to input/.
# The default defined in defaults.txt is "para.txt". Change to use another file, e.g.:
#   param_file <- "para_optimized_NB3_NSE_20260101_120000.txt"
param_file <- "para_ini_mod_somehow.txt"

subbasins <- c("001", "002", "003")

# =============================================================================
# RUN COSERO
# =============================================================================

result <- run_cosero(
  project_path = project_path,
  defaults_settings = list(
    STARTDATE  = start_date,
    ENDDATE    = end_date,
    SPINUP     = spinup,
    OUTPUTTYPE = 1#,
    #PARAFILE   = param_file
  ),
  statevar_source = 1,  # cold start
  tmmon_option = 1,
  quiet = FALSE
)

launch_cosero_app(project_path)

# =============================================================================
# PERFORMANCE METRICS
# =============================================================================

cat("\n=== Performance Metrics ===\n")
for (sb in subbasins) {
  nse <- extract_run_metrics(result, sb, "NSE")
  kge <- extract_run_metrics(result, sb, "KGE")
  cat(sprintf("  Subbasin %s:  NSE = %.4f  |  KGE = %.4f\n", sb, nse, kge))
}


# =============================================================================
# ANNUAL PRECIPITATION
# =============================================================================

cat("\n=== Annual Precipitation (mm) ===\n")
precip  <- result$output_data$precipitation
sb_cols <- grep("PRAINGEB", names(precip), value = TRUE)

precip$Year   <- as.integer(format(precip$DateTime, "%Y"))
annual_precip <- aggregate(precip[, sb_cols], by = list(Year = precip$Year), FUN = sum, na.rm = TRUE)

print(annual_precip, digits = 1)
colMeans(annual_precip)
