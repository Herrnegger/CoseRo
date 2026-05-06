library(CoseRo)

project_path <- "D:/temp/COSERO_class"


setup_cosero_project_example(project_path)

subbasins    <- c("001", "002", "003")     # adjust to your catchment

base_settings <- list(
  STARTDATE  = "2010 1 1 0 0",
  ENDDATE    = "2020 12 31 0 0",
  SPINUP     = 365,
  OUTPUTTYPE = 1,
  PARAFILE   = "para_ini.txt"
)

# Run COSERO with initial parameters
result_base <- run_cosero(
  project_path      = project_path,
  defaults_settings = base_settings,
  statevar_source   = 1   # cold start
)

# Check performance for each subbasin
for (sb in subbasins) {
  nse <- extract_run_metrics(result_base, sb, "NSE")
  kge <- extract_run_metrics(result_base, sb, "KGE")
  cat(sprintf("Subbasin %s:  NSE = %.3f  |  KGE = %.3f\n", sb, nse, kge))
}


launch_cosero_app(project_path)
