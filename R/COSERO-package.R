#' @keywords internal
"_PACKAGE"

#' COSERO: R Interface for COSERO Hydrological Model
#'
#' @description
#' The COSERO R package provides a R interface for the COSERO hydrological model, including:
#' \itemize{
#'   \item Automated model execution with run_cosero()
#'   \item Output reading for 13+ file formats
#'   \item Interactive Shiny visualization app
#'   \item Sobol-based global sensitivity analysis framework
#'   \item SPARTACUS climate data preprocessing
#' }
#'
#' @section Main Functions:
#'
#' **Model Execution:**
#' \itemize{
#'   \item \code{\link{run_cosero}} - Execute COSERO simulations
#'   \item \code{\link{launch_cosero_app}} - Launch interactive Shiny app
#' }
#'
#' **Project Setup:**
#' \itemize{
#'   \item \code{\link{setup_cosero_project_example}} - Create example project with Wildalpen data
#'   \item \code{\link{setup_cosero_project}} - Create empty project structure
#' }
#'
#' **Output Reading:**
#' \itemize{
#'   \item \code{\link{read_cosero_output}} - Read all COSERO output files
#'   \item \code{\link{read_cosero_parameters}} - Read parameter files
#'   \item \code{\link{get_subbasin_data}} - Extract subbasin-specific data
#' }
#'
#' **Single Run Metrics:**
#' \itemize{
#'   \item \code{\link{extract_run_metrics}} - Extract metrics from single run statistics
#'   \item \code{\link{calculate_run_metrics}} - Calculate metrics from single run output
#' }
#'
#' **Sensitivity Analysis:**
#' \itemize{
#'   \item \code{\link{load_parameter_bounds}} - Load parameter bounds for sampling
#'   \item \code{\link{generate_sobol_samples}} - Generate Sobol parameter sets
#'   \item \code{\link{run_cosero_ensemble}} - Run ensemble simulations (sequential)
#'   \item \code{\link{run_cosero_ensemble_parallel}} - Run ensemble simulations (parallel)
#'   \item \code{\link{extract_ensemble_metrics}} - Extract metrics from ensemble statistics
#'   \item \code{\link{calculate_ensemble_metrics}} - Calculate metrics from ensemble output
#'   \item \code{\link{calculate_sobol_indices}} - Calculate sensitivity indices
#'   \item \code{\link{plot_sobol}} - Visualize sensitivity indices
#' }
#'
#' **SPARTACUS Preprocessing:**
#' \itemize{
#'   \item \code{\link{write_spartacus_precip}} - Convert precipitation to COSERO format
#'   \item \code{\link{write_spartacus_temp}} - Convert temperature to COSERO format
#' }
#'
#' @section Getting Started:
#'
#' \preformatted{
#' # 1. Create example project
#' library(COSERO)
#' setup_cosero_project_example("D:/COSERO_example")
#'
#' # 2. Launch interactive app
#' launch_cosero_app("D:/COSERO_example")
#'
#' # 3. Or run from R script
#' result <- run_cosero("D:/COSERO_example")
#' nse <- extract_run_metrics(result, subbasin_id = "001", metric = "NSE")
#' }
#'
#' @docType _PACKAGE
#' @name COSERO-package
#' @aliases COSERO
NULL
