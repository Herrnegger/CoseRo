#' @keywords internal
"_PACKAGE"

#' CoseRo: Hydrological Modelling Library
#'
#' @description
#' The CoseRo R package provides an R interface for the COSERO hydrological model, including:
#' \itemize{
#'   \item Automated model execution with run_cosero()
#'   \item Output reading for 13+ file formats
#'   \item Interactive Shiny visualization app
#'   \item Sobol-based global sensitivity analysis framework
#'   \item Parameter optimization with DDS and SCE-UA algorithms
#'   \item GeoSphere Austria data download (SPARTACUS, WINFORE)
#'   \item SPARTACUS/WINFORE climate data and eHYD discharge preprocessing for COSERO input
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
#'   \item \code{\link{plot_sobol}} - Visualize Sobol sensitivity indices
#'   \item \code{\link{plot_dotty}} - Parameter-output scatter plots (dotty plots)
#'   \item \code{\link{plot_ensemble_uncertainty}} - Visualize ensemble discharge with observed data
#'   \item \code{\link{plot_metric_distribution}} - Visualize distribution of performance metrics
#' }
#'
#' **Parameter Optimization:**
#' \itemize{
#'   \item \code{\link{optimize_cosero_dds}} - DDS algorithm for fast optimization
#'   \item \code{\link{optimize_cosero_sce}} - SCE-UA algorithm for robust global optimization
#'   \item \code{\link{create_optimization_bounds}} - Define parameter bounds for optimization
#'   \item \code{\link{plot_cosero_optimization}} - Visualize optimization convergence
#'   \item \code{\link{export_cosero_optimization}} - Export optimization results to CSV
#' }
#'
#' **Input Data Preprocessing:**
#' \itemize{
#'   \item \code{\link{download_geosphere_data}} - Download SPARTACUS/WINFORE NetCDF files from GeoSphere Austria
#'   \item \code{\link{write_spartacus_precip}} - Convert precipitation to COSERO format
#'   \item \code{\link{write_spartacus_temp}} - Convert temperature to COSERO format
#'   \item \code{\link{write_winfore_et0}} - Convert WINFORE ET0 to COSERO format
#'   \item \code{\link{write_ehyd_qobs}} - Convert eHYD discharge CSV files to COSERO QOBS format
#' }
#'
#' @seealso
#' \itemize{
#'   \item GitHub: \url{https://github.com/Herrnegger/CoseRo}
#'   \item Issues: \url{https://github.com/Herrnegger/CoseRo/issues}
#' }
#'
#' @section Getting Started:
#'
#' \preformatted{
#' # 1. Create example project
#' library(CoseRo)
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
#' @name CoseRo-package
#' @aliases CoseRo
NULL
