# COSERO Run Metrics
# Performance metric extraction and calculation for single runs
# Extracted from cosero_run.R for clarity

# ============================================================================
# Extract Pre-calculated Metrics
# ============================================================================

#' Extract Pre-calculated Metrics from Single COSERO Run
#'
#' Extracts performance metrics (NSE, KGE, etc.) from a single
#' COSERO run's statistics output. This is the recommended approach
#' for standard metrics as it uses COSERO's pre-calculated values.
#'
#' @param run_result Result object from run_cosero()
#' @param subbasin_id Subbasin ID(s) to extract metrics for:
#'   - Single ID: "001" or 1 (returns scalar)
#'   - Multiple IDs: c("001", "002") (returns named vector)
#'   - "all": Extract for all subbasins (returns named vector)
#' @param metric Performance metric to extract
#'   ("NSE", "KGE", "BIAS", "RMSE", etc.).
#'   Must be a column name in the statistics data frame.
#'
#' @return Single numeric value (if one subbasin) or named vector
#'   (if multiple/all)
#'
#' @seealso
#' \code{\link{calculate_run_metrics}} to calculate metrics from
#'   QSIM/QOBS (slower but more flexible),
#' \code{\link{extract_ensemble_metrics}} for ensemble results,
#' \code{\link{calculate_ensemble_metrics}} to calculate metrics
#'   for ensembles
#'
#' @export
#' @examples
#' \dontrun{
#' result <- run_cosero("path/to/project")
#'
#' # Single subbasin (returns scalar)
#' nse <- extract_run_metrics(result,
#'                            subbasin_id = "001",
#'                            metric = "NSE")
#'
#' # Multiple subbasins (returns named vector)
#' nse_multi <- extract_run_metrics(result,
#'                                  c("001", "002"), "NSE")
#'
#' # All subbasins (returns named vector)
#' kge_all <- extract_run_metrics(result,
#'                                subbasin_id = "all",
#'                                metric = "KGE")
#' }
extract_run_metrics <- function(run_result,
                                subbasin_id = "001",
                                metric = "NSE") {

  # Check if run was successful
  if (!run_result$success) {
    stop("Run failed - cannot extract metrics", call. = FALSE)
  }

  # Check if statistics data exists
  if (is.null(run_result$output_data) ||
      is.null(run_result$output_data$statistics)) {
    stop("No statistics data found in run result",
         call. = FALSE)
  }

  stats <- run_result$output_data$statistics

  # Check if metric exists
  if (!metric %in% colnames(stats)) {
    available <- setdiff(colnames(stats), "sb")
    stop("Metric '", metric, "' not found. Available: ",
         paste(available, collapse = ", "),
         call. = FALSE)
  }

  # Handle "all" subbasins case
  if (length(subbasin_id) == 1 &&
      is.character(subbasin_id) &&
      tolower(subbasin_id) == "all") {
    result <- stats[[metric]]
    names(result) <- stats$sb
    return(result)
  }

  # Handle multiple subbasin IDs - try multiple formats
  sb_id_nums <- as.numeric(subbasin_id)
  sb_id_strs_3 <- sprintf("%03d", sb_id_nums)
  sb_id_strs_4 <- sprintf("%04d", sb_id_nums)

  # Filter for specified subbasins (try both formats)
  mask <- stats$sb %in% sb_id_strs_3 | stats$sb %in% sb_id_strs_4 |
          stats$sb %in% sb_id_nums
  sb_stats <- stats[mask, ]

  if (nrow(sb_stats) == 0) {
    stop("Subbasin(s) '",
         paste(subbasin_id, collapse = ", "),
         "' not found in statistics. Available: ",
         paste(stats$sb, collapse = ", "),
         call. = FALSE)
  }

  result <- sb_stats[[metric]]
  names(result) <- sb_stats$sb

  # Return scalar if single subbasin, named vector if multiple
  if (length(result) == 1) {
    as.numeric(result)
  } else {
    result
  }
}

# ============================================================================
# Calculate Metrics from Simulated/Observed Data
# ============================================================================

#' Calculate Performance Metrics from Single COSERO Run
#'
#' Calculates performance metrics by comparing simulated (QSIM)
#' against observed (QOBS) discharge from a single COSERO run.
#' Automatically applies spin-up period from run settings.
#'
#' For standard metrics (NSE, KGE) already calculated by COSERO,
#' use \code{\link{extract_run_metrics}} instead - it's faster
#' and guaranteed to match COSERO's evaluation.
#'
#' @param run_result Result object from run_cosero()
#' @param subbasin_id Subbasin ID(s) to calculate metrics for:
#'   - Single ID: "001" or 1 (returns scalar)
#'   - Multiple IDs: c("001", "002") (returns named vector)
#'   - "all": Calculate for all subbasins (returns named vector)
#' @param metric Performance metric
#'   ("NSE", "KGE", "RMSE", "PBIAS")
#' @param spinup Spin-up period in timesteps (optional).
#'   If NULL, reads from run_result$defaults_settings$SPINUP
#'
#' @return Single numeric value (if one subbasin) or named vector
#'   (if multiple/all)
#'
#' @seealso
#' \code{\link{extract_run_metrics}} to extract pre-calculated
#'   metrics (faster),
#' \code{\link{extract_ensemble_metrics}} for ensemble results,
#' \code{\link{calculate_ensemble_metrics}} to calculate metrics
#'   for ensembles
#'
#' @export
#' @examples
#' \dontrun{
#' result <- run_cosero("path/to/project",
#'   defaults_settings = list(SPINUP = 365))
#'
#' # Calculate PBIAS for subbasin 001 (auto-detects spin-up)
#' pbias <- calculate_run_metrics(result,
#'                                subbasin_id = "001",
#'                                metric = "PBIAS")
#'
#' # Multiple subbasins (returns named vector)
#' nse_multi <- calculate_run_metrics(result,
#'                                    c("001", "002"), "NSE")
#'
#' # All subbasins (returns named vector)
#' nse_all <- calculate_run_metrics(result,
#'                                  subbasin_id = "all",
#'                                  metric = "NSE")
#'
#' # Override spin-up period
#' kge <- calculate_run_metrics(result,
#'                              subbasin_id = "001",
#'                              metric = "KGE",
#'                              spinup = 100)
#' }
calculate_run_metrics <- function(run_result,
                                  subbasin_id = "001",
                                  metric = "KGE",
                                  spinup = NULL) {

  # Check if run was successful
  if (!run_result$success) {
    stop("Run failed - cannot calculate metrics",
         call. = FALSE)
  }

  # Check if runoff data exists
  if (is.null(run_result$output_data) ||
      is.null(run_result$output_data$runoff)) {
    stop("No runoff data found in run result",
         call. = FALSE)
  }

  runoff <- run_result$output_data$runoff

  # Get spin-up duration
  if (!is.null(spinup)) {
    spinup_duration <- as.numeric(spinup)
  } else {
    spinup_duration <- 0
    if (!is.null(run_result$defaults_settings) &&
        !is.null(run_result$defaults_settings$SPINUP)) {
      spinup_duration <- as.numeric(
        run_result$defaults_settings$SPINUP
      )
    }
  }

  # Helper function to calculate metric for one subbasin
  calc_single_subbasin <- function(sb_id) {
    # Auto-detect subbasin column format
    sb_id_num <- as.numeric(sb_id)
    possible_formats <- c(
      sprintf("%03d", sb_id_num),
      sprintf("%04d", sb_id_num),
      sprintf("%d", sb_id_num)
    )

    qsim_col <- NULL
    qobs_col <- NULL
    for (fmt in possible_formats) {
      qsim_try <- paste0("QSIM_", fmt)
      qobs_try <- paste0("QOBS_", fmt)
      if (qsim_try %in% colnames(runoff) && qobs_try %in% colnames(runoff)) {
        qsim_col <- qsim_try
        qobs_col <- qobs_try
        break
      }
    }

    if (is.null(qsim_col)) {
      stop("Column 'QSIM_", possible_formats[1],
           "' not found in runoff data",
           call. = FALSE)
    }
    if (is.null(qobs_col)) {
      stop("Column 'QOBS_", possible_formats[1],
           "' not found in runoff data",
           call. = FALSE)
    }

    # Extract data
    simulated <- runoff[[qsim_col]]
    observed <- runoff[[qobs_col]]

    # Apply spin-up exclusion
    if (spinup_duration > 0 &&
        length(simulated) > spinup_duration) {
      idx <- (spinup_duration + 1):length(simulated)
      simulated <- simulated[idx]
      observed <- observed[idx]
    }

    # Calculate metric
    tryCatch({
      if (metric == "KGE") {
        hydroGOF::KGE(simulated, observed)
      } else if (metric == "NSE") {
        hydroGOF::NSE(simulated, observed)
      } else if (metric == "RMSE") {
        hydroGOF::rmse(simulated, observed)
      } else if (metric == "PBIAS") {
        hydroGOF::pbias(simulated, observed)
      } else {
        stop("Unknown metric: ", metric,
             ". Supported: KGE, NSE, RMSE, PBIAS",
             call. = FALSE)
      }
    }, error = function(e) {
      warning("Error calculating ", metric,
              " for subbasin ", sb_id, ": ", e$message)
      NA
    })
  }

  # Handle "all" subbasins case
  if (length(subbasin_id) == 1 &&
      is.character(subbasin_id) &&
      tolower(subbasin_id) == "all") {
    qsim_cols <- grep("^QSIM_", colnames(runoff),
                       value = TRUE)
    if (length(qsim_cols) == 0) {
      stop("No QSIM columns found in runoff data",
           call. = FALSE)
    }
    subbasin_ids <- gsub("^QSIM_", "", qsim_cols)
    results <- sapply(subbasin_ids, calc_single_subbasin)
    names(results) <- subbasin_ids
    return(results)
  }

  # Handle multiple subbasins
  if (length(subbasin_id) > 1) {
    results <- sapply(subbasin_id, calc_single_subbasin)
    names(results) <- sprintf("%03d",
                              as.numeric(subbasin_id))
    return(results)
  }

  # Single subbasin case
  calc_single_subbasin(subbasin_id)
}
