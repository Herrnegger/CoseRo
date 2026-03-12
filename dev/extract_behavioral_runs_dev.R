#' Extract Behavioral Runs and Plot Multi-Objective Scatter
#'
#' Evaluates an ensemble of model runs against multiple performance criteria 
#' (NSE, KGE, and percent bias). It visualizes the parameter space and extracts 
#' only the behavioral runs into a new ensemble object.
#'
#' @param ensemble_output The raw output list from run_cosero_ensemble_parallel()
#' @param metrics_df Data frame containing run_id, NSE, KGE, pBias, and optionally a subbasin column.
#' @param subbasin_id Character or Numeric. The subbasin to evaluate and plot (e.g., "0001", 1). Default is "0001".
#' @param nse_thresh Numeric. Minimum acceptable NSE.
#' @param kge_thresh Numeric. Minimum acceptable KGE.
#' @param pbias_thresh Numeric vector of length 2. Min and max acceptable pBias (e.g., c(-20, 20)).
#' @param xlim Optional numeric vector of length 2 for KGE axis limits.
#' @param ylim Optional numeric vector of length 2 for NSE axis limits.
#' @param plot_uncertainty Logical. If TRUE, also generates the ensemble uncertainty plot for behavioral runs.
#' @param ... Additional arguments passed directly to plot_ensemble_uncertainty() (e.g., date_range, q_max).
#'
#' @return A list containing `scatter_plot`, `uncertainty_plot` (if requested), 
#'   `filtered_ensemble`, and `behavioral_run_ids`.
#'
#' @import ggplot2
#' @importFrom dplyr mutate filter case_when sym
#' @export
#' 
#' @examples
#' \dontrun{
#' # 1. Assume you have your raw ensemble results and a metrics dataframe
#' # metrics_df needs columns: run_id, NSE, KGE, pBias, subbasin_id
#' 
#' # 2. Extract runs that pass strict criteria and generate both plots
#' eval_results <- extract_behavioral_runs(
#'   ensemble_output  = results,
#'   metrics_df       = my_metrics,
#'   subbasin_id      = "001",
#'   nse_thresh       = 0.65,
#'   kge_thresh       = 0.60,
#'   pbias_thresh     = c(-15, 15),
#'   plot_uncertainty = TRUE,
#'   date_range       = c("2010-01-01", "2010-12-31") # Passed to uncertainty plot via ...
#' )
#' 
#' # 3. View the multi-objective scatter plot
#' print(eval_results$scatter_plot)
#' 
#' # 4. View the hydrograph showing ONLY the behavioral runs
#' print(eval_results$uncertainty_plot)
#' 
#' # 5. Use the filtered ensemble for downstream analysis (like dotty plots)
#' behavioral_nse <- my_metrics$NSE[my_metrics$run_id %in% eval_results$behavioral_run_ids]
#' plot_dotty(eval_results$filtered_ensemble$parameter_sets, Y = behavioral_nse)
#' }
extract_behavioral_runs <- function(ensemble_output, metrics_df, 
                                    subbasin_id = "0001",
                                    nse_thresh = 0.6, kge_thresh = 0.6, 
                                    pbias_thresh = c(-20, 20),
                                    xlim = NULL, ylim = NULL,
                                    plot_uncertainty = FALSE, ...) {
  
  # Fix for R CMD check
  NSE <- KGE <- pBias <- category <- pass_nse_kge <- pass_pbias <- NULL

  # 0. Format subbasin_id safely (pad to 4 digits if needed)
  subbasin_num <- as.numeric(subbasin_id)
  subbasin_padded <- sprintf("%04d", subbasin_num)

  # 1. Filter metrics_df to the specific subbasin (if a subbasin column exists)
  sub_cols <- grep("subbasin", tolower(colnames(metrics_df)), value = TRUE)
  if (length(sub_cols) > 0) {
    target_col <- sub_cols[1]
    metrics_df <- metrics_df %>% 
      filter(as.character(!!sym(target_col)) == subbasin_padded | 
             as.numeric(!!sym(target_col)) == subbasin_num)
             
    if (nrow(metrics_df) == 0) {
      stop(sprintf("No metrics found for subbasin '%s' in the provided metrics_df.", subbasin_id))
    }
  }

  # 2. Categorize the runs
  metrics_df <- metrics_df %>%
    mutate(
      pass_nse_kge = (NSE >= nse_thresh & KGE >= kge_thresh),
      pass_pbias   = (pBias >= pbias_thresh[1] & pBias <= pbias_thresh[2]),
      
      category = case_when(
        pass_nse_kge & pass_pbias  ~ "Behavioral",
        pass_nse_kge & !pass_pbias ~ "Fails pBias Only",
        TRUE                       ~ "Outside Box"
      )
    )
  
  # 3. Extract behavioral run IDs and filter the ensemble
  behavioral_ids <- metrics_df$run_id[metrics_df$category == "Behavioral"]
  
  filtered_ensemble <- ensemble_output
  
  if (length(behavioral_ids) == 0) {
    warning("No runs met all behavioral criteria! Returning empty filtered ensemble.")
    filtered_ensemble$results <- list()
  } else {
    filtered_ensemble$results <- ensemble_output$results[behavioral_ids]
  }
  
  if (!is.null(filtered_ensemble$parameter_sets) && length(behavioral_ids) > 0) {
    filtered_ensemble$parameter_sets <- filtered_ensemble$parameter_sets[behavioral_ids, , drop = FALSE]
  }
  
  # 4. Build the Scatter Plot
  df_outside <- filter(metrics_df, category == "Outside Box")
  df_alarming <- filter(metrics_df, category == "Fails pBias Only")
  df_behavioral <- filter(metrics_df, category == "Behavioral")
  
  p_scatter <- ggplot() +
    geom_rect(aes(xmin = kge_thresh, xmax = Inf, ymin = nse_thresh, ymax = Inf),
              fill = NA, color = "black", linetype = "dashed", linewidth = 0.6) +
    geom_point(data = df_outside, aes(x = KGE, y = NSE), 
               color = "grey75", size = 1.5, alpha = 0.5) +
    geom_point(data = df_alarming, aes(x = KGE, y = NSE), 
               color = "indianred", size = 2.5, alpha = 0.8) +
    geom_point(data = df_behavioral, aes(x = KGE, y = NSE, fill = pBias), 
               shape = 21, color = "black", size = 3.5, alpha = 0.9, stroke = 0.5) +
    scale_fill_gradient2(low = "#8c510a", mid = "white", high = "#01665e", midpoint = 0,
                         name = "pBias (%)") +
    theme_bw() +
    labs(title = paste0("Multi-Objective Evaluation - Subbasin ", subbasin_num),
         subtitle = paste0("Box: NSE >= ", nse_thresh, " & KGE >= ", kge_thresh, 
                           " | Grey = Fails Box | Red = Fails pBias | Colored = Behavioral"),
         x = "KGE", y = "NSE") +
    theme(legend.position = "right")
  
  if (!is.null(xlim) || !is.null(ylim)) {
    p_scatter <- p_scatter + coord_cartesian(xlim = xlim, ylim = ylim)
  }

  # 5. Generate the Uncertainty Plot and overwrite its title
  p_uncert <- NULL
  if (plot_uncertainty) {
    if (length(behavioral_ids) > 0) {
      
      # Generate the base plot by passing everything through `...`
      p_uncert <- plot_ensemble_uncertainty(filtered_ensemble, subbasin_id = subbasin_padded, ...)
      
      # Build a clear subtitle string describing the exact thresholds used
      thresh_string <- sprintf("Selected Runs (NSE >= %s | KGE >= %s | pBias: [%s, %s])", 
                               nse_thresh, kge_thresh, pbias_thresh[1], pbias_thresh[2])
      
      # Overwrite the default title and subtitle of the uncertainty plot
      p_uncert <- p_uncert + 
        labs(title = paste0("Behavioral Ensemble Uncertainty - Subbasin ", subbasin_num),
             subtitle = thresh_string)
             
    } else {
      warning("Skipping uncertainty plot: No behavioral runs to plot.")
    }
  }
  
  return(list(
    scatter_plot = p_scatter,
    uncertainty_plot = p_uncert,
    filtered_ensemble = filtered_ensemble,
    behavioral_run_ids = behavioral_ids
  ))
}