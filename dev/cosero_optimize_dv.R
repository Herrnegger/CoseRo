# =============================================================================
# COSERO Parameter Optimization
# DDS and SCE-UA optimization with multi-objective support
# =============================================================================
#
# DEPENDENCIES - SOURCE IN THIS ORDER:
# 1. source("cosero_run.R")
# 2. source("cosero_readers.R")
# 3. source("sensitivity_analysis.R")
# 4. source("cosero_optimize.R")
#
# =============================================================================

# 1 Parameter Setup #####

#' Create Optimization Parameter Bounds
#'
#' Defines parameter bounds for optimization. Matches sensitivity_analysis.R structure.
#' Compatible with inst/extdata/parameter_bounds.csv format.
#'
#' @param parameters Vector of parameter names
#' @param lower Lower bounds for optimization
#' @param upper Upper bounds for optimization
#' @param default Default parameter values (optional, uses midpoint if NULL)
#' @param modification_type "relchg" (relative) or "abschg" (absolute) for each parameter
#' @param description Parameter descriptions (optional)
#' @param category Parameter categories (optional, e.g., "runoff", "snow", "soil")
#'
#' @return Tibble with columns: parameter, description, min, max, default, modification_type, category
#' @export
#' @examples
#' # Basic usage with automatic defaults
#' par_bounds <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   lower = c(1, 2),
#'   upper = c(6, 8)
#' )
#'
#' # Full specification with all metadata
#' par_bounds_full <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX", "TCOR"),
#'   lower = c(1, 2, -1),
#'   upper = c(6, 8, 2),
#'   default = c(4.5, 5, 0),
#'   modification_type = c("relchg", "relchg", "abschg"),
#'   description = c(
#'     "Runoff generation parameter",
#'     "Maximum snow melt factor",
#'     "Temperature correction"
#'   ),
#'   category = c("runoff", "snow", "temperature")
#' )
#'
#' # Load from standard parameter bounds file
#' \dontrun{
#' # Use existing bounds from package
#' full_bounds <- load_parameter_bounds()
#' # Subset for optimization
#' opt_params <- c("BETA", "CTMAX", "LP")
#' par_bounds <- full_bounds[full_bounds$parameter %in% opt_params, ]
#' }
create_optimization_bounds <- function(parameters,
                                       lower,
                                       upper,
                                       default = NULL,
                                       modification_type = NULL,
                                       description = NULL,
                                       category = NULL) {

  n_params <- length(parameters)

  if (length(lower) != n_params) stop("'lower' must match 'parameters' length")
  if (length(upper) != n_params) stop("'upper' must match 'parameters' length")
  if (any(lower >= upper)) stop("All lower < upper required")

  # Default values: use midpoint if not specified
  if (is.null(default)) {
    default <- (lower + upper) / 2
  }
  if (length(default) != n_params) {
    stop("'default' must match 'parameters' length")
  }

  # Modification type: default to relchg
  if (is.null(modification_type)) {
    modification_type <- rep("relchg", n_params)
  }
  if (length(modification_type) != n_params) {
    stop("'modification_type' must match 'parameters' length")
  }
  if (!all(modification_type %in% c("relchg", "abschg"))) {
    stop("modification_type must be 'relchg' or 'abschg'")
  }

  # Description: use parameter name if not specified
  if (is.null(description)) {
    description <- paste("Optimization parameter:", parameters)
  }
  if (length(description) != n_params) {
    stop("'description' must match 'parameters' length")
  }

  # Category: use "optimization" if not specified
  if (is.null(category)) {
    category <- rep("optimization", n_params)
  }
  if (length(category) != n_params) {
    stop("'category' must match 'parameters' length")
  }

  tibble::tibble(
    parameter = parameters,
    description = description,
    min = lower,
    max = upper,
    default = default,
    modification_type = modification_type,
    category = category
  )
}

# 2 Metric Calculation #####

#' Read Minimal COSERO Output
#'
#' Fast reading of only statistics.txt and COSERO.runoff for optimization.
#' Reduces I/O overhead compared to read_cosero_output().
#'
#' @param output_dir COSERO output directory path
#' @param quiet Suppress warnings
#'
#' @return List with statistics and runoff data frames (or NULL if files missing)
#' @keywords internal
read_cosero_minimal <- function(output_dir, quiet = TRUE) {
  output_data <- list()
  
  stats_file <- file.path(output_dir, "statistics.txt")
  runoff_file <- file.path(output_dir, "COSERO.runoff")
  
  if (file.exists(stats_file)) {
    output_data$statistics <- tryCatch({
      read_statistics(output_dir, quiet = quiet)
    }, error = function(e) NULL)
  }
  
  if (file.exists(runoff_file)) {
    output_data$runoff <- tryCatch({
      read_runoff(output_dir, quiet = quiet)
    }, error = function(e) NULL)
  }
  
  return(output_data)
}

#' Calculate Single Metric
#'
#' Calculate performance metric for a single subbasin. Tries pre-calculated metrics
#' from statistics.txt first, then calculates from runoff data if needed.
#'
#' @param result COSERO run result from run_cosero()
#' @param subbasin Subbasin ID (character or numeric)
#' @param metric Metric name ("NSE", "KGE", "lnNSE", "rNSE", "RMSE", "PBIAS", "VE")
#' @param spinup_value Number of timesteps to skip for spinup
#'
#' @return Numeric metric value (NA if calculation fails)
#' @keywords internal
calculate_single_metric <- function(result, subbasin, metric, spinup_value = 0) {
  
  # Try pre-calculated
  if (metric %in% c("NSE", "KGE", "BIAS", "RMSE")) {
    val <- tryCatch({
      extract_run_metrics(result, subbasin_id = subbasin, metric = metric)
    }, error = function(e) NA)
    
    if (!is.na(val)) {
      if (metric == "RMSE") val <- -val
      return(val)
    }
  }
  
  # Calculate from data
  runoff <- result$output_data$runoff
  sb_formatted <- sprintf("%04d", as.numeric(subbasin))
  
  qsim_col <- paste0("QSIM_", sb_formatted)
  qobs_col <- paste0("QOBS_", sb_formatted)
  
  if (!qsim_col %in% colnames(runoff) || !qobs_col %in% colnames(runoff)) {
    return(NA)
  }
  
  sim <- runoff[[qsim_col]]
  obs <- runoff[[qobs_col]]
  
  if (spinup_value > 0 && length(sim) > spinup_value) {
    sim <- sim[(spinup_value + 1):length(sim)]
    obs <- obs[(spinup_value + 1):length(obs)]
  }
  
  val <- tryCatch({
    switch(metric,
      "NSE" = hydroGOF::NSE(sim, obs),
      "KGE" = hydroGOF::KGE(sim, obs),
      "lnNSE" = hydroGOF::NSE(log(sim + 0.01), log(obs + 0.01)),
      "rNSE" = hydroGOF::NSE(sqrt(sim), sqrt(obs)),
      "RMSE" = -hydroGOF::rmse(sim, obs),
      "PBIAS" = 1 - abs(hydroGOF::pbias(sim, obs)) / 100,
      "VE" = 1 - abs(sum(sim) - sum(obs)) / sum(obs),
      stop("Unknown metric: ", metric)
    )
  }, error = function(e) NA)
  
  return(val)
}

# 3 Objective Function #####

#' Create Objective Function
#' 
#' @param cosero_path COSERO project path
#' @param par_bounds Parameter bounds from create_optimization_bounds()
#' @param target_subbasins Subbasin IDs for calibration
#' @param zones_to_modify Zone IDs to modify (NULL = all zones)
#' @param metric Single or vector of metrics
#' @param metric_weights Weights for multiple metrics (sum to 1)
#' @param subbasin_weights Weights for subbasins (sum to 1)
#' @param aggregation Aggregation: "mean", "weighted", "min", "product"
#' @param defaults_settings COSERO settings
#' @param verbose Print progress
#' @param use_minimal_reading Fast reading (statistics + runoff only)
create_objective_function <- function(cosero_path,
                                      par_bounds,
                                      target_subbasins = "001",
                                      zones_to_modify = NULL,
                                      metric = "NSE",
                                      metric_weights = NULL,
                                      subbasin_weights = NULL,
                                      aggregation = "mean",
                                      defaults_settings = NULL,
                                      verbose = TRUE,
                                      use_minimal_reading = TRUE) {
  
  n_subbasins <- length(target_subbasins)
  n_metrics <- length(metric)
  
  # Validate weights
  if (n_metrics > 1) {
    if (is.null(metric_weights)) {
      metric_weights <- rep(1/n_metrics, n_metrics)
    }
    if (length(metric_weights) != n_metrics) {
      stop("metric_weights must match metric length")
    }
    if (abs(sum(metric_weights) - 1) > 1e-6) {
      stop("metric_weights must sum to 1")
    }
  }
  
  if (aggregation == "weighted") {
    if (is.null(subbasin_weights)) {
      stop("subbasin_weights required for aggregation='weighted'")
    }
    if (length(subbasin_weights) != n_subbasins) {
      stop("subbasin_weights must match target_subbasins length")
    }
    if (abs(sum(subbasin_weights) - 1) > 1e-6) {
      stop("subbasin_weights must sum to 1")
    }
  }
  
  if (!aggregation %in% c("mean", "weighted", "min", "product")) {
    stop("aggregation must be: mean, weighted, min, or product")
  }
  
  # Get parameter filename
  if (!is.null(defaults_settings) && !is.null(defaults_settings$PARAFILE)) {
    par_filename <- defaults_settings$PARAFILE
  } else {
    defaults_file <- file.path(cosero_path, "input", "defaults.txt")
    if (file.exists(defaults_file)) {
      defaults <- read_defaults(defaults_file)
      par_filename <- ifelse(!is.null(defaults$PARAFILE), defaults$PARAFILE, "para.txt")
    } else {
      par_filename <- "para.txt"
    }
  }
  
  par_file <- file.path(cosero_path, "input", par_filename)
  if (!file.exists(par_file)) stop("Parameter file not found: ", par_file)

  if (verbose) cat("Loading parameter file:", par_filename, "\n")
  
  # Load parameter structure
  first_line <- readLines(par_file, n = 1)
  param_data_original <- read.table(
    par_file, header = TRUE, sep = "\t", skip = 1,
    stringsAsFactors = FALSE, check.names = FALSE, comment.char = ""
  )
  
  param_structure <- list(
    is_tabular = TRUE,
    first_line = first_line,
    param_data_original = param_data_original,
    param_data_working = param_data_original
  )
  
  # Read original values
  original_values <- list()
  for (param_name in par_bounds$parameter) {
    param_cols <- find_parameter_column(param_name, colnames(param_data_original), 
                                        return_all = TRUE)
    if (length(param_cols) > 0) {
      all_values <- numeric()
      for (col in param_cols) {
        all_values <- c(all_values, param_data_original[[col]])
      }
      original_values[[param_name]] <- all_values
    }
  }
  
  if (verbose) cat("Loaded", length(original_values), "parameters\n")
  
  # Return objective function
  eval_count <- 0
  
  function(x) {
    eval_count <<- eval_count + 1
    
    params_to_apply <- setNames(as.list(x), par_bounds$parameter)
    
    # Modify parameters
    mod_success <- tryCatch({
      modify_parameter_table_fast(
        par_file = par_file,
        params = params_to_apply,
        par_bounds = par_bounds,
        original_values = original_values,
        param_structure = param_structure,
        zones = zones_to_modify,
        quiet = TRUE
      )
      TRUE
    }, error = function(e) {
      if (verbose) warning("Modification failed: ", e$message)
      FALSE
    })
    
    if (!mod_success) return(1e6)
    
    # Run COSERO
    result <- tryCatch({
      if (use_minimal_reading) {
        run_result <- run_cosero(
          cosero_path = cosero_path,
          defaults_settings = defaults_settings,
          read_outputs = FALSE,
          quiet = !verbose
        )
        if (run_result$success) {
          output_dir <- file.path(cosero_path, "output")
          run_result$output_data <- read_cosero_minimal(output_dir, quiet = TRUE)
        }
        run_result
      } else {
        run_cosero(
          cosero_path = cosero_path,
          defaults_settings = defaults_settings,
          quiet = !verbose
        )
      }
    }, error = function(e) {
      if (verbose) warning("COSERO failed: ", e$message)
      return(list(success = FALSE))
    })
    
    if (!result$success) {
      if (verbose) cat("Run", eval_count, "failed\n")
      return(1e6)
    }
    
    # Calculate metrics
    metric_matrix <- matrix(NA, nrow = n_subbasins, ncol = n_metrics)
    
    spinup_value <- 0
    if (!is.null(defaults_settings) && !is.null(defaults_settings$SPINUP)) {
      spinup_value <- as.numeric(defaults_settings$SPINUP)
    }
    
    for (i in seq_along(target_subbasins)) {
      for (j in seq_along(metric)) {
        metric_matrix[i, j] <- calculate_single_metric(
          result, target_subbasins[i], metric[j], spinup_value
        )
      }
    }
    
    if (any(is.na(metric_matrix))) {
      if (verbose) cat("Run", eval_count, "- metric failed\n")
      return(1e6)
    }
    
    # Combine metrics
    if (n_metrics == 1) {
      metric_values <- metric_matrix[, 1]
    } else {
      metric_values <- as.vector(metric_matrix %*% metric_weights)
    }
    
    # Aggregate subbasins
    obj_value <- switch(aggregation,
      "mean" = mean(metric_values),
      "weighted" = sum(metric_values * subbasin_weights),
      "min" = min(metric_values),
      "product" = prod(metric_values)
    )
    
    obj_value <- -obj_value
    
    if (verbose) {
      metric_str <- if (n_metrics == 1) {
        sprintf("%s = %.4f", metric, -obj_value)
      } else {
        sprintf("Combined = %.4f", -obj_value)
      }
      cat(sprintf("Run %d: %s\n", eval_count, metric_str))
    }
    
    return(obj_value)
  }
}

# 4 DDS Algorithm #####

#' Dynamically Dimensioned Search (DDS) Algorithm
#'
#' Implementation of DDS optimization algorithm (Tolson & Shoemaker, 2007).
#' Efficient single-objective optimization for hydrological model calibration.
#'
#' @param obj_fun Objective function to minimize
#' @param lower Lower parameter bounds
#' @param upper Upper parameter bounds
#' @param max_iter Maximum iterations
#' @param r Perturbation parameter (0.2 recommended)
#' @param verbose Print progress
#'
#' @return List with par (best parameters), value (best objective), history (data frame)
#' @keywords internal
#'
#' @references
#' Tolson, B.A. and Shoemaker, C.A. (2007). Dynamically dimensioned search algorithm
#' for computationally efficient watershed model calibration. Water Resources Research, 43(1).
dds_optimize <- function(obj_fun, lower, upper, max_iter = 1000,
                         r = 0.2, verbose = TRUE) {
  
  n_params <- length(lower)
  x_best <- runif(n_params, lower, upper)
  f_best <- obj_fun(x_best)

  history <- data.frame(iteration = 1, objective = f_best)

  if (verbose) {
    cat(sprintf("Iteration %d/%d | Best objective: %.6f\n", 1, max_iter, -f_best))
  }

  # Progress bar setup
  if (verbose && requireNamespace("pbapply", quietly = TRUE)) {
    pb <- pbapply::startpb(min = 2, max = max_iter)
    on.exit(pbapply::closepb(pb))
    show_pb <- TRUE
  } else {
    show_pb <- FALSE
  }

  for (i in 2:max_iter) {
    prob_select <- 1 - log(i) / log(max_iter)

    x_new <- x_best
    selected <- runif(n_params) < prob_select

    for (j in which(selected)) {
      sigma <- r * (upper[j] - lower[j])
      x_new[j] <- x_best[j] + rnorm(1, 0, sigma)

      if (x_new[j] < lower[j]) x_new[j] <- lower[j] + (lower[j] - x_new[j])
      if (x_new[j] > upper[j]) x_new[j] <- upper[j] - (x_new[j] - upper[j])
      x_new[j] <- max(lower[j], min(upper[j], x_new[j]))
    }

    f_new <- obj_fun(x_new)

    if (f_new < f_best) {
      x_best <- x_new
      f_best <- f_new
    }

    history <- rbind(history, data.frame(iteration = i, objective = f_best))

    # Update progress
    if (show_pb) {
      pbapply::setpb(pb, i)
    } else if (verbose && (i %% max(1, floor(max_iter / 20)) == 0 || i == max_iter)) {
      pct <- round(100 * i / max_iter)
      cat(sprintf("Iteration %d/%d (%d%%) | Best objective: %.6f\n",
                  i, max_iter, pct, -f_best))
    }
  }

  list(par = x_best, value = f_best, history = history)
}

# 5 Helper Functions #####

#' Run Final COSERO with Optimal Parameters
#'
#' Internal helper function to run COSERO with optimal parameters and full outputs.
#'
#' @param cosero_path COSERO project path
#' @param par_bounds Parameter bounds with optimal_value column
#' @param zones_to_modify Zone IDs to modify (NULL = all zones)
#' @param defaults_settings COSERO settings
#' @param verbose Print progress
#'
#' @return COSERO run result or NULL if failed
#' @keywords internal
run_final_optimization <- function(cosero_path, par_bounds, zones_to_modify,
                                   defaults_settings, verbose = TRUE) {

  if (verbose) cat("\nRunning final COSERO with optimal parameters...\n")

  params_optimal <- setNames(as.list(par_bounds$optimal_value), par_bounds$parameter)

  # Get parameter filename
  if (!is.null(defaults_settings) && !is.null(defaults_settings$PARAFILE)) {
    par_filename <- defaults_settings$PARAFILE
  } else {
    defaults_file <- file.path(cosero_path, "input", "defaults.txt")
    if (file.exists(defaults_file)) {
      defaults <- read_defaults(defaults_file)
      par_filename <- ifelse(!is.null(defaults$PARAFILE), defaults$PARAFILE, "para.txt")
    } else {
      par_filename <- "para.txt"
    }
  }
  par_file <- file.path(cosero_path, "input", par_filename)

  # Load structure
  first_line <- readLines(par_file, n = 1)
  param_data_original <- read.table(
    par_file, header = TRUE, sep = "\t", skip = 1,
    stringsAsFactors = FALSE, check.names = FALSE, comment.char = ""
  )

  param_structure <- list(
    is_tabular = TRUE,
    first_line = first_line,
    param_data_original = param_data_original,
    param_data_working = param_data_original
  )

  original_values <- list()
  for (param_name in par_bounds$parameter) {
    param_cols <- find_parameter_column(param_name, colnames(param_data_original),
                                        return_all = TRUE)
    if (length(param_cols) > 0) {
      all_values <- numeric()
      for (col in param_cols) {
        all_values <- c(all_values, param_data_original[[col]])
      }
      original_values[[param_name]] <- all_values
    }
  }

  # Apply optimal parameters
  modify_parameter_table_fast(
    par_file, params_optimal, par_bounds, original_values,
    param_structure, zones_to_modify, quiet = TRUE
  )

  # Run with full outputs
  final_run <- tryCatch({
    run_cosero(
      cosero_path = cosero_path,
      defaults_settings = defaults_settings,
      read_outputs = TRUE,
      quiet = TRUE
    )
  }, error = function(e) {
    warning("Final run failed: ", e$message)
    NULL
  })

  if (!is.null(final_run) && final_run$success) {
    if (verbose) cat("Final run completed\n")
    return(final_run)
  }

  return(NULL)
}

# 6 Main Optimization Functions #####

#' Optimize with DDS
#' 
#' @param cosero_path COSERO project path
#' @param par_bounds Parameter bounds from create_optimization_bounds()
#' @param target_subbasins Subbasin IDs for calibration
#' @param zones_to_modify Zone IDs to modify (NULL = all zones)
#' @param metric Single or vector of metrics ("NSE", "KGE", "lnNSE", "rNSE", etc.)
#' @param metric_weights Weights for multiple metrics
#' @param subbasin_weights Weights for multiple subbasins
#' @param aggregation Subbasin aggregation method
#' @param defaults_settings COSERO settings
#' @param max_iter Maximum iterations
#' @param r DDS perturbation parameter (default 0.2)
#' @param verbose Print progress
#' @param read_final_outputs Read full outputs for final run
#' @param use_minimal_reading Fast mode (statistics + runoff only)
#' 
#' @return List with par, value, history, par_bounds, final_run, etc.
#' @export
#' @examples
#' \dontrun{
#' # Example 1: Basic single-objective optimization
#' par_bounds <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   lower = c(1, 2),
#'   upper = c(6, 8)
#' )
#'
#' result <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   defaults_settings = list(SPINUP = 365),
#'   max_iter = 1000
#' )
#'
#' # View results
#' print(result$par_bounds)
#' plot_cosero_optimization(result)
#'
#' # Example 2: Multi-objective optimization (NSE + KGE)
#' result_multi <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = c("NSE", "KGE"),
#'   metric_weights = c(0.6, 0.4),
#'   max_iter = 2000
#' )
#'
#' # Example 3: Multiple subbasins with weighted aggregation
#' result_multi_basin <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = c("001", "002", "003"),
#'   metric = "NSE",
#'   aggregation = "weighted",
#'   subbasin_weights = c(0.5, 0.3, 0.2),  # Weight by area/importance
#'   max_iter = 1500
#' )
#'
#' # Example 4: Optimize only specific zones
#' result_zones <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   zones_to_modify = c(1, 2, 5),  # Only modify zones 1, 2, and 5
#'   target_subbasins = "001",
#'   metric = "KGE",
#'   max_iter = 1000
#' )
#'
#' # Example 5: Complete workflow with export
#' # Define parameters
#' par_bounds <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX", "LP", "FC"),
#'   lower = c(1, 2, 0.3, 100),
#'   upper = c(6, 8, 1.0, 500)
#' )
#'
#' # Run optimization
#' result <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   defaults_settings = list(
#'     SPINUP = 365,
#'     OUTPUTTYPE = 1
#'   ),
#'   max_iter = 2000,
#'   verbose = TRUE
#' )
#'
#' # Visualize and export
#' plot_cosero_optimization(result)
#' export_cosero_optimization(result, "D:/optimization_results")
#'
#' # Access optimal parameters
#' optimal_params <- result$par_bounds[, c("parameter", "optimal_value")]
#' print(optimal_params)
#' }
optimize_cosero_dds <- function(cosero_path,
                         par_bounds,
                         target_subbasins = "001",
                         zones_to_modify = NULL,
                         metric = "NSE",
                         metric_weights = NULL,
                         subbasin_weights = NULL,
                         aggregation = "mean",
                         defaults_settings = NULL,
                         max_iter = 1000,
                         r = 0.2,
                         verbose = TRUE,
                         read_final_outputs = TRUE,
                         use_minimal_reading = TRUE) {
  
  n_params <- nrow(par_bounds)
  
  if (verbose) {
    cat("DDS Optimization:\n")
    cat("  Parameters:", n_params, "\n")
    cat("  Target subbasins:", paste(target_subbasins, collapse = ", "), "\n")
    cat("  Metrics:", paste(metric, collapse = ", "), "\n")
    if (length(metric) > 1) {
      cat("  Metric weights:", paste(round(metric_weights, 3), collapse = ", "), "\n")
    }
    cat("  Aggregation:", aggregation, "\n")
    cat("  Max iterations:", max_iter, "\n\n")
  }
  
  obj_fun <- create_objective_function(
    cosero_path, par_bounds, target_subbasins, zones_to_modify,
    metric, metric_weights, subbasin_weights, aggregation,
    defaults_settings, verbose, use_minimal_reading
  )
  
  lower <- par_bounds$min
  upper <- par_bounds$max
  
  cat("Starting DDS...\n")
  start_time <- Sys.time()
  
  result <- dds_optimize(obj_fun, lower, upper, max_iter, r, verbose)
  
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat("\nCompleted in", round(runtime, 1), "seconds\n")
    cat("Best objective:", round(-result$value, 4), "\n")
  }
  
  # Add parameter info
  par_bounds$optimal_value <- result$par

  # Final run with full outputs
  final_run_data <- NULL
  if (read_final_outputs) {
    final_run_data <- run_final_optimization(
      cosero_path, par_bounds, zones_to_modify, defaults_settings, verbose
    )
  }
  
  list(
    par = result$par,
    value = result$value,
    history = result$history,
    par_bounds = par_bounds,
    runtime_seconds = runtime,
    metric = metric,
    metric_weights = metric_weights,
    aggregation = aggregation,
    algorithm = "DDS",
    final_run = final_run_data,
    target_subbasins = target_subbasins,
    zones_to_modify = zones_to_modify,
    cosero_path = cosero_path,
    defaults_settings = defaults_settings
  )
}

#' Optimize with SCE-UA
#' 
#' Requires rtop package.
#' 
#' @inheritParams optimize_cosero_dds
#' @param maxn Max evaluations
#' @param kstop Convergence check frequency
#' @param pcento Convergence threshold
#' @param ngs Number of complexes
#' 
#' @export
#' @examples
#' \dontrun{
#' # SCE-UA requires rtop package
#' # install.packages("rtop")
#'
#' # Example 1: Basic SCE-UA optimization
#' par_bounds <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX", "LP"),
#'   lower = c(1, 2, 0.3),
#'   upper = c(6, 8, 1.0)
#' )
#'
#' result <- optimize_cosero_sce(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   maxn = 5000,
#'   defaults_settings = list(SPINUP = 365)
#' )
#'
#' # Example 2: SCE-UA with custom convergence criteria
#' result_custom <- optimize_cosero_sce(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "KGE",
#'   maxn = 10000,
#'   kstop = 5,      # Check convergence every 5 shuffles
#'   pcento = 0.001, # Stricter convergence threshold
#'   ngs = 3,        # More complexes for better exploration
#'   verbose = TRUE
#' )
#'
#' # Compare DDS vs SCE-UA
#' result_dds <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   max_iter = 2000
#' )
#'
#' result_sce <- optimize_cosero_sce(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   maxn = 5000
#' )
#'
#' cat("DDS final NSE:", -result_dds$value, "\n")
#' cat("SCE final NSE:", -result_sce$value, "\n")
#' }
optimize_cosero_sce <- function(cosero_path,
                         par_bounds,
                         target_subbasins = "001",
                         zones_to_modify = NULL,
                         metric = "NSE",
                         metric_weights = NULL,
                         subbasin_weights = NULL,
                         aggregation = "mean",
                         defaults_settings = NULL,
                         maxn = 10000,
                         kstop = 10,
                         pcento = 0.01,
                         ngs = 2,
                         verbose = TRUE,
                         read_final_outputs = TRUE,
                         use_minimal_reading = TRUE) {
  
  if (!requireNamespace("rtop", quietly = TRUE)) {
    stop("Package 'rtop' required for SCE-UA")
  }
  
  n_params <- nrow(par_bounds)
  
  if (verbose) {
    cat("SCE-UA Optimization:\n")
    cat("  Parameters:", n_params, "\n")
    cat("  Max evaluations:", maxn, "\n\n")
  }
  
  obj_fun <- create_objective_function(
    cosero_path, par_bounds, target_subbasins, zones_to_modify,
    metric, metric_weights, subbasin_weights, aggregation,
    defaults_settings, verbose, use_minimal_reading
  )
  
  lower <- par_bounds$min
  upper <- par_bounds$max
  
  cat("Starting SCE-UA...\n")
  start_time <- Sys.time()
  
  result <- rtop::sceua(
    FUN = obj_fun,
    par = runif(n_params, lower, upper),
    lower = lower,
    upper = upper,
    maxn = maxn,
    kstop = kstop,
    pcento = pcento,
    ngs = ngs
  )
  
  runtime <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  
  if (verbose) {
    cat("\nCompleted in", round(runtime, 1), "seconds\n")
    cat("Best objective:", round(-result$value, 4), "\n")
  }
  
  par_bounds$optimal_value <- result$par

  # Final run with full outputs
  final_run_data <- NULL
  if (read_final_outputs) {
    final_run_data <- run_final_optimization(
      cosero_path, par_bounds, zones_to_modify, defaults_settings, verbose
    )
  }
  
  list(
    par = result$par,
    value = result$value,
    convergence = result$convergence,
    par_bounds = par_bounds,
    runtime_seconds = runtime,
    metric = metric,
    metric_weights = metric_weights,
    aggregation = aggregation,
    algorithm = "SCE-UA",
    final_run = final_run_data,
    target_subbasins = target_subbasins,
    zones_to_modify = zones_to_modify,
    cosero_path = cosero_path,
    defaults_settings = defaults_settings
  )
}

# 7 Visualization #####

#' Plot Optimization History
#'
#' Visualize the convergence of the optimization algorithm over iterations.
#' Shows best objective value (maximized metric) vs iteration number.
#'
#' @param opt_result Optimization result from optimize_cosero_dds() or optimize_cosero_sce()
#'
#' @return ggplot2 plot object
#' @export
#' @examples
#' \dontrun{
#' # Run optimization
#' par_bounds <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   lower = c(1, 2),
#'   upper = c(6, 8)
#' )
#'
#' result <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   max_iter = 1000
#' )
#'
#' # Plot convergence
#' p <- plot_cosero_optimization(result)
#' print(p)
#'
#' # Save plot
#' ggplot2::ggsave("optimization_history.png", p, width = 8, height = 6)
#'
#' # Compare multiple runs
#' result1 <- optimize_cosero_dds(cosero_path, par_bounds, max_iter = 1000)
#' result2 <- optimize_cosero_dds(cosero_path, par_bounds, max_iter = 2000)
#'
#' p1 <- plot_cosero_optimization(result1) + ggplot2::ggtitle("1000 iterations")
#' p2 <- plot_cosero_optimization(result2) + ggplot2::ggtitle("2000 iterations")
#'
#' # Using patchwork or gridExtra to combine plots
#' # library(patchwork)
#' # p1 + p2
#' }
plot_cosero_optimization <- function(opt_result) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' required")
  }
  
  if (!is.null(opt_result$history)) {
    df_plot <- data.frame(
      iteration = opt_result$history$iteration,
      metric = -opt_result$history$objective
    )
    
    ggplot2::ggplot(df_plot, ggplot2::aes(x = iteration, y = metric)) +
      ggplot2::geom_line(color = "blue", linewidth = 1) +
      ggplot2::theme_bw() +
      ggplot2::labs(
        x = "Iteration",
        y = if (length(opt_result$metric) == 1) opt_result$metric else "Combined",
        title = paste(opt_result$algorithm, "Optimization History")
      )
  } else {
    warning("No history available")
    NULL
  }
}

# 8 Export #####

#' Export Optimization Results
#'
#' Export optimization results to CSV and JSON files including optimal parameters,
#' optimization history, and summary statistics.
#'
#' @param opt_result Optimization result from optimize_cosero_dds() or optimize_cosero_sce()
#' @param output_dir Output directory path (created if doesn't exist)
#'
#' @details
#' Creates three files in output_dir:
#' \itemize{
#'   \item optimal_parameters.csv - Parameter bounds with optimal values
#'   \item optimization_history.csv - Iteration-by-iteration convergence (DDS only)
#'   \item optimization_summary.csv - Summary metadata (algorithm, runtime, metrics)
#' }
#'
#' @return Invisible NULL (called for side effects)
#' @export
#' @examples
#' \dontrun{
#' # Run optimization
#' par_bounds <- create_optimization_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   lower = c(1, 2),
#'   upper = c(6, 8)
#' )
#'
#' result <- optimize_cosero_dds(
#'   cosero_path = "D:/COSERO_project",
#'   par_bounds = par_bounds,
#'   target_subbasins = "001",
#'   metric = "NSE",
#'   max_iter = 1000
#' )
#'
#' # Export to specific directory
#' export_cosero_optimization(result, "D:/optimization_results")
#'
#' # Export to dated subdirectory
#' date_str <- format(Sys.Date(), "%Y%m%d")
#' output_path <- file.path("D:/results", paste0("opt_", date_str))
#' export_cosero_optimization(result, output_path)
#'
#' # Read exported results
#' opt_params <- read.csv(file.path(output_path, "optimal_parameters.csv"))
#' opt_history <- read.csv(file.path(output_path, "optimization_history.csv"))
#' opt_summary <- read.csv(file.path(output_path, "optimization_summary.csv"))
#' }
  
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # Parameters
  write.csv(opt_result$par_bounds, 
            file.path(output_dir, "optimal_parameters.csv"),
            row.names = FALSE)
  
  # History
  if (!is.null(opt_result$history)) {
    write.csv(opt_result$history,
              file.path(output_dir, "optimization_history.csv"),
              row.names = FALSE)
  }
  
  # Summary
  summary <- data.frame(
    algorithm = opt_result$algorithm,
    optimal_value = -opt_result$value,
    runtime_seconds = opt_result$runtime_seconds,
    n_parameters = length(opt_result$par),
    metric = paste(opt_result$metric, collapse = ", "),
    target_subbasins = paste(opt_result$target_subbasins, collapse = ", "),
    aggregation = opt_result$aggregation,
    converged = if (!is.null(opt_result$convergence)) opt_result$convergence else NA
  )

  write.csv(summary,
            file.path(output_dir, "optimization_summary.csv"),
            row.names = FALSE)

  cat("Results exported to:", output_dir, "\n")

  invisible(NULL)
}
