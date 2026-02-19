# =============================================================================
# COSERO Parameter Optimization
# DDS and SCE-UA optimization with multi-objective support
# =============================================================================

#' @importFrom stats rnorm runif setNames
#' @importFrom utils head read.table write.csv
NULL

utils::globalVariables(c("iteration", "metric"))

# 0 Helper: Zone-Subbasin Mapping #####

#' Get Zones for Subbasins
#'
#' Returns zone IDs belonging to specified subbasins using topology.txt.
#'
#' @param cosero_path COSERO project path
#' @param subbasins Subbasin IDs (character or numeric), or "all" for all subbasins
#' @param quiet Suppress messages
#'
#' @return List with zones (integer vector) and subbasins (character vector)
#' @keywords internal
get_zones_for_subbasins <- function(cosero_path, subbasins = "all", quiet = FALSE) {
  # Try to read topology from output folder

  topology <- read_topology(file.path(cosero_path, "output"), quiet = TRUE)

  if (is.null(topology)) {
    if (!quiet) message("No topology.txt found - will modify all zones")
    return(list(zones = NULL, subbasins = subbasins))
  }

  # Get all available subbasins
  all_subbasins <- unique(topology$NB)

  # Handle "all"

  if (length(subbasins) == 1 && tolower(subbasins) == "all") {
    subbasins <- sprintf("%03d", all_subbasins)
    zones <- topology$NZ
    if (!quiet) message("Using all ", length(all_subbasins), " subbasins (", length(zones), " zones)")
    return(list(zones = zones, subbasins = subbasins))
  }

  # Convert to numeric for matching
  subbasin_nums <- as.numeric(subbasins)

  # Check if requested subbasins exist
  missing <- subbasin_nums[!subbasin_nums %in% all_subbasins]
  if (length(missing) > 0) {
    warning("Subbasins not found in topology: ", paste(missing, collapse = ", "))
  }

  # Get zones for requested subbasins
  zones <- topology$NZ[topology$NB %in% subbasin_nums]

  if (!quiet) {
    message("Subbasins ", paste(subbasins, collapse = ", "),
            " -> ", length(zones), " zones: ",
            paste(head(zones, 10), collapse = ", "),
            if (length(zones) > 10) "..." else "")
  }

  return(list(zones = zones, subbasins = subbasins))
}

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

  if (length(lower) != n_params) stop("'lower' must match 'parameters' length", call. = FALSE)
  if (length(upper) != n_params) stop("'upper' must match 'parameters' length", call. = FALSE)
  if (any(lower >= upper)) stop("All lower < upper required", call. = FALSE)

  # Default values: use midpoint if not specified
  if (is.null(default)) {
    default <- (lower + upper) / 2
  }
  if (length(default) != n_params) {
    stop("'default' must match 'parameters' length", call. = FALSE)
  }

  # Modification type: default to relchg
  if (is.null(modification_type)) {
    modification_type <- rep("relchg", n_params)
  }
  if (length(modification_type) != n_params) {
    stop("'modification_type' must match 'parameters' length", call. = FALSE)
  }
  if (!all(modification_type %in% c("relchg", "abschg"))) {
    stop("modification_type must be 'relchg' or 'abschg'", call. = FALSE)
  }

  # Description: use parameter name if not specified
  if (is.null(description)) {
    description <- paste("Optimization parameter:", parameters)
  }
  if (length(description) != n_params) {
    stop("'description' must match 'parameters' length", call. = FALSE)
  }

  # Category: use "optimization" if not specified
  if (is.null(category)) {
    category <- rep("optimization", n_params)
  }
  if (length(category) != n_params) {
    stop("'category' must match 'parameters' length", call. = FALSE)
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
      stop("Unknown metric: ", metric, call. = FALSE)
    )
  }, error = function(e) NA)
  
  return(val)
}

# 3 Objective Function #####

#' Create Objective Function
#'
#' Creates a closure that evaluates COSERO model performance for optimization.
#' Handles parameter modification, model execution, and metric calculation.
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
#'
#' @return A function that takes a parameter vector and returns the objective value
#'   (negative metric for minimization). Has attributes "backup_file" and "par_file"
#'   for restoring the original parameter file after optimization.
#' @keywords internal
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


  # Force evaluation of all closure variables to avoid lazy evaluation issues
  force(cosero_path)
  force(par_bounds)
  force(target_subbasins)
  force(zones_to_modify)
  force(metric)
  force(metric_weights)
  force(subbasin_weights)
  force(aggregation)
  force(defaults_settings)
  force(verbose)
  force(use_minimal_reading)

  n_subbasins <- length(target_subbasins)
  n_metrics <- length(metric)
  
  # Validate weights
  if (n_metrics > 1) {
    if (is.null(metric_weights)) {
      metric_weights <- rep(1/n_metrics, n_metrics)
    }
    if (length(metric_weights) != n_metrics) {
      stop("metric_weights must match metric length", call. = FALSE)
    }
    if (abs(sum(metric_weights) - 1) > 1e-6) {
      stop("metric_weights must sum to 1", call. = FALSE)
    }
  }
  
  if (aggregation == "weighted") {
    if (is.null(subbasin_weights)) {
      stop("subbasin_weights required for aggregation='weighted'", call. = FALSE)
    }
    if (length(subbasin_weights) != n_subbasins) {
      stop("subbasin_weights must match target_subbasins length", call. = FALSE)
    }
    if (abs(sum(subbasin_weights) - 1) > 1e-6) {
      stop("subbasin_weights must sum to 1", call. = FALSE)
    }
  }
  
  if (!aggregation %in% c("mean", "weighted", "min", "product")) {
    stop("aggregation must be: mean, weighted, min, or product", call. = FALSE)
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
  if (!file.exists(par_file)) stop("Parameter file not found: ", par_file, call. = FALSE)

  # Create backup of original parameter file in parameterfile_backup folder
  backup_dir <- file.path(cosero_path, "input", "parameterfile_backup")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)

  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  backup_filename <- paste0(par_filename, ".backup_", timestamp)
  backup_file <- file.path(backup_dir, backup_filename)
  file.copy(par_file, backup_file, overwrite = TRUE)
  if (verbose) cat("Backed up parameter file to:", file.path("parameterfile_backup", backup_filename), "\n")

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

  # Return objective function with backup info as attribute
  eval_count <- 0

  obj_fun <- function(x) {
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
    
    # Run COSERO (always quiet - DDS verbosity is separate)
    result <- tryCatch({
      if (use_minimal_reading) {
        run_result <- run_cosero(
          project_path = cosero_path,
          defaults_settings = defaults_settings,
          read_outputs = FALSE,
          quiet = TRUE,
          create_backup = FALSE
        )
        if (run_result$success) {
          output_dir <- file.path(cosero_path, "output")
          run_result$output_data <- read_cosero_minimal(output_dir, quiet = TRUE)
        }
        run_result
      } else {
        run_cosero(
          project_path = cosero_path,
          defaults_settings = defaults_settings,
          quiet = TRUE,
          create_backup = FALSE
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

    return(obj_value)
  }

  # Store backup path as attribute for restoration after optimization
  attr(obj_fun, "backup_file") <- backup_file
  attr(obj_fun, "par_file") <- par_file

  return(obj_fun)
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
      project_path = cosero_path,
      defaults_settings = defaults_settings,
      read_outputs = TRUE,
      quiet = TRUE,
      create_backup = FALSE
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

#' Optimize COSERO Parameters with DDS Algorithm
#'
#' Dynamically Dimensioned Search (DDS) optimization for COSERO model calibration.
#' Supports single/multi-objective optimization, multiple subbasins, and automatic
#' zone-subbasin mapping via topology.txt.
#'
#' @param cosero_path Character. Path to the COSERO project directory containing
#'   \code{input/} and \code{output/} folders, and \code{COSERO.exe}.
#'
#' @param par_bounds Data frame with parameter bounds. Create using
#'   \code{\link{create_optimization_bounds}} for custom bounds or
#'   \code{\link{load_parameter_bounds}} to use package defaults.
#'   Required columns: parameter, min, max, default, modification_type.
#'
#' @param target_subbasins Character vector. Subbasin IDs to calibrate for
#'   (e.g., \code{"001"}, \code{c("001", "002")}, or \code{"all"}).
#'   Controls both which zones are modified AND which subbasins are used
#'   for metric calculation. Zone-subbasin mapping requires \code{topology.txt}
#'   in output folder (generated by running COSERO at least once).
#'
#' @param zones_to_modify Integer vector or NULL. Specific zone IDs to modify.
#'   If NULL (default), zones are automatically determined from \code{target_subbasins}
#'   using topology.txt. Use this for manual control, e.g., to optimize only
#'   certain elevation bands or land use types.
#'
#' @param metric Character vector. Performance metric(s) to optimize.
#'   Options: \code{"NSE"} (default), \code{"KGE"}, \code{"lnNSE"}, \code{"rNSE"},
#'   \code{"RMSE"}, \code{"PBIAS"}, \code{"VE"}.
#'   For multi-objective, provide vector: \code{c("NSE", "KGE")}.
#'
#' @param metric_weights Numeric vector or NULL. Weights for combining multiple
#'   metrics (must sum to 1). Required when \code{metric} has length greater than 1.
#'   Example: \code{c(0.6, 0.4)} for 0.6 NSE plus 0.4 KGE.
#'
#' @param subbasin_weights Numeric vector or NULL. Weights for aggregating metrics
#'   across subbasins. Required when \code{aggregation = "weighted"}.
#'   Must have same length as \code{target_subbasins} and sum to 1.
#'
#' @param aggregation Character. Method to aggregate metrics across multiple subbasins.
#'   Options: \code{"mean"} (default), \code{"weighted"} (requires subbasin_weights),
#'   \code{"min"} (worst subbasin), \code{"product"}.
#'
#' @param defaults_settings Named list. COSERO configuration settings including
#'   \code{STARTDATE}, \code{ENDDATE}, \code{SPINUP}, \code{OUTPUTTYPE}, \code{PARAFILE}.
#'
#' @param max_iter Integer. Maximum number of DDS iterations.
#'   Recommended: 50-100 (quick test), 500-1000 (standard), 2000-5000 (production).
#'
#' @param r Numeric. DDS perturbation factor controlling neighborhood size.
#'   Default 0.2 works well for most cases. Range: 0.1 (more local) to 0.3 (more global).
#'
#' @param verbose Logical. If TRUE (default), prints progress bar and messages.
#'
#' @param read_final_outputs Logical. If TRUE (default), runs COSERO once more
#'   with optimal parameters and returns full output data.
#'
#' @param use_minimal_reading Logical. If TRUE (default), only reads statistics.txt
#'   and COSERO.runoff during optimization for faster I/O.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{par}: Numeric vector of optimal parameter values
#'     \item \code{value}: Best objective value (negative of metric, since DDS minimizes)
#'     \item \code{history}: Data frame with columns: iteration, objective (for plotting convergence)
#'     \item \code{par_bounds}: Input par_bounds with added \code{optimal_value} column
#'     \item \code{runtime_seconds}: Total optimization time in seconds
#'     \item \code{final_run}: COSERO run result with optimal parameters (if read_final_outputs=TRUE)
#'     \item \code{optimized_par_file}: Path to the saved optimized parameter file
#'     \item \code{target_subbasins}: Subbasins used for calibration
#'     \item \code{zones_to_modify}: Zone IDs that were modified
#'     \item \code{metric}: Metric(s) used
#'     \item \code{algorithm}: "DDS"
#'   }
#'
#' @details
#' \strong{File handling:}
#' The original parameter file is backed up before optimization and automatically
#' restored after completion. Optimized parameters are saved to a new file:
#' \code{output/para_optimized_NB\{subbasins\}_\{metric\}_\{timestamp\}.txt}
#'
#' \strong{Zone-subbasin mapping:}
#' Requires \code{topology.txt} in the output folder, which contains NZ (zone) and
#' NB (subbasin) columns. This file is generated when COSERO runs. If not found,
#' all zones will be modified regardless of \code{target_subbasins}.
#'
#' \strong{Algorithm Details:}
#' DDS is a single-solution heuristic that scales the search dimension based on the
#' remaining computational budget. The algorithm starts by modifying all parameters
#' (global search) and linearly decreases the probability of perturbing any individual
#' parameter as the iteration count approaches \code{max_iter} (local search).
#' New candidate solutions are generated by perturbing the current best parameters
#' with a standard normal random variable scaled by the factor \code{r}.
#' The solution is updated only if the objective function value improves (greedy acceptance).
#' This dynamic adjustment of the search dimension allows DDS to converge rapidly
#' to good solutions in high-dimensional parameter spaces without requiring the extensive
#' sampling of population-based methods.
#'
#' @seealso
#' \code{\link{optimize_cosero_sce}} for SCE-UA algorithm (more robust, slower),
#' \code{\link{create_optimization_bounds}} to define custom parameter bounds,
#' \code{\link{load_parameter_bounds}} to load predefined bounds from CSV,
#' \code{\link{plot_cosero_optimization}} to visualize convergence history,
#' \code{\link{export_cosero_optimization}} to export results to CSV files
#'
#' @references
#' Tolson, B.A. and Shoemaker, C.A. (2007). Dynamically dimensioned search algorithm
#' for computationally efficient watershed model calibration. Water Resources Research, 43(1).
#'
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

  # Auto-determine zones from subbasins if not specified
  if (is.null(zones_to_modify)) {
    zone_mapping <- get_zones_for_subbasins(cosero_path, target_subbasins, quiet = !verbose)
    zones_to_modify <- zone_mapping$zones
    target_subbasins <- zone_mapping$subbasins
  }

  if (verbose) {
    cat("DDS Optimization:\n")
    cat("  Parameters:", n_params, "\n")
    cat("  Target subbasins:", paste(target_subbasins, collapse = ", "), "\n")
    cat("  Zones to modify:", if (is.null(zones_to_modify)) "all" else length(zones_to_modify), "\n")
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
  
  if (verbose) cat("Starting DDS...\n")
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

  # Restore original parameter file from backup
  backup_file <- attr(obj_fun, "backup_file")
  par_file <- attr(obj_fun, "par_file")
  if (!is.null(backup_file) && file.exists(backup_file)) {
    file.copy(backup_file, par_file, overwrite = TRUE)
    file.remove(backup_file)
    if (verbose) cat("Restored original parameter file\n")
  }

  # Save optimized parameter file
  subbasin_str <- paste(gsub("^0+", "", target_subbasins), collapse = "_")
  metric_str <- paste(metric, collapse = "_")
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  opt_filename <- sprintf("para_optimized_NB%s_%s_%s.txt", subbasin_str, metric_str, timestamp)
  
  # Saved to output folder
  opt_file <- file.path(cosero_path, "output", opt_filename)

  # Copy original and apply optimal parameters
  file.copy(par_file, opt_file, overwrite = TRUE)
  optimal_params <- setNames(as.list(par_bounds$optimal_value), par_bounds$parameter)

  # Read original values and apply modifications
  param_data_orig <- read.table(par_file, header = TRUE, sep = "\t", skip = 1,
                                stringsAsFactors = FALSE, check.names = FALSE, comment.char = "")
  original_values <- list()
  for (param_name in par_bounds$parameter) {
    param_cols <- find_parameter_column(param_name, colnames(param_data_orig), return_all = TRUE)
    if (length(param_cols) > 0) {
      all_values <- numeric()
      for (col in param_cols) all_values <- c(all_values, param_data_orig[[col]])
      original_values[[param_name]] <- all_values
    }
  }
  modify_parameter_table(opt_file, optimal_params, par_bounds, original_values,
                         zones = zones_to_modify, quiet = TRUE, add_timestamp = TRUE)
  if (verbose) cat("Saved optimized parameters to:", opt_filename, "\n")

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
    defaults_settings = defaults_settings,
    optimized_par_file = opt_file
  )
}
#' Optimize COSERO Parameters with SCE-UA Algorithm
#'
#' Shuffled Complex Evolution (SCE-UA) optimization for COSERO model calibration.
#' Supports single/multi-objective optimization, multiple subbasins, and automatic
#' zone-subbasin mapping via topology.txt. Requires the \code{rtop} package.
#'
#' @inheritParams optimize_cosero_dds
#' @param maxn Maximum number of function evaluations
#' @param kstop Number of shuffling loops to check convergence
#' @param pcento Percentage change for convergence criterion
#' @param ngs Number of complexes (sub-populations). Higher values increase global
#'   search capability but slow down convergence.
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{par}: Numeric vector of optimal parameter values
#'     \item \code{value}: Best objective value (negative of metric, since SCE minimizes)
#'     \item \code{convergence}: Convergence status from SCE-UA
#'     \item \code{par_bounds}: Input par_bounds with added \code{optimal_value} column
#'     \item \code{runtime_seconds}: Total optimization time in seconds
#'     \item \code{final_run}: COSERO run result with optimal parameters (if read_final_outputs=TRUE)
#'     \item \code{optimized_par_file}: Path to the saved optimized parameter file
#'     \item \code{target_subbasins}: Subbasins used for calibration
#'     \item \code{zones_to_modify}: Zone IDs that were modified
#'     \item \code{metric}: Metric(s) used
#'     \item \code{algorithm}: "SCE-UA"
#'   }
#'
#' @details
#' \strong{File handling:}
#' The original parameter file is backed up before optimization and automatically
#' restored after completion. Optimized parameters are saved to a new file:
#' \code{output/para_optimized_NB\{subbasins\}_\{metric\}_\{timestamp\}.txt}
#'
#' \strong{Algorithm Details:}
#' SCE-UA is a global optimization strategy that combines the simplex procedure with
#' the concepts of controlled random search, competitive evolution, and complex shuffling.
#' The population is divided into \code{ngs} complexes, each evolving independently
#' via the Competitive Complex Evolution (CCE) algorithm (using Nelder-Mead simplex).
#' Periodically, the complexes are shuffled to share information, ensuring global
#' exploration and preventing the search from becoming trapped in local optima.
#'
#' @seealso
#' \code{\link{optimize_cosero_dds}} for DDS algorithm (faster, simpler),
#' \code{\link{create_optimization_bounds}} to define custom parameter bounds
#'
#' @references
#' Duan, Q., Sorooshian, S., and Gupta, V. (1992). Effective and efficient global
#' optimization for conceptual rainfall-runoff models. Water Resources Research, 28(4), 1015-1031.
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
    stop("Package 'rtop' required for SCE-UA", call. = FALSE)
  }

  n_params <- nrow(par_bounds)

  # Auto-determine zones from subbasins if not specified
  if (is.null(zones_to_modify)) {
    zone_mapping <- get_zones_for_subbasins(cosero_path, target_subbasins, quiet = !verbose)
    zones_to_modify <- zone_mapping$zones
    target_subbasins <- zone_mapping$subbasins
  }

  if (verbose) {
    cat("SCE-UA Optimization:\n")
    cat("  Parameters:", n_params, "\n")
    cat("  Target subbasins:", paste(target_subbasins, collapse = ", "), "\n")
    cat("  Zones to modify:", if (is.null(zones_to_modify)) "all" else length(zones_to_modify), "\n")
    cat("  Max evaluations:", maxn, "\n\n")
  }

  obj_fun <- create_objective_function(
    cosero_path, par_bounds, target_subbasins, zones_to_modify,
    metric, metric_weights, subbasin_weights, aggregation,
    defaults_settings, verbose, use_minimal_reading
  )
  
  lower <- par_bounds$min
  upper <- par_bounds$max
  
  if (verbose) cat("Starting SCE-UA...\n")
  start_time <- Sys.time()

  # Custom progress tracker (native SCE output is inconsistent)
  eval_count <- 0
  best_value <- Inf
  print_interval <- max(1, floor(maxn / 20))  # ~20 progress updates

  obj_fun_wrapped <- function(x) {
    eval_count <<- eval_count + 1
    val <- obj_fun(x)
    if (val < best_value) best_value <<- val
    if (verbose && (eval_count %% print_interval == 0 || eval_count == 1)) {
      cat(sprintf("Eval %d | Best %s: %.4f\n",
                  eval_count, paste(metric, collapse = "+"), -best_value))
    }
    val
  }

  result <- rtop::sceua(
    OFUN = obj_fun_wrapped,
    pars = runif(n_params, lower, upper),
    lower = lower,
    upper = upper,
    maxn = maxn,
    kstop = kstop,
    pcento = pcento,
    ngs = ngs,
    iprint = 0  # Suppress native output, use our tracker
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

  # Restore original parameter file from backup
  backup_file <- attr(obj_fun, "backup_file")
  par_file <- attr(obj_fun, "par_file")
  if (!is.null(backup_file) && file.exists(backup_file)) {
    file.copy(backup_file, par_file, overwrite = TRUE)
    file.remove(backup_file)
    if (verbose) cat("Restored original parameter file\n")
  }

  # Save optimized parameter file
  subbasin_str <- paste(gsub("^0+", "", target_subbasins), collapse = "_")
  metric_str <- paste(metric, collapse = "_")
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  opt_filename <- sprintf("para_optimized_NB%s_%s_%s.txt", subbasin_str, metric_str, timestamp)
  
  # Saved to output folder
  opt_file <- file.path(cosero_path, "output", opt_filename)

  # Copy original and apply optimal parameters
  file.copy(par_file, opt_file, overwrite = TRUE)
  optimal_params <- setNames(as.list(par_bounds$optimal_value), par_bounds$parameter)

  # Read original values and apply modifications
  param_data_orig <- read.table(par_file, header = TRUE, sep = "\t", skip = 1,
                                stringsAsFactors = FALSE, check.names = FALSE, comment.char = "")
  original_values <- list()
  for (param_name in par_bounds$parameter) {
    param_cols <- find_parameter_column(param_name, colnames(param_data_orig), return_all = TRUE)
    if (length(param_cols) > 0) {
      all_values <- numeric()
      for (col in param_cols) all_values <- c(all_values, param_data_orig[[col]])
      original_values[[param_name]] <- all_values
    }
  }
  modify_parameter_table(opt_file, optimal_params, par_bounds, original_values,
                         zones = zones_to_modify, quiet = TRUE, add_timestamp = TRUE)
  if (verbose) cat("Saved optimized parameters to:", opt_filename, "\n")

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
    defaults_settings = defaults_settings,
    optimized_par_file = opt_file
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
    stop("Package 'ggplot2' required", call. = FALSE)
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
#' Export optimization results to CSV files including optimal parameters,
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
export_cosero_optimization <- function(opt_result, output_dir) {

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

