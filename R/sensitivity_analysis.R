# COSERO Sensitivity Analysis
# Sobol-based global sensitivity analysis for COSERO parameters
# Date: 2025-11-04

#' @importFrom dplyr filter mutate select arrange group_by summarise bind_rows %>% desc
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_dbl map_dfr map2_df
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_line geom_ribbon geom_hline
#' @importFrom ggplot2 geom_histogram geom_density geom_vline annotate after_stat
#' @importFrom ggplot2 coord_flip coord_cartesian facet_wrap
#' @importFrom ggplot2 theme_minimal theme_bw theme element_text element_rect element_blank element_line
#' @importFrom ggplot2 labs ggtitle scale_color_manual scale_fill_gradient2 guide_legend
#' @importFrom sensobol sobol_matrices sobol_indices
#' @importFrom hydroGOF NSE KGE
#' @importFrom parallel makeCluster stopCluster clusterExport clusterEvalQ detectCores
#' @importFrom foreach foreach %dopar%
#' @importFrom doSNOW registerDoSNOW
#' @importFrom grDevices dev.cur dev.off
#' @importFrom stats median quantile
#' @importFrom utils flush.console read.csv read.table write.csv
NULL

utils::globalVariables(c(
  "year_month", "year", "output", "parameter", "value", "date",
  "sensitivity", "original", "parameters", "std.error", "low.ci", "high.ci",
  "q05", "q25", "q75", "q95", ".", "batch_position", "density",
  # extract_behavioral_runs
  "NSE", "KGE", "pBias", "category", "pass_nse_kge", "pass_pbias"
))

# Helper Functions #####

#' Display Progress for Ensemble Runs
#'
#' @param current Current run number
#' @param total Total number of runs
#' @param start_time Start time of ensemble
#' @param run_type Type of run ("Simulation" or other label)
#' @param quiet Suppress output
#' @keywords internal
display_progress <- function(current, total, start_time, run_type = "Simulation", quiet = FALSE) {
  if (quiet) return(invisible(NULL))

  # Calculate progress
  pct <- round(100 * current / total, 1)

  # Calculate time remaining
  elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  avg_per_run <- elapsed / current
  remaining <- avg_per_run * (total - current)

  # Format time
  format_time <- function(seconds) {
    if (seconds < 60) {
      return(sprintf("%.0fs", seconds))
    } else if (seconds < 3600) {
      mins <- floor(seconds / 60)
      secs <- seconds %% 60
      return(sprintf("%dm %02.0fs", mins, secs))
    } else {
      hours <- floor(seconds / 3600)
      mins <- floor((seconds %% 3600) / 60)
      return(sprintf("%dh %02dm", hours, mins))
    }
  }

  # Create progress message
  msg <- sprintf("\r[%s %d/%d] (%.1f%%) - Elapsed: %s - Est. remaining: %s          ",
                 run_type, current, total, pct,
                 format_time(elapsed),
                 format_time(remaining))

  cat(msg)
  flush.console()

  # Newline on completion
  if (current == total) {
    cat("\n")
  }
}

#' Format Time Duration
#'
#' @param seconds Time in seconds
#' @return Formatted string
#' @keywords internal
format_time_duration <- function(seconds) {
  if (seconds < 60) {
    return(sprintf("%.1fs", seconds))
  } else if (seconds < 3600) {
    mins <- floor(seconds / 60)
    secs <- seconds %% 60
    return(sprintf("%dm %02.0fs", mins, secs))
  } else {
    hours <- floor(seconds / 3600)
    mins <- floor((seconds %% 3600) / 60)
    return(sprintf("%dh %02dm", hours, mins))
  }
}

#' Find Parameter Column with Case-Insensitive Matching
#'
#' Searches for a parameter name in column names, handling:
#' - Case insensitivity (TCOR, TCor, tcor all match)
#' - Optional trailing underscore (TCOR_ vs TCOR)
#' - Monthly variants (TCOR matches TCor1, TCor2, ..., TCor12)
#'
#' @param param_name Parameter name to search for
#' @param col_names Column names to search in
#' @param return_all If TRUE, return all monthly variants (1-12); if FALSE, return first match
#' @return Character vector of matching column names (empty if none found)
#' @export
find_parameter_column <- function(param_name, col_names, return_all = FALSE) {
  # Try direct match with case insensitivity and underscore variants
  patterns <- c(
    paste0("^", param_name, "_$"),  # Exact match with underscore
    paste0("^", param_name, "$")    # Exact match without underscore
  )

  for (pattern in patterns) {
    matches <- grep(pattern, col_names, ignore.case = TRUE, value = TRUE)
    if (length(matches) > 0) {
      return(matches[1])  # Return first match
    }
  }

  # Check for monthly variants (parameter1-12)
  # Remove trailing underscore from param_name if present
  base_param <- sub("_$", "", param_name)

  monthly_matches <- character()
  for (month in 1:12) {
    monthly_patterns <- c(
      paste0("^", base_param, month, "_$"),
      paste0("^", base_param, month, "$")
    )
    for (pattern in monthly_patterns) {
      matches <- grep(pattern, col_names, ignore.case = TRUE, value = TRUE)
      if (length(matches) > 0) {
        monthly_matches <- c(monthly_matches, matches[1])
        break
      }
    }
  }

  if (length(monthly_matches) > 0) {
    if (return_all) {
      return(monthly_matches)  # Return all 12 months
    } else {
      return(monthly_matches[1])  # Return first month only
    }
  }

  return(character(0))  # No match found
}

# 2 Parameter Setup #####

#' Create Custom Parameter Bounds
#'
#' Create parameter bounds manually without using CSV file. This function defines
#' the sampling ranges and modification types for sensitivity analysis parameters.
#'
#' @param parameters Vector of parameter names (e.g., c("BETA", "CTMAX", "TAB1"))
#' @param min Vector of minimum physical values (same length as parameters)
#' @param max Vector of maximum physical values (same length as parameters)
#' @param modification_type Vector of modification types (same length as parameters):
#'   \itemize{
#'     \item "relchg" - Relative change (scales original value to match sampled target relative to default)
#'     \item "abschg" - Absolute change (shifts original value to match sampled target relative to default)
#'   }
#' @param default Vector of default values (required for determining modification factors)
#' @param description Vector of parameter descriptions (optional)
#' @param category Vector of parameter categories (optional)
#' @param sample_min Vector of minimum sampling values (defaults to physical min)
#' @param sample_max Vector of maximum sampling values (defaults to physical max)
#'
#' @return Tibble with parameter bounds containing columns: parameter, description, min, max,
#'   default, modification_type, category, sample_min, sample_max
#'
#' @export
#' @examples
#' \dontrun{
#' # Create custom bounds for 3 parameters
#' custom_bounds <- create_custom_bounds(
#'   parameters = c("BETA", "CTMAX", "TAB1"),
#'   min = c(0.1, 2, 0.1),
#'   max = c(10, 12, 5),
#'   modification_type = c("relchg", "relchg", "abschg"),
#'   default = c(4.5, 5, 1)
#' )
#'
#' # Use in sensitivity analysis
#' sobol_samples <- generate_sobol_samples(
#'   par_bounds = create_sobol_bounds(custom_bounds),
#'   n = 100
#' )
#' }
create_custom_bounds <- function(parameters,
                                  min,
                                  max,
                                  modification_type,
                                  default = NULL,
                                  description = NULL,
                                  category = NULL,
                                  sample_min = NULL,
                                  sample_max = NULL) {

  # Validate inputs
  n_params <- length(parameters)

  if (length(min) != n_params) {
    stop("Length of 'min' must match length of 'parameters'", call. = FALSE)
  }
  if (length(max) != n_params) {
    stop("Length of 'max' must match length of 'parameters'", call. = FALSE)
  }
  if (length(modification_type) != n_params) {
    stop("Length of 'modification_type' must match length of 'parameters'", call. = FALSE)
  }

  # Validate modification types
  valid_types <- c("relchg", "abschg")
  invalid_types <- modification_type[!modification_type %in% valid_types]
  if (length(invalid_types) > 0) {
    stop("Invalid modification_type: ", paste(invalid_types, collapse = ", "),
         "\nValid types are: ", paste(valid_types, collapse = ", "),
         call. = FALSE)
  }

  # Validate min < max
  if (any(min >= max)) {
    invalid_idx <- which(min >= max)
    stop("min must be less than max for parameters: ",
         paste(parameters[invalid_idx], collapse = ", "), call. = FALSE)
  }

  # Set sampling ranges (for Sobol) - Defaults to physical bounds (Target Value strategy)
  if (is.null(sample_min)) {
    sample_min <- min
  }
  if (is.null(sample_max)) {
    sample_max <- max
  }

  # Set defaults for optional parameters
  if (is.null(default)) {
    default <- ifelse(modification_type == "relchg", 1.0, (min + max) / 2)
  } else if (length(default) != n_params) {
    stop("Length of 'default' must match length of 'parameters'", call. = FALSE)
  }

  if (is.null(description)) {
    description <- paste("Parameter:", parameters)
  } else if (length(description) != n_params) {
    stop("Length of 'description' must match length of 'parameters'", call. = FALSE)
  }

  if (is.null(category)) {
    category <- rep("custom", n_params)
  } else if (length(category) != n_params) {
    stop("Length of 'category' must match length of 'parameters'", call. = FALSE)
  }

  # Create tibble in same format as CSV
  bounds <- tibble::tibble(
    parameter = parameters,
    description = description,
    min = min,
    max = max,
    default = default,
    modification_type = modification_type,
    category = category,
    sample_min = sample_min,
    sample_max = sample_max
  )

  return(bounds)
}

#' Load COSERO Parameter Bounds
#'
#' Load parameter bounds from CSV file or use custom bounds for sensitivity analysis.
#' Custom bounds take precedence over CSV file. This function is the first step in
#' setting up a sensitivity analysis workflow.
#'
#' @param bounds_file Path to parameter bounds CSV file (NULL = use package default)
#' @param parameters Vector of parameter names to include (NULL = all)
#' @param custom_bounds Custom parameter bounds tibble from create_custom_bounds() (optional)
#' @param custom_min Vector of minimum values for custom bounds (optional)
#' @param custom_max Vector of maximum values for custom bounds (optional)
#' @param custom_modification_type Vector of modification types for custom bounds (optional)
#'
#' @return Tibble with parameter bounds containing columns: parameter, description, min, max,
#'   default, modification_type, category, sample_min, sample_max
#'
#' @export
#' @examples
#' \dontrun{
#' # Load from package default CSV
#' bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX"))
#'
#' # Load from custom CSV file
#' bounds <- load_parameter_bounds(bounds_file = "my_bounds.csv")
#'
#' # Use custom bounds with create_custom_bounds()
#' custom <- create_custom_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   min = c(0.5, 3),
#'   max = c(8, 10),
#'   modification_type = c("relchg", "relchg")
#' )
#' bounds <- load_parameter_bounds(custom_bounds = custom)
#'
#' # Use custom bounds with direct parameters
#' bounds <- load_parameter_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   custom_min = c(0.5, 3),
#'   custom_max = c(8, 10),
#'   custom_modification_type = c("relchg", "relchg")
#' )
#' }
load_parameter_bounds <- function(bounds_file = NULL,
                                   parameters = NULL,
                                   custom_bounds = NULL,
                                   custom_min = NULL,
                                   custom_max = NULL,
                                   custom_modification_type = NULL) {

  # Option 1: Use pre-built custom_bounds tibble
  if (!is.null(custom_bounds)) {
    if (!is.null(parameters)) {
      custom_bounds <- custom_bounds %>% filter(parameter %in% parameters)
    }
    cat("Using custom parameter bounds (", nrow(custom_bounds), " parameters)\n")
    return(custom_bounds)
  }

  # Option 2: Build custom bounds from individual parameters
  if (!is.null(custom_min) && !is.null(custom_max) && !is.null(custom_modification_type)) {
    if (is.null(parameters)) {
      stop("'parameters' must be specified when using custom_min, custom_max, and custom_modification_type",
           call. = FALSE)
    }

    custom_bounds <- create_custom_bounds(
      parameters = parameters,
      min = custom_min,
      max = custom_max,
      modification_type = custom_modification_type
    )

    cat("Using custom parameter bounds (", nrow(custom_bounds), " parameters)\n")
    return(custom_bounds)
  }

  # Option 3: Load from CSV file
  # Use default parameter bounds file from package if not specified
  if (is.null(bounds_file)) {
    bounds_file <- system.file("extdata", "parameter_bounds.csv", package = "CoseRo")
    if (bounds_file == "") {
      stop("Could not find parameter_bounds.csv in package installation.\n",
           "Either install the package data or provide a custom bounds_file path.",
           call. = FALSE)
    }
  } else if (!file.exists(bounds_file)) {
    stop("Parameter bounds file not found: ", bounds_file,
         "\nEither provide a valid CSV file path or use custom bounds parameters",
         call. = FALSE)
  }

  bounds <- read.csv(bounds_file, stringsAsFactors = FALSE)

  if (!is.null(parameters)) {
    bounds <- bounds %>% filter(parameter %in% parameters)
  }

  # Add sample_min/sample_max if missing - Defaults to physical bounds (Target Value strategy)
  if (!"sample_min" %in% colnames(bounds)) {
    bounds$sample_min <- bounds$min
  }
  if (!"sample_max" %in% colnames(bounds)) {
    bounds$sample_max <- bounds$max
  }

  cat("Loading parameter bounds from CSV (", nrow(bounds), " parameters)\n")
  return(bounds)
}

#' Create Parameter Bounds Matrix for Sobol Sampling
#'
#' @param par_bounds Tibble from load_parameter_bounds()
#' @return Named tibble with min/max rows for sobol_matrices()
#' @export
create_sobol_bounds <- function(par_bounds) {
  # Use sample_min/sample_max for Sobol sampling (not physical min/max)
  bounds_matrix <- data.frame(
    min = par_bounds$sample_min,
    max = par_bounds$sample_max,
    row.names = par_bounds$parameter
  )

  # Transpose to have parameters as columns, min/max as rows
  bounds_t <- as.data.frame(t(bounds_matrix))

  return(as_tibble(bounds_t))
}

# 3 Sobol Sampling #####

#' Generate Sobol Parameter Sets
#'
#' Generates Sobol quasi-random parameter samples for global sensitivity analysis.
#' Uses the sensobol package to create space-filling parameter combinations.
#'
#' @param par_bounds Parameter bounds tibble from create_sobol_bounds()
#' @param n Sample size (total runs = n * (k + 2), where k is number of parameters)
#' @param order Sobol order: "first" for first-order indices only, "second" for first and total-order
#'
#' @return List containing:
#'   \item{sobol_matrix}{Raw Sobol matrices}
#'   \item{parameter_sets}{Scaled parameter sets (tibble with n*(k+2) rows)}
#'   \item{n}{Base sample size}
#'   \item{par_names}{Parameter names}
#'
#' @seealso
#' \code{\link{load_parameter_bounds}} to load parameter bounds from CSV,
#' \code{\link{create_sobol_bounds}} to create bounds matrix for Sobol sampling,
#' \code{\link{calculate_sobol_indices}} to compute sensitivity indices from results
#'
#' @export
#' @examples
#' \dontrun{
#' # Load parameter bounds
#' bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "TAB1"))
#'
#' # Create Sobol bounds
#' sobol_bounds <- create_sobol_bounds(bounds)
#'
#' # Generate 100 Sobol samples (300 total runs for 3 parameters)
#' samples <- generate_sobol_samples(sobol_bounds, n = 100)
#'
#' # Check number of parameter sets
#' nrow(samples$parameter_sets)  # 500 = 100 * (3 + 2)
#' }
generate_sobol_samples <- function(par_bounds, n = 50, order = "first") {
  par_names <- colnames(par_bounds)

  # Generate Sobol matrices
  sobol_mat <- sobol_matrices(N = n, params = par_names, order = order)

  # Scale to parameter bounds
  par_scaled <- sobol_mat %>%
    as_tibble() %>%
    map2_df(., par_bounds, ~ (.x * (.y[2] - .y[1]) + .y[1]))

  cat("Generated", nrow(par_scaled), "parameter sets for", length(par_names), "parameters\n")

  return(list(
    sobol_matrix   = sobol_mat,
    parameter_sets = par_scaled,
    n              = n,
    par_names      = par_names,
    order          = order
  ))
}

# 4 Ensemble Execution #####

#' Run COSERO Ensemble with Parameter Sets (Sequential)
#'
#' Runs multiple COSERO simulations sequentially with different parameter sets.
#' For parallel execution, use run_cosero_ensemble_parallel().
#'
#' @param project_path Path to COSERO project directory
#' @param parameter_sets Tibble with parameter combinations from generate_sobol_samples()
#' @param par_bounds Parameter bounds table with modification types from load_parameter_bounds()
#' @param base_settings List of base COSERO settings (e.g., STARTDATE, ENDDATE, SPINUP)
#' @param par_file Path to parameter file to modify (NULL = read from defaults.txt)
#' @param quiet Logical. If TRUE, suppresses progress messages.
#' @param statevar_source State variable source: 1 = read from parameter file (default), 2 = read from statevar.dmp file (warm start)
#'
#' @return List containing:
#'   \item{results}{List of COSERO output data for each run}
#'   \item{parameter_sets}{Original parameter sets used}
#'   \item{runtime_minutes}{Total runtime in minutes}
#'
#' @seealso
#' \code{\link{run_cosero_ensemble_parallel}} for parallel execution,
#' \code{\link{extract_ensemble_metrics}} to extract pre-calculated metrics from results,
#' \code{\link{calculate_ensemble_metrics}} to calculate metrics from QSIM/QOBS
#'
#' @export
#' @examples
#' \dontrun{
#' # Set up sensitivity analysis
#' bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX"))
#' sobol_bounds <- create_sobol_bounds(bounds)
#' samples <- generate_sobol_samples(sobol_bounds, n = 50)
#'
#' # Run ensemble
#' results <- run_cosero_ensemble(
#'   project_path = "path/to/project",
#'   parameter_sets = samples$parameter_sets,
#'   par_bounds = bounds
#' )
#'
#' # Extract performance metrics
#' nse <- extract_ensemble_metrics(results, subbasin_id = "0001", metric = "NSE")
#' }
run_cosero_ensemble <- function(project_path,
                                parameter_sets,
                                par_bounds,
                                base_settings = NULL,
                                par_file = NULL,
                                quiet = FALSE,
                                statevar_source = 1) {

  # Get parameter file from defaults.txt if not specified
  if (is.null(par_file)) {
    defaults_file <- file.path(project_path, "input", "defaults.txt")
    if (file.exists(defaults_file)) {
      defaults <- read_defaults(defaults_file)
      if (!is.null(defaults$PARAFILE)) {
        par_file <- file.path(project_path, "input", defaults$PARAFILE)
      } else {
        par_file <- file.path(project_path, "input", "para.txt")
      }
    } else {
      par_file <- file.path(project_path, "input", "para.txt")
    }
  }

  if (!file.exists(par_file)) {
    stop("Parameter file not found: ", par_file, call. = FALSE)
  }

  if (!quiet) cat("Using parameter file:", par_file, "\n")

  n_runs <- nrow(parameter_sets)
  results <- vector("list", n_runs)

  # Create backup folder for parameter files
  backup_dir <- file.path(dirname(par_file), "parameterfile_backup")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
  backup_file <- file.path(backup_dir, paste0(basename(par_file), ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.copy(par_file, backup_file)

  # Read original parameter values once
  original_values <- read_parameter_table(par_file, names(parameter_sets), zone_id = "all", quiet = TRUE)

  # Pre-load parameter structure for fast modification
  first_line <- readLines(par_file, n = 1)
  param_data_original <- read.table(
    par_file, header = TRUE, sep = "\t", skip = 1,
    stringsAsFactors = FALSE, check.names = FALSE, comment.char = ""
  )

  param_file_structure <- list(
    first_line = first_line,
    param_data_original = param_data_original,
    param_data_working = param_data_original
  )

  start_time <- Sys.time()

  # Report initial message
  if (!quiet) {
    cat(sprintf("\nStarting %d COSERO simulations (sequential)\n", n_runs))
    param_names_str <- paste(names(parameter_sets), collapse = ", ")
    cat(sprintf("Parameters: %s\n", param_names_str))
    cat("Using fast parameter modification (in-memory)\n")
    cat("\n")
  }

  for (i in 1:n_runs) {
    # Modify parameters using pre-loaded structure (fast path)
    tryCatch({
      modify_parameter_table_fast(par_file, parameter_sets[i, ], par_bounds,
                                  original_values, param_file_structure, quiet = TRUE)
    }, error = function(e) {
      # Fallback to standard method
      file.copy(backup_file, par_file, overwrite = TRUE)
      modify_parameter_table(par_file, parameter_sets[i, ], par_bounds, original_values, quiet = TRUE)
    })

    # Run COSERO
    tryCatch({
      result <- run_cosero(
        project_path = project_path,
        defaults_settings = base_settings,
        quiet = TRUE,
        read_outputs = TRUE,
        statevar_source = statevar_source
      )
      results[[i]] <- result

    }, error = function(e) {
      warning("Run ", i, " failed: ", e$message)
      results[[i]] <- list(success = FALSE, error = e$message)
    })

    # Display progress after completing run
    if (!quiet) {
      display_progress(i, n_runs, start_time, "Run")
    }

    # MEMORY MANAGEMENT: Clean up every 50 simulations to prevent memory accumulation
    if (i %% 50 == 0) {
      # Close any orphaned graphics devices
      tryCatch({
        while (dev.cur() > 1) dev.off()
      }, error = function(e) invisible(NULL))

      # Close any open file connections
      tryCatch({
        closeAllConnections()
      }, error = function(e) invisible(NULL))

      # Force garbage collection to free unused memory
      gc(full = TRUE)

      if (!quiet) {
        cat(sprintf("\n  [Memory cleanup at run %d/%d]\n", i, n_runs))
      }
    }
  }

  # Restore original parameter file
  file.copy(backup_file, par_file, overwrite = TRUE)

  # FINAL MEMORY CLEANUP: Ensure all resources are freed
  tryCatch({
    while (dev.cur() > 1) dev.off()
  }, error = function(e) invisible(NULL))

  tryCatch({
    closeAllConnections()
  }, error = function(e) invisible(NULL))

  gc(full = TRUE)

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  if (!quiet) {
    cat(sprintf("\nDone: Ensemble completed in %s\n", format_time_duration(as.numeric(runtime) * 60)))
    cat(sprintf("  Average per run: %s\n", format_time_duration(runtime * 60 / n_runs)))
  }

  return(list(
    results = results,
    parameter_sets = parameter_sets,
    runtime_minutes = as.numeric(runtime)
  ))
}

#' Read Parameter Values from Tabular File
#'
#' @param par_file Path to parameter file (tabular format with zones)
#' @param param_names Vector of parameter names to read
#' @param zone_id Zone ID to extract parameters from (default: first zone). Use "all" to read all zones.
#' @param quiet Suppress messages
#' @return If zone_id is specified: Named list of parameter values.
#'         If zone_id = "all": Data frame with columns NZ_ (zone ID) and one column per parameter
#' @export
read_parameter_table <- function(par_file, param_names, zone_id = NULL, quiet = FALSE) {
  # Use the existing reader from cosero_readers.R
  param_data <- read_cosero_parameters(par_file, skip_lines = 1, quiet = quiet)

  # If zone_id = "all", return data frame with all zones
  if (!is.null(zone_id) && zone_id == "all") {
    result <- data.frame(NZ_ = param_data$NZ_)

    for (param_name in param_names) {
      param_col <- find_parameter_column(param_name, colnames(param_data), return_all = FALSE)

      if (length(param_col) > 0) {
        result[[param_name]] <- param_data[[param_col]]
      } else {
        warning("Parameter ", param_name, " not found in file")
        result[[param_name]] <- NA
      }
    }

    if (!quiet) cat(sprintf("Successfully read parameters for %d zones/subbasins\n", nrow(result)))
    return(result)
  }

  # Single zone behavior (original)
  if (is.null(zone_id)) {
    zone_id <- param_data$NZ_[1]
  }

  zone_data <- param_data[param_data$NZ_ == zone_id, ]

  if (nrow(zone_data) == 0) {
    stop("Zone ", zone_id, " not found in parameter file", call. = FALSE)
  }

  values <- list()
  for (param_name in param_names) {
    param_col <- find_parameter_column(param_name, colnames(param_data), return_all = FALSE)

    if (length(param_col) > 0) {
      values[[param_name]] <- zone_data[[param_col]][1]
    } else {
      warning("Parameter ", param_name, " not found in file")
    }
  }

  return(values)
}

#' Modify COSERO Parameter File (Tabular Format) - Fast Version
#'
#' Uses pre-loaded parameter data to avoid repeated file reads
#'
#' @param par_file Path to parameter file
#' @param params Named vector/list of sampled parameter values
#' @param par_bounds Parameter bounds table with modification types
#' @param original_values Original parameter values from file
#' @param param_structure Pre-loaded parameter file structure (from ensemble function)
#' @param zones Vector of zone IDs to modify (NULL = all zones)
#' @param quiet Suppress messages
#'
#' @seealso \code{\link{modify_parameter_table}} for the standard version
#' @keywords internal
modify_parameter_table_fast <- function(par_file, params, par_bounds, original_values,
                                        param_structure, zones = NULL, quiet = FALSE) {
  # Reset working copy from original (fast - no file I/O!)
  first_line <- param_structure$first_line

  # Reset working data to original state (overwrites previous modifications)
  # This is MUCH faster than re-reading from disk!
  param_structure$param_data_working[] <- param_structure$param_data_original
  param_data <- param_structure$param_data_working

  # Modify parameters (same logic as modify_parameter_table)
  for (param_name in names(params)) {
    sampled_value <- params[[param_name]]

    # Use case-insensitive matching and check for monthly variants
    param_cols <- find_parameter_column(param_name, colnames(param_data), return_all = TRUE)

    if (length(param_cols) == 0) {
      warning("Parameter ", param_name, " not found in file")
      next
    }

    # Get modification type
    mod_type_idx <- which(par_bounds$parameter == param_name)
    if (length(mod_type_idx) == 0) {
      mod_type_idx <- which(tolower(par_bounds$parameter) == tolower(param_name))
    }

    if (length(mod_type_idx) == 0) {
      warning("Modification type not found for ", param_name)
      next
    }

    mod_type <- par_bounds$modification_type[mod_type_idx[1]]
    param_min <- par_bounds$min[mod_type_idx[1]]
    param_max <- par_bounds$max[mod_type_idx[1]]

    # Determine which zones to modify
    if (is.null(zones)) {
      zone_mask <- rep(TRUE, nrow(param_data))
    } else {
      zone_mask <- param_data$NZ_ %in% zones
    }

    # Apply modification to ALL matched columns
    for (param_col in param_cols) {
      # Calculate spatial mean of original values for this column
      spatial_mean <- mean(param_data[zone_mask, param_col], na.rm = TRUE)
      
      if (mod_type == "relchg") {
        # Spatial Mean Strategy:
        # Factor = Sampled / SpatialMean ensures catchment-average equals sampled value
        if (abs(spatial_mean) < 1e-10) {
           warning(sprintf("Parameter %s: Spatial mean is 0, cannot calculate relative change factor. Using factor 1.0.", param_name))
           factor <- 1.0
        } else {
           factor <- sampled_value / spatial_mean
        }
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] * factor
        
      } else if (mod_type == "abschg") {
        # Spatial Mean Strategy:
        # Offset = Sampled - SpatialMean ensures catchment-average equals sampled value
        offset <- sampled_value - spatial_mean
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] + offset
        
      } else {
        stop("Unknown modification type: ", mod_type, ". Use 'relchg' or 'abschg'.",
             call. = FALSE)
      }

      # Apply physical bounds
      if (length(param_min) > 0 && length(param_max) > 0) {
        param_data[zone_mask, param_col] <- pmax(param_min, pmin(param_max, param_data[zone_mask, param_col]))
      }
    }

    # Report modifications (only if not quiet and multiple columns)
    if (!quiet && length(param_cols) > 1) {
      cat(sprintf("Modified %d monthly variants of %s: %s\n",
                  length(param_cols), param_name, paste(param_cols, collapse = ", ")))
    }
  }

  # Write back to file using write_cosero_parameters()
  write_cosero_parameters(par_file, param_data,
                          project_info = first_line,
                          add_timestamp = FALSE,
                          quiet = TRUE)
}

#' Modify COSERO Parameter File (Tabular Format)
#'
#' Modifies parameter values in a tabular COSERO parameter file. Applies spatial
#' mean strategy for parameter modification: sampled values represent catchment-average
#' targets, with modifications calculated relative to the spatial mean.
#'
#' @param par_file Path to parameter file
#' @param params Named vector/list of sampled parameter values
#' @param par_bounds Parameter bounds table with modification types (from
#'   \code{\link{load_parameter_bounds}} or \code{\link{create_optimization_bounds}})
#' @param original_values Original parameter values from file (from \code{\link{read_parameter_table}})
#' @param zones Vector of zone IDs to modify (NULL = all zones)
#' @param quiet Suppress messages
#' @param add_timestamp Add modification timestamp to file header (default FALSE for loops)
#'
#' @return Invisible NULL (called for side effects - modifies file in place)
#'
#' @seealso
#' \code{\link{read_parameter_table}} to read original values,
#' \code{\link{write_cosero_parameters}} for lower-level file writing
#'
#' @export
#' @examples
#' \dontrun{
#' # Load parameter bounds
#' par_bounds <- load_parameter_bounds(parameters = c("BETA", "M"))
#'
#' # Read original values
#' original_values <- read_parameter_table("para.txt", c("BETA", "M"), zone_id = "all")
#'
#' # Define new parameter values
#' new_params <- list(BETA = 5.0, M = 250)
#'
#' # Modify all zones
#' modify_parameter_table("para.txt", new_params, par_bounds, original_values)
#'
#' # Modify only specific zones with timestamp
#' modify_parameter_table("para.txt", new_params, par_bounds, original_values,
#'                        zones = c(1, 2, 3), add_timestamp = TRUE)
#' }
modify_parameter_table <- function(par_file, params, par_bounds, original_values,
                                   zones = NULL, quiet = FALSE, add_timestamp = FALSE) {
  # Read first line (project info)
  first_line <- readLines(par_file, n = 1)

  # Read parameter table (skip first line)
  param_data <- read.table(
    par_file,
    header = TRUE,
    sep = "\t",
    skip = 1,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    comment.char = ""
  )

  # Modify parameters
  for (param_name in names(params)) {
    sampled_value <- params[[param_name]]

    # Use case-insensitive matching and check for monthly variants
    # This returns ALL monthly columns if they exist (e.g., TCor1-TCor12)
    param_cols <- find_parameter_column(param_name, colnames(param_data), return_all = TRUE)

    if (length(param_cols) == 0) {
      warning("Parameter ", param_name, " not found in file")
      next
    }

    # Get modification type (case-insensitive match in par_bounds)
    # First try exact match, then case-insensitive
    mod_type_idx <- which(par_bounds$parameter == param_name)
    if (length(mod_type_idx) == 0) {
      mod_type_idx <- which(tolower(par_bounds$parameter) == tolower(param_name))
    }

    if (length(mod_type_idx) == 0) {
      warning("Modification type not found for ", param_name)
      next
    }

    mod_type <- par_bounds$modification_type[mod_type_idx[1]]

    # Get bounds (case-insensitive match)
    param_min <- par_bounds$min[mod_type_idx[1]]
    param_max <- par_bounds$max[mod_type_idx[1]]

    # Determine which zones to modify
    if (is.null(zones)) {
      zone_mask <- rep(TRUE, nrow(param_data))
    } else {
      zone_mask <- param_data$NZ_ %in% zones
    }

    # Apply modification to ALL matched columns (e.g., all 12 months)
    for (param_col in param_cols) {
      # Calculate spatial mean of original values for this column
      spatial_mean <- mean(param_data[zone_mask, param_col], na.rm = TRUE)
      
      # Calculate final value based on modification type
      if (mod_type == "relchg") {
        # Spatial Mean Strategy:
        # Factor = Sampled / SpatialMean ensures catchment-average equals sampled value
         if (abs(spatial_mean) < 1e-10) {
           warning(sprintf("Parameter %s: Spatial mean is 0, cannot calculate relative change factor. Using factor 1.0.", param_name))
           factor <- 1.0
        } else {
           factor <- sampled_value / spatial_mean
        }
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] * factor
        
      } else if (mod_type == "abschg") {
        # Spatial Mean Strategy:
        # Offset = Sampled - SpatialMean ensures catchment-average equals sampled value
        offset <- sampled_value - spatial_mean
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] + offset
        
      } else {
        stop("Unknown modification type: ", mod_type, ". Use 'relchg' or 'abschg'.",
             call. = FALSE)
      }

      # Apply physical bounds as safeguards
      if (length(param_min) > 0 && length(param_max) > 0) {
        # Clip values to physical bounds
        param_data[zone_mask, param_col] <- pmax(param_min, pmin(param_max, param_data[zone_mask, param_col]))

        # Warn if clipping occurred
        n_clipped_min <- sum(param_data[zone_mask, param_col] <= param_min + 1e-10)
        n_clipped_max <- sum(param_data[zone_mask, param_col] >= param_max - 1e-10)

        if (n_clipped_min > 0 || n_clipped_max > 0) {
          warning(sprintf("Parameter %s (column %s): %d zones clipped to min (%.3f), %d zones clipped to max (%.3f)",
                          param_name, param_col, n_clipped_min, param_min, n_clipped_max, param_max))
        }
      }
    }

    # Report how many columns were modified
    if (!quiet && length(param_cols) > 1) {
      cat(sprintf("Modified %d monthly variants of %s: %s\n",
                  length(param_cols), param_name, paste(param_cols, collapse = ", ")))
    }
  }

  # Write back to file using write_cosero_parameters()
  write_cosero_parameters(par_file, param_data,
                          project_info = first_line,
                          add_timestamp = add_timestamp,
                          quiet = TRUE)
}

#' Run COSERO Ensemble in Parallel
#'
#' @param project_path Path to COSERO project
#' @param parameter_sets Tibble with parameter combinations
#' @param par_bounds Parameter bounds table with modification types
#' @param base_settings List of base COSERO settings
#' @param n_cores Number of parallel cores (NULL = detect automatically)
#' @param temp_dir Directory for temporary project copies (NULL = use system temp)
#' @param quiet Suppress output
#' @param statevar_source State variable source: 1 = read from parameter file (default), 2 = read from statevar.dmp file (warm start)
#' @return List with results and parameters
#'
#' @seealso
#' \code{\link{run_cosero_ensemble}} for sequential execution,
#' \code{\link{extract_ensemble_metrics}} to extract pre-calculated metrics from results,
#' \code{\link{calculate_ensemble_metrics}} to calculate metrics from QSIM/QOBS
#'
#' @export
run_cosero_ensemble_parallel <- function(project_path,
                                         parameter_sets,
                                         par_bounds,
                                         base_settings = NULL,
                                         n_cores = NULL,
                                         temp_dir = NULL,
                                         quiet = FALSE,
                                         statevar_source = 1) {

  # Detect cores
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 1)
  }

  n_runs <- nrow(parameter_sets)

  # Create temp directory for parallel runs
  if (is.null(temp_dir)) {
    temp_dir <- file.path(tempdir(), "cosero_parallel")
  }
  if (!dir.exists(temp_dir)) dir.create(temp_dir, recursive = TRUE)

  # Get parameter file path
  defaults_file <- file.path(project_path, "input", "defaults.txt")
  if (file.exists(defaults_file)) {
    defaults <- read_defaults(defaults_file)
    par_filename <- ifelse(!is.null(defaults$PARAFILE), defaults$PARAFILE, "para.txt")
  } else {
    par_filename <- "para.txt"
  }

  par_file <- file.path(project_path, "input", par_filename)
  if (!file.exists(par_file)) {
    stop("Parameter file not found: ", par_file, call. = FALSE)
  }

  # Read original parameter values (detect format) - always quiet
  original_values <- read_parameter_table(par_file, names(parameter_sets), zone_id = NULL, quiet = TRUE)

  # Split runs into chunks for parallel processing
  run_indices <- 1:n_runs
  chunks <- split(run_indices, cut(run_indices, n_cores, labels = FALSE))

  if (!quiet) cat("Starting parallel execution...\n")
  start_time <- Sys.time()

  # Create cluster
  cl <- makeCluster(n_cores)
  on.exit(stopCluster(cl))

  # Export necessary objects and functions to cluster
  clusterExport(cl, c("project_path", "parameter_sets", "par_bounds", "base_settings",
                      "par_filename", "original_values", "temp_dir",
                      "modify_parameter_table", "read_parameter_table", "find_parameter_column"),
                envir = environment())

  # Get package path for workers to load from
  pkg_source_path <- getwd()

  # Export package path to cluster
  clusterExport(cl, c("pkg_source_path"), envir = environment())

  # Load CoseRo package on each worker (brings all Imports along)
  clusterEvalQ(cl, {
    if (requireNamespace("CoseRo", quietly = TRUE)) {
      library(CoseRo)
    } else {
      # If not installed, try devtools::load_all() for development
      if (requireNamespace("devtools", quietly = TRUE)) {
        devtools::load_all(pkg_source_path, quiet = TRUE)
      } else {
        stop("CoseRo package not installed and devtools not available",
             call. = FALSE)
      }
    }
  })

  # Run parallel jobs with live progress updates
  if (!quiet) {
    cat(sprintf("\nStarting %d COSERO simulations (parallel)\n", n_runs))
    cat(sprintf("Using %d cores\n\n", n_cores))

    # Setup progress function for live updates
    progress_start <- Sys.time()
    progress <- function(n) {
      display_progress(n, n_runs, progress_start, "Run", quiet = FALSE)
    }
    opts <- list(progress = progress)
  } else {
    opts <- list()
  }

  parallel_start <- Sys.time()

  # Register cluster with doSNOW for progress tracking
  registerDoSNOW(cl)

  # SCALABLE BATCHED HYBRID FIX:
  # - Create thread pool (n_cores directories)
  # - Process runs in small batches to maintain parallelism
  # - Each batch processed truly in parallel across threads
  # - Batch size chosen to balance parallelism vs overhead

  n_threads <- min(n_cores, n_runs)
  thread_dirs <- file.path(temp_dir, paste0("thread_", seq_len(n_threads)))

  # Determine batch size based on total runs
  # Small batches = more parallelism, large batches = less overhead
  if (n_runs <= 100) {
    batch_size <- 4  # For n=25 (275 runs): 69 batches of 4
  } else if (n_runs <= 500) {
    batch_size <- 10  # For n=100 (1100 runs): 110 batches of 10
  } else {
    batch_size <- 25  # For n=500 (5500 runs): 220 batches of 25
  }

  n_batches <- ceiling(n_runs / batch_size)

  if (!quiet) {
    cat("Preparing", n_threads, "thread directories...\n")
    cat(sprintf("Will process %d runs in %d batches of ~%d runs each\n",
                n_runs, n_batches, batch_size))
  }

  prep_start <- Sys.time()

  # Create thread directories
  for (w in 1:n_threads) {
    wdir <- thread_dirs[w]

    if (dir.exists(wdir)) unlink(wdir, recursive = TRUE)
    dir.create(wdir, recursive = TRUE, showWarnings = FALSE)

    # Use robocopy for binary file integrity (file.copy corrupts large .bin files)
    if (.Platform$OS.type == "windows") {
      src <- normalizePath(project_path, winslash = "\\", mustWork = TRUE)
      dst <- normalizePath(wdir, winslash = "\\", mustWork = FALSE)

      # robocopy: /E=subdirs, /XJ=no junctions, /MT:8=8 threads, quiet flags
      cmd <- sprintf('robocopy "%s" "%s" /E /XJ /MT:8 /NFL /NDL /NJH /NJS /NC /NS /NP', src, dst)
      exit_code <- system(cmd, intern=FALSE, ignore.stdout=TRUE, show.output.on.console=FALSE)

      # robocopy codes: 0-7=success, 8+=error
      if (exit_code >= 8) {
        stop(sprintf("robocopy failed (exit code %d) copying to: %s", exit_code, wdir),
             call. = FALSE)
      }
    } else {
      # Unix: prefer rsync if available, fallback to cp
      if (Sys.which("rsync") != "") {
        system(sprintf('rsync -a "%s/" "%s/"', project_path, wdir))
      } else {
        system(sprintf('cp -r "%s"/* "%s"/', project_path, wdir))
      }
    }

    if (!quiet) cat(sprintf("  Thread %d/%d ready\n", w, n_threads))
  }

  prep_time <- difftime(Sys.time(), prep_start, units = "secs")
  if (!quiet) cat(sprintf("Thread setup completed in %.1fs\n\n", prep_time))

  # Export to threads
  clusterExport(cl, c("thread_dirs", "statevar_source"), envir = environment())

  # Process in batches for scalability
  # Ensure each run in a batch uses a UNIQUE worker (no conflicts)
  if (!quiet) cat(sprintf("Starting %d runs (%d batches of ~%d)...\n", n_runs, n_batches, batch_size))

  all_results <- vector("list", n_runs)
  completed_count <- 0

  for (batch_idx in 1:n_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, n_runs)
    batch_run_ids <- start_idx:end_idx
    batch_size_actual <- length(batch_run_ids)

    # Process this batch in parallel
    # CRITICAL FIX: Use index within batch (0, 1, 2, 3) not global run ID
    batch_results <- foreach(
      batch_position = seq_along(batch_run_ids),
      .packages = c("dplyr", "readr", "tibble", "stringr", "lubridate", "data.table")
    ) %dopar% {
      # Get the actual run ID
      i <- batch_run_ids[batch_position]

      # Assign thread based on position IN BATCH (ensures no conflicts within batch)
      thread_idx <- ((batch_position - 1) %% length(thread_dirs)) + 1
      thread_dir <- thread_dirs[thread_idx]
      thread_par_file <- file.path(thread_dir, "input", par_filename)

      tryCatch({
        # Modify parameters
        modify_parameter_table(thread_par_file, parameter_sets[i, ],
                               par_bounds, original_values, quiet = TRUE)

        # Run COSERO
        result <- run_cosero(
          project_path = thread_dir,
          defaults_settings = base_settings,
          quiet = TRUE,
          read_outputs = TRUE,
          statevar_source = statevar_source
        )

        return(result)

      }, error = function(e) {
        error_msg <- paste0("Run ", i, " failed: ", e$message)
        warning(error_msg)
        return(list(success = FALSE, error = error_msg, run_id = i))
      })
    }

    # Store batch results
    all_results[batch_run_ids] <- batch_results

    # CRITICAL: Cleanup between batches to prevent file handle issues
    rm(batch_results)

    # Only close non-cluster connections (don't close the parallel cluster!)
    tryCatch({
      # Get all connections
      all_cons <- showConnections(all = TRUE)
      if (nrow(all_cons) > 0) {
        # Close file connections but NOT socket connections (cluster uses those)
        for (i in 1:nrow(all_cons)) {
          con_class <- all_cons[i, "class"]
          if (con_class %in% c("file", "textConnection")) {
            con_num <- as.integer(rownames(all_cons)[i])
            tryCatch(close(getConnection(con_num)), error = function(e) invisible(NULL))
          }
        }
      }
    }, error = function(e) invisible(NULL))

    gc(full = TRUE)

    # Small delay to ensure all file handles are released
    Sys.sleep(0.5)

    completed_count <- completed_count + batch_size_actual
    if (!quiet) {
      display_progress(
        current    = completed_count,
        total      = n_runs,
        start_time = parallel_start,
        run_type   = "Parallel"
      )
    }
  }

  results <- all_results

  parallel_time <- as.numeric(difftime(Sys.time(), parallel_start, units = "secs"))

  # CRITICAL MEMORY CLEANUP STEP 1: Stop cluster IMMEDIATELY to free thread memory
  if (!quiet) cat("\nCleaning up parallel threads...\n")
  stopCluster(cl)
  on.exit()

  # CRITICAL MEMORY CLEANUP STEP 2: Remove SOME large output data
  # KEEP runoff data (needed for NSE/KGE calculation)
  # Remove other large time series arrays
  if (!quiet) cat("Pruning non-essential output data from results...\n")
  for (i in seq_along(results)) {
    if (!is.null(results[[i]]$output_data)) {
      # DON'T remove runoff - it's needed for calculate_ensemble_metrics()
      # Remove other large arrays to save memory
      if (!is.null(results[[i]]$output_data$evapotranspiration)) {
        results[[i]]$output_data$evapotranspiration <- NULL
      }
      if (!is.null(results[[i]]$output_data$soil_moisture)) {
        results[[i]]$output_data$soil_moisture <- NULL
      }
      if (!is.null(results[[i]]$output_data$snow)) {
        results[[i]]$output_data$snow <- NULL
      }
      if (!is.null(results[[i]]$output_data$groundwater)) {
        results[[i]]$output_data$groundwater <- NULL
      }
    }
  }

  # CRITICAL MEMORY CLEANUP STEP 3: Force aggressive garbage collection
  if (!quiet) cat("Running garbage collection...\n")
  for (iter in 1:3) {
    gc(full = TRUE)
    Sys.sleep(0.2)  # Give system time to release memory
  }

  # CRITICAL MEMORY CLEANUP STEP 4: Close all connections
  tryCatch({
    closeAllConnections()
  }, error = function(e) invisible(NULL))

  # CRITICAL MEMORY CLEANUP STEP 5: Close any orphaned graphics devices
  tryCatch({
    while (dev.cur() > 1) dev.off()
  }, error = function(e) invisible(NULL))

  # CRITICAL MEMORY CLEANUP STEP 6: Cleanup run directories
  if (!quiet) cat("Removing run directories...\n")
  # Clean up in batches to avoid overwhelming the file system
  cleanup_batch_size <- 50
  n_cleanup_batches <- ceiling(length(thread_dirs) / cleanup_batch_size)

  for (batch_idx in 1:n_cleanup_batches) {
    start_idx <- (batch_idx - 1) * cleanup_batch_size + 1
    end_idx <- min(batch_idx * cleanup_batch_size, length(thread_dirs))

    for (i in start_idx:end_idx) {
      wdir <- thread_dirs[i]
      if (dir.exists(wdir)) {
        tryCatch({
          unlink(wdir, recursive = TRUE, force = TRUE)
        }, error = function(e) {
          # If removal fails, try again after short delay
          Sys.sleep(0.1)
          unlink(wdir, recursive = TRUE, force = TRUE)
        })
      }
    }

    if (!quiet && n_cleanup_batches > 1) {
      cat(sprintf("  Cleanup batch %d/%d complete\n", batch_idx, n_cleanup_batches))
    }
  }

  # Final garbage collection before returning
  gc(full = TRUE)

  runtime <- difftime(Sys.time(), start_time, units = "mins")

  if (!quiet) {
    cat(sprintf("\nDone: Ensemble completed in %s\n", format_time_duration(as.numeric(runtime) * 60)))
    
    # Calculate effective throughput
    effective_run_time <- as.numeric(runtime) * 60 / n_runs
    
    cat(sprintf("  Effective throughput: %.2f seconds per run (across %d cores)\n", 
                effective_run_time, n_cores))
  }

  if (!quiet) cat("Done: Memory cleanup complete\n\n")

  return(list(
    results = results,
    parameter_sets = parameter_sets,
    runtime_minutes = as.numeric(runtime),
    parallel_time_seconds = parallel_time,
    n_cores = n_cores
  ))
}

# 5 Result Extraction #####

#' Extract Simulation Results for Sensitivity Analysis
#'
#' @param ensemble_results Results from run_cosero_ensemble()
#' @param variable Output variable to extract ("runoff", "et", etc.)
#' @param aggregation Temporal aggregation ("daily", "monthly", "annual")
#' @return Matrix with rows=time, cols=runs
#' @export
extract_ensemble_output <- function(ensemble_results,
                                     variable = "runoff",
                                     aggregation = "monthly") {

  n_runs <- length(ensemble_results$results)
  output_list <- vector("list", n_runs)

  for (i in 1:n_runs) {
    result <- ensemble_results$results[[i]]

    if (result$success && !is.null(result$output_data)) {
      # Extract variable
      if (variable == "runoff") {
        data <- result$output_data$runoff
      } else if (variable == "et") {
        data <- result$output_data$evapotranspiration
      } else {
        stop("Unknown variable: ", variable, call. = FALSE)
      }

      # Aggregate
      if (aggregation == "monthly") {
        data <- aggregate_to_monthly(data)
      } else if (aggregation == "annual") {
        data <- aggregate_to_annual(data)
      }

      output_list[[i]] <- data
    } else {
      output_list[[i]] <- NA
    }
  }

  return(output_list)
}

#' Extract Pre-calculated Metrics from Ensemble Statistics
#'
#' Extracts performance metrics (NSE, KGE, etc.) that were pre-calculated by COSERO
#' and stored in statistics.txt for each ensemble run. This is the recommended approach
#' for standard metrics as it is faster and guaranteed to match COSERO's internal
#' evaluation (including proper spin-up handling).
#'
#' @param ensemble_results Results from run_cosero_ensemble() or run_cosero_ensemble_parallel()
#' @param subbasin_id Subbasin ID to extract metrics for (e.g., "001" or 1)
#' @param metric Performance metric to extract ("NSE", "KGE", "BIAS", "RMSE", etc.)
#'               Must be a column name in the statistics data frame
#' @param warn_nan If TRUE, warns when metrics are NaN (no observed data for subbasin)
#' @return Numeric vector of metric values for each ensemble run
#'
#' @seealso
#' \code{\link{calculate_ensemble_metrics}} to calculate metrics from QSIM/QOBS (slower but more flexible),
#' \code{\link{extract_run_metrics}} for single run results,
#' \code{\link{calculate_run_metrics}} to calculate metrics for single runs
#'
#' @export
#' @examples
#' \dontrun{
#' # Run ensemble
#' results <- run_cosero_ensemble_parallel(project_path, parameter_sets, n_cores = 4)
#'
#' # Extract NSE for subbasin 001
#' nse_values <- extract_ensemble_metrics(results, subbasin_id = "001", metric = "NSE")
#'
#' # Use in sensitivity analysis
#' sobol_indices <- calculate_sobol_indices(nse_values, sobol_samples)
#' plot_sobol(sobol_indices)
#' }
extract_ensemble_metrics <- function(ensemble_results, subbasin_id = "001",
                                   metric = "NSE", warn_nan = TRUE) {

  n_runs <- length(ensemble_results$results)
  metrics <- numeric(n_runs)

  # Format subbasin ID consistently (always ensure 4-digit format)
  if (is.numeric(subbasin_id)) {
    sb_id_num <- subbasin_id
  } else {
    sb_id_num <- as.numeric(subbasin_id)
  }
  sb_id_str <- sprintf("%04d", sb_id_num)  # Always 4-digit format

  for (i in 1:n_runs) {
    result <- ensemble_results$results[[i]]

    if (result$success && !is.null(result$output_data) &&
        !is.null(result$output_data$statistics)) {

      stats <- result$output_data$statistics

      # Filter for specific subbasin (handle both string and numeric ID formats)
      sb_stats <- stats[stats$sb == sb_id_str | stats$sb == sb_id_num, ]

      if (nrow(sb_stats) > 0 && metric %in% colnames(sb_stats)) {
        metrics[i] <- sb_stats[[metric]][1]
      } else {
        metrics[i] <- NA
      }
    } else {
      metrics[i] <- NA
    }
  }

  # Check for NaN values (indicates no observed data)
  n_nan <- sum(is.nan(metrics))
  if (warn_nan && n_nan > 0) {
    warning(sprintf("Subbasin %s: %d/%d runs have NaN %s (no observed data available)",
                    subbasin_id, n_nan, n_runs, metric))
  }

  return(metrics)
}

#' Calculate Performance Metrics for Ensemble
#'
#' Calculates performance metrics by comparing simulated (QSIM) against observed (QOBS)
#' discharge from ensemble results. The spin-up period is automatically read from
#' defaults_settings$SPINUP (if available) or can be manually specified via the spinup parameter.
#'
#' For standard metrics (NSE, KGE, BIAS, RMSE) already calculated by COSERO, use
#' \code{\link{extract_ensemble_metrics}} instead - it's faster and guaranteed to match
#' COSERO's evaluation.
#'
#' @param ensemble_results Results from run_cosero_ensemble() or run_cosero_ensemble_parallel()
#' @param subbasin_id Subbasin ID to calculate metrics for (e.g., "001" or 1)
#' @param metric Performance metric ("NSE", "KGE", "RMSE", "PBIAS")
#' @param spinup Spin-up period in timesteps to exclude from metric calculation. If NULL (default),
#'               automatically reads from defaults_settings$SPINUP. Set to 0 to use all timesteps.
#' @return Numeric vector of metric values for each ensemble run
#'
#' @seealso
#' \code{\link{extract_ensemble_metrics}} to extract pre-calculated metrics (faster),
#' \code{\link{extract_run_metrics}} for single run results,
#' \code{\link{calculate_run_metrics}} to calculate metrics for single runs
#'
#' @export
#' @examples
#' \dontrun{
#' # Calculate PBIAS (auto-detects spin-up from settings)
#' pbias <- calculate_ensemble_metrics(results, subbasin_id = "001", metric = "PBIAS")
#'
#' # Override spin-up period to 100 timesteps
#' pbias <- calculate_ensemble_metrics(results, subbasin_id = "001",
#'                                     metric = "PBIAS", spinup = 100)
#'
#' # Use all timesteps (no spin-up exclusion)
#' nse_no_spinup <- calculate_ensemble_metrics(results, subbasin_id = "001",
#'                                             metric = "NSE", spinup = 0)
#' }
calculate_ensemble_metrics <- function(ensemble_results,
                                       subbasin_id = "001",
                                       metric = "KGE",
                                       spinup = NULL) {

  n_runs <- length(ensemble_results$results)
  metrics <- numeric(n_runs)

  # Format subbasin ID (always ensure 4-digit format)
  if (is.numeric(subbasin_id)) {
    sb_id_num <- subbasin_id
  } else {
    sb_id_num <- as.numeric(subbasin_id)
  }
  sb_id <- sprintf("%04d", sb_id_num)  # Always 4-digit format

  # Column names
  qsim_col <- paste0("QSIM_", sb_id)
  qobs_col <- paste0("QOBS_", sb_id)

  for (i in 1:n_runs) {
    result <- ensemble_results$results[[i]]

    if (result$success && !is.null(result$output_data) &&
        !is.null(result$output_data$runoff)) {

      runoff <- result$output_data$runoff

      # Extract simulated and observed discharge
      if (qsim_col %in% colnames(runoff) && qobs_col %in% colnames(runoff)) {
        simulated <- runoff[[qsim_col]]
        observed <- runoff[[qobs_col]]

        # Get spin-up duration (with override option)
        if (!is.null(spinup)) {
          # User-provided override
          spinup_duration <- as.numeric(spinup)
        } else {
          # Try to read from output_data$defaults_settings (character -> numeric)
          spinup_duration <- 0
          if (!is.null(result$output_data$defaults_settings) &&
              !is.null(result$output_data$defaults_settings$SPINUP)) {
            spinup_duration <- as.numeric(result$output_data$defaults_settings$SPINUP)
          }
        }

        # Apply spin-up exclusion to both simulated and observed
        if (spinup_duration > 0 && length(simulated) > spinup_duration) {
          simulated <- simulated[(spinup_duration + 1):length(simulated)]
          observed <- observed[(spinup_duration + 1):length(observed)]
        }

        # Calculate metric
        metrics[i] <- tryCatch({
          if (metric == "KGE") {
            hydroGOF::KGE(simulated, observed)
          } else if (metric == "NSE") {
            hydroGOF::NSE(simulated, observed)
          } else if (metric == "RMSE") {
            hydroGOF::rmse(simulated, observed)
          } else if (metric == "PBIAS") {
            hydroGOF::pbias(simulated, observed)
          } else {
            stop("Unknown metric: ", metric, call. = FALSE)
          }
        }, error = function(e) {
          NA
        })
      } else {
        metrics[i] <- NA
      }
    } else {
      metrics[i] <- NA
    }
  }

  return(metrics)
}

# 6 Aggregation Functions #####

#' Aggregate Daily to Monthly
#'
#' @param data Data frame with date and value columns
#' @return Monthly aggregated data
aggregate_to_monthly <- function(data) {
  data %>%
    mutate(year_month = format(date, "%Y-%m")) %>%
    group_by(year_month) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = "drop")
}

#' Aggregate Daily to Annual
#'
#' @param data Data frame with date and value columns
#' @return Annual aggregated data
aggregate_to_annual <- function(data) {
  data %>%
    mutate(year = format(date, "%Y")) %>%
    group_by(year) %>%
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

# 7 Sensitivity Indices #####

#' Calculate Sobol Sensitivity Indices
#'
#' Calculates first-order (Si) and total-order (Ti) Sobol sensitivity indices
#' from COSERO ensemble outputs using the Jansen estimator. Bootstrap confidence
#' intervals are supported.
#'
#' @section What the indices mean:
#' \itemize{
#'   \item \strong{Si (first-order)}: Fraction of output variance explained by
#'     each parameter acting alone. A high Si means that parameter is an
#'     important individual driver.
#'   \item \strong{Ti (total-order)}: Si plus all higher-order interaction effects
#'     involving that parameter. If Ti >> Si, the parameter is involved in
#'     significant interactions with others.
#'   \item \strong{Sum of Si}: Values well below 1 indicate that parameter
#'     interactions explain a large share of variance.
#' }
#'
#' @section The \code{order} argument (set via \code{generate_sobol_samples}):
#' The \code{order} is inherited from the \code{sobol_samples} object — it is
#' not set here directly. It controls which indices are computed and how many
#' model runs are required:
#' \itemize{
#'   \item \strong{\code{"first"} (default, recommended)}: Computes Si and Ti.
#'     Requires \code{N * (k + 2)} model runs (e.g. N=500, k=6 → 4000 runs).
#'   \item \strong{\code{"second"}}: Additionally computes Sij (pairwise
#'     parameter interactions). Requires \code{N * (k + 2 + k*(k-1)/2)} runs —
#'     substantially more expensive and rarely needed for hydrological applications.
#' }
#' For most COSERO sensitivity analyses \code{order = "first"} is sufficient.
#'
#' @section NA handling:
#' Failed model runs (NA in Y) are handled structurally. The Sobol sequence is
#' organised as \code{n_blocks} groups of N rows each (A, B, AB_1 ... AB_k).
#' Any sample index that is NA in \emph{any} block is dropped from \emph{all}
#' blocks simultaneously, preserving the mathematical structure of the Jansen
#' estimator. The effective sample size \code{N_eff} is reported. A warning is
#' issued if more than 20\% of sample points are dropped.
#'
#' @param Y Numeric vector of model outputs. Length must equal
#'   \code{nrow(sobol_samples$parameter_sets)}, i.e. \code{N * (k + 2)} for
#'   \code{order = "first"}. NAs are allowed (see NA handling section).
#' @param sobol_samples List returned by \code{\link{generate_sobol_samples}},
#'   containing \code{n}, \code{par_names}, and \code{order}.
#' @param boot Logical. If \code{TRUE}, bootstrap confidence intervals are
#'   computed. Recommended. Default: \code{TRUE}.
#' @param R Integer. Number of bootstrap resamples. Higher values give tighter
#'   CI estimates but take longer. Default: \code{100}. Use \code{500} or more
#'   for publication-quality results.
#'
#' @return A \code{sensobol} object (list) with:
#'   \item{results}{Data table with columns \code{parameters}, \code{sensitivity}
#'     (\code{"Si"} or \code{"Ti"}), \code{original} (index value),
#'     \code{std.error}, \code{low.ci}, \code{high.ci}}
#'   \item{N_eff}{Effective sample size after structural NA removal}
#'   \item{n_dropped}{Number of sample points dropped due to NA}
#'
#' @seealso \code{\link{generate_sobol_samples}} to create the sample design,
#'   \code{\link{plot_sobol}} to visualise results,
#'   \code{\link{plot_dotty}} for parameter-output scatter plots,
#'   \code{\link{extract_ensemble_metrics}} to compute Y from ensemble results.
#'
#' @export
#' @examples
#' \dontrun{
#' # 1. Define parameters and generate Sobol sample design
#' par_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "M", "TCOR"))
#' sobol_bounds <- create_sobol_bounds(par_bounds)
#' samples <- generate_sobol_samples(sobol_bounds, n = 500, order = "first")
#' # → generates 500 * (4 + 2) = 3000 parameter sets
#'
#' # 2. Run COSERO ensemble (parallel recommended)
#' ensemble <- run_cosero_ensemble_parallel(
#'   project_path = "D:\\Projects\\COSERO",
#'   parameter_sets = samples$parameter_sets,
#'   par_bounds = par_bounds,
#'   base_settings = list(SPINUP = 365, OUTPUTTYPE = 1),
#'   n_cores = 4
#' )
#'
#' # 3. Extract metric vector Y (NAs from failed runs are fine)
#' nse <- extract_ensemble_metrics(ensemble, subbasin_id = "0001", metric = "NSE")
#'
#' # 4. Compute sensitivity indices
#' sobol_nse <- calculate_sobol_indices(Y = nse, sobol_samples = samples,
#'                                      boot = TRUE, R = 500)
#' cat("N_eff:", sobol_nse$N_eff, "  Dropped:", sobol_nse$n_dropped, "\n")
#'
#' # 5. Visualise
#' plot_sobol(sobol_nse, title = "Parameter Sensitivity (NSE)")
#' }
calculate_sobol_indices <- function(Y, sobol_samples, boot = TRUE, R = 100) {

  N      <- sobol_samples$n
  k      <- length(sobol_samples$par_names)
  order  <- if (!is.null(sobol_samples$order)) sobol_samples$order else "first"

  # Number of matrix blocks depends on order
  n_blocks <- switch(order,
    "first"  = k + 2L,
    "second" = k + 2L + k * (k - 1L) / 2L,
    stop("Unsupported order: ", order, call. = FALSE)
  )

  expected_len <- N * n_blocks
  if (length(Y) != expected_len) {
    stop(sprintf(
      "Y has %d elements but expected %d (N=%d x %d blocks for %d params, order='%s').\n",
      length(Y), expected_len, N, n_blocks, k, order
    ), call. = FALSE)
  }

  # Reshape Y into [N x n_blocks] — each column is one matrix block (A, B, AB_i)
  # NA removal must be done row-wise: drop a sample point from ALL blocks together
  Y_mat <- matrix(Y, nrow = N, ncol = n_blocks)

  # A sample point is invalid if it is NA in ANY block
  invalid_rows <- apply(Y_mat, 1, anyNA)
  n_dropped    <- sum(invalid_rows)

  if (n_dropped > 0) {
    pct <- round(100 * n_dropped / N, 1)
    msg <- sprintf(
      "%d of %d sample points dropped due to NA (%.1f%% of N). N_eff = %d.",
      n_dropped, N, pct, N - n_dropped
    )
    if (pct > 20) {
      warning(msg, " Results may be unreliable with >20%% missing runs.", call. = FALSE)
    } else {
      message(msg)
    }
  }

  Y_clean <- as.vector(Y_mat[!invalid_rows, ])
  N_eff   <- N - n_dropped

  if (N_eff < 10L) {
    stop(sprintf(
      "Only %d valid sample points remain after NA removal. Cannot compute reliable Sobol indices.",
      N_eff
    ), call. = FALSE)
  }

  sobol_result <- sobol_indices(
    Y      = Y_clean,
    N      = N_eff,
    params = sobol_samples$par_names,
    first  = "jansen",
    total  = "jansen",
    order  = order,
    boot   = boot,
    R      = R
  )

  # Attach diagnostics so the user can inspect them
  sobol_result$N_eff    <- N_eff
  sobol_result$n_dropped <- n_dropped

  return(sobol_result)
}

# 8 Visualization #####

#' Plot Sobol Sensitivity Indices
#'
#' Creates a bar plot of first-order (Si) and total-order (Ti) Sobol sensitivity
#' indices for each parameter. Uses the default plot method from the sensobol
#' package with additional theme customization.
#'
#' @param sobol_indices Result from \code{\link{calculate_sobol_indices}}
#' @param title Plot title. Default is "Sobol Sensitivity Indices"
#'
#' @return ggplot object showing Si and Ti indices per parameter
#'
#' @export
#' @seealso \code{\link{calculate_sobol_indices}} to compute indices,
#'   \code{\link{plot_dotty}} for parameter-output scatter plots,
#'   \code{\link{plot_metric_distribution}} for metric histograms
#' @examples
#' \dontrun{
#' # Full workflow: generate samples, run ensemble, compute indices, plot
#' par_bounds <- load_parameter_bounds(parameters = c("BETA", "CTMAX", "M", "TAB1"))
#' sobol_bounds <- create_sobol_bounds(par_bounds)
#' sobol_samples <- generate_sobol_samples(sobol_bounds, n = 50)
#'
#' results <- run_cosero_ensemble_parallel(
#'   project_path = "D:/COSERO_project",
#'   parameter_sets = sobol_samples$parameter_sets,
#'   par_bounds = par_bounds,
#'   base_settings = list(SPINUP = 365, OUTPUTTYPE = 1),
#'   n_cores = 4
#' )
#'
#' nse <- extract_ensemble_metrics(results, subbasin_id = "001", metric = "NSE")
#' sobol_ind <- calculate_sobol_indices(Y = nse, sobol_samples = sobol_samples)
#'
#' # Basic plot
#' p <- plot_sobol(sobol_ind)
#' print(p)
#'
#' # Custom title
#' p <- plot_sobol(sobol_ind, title = "Parameter Sensitivity (NSE)")
#'
#' # Save to file
#' ggplot2::ggsave("sobol_indices.png", p, width = 10, height = 6, dpi = 300)
#' }
plot_sobol <- function(sobol_indices, title = "Sobol Sensitivity Indices") {
  p <- plot(sobol_indices) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
    ggtitle(title)

  return(p)
}

#' Create Dotty Plots
#'
#' Creates faceted scatter plots showing the relationship between each parameter
#' value and the model output (e.g., NSE, KGE). Each panel shows one parameter
#' on the x-axis versus the output metric on the y-axis. Useful for identifying
#' parameter sensitivity and equifinality.
#'
#' @param parameter_sets Parameter sets tibble, data frame, or matrix (one column per
#'   parameter, one row per ensemble run).
#' @param Y Numeric vector of model outputs (must be the same length as rows in parameter_sets).
#' @param y_label Character. Y-axis label. Default is "Output".
#' @param n_col Numeric. Number of columns in facet grid. Default is 3.
#' @param reference_line Optional numeric value to show as a horizontal red
#'   dashed line (e.g., acceptable performance threshold).
#' @param y_min Optional minimum Y-axis value (e.g., 0 to start from zero).
#'   If NULL, ggplot2 auto-scales.
#' @param show_envelope Logical. If TRUE (default), adds a smoothing curve tracking 
#'   the upper performance envelope of the parameter space.
#' @param envelope_quantile Numeric. The quantile used to calculate the upper envelope 
#'   (default is 0.95). Only applies if \code{show_envelope = TRUE}.
#'
#' @return ggplot object with faceted scatter plots
#'
#' @details
#' Points are colored dark blue with transparency (alpha = 0.5). Each facet has 
#' independent x-axis scaling (\code{scales = "free_x"}) since parameters have 
#' different ranges. A clear vertical spread of points for a parameter indicates 
#' low sensitivity; a structured pattern (e.g., funnel, trend) suggests the 
#' parameter has a strong influence on the output. 
#' 
#' When \code{show_envelope = TRUE}, the parameter space is binned, and a LOESS 
#' curve is fitted to the upper quantiles of each bin. This isolates the parameter's 
#' maximum behavioral potential from the noise of poorly performing parameter combinations.
#'
#' @references
#' Beven, K., & Binley, A. (1992). The future of distributed models: Model calibration 
#' and uncertainty prediction. \emph{Hydrological Processes}, 6(3), 279-298.
#'
#' @seealso 
#' \code{\link{plot_sobol}} for Sobol sensitivity bar plots, 
#' \code{\link{plot_metric_distribution}} for metric histograms, 
#' \code{\link{plot_ensemble_uncertainty}} for ensemble discharge visualization
#' 
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate filter group_by summarize %>%
#' @export
#' 
#' @examples
#' \dontrun{
#' # After running ensemble and extracting metrics
#' nse <- extract_ensemble_metrics(results, subbasin_id = "001", metric = "NSE")
#'
#' # Basic dotty plot with 95% upper envelope curve
#' p <- plot_dotty(sobol_samples$parameter_sets, Y = nse, y_label = "NSE")
#' print(p)
#'
#' # With reference line at NSE = 0.7, tracking the absolute max (99th percentile)
#' p <- plot_dotty(sobol_samples$parameter_sets, Y = nse, y_label = "NSE", 
#'                 reference_line = 0.7, envelope_quantile = 0.99)
#' }
plot_dotty <- function(parameter_sets, Y, y_label = "Output",
                       n_col = 3, reference_line = NULL, y_min = NULL,
                       show_envelope = TRUE, envelope_quantile = 0.95) {

  # Fix for R CMD check: "no visible binding for global variable"
  parameter <- output <- value <- x_bin <- NULL

  # 1. Input Validation
  if (nrow(parameter_sets) != length(Y)) {
    stop(sprintf("Dimension mismatch: 'parameter_sets' has %d rows, but 'Y' has length %d.", 
                 nrow(parameter_sets), length(Y)), call. = FALSE)
  }

  # 2. Ensure data structure is a data frame
  parameter_sets <- as.data.frame(parameter_sets)

  # 3. Wrangle the raw data
  dotty_data <- parameter_sets %>%
    mutate(output = Y) %>%
    pivot_longer(cols = -output, names_to = "parameter", values_to = "value")

  # 4. Generate the base plot (Dots are slightly larger and less transparent now)
  p <- ggplot(dotty_data, aes(x = value, y = output)) +
    geom_point(alpha = 0.5, size = 1.0, color = "#1f3b73", stroke = 0) +
    facet_wrap(~ parameter, scales = "free_x", ncol = n_col) +
    theme_bw() +
    labs(x = "Parameter Value", y = y_label) +
    theme(
      strip.background = element_blank(),
      strip.text = element_text(size = 11, face = "bold", color = "black"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "grey70", fill = NA)
    )

  # 5. Calculate and Add Upper Envelope Curve
  if (show_envelope) {
    # Isolate the highest performers by binning the x-axis
    envelope_data <- dotty_data %>%
      group_by(parameter) %>%
      mutate(x_bin = cut(value, breaks = 30)) %>%
      group_by(parameter, x_bin) %>%
      summarize(
        value = mean(value, na.rm = TRUE),
        output = quantile(output, probs = envelope_quantile, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(value) & !is.na(output))

    # Add the smoothed curve and dynamically add the subtitle
    p <- p + 
      geom_smooth(data = envelope_data, method = "loess", formula = y ~ x, 
                  color = "darkorange", linewidth = 0.6, 
                  se = FALSE, alpha = 0.9) +
      labs(subtitle = paste0("Orange line: LOESS fit of the ", 
                             envelope_quantile * 100, "% upper envelope"))
  }

  # 6. Add Y-axis minimum if specified
  if (!is.null(y_min)) {
    p <- p + coord_cartesian(ylim = c(y_min, NA))
  }

  # 7. Add Reference Line
  if (!is.null(reference_line)) {
    p <- p + geom_hline(yintercept = reference_line,
                        linetype = "dashed", color = "#d90429", linewidth = 0.8, alpha = 0.8)
  }

  return(p)
}

#' Plot Ensemble Uncertainty
#'
#' Visualizes ensemble uncertainty by plotting median and user-defined quantile ranges
#' across multiple model runs. Works directly with output from
#' run_cosero_ensemble() or run_cosero_ensemble_parallel().
#'
#' By default, if you are plotting simulated discharge (\code{output_variable = "QSIM"}) 
#' and \code{show_observed = TRUE}, the function will automatically find and plot 
#' the observed discharge (\code{QOBS}) from the COSERO results as a black line. 
#' You do not need to provide observed data manually unless you want to use a custom dataset.
#'
#' @param ensemble_output Output from run_cosero_ensemble_parallel() or
#'   run_cosero_ensemble(), or a list of individual run results.
#' @param observed Optional data frame with your own observed data (must have 
#'   'date' and 'value' columns). Leave this as NULL to let the function 
#'   automatically extract QOBS from the COSERO results.
#' @param subbasin_id Character or Numeric. Subbasin identifier (e.g., "0001", "1", or 1). 
#'   The function automatically pads this to 4 digits for matching COSERO outputs. Default is "0001".
#' @param output_variable Character. The prefix of the model output to plot 
#'   (e.g., "QSIM" for simulated discharge, "SWE" for snow water equivalent). Default is "QSIM".
#' @param lower_quantile Numeric. Lower bound for the confidence interval (default: 0.10).
#' @param upper_quantile Numeric. Upper bound for the confidence interval (default: 0.90).
#' @param show_observed Logical. If TRUE (default), show the observed data as a black line.
#' @param date_range Optional vector of two dates or character strings (e.g., 
#'   \code{c("2010-01-01", "2010-12-31")}) to zoom in on a specific time period.
#' @param q_max Numeric or Logical. If TRUE (default), the y-axis automatically
#'   scales to the maximum of the simulated and observed data. If a numeric value
#'   is provided (e.g., 500), it forces the upper limit of the y-axis to that value.
#' @param y_label Character. Y-axis label. If \code{NULL} (default), a label is
#'   chosen automatically based on \code{output_variable}: \code{"QSIM"} and
#'   \code{"QOBS"} produce \code{"Discharge (m\u00b3/s)"}, \code{"SWE"} produces
#'   \code{"Snow Water Equivalent (mm)"}. For unrecognised variables the raw
#'   \code{output_variable} string is used as-is.
#'
#' @return ggplot object showing the median (red line), user-defined confidence interval
#'   (orange ribbon), and observed data (black line, if available).
#'
#' @import ggplot2
#' @importFrom stats median quantile
#' @export
#' @seealso \code{\link{plot_metric_distribution}} for visualizing ensemble metric distributions
#' 
#' @examples
#' \dontrun{
#' # After running ensemble
#' results <- run_cosero_ensemble_parallel(...)
#'
#' # Plot uncertainty with auto-extracted QOBS (auto-scales y-axis)
#' p <- plot_ensemble_uncertainty(results, subbasin_id = "0001")
#' print(p)
#'
#' # Without observed discharge
#' p <- plot_ensemble_uncertainty(results, subbasin_id = "0001", show_observed = FALSE)
#'
#' # With custom observed data
#' obs <- data.frame(date = dates, value = observed_discharge)
#' p <- plot_ensemble_uncertainty(results, observed = obs, subbasin_id = "0001")
#' 
#' # Zooming in on a specific date range, and forcing the y-axis maximum to 150
#' p_zoom <- plot_ensemble_uncertainty(results, subbasin_id = "3", 
#'                                     date_range = c("2012-01-01", "2012-12-31"),
#'                                     q_max = 150) +
#'   labs(title = "Ensemble Discharge Uncertainty — Subbasin 3 (2012)")
#' print(p_zoom)
#' }
plot_ensemble_uncertainty <- function(ensemble_output, observed = NULL,
                                      subbasin_id = "0001", output_variable = "QSIM",
                                      lower_quantile = 0.10, upper_quantile = 0.90,
                                      show_observed = TRUE, date_range = NULL,
                                      q_max = TRUE, y_label = NULL) {

  # Fix for R CMD check: "no visible binding for global variable"
  date <- q_low <- q_high <- median <- value <- NULL

  # Check if ensemble_output is the standard list output
  if (!is.null(ensemble_output$results)) {
    results_list <- ensemble_output$results
  } else {
    results_list <- ensemble_output
  }

  n_runs <- length(results_list)

  # Automatically pad the subbasin_id to 4 digits (e.g., "3" -> "0003")
  subbasin_num    <- as.numeric(subbasin_id)
  subbasin_padded <- sprintf("%04d", subbasin_num)

  # Resolve y-axis label: use supplied value or look up a human-readable default
  if (is.null(y_label)) {
    y_label_lookup <- c(
      QSIM = "Discharge (m\u00b3/s)",
      QOBS = "Discharge (m\u00b3/s)",
      SWE  = "Snow Water Equivalent (mm)",
      PREC = "Precipitation (mm)",
      ET   = "Evapotranspiration (mm)"
    )
    y_label <- if (output_variable %in% names(y_label_lookup)) {
      y_label_lookup[[output_variable]]
    } else {
      output_variable  # fall back to raw variable name
    }
  }

  # Extract timeseries data from each run
  timeseries_list <- vector("list", n_runs)
  dates <- NULL
  n_time <- NULL
  qobs_data <- NULL  

  for (i in 1:n_runs) {
    result <- results_list[[i]]

    if (result$success && !is.null(result$output_data) &&
        !is.null(result$output_data$runoff)) {

      runoff <- result$output_data$runoff
      
      # Use the 4-digit padded ID to find the column in the dataframe
      col_name <- paste0(output_variable, "_", subbasin_padded)

      if (col_name %in% colnames(runoff)) {
        timeseries_list[[i]] <- runoff[[col_name]]

        # Get dates and QOBS from the first successful run
        if (is.null(dates)) {
          if ("date" %in% colnames(runoff)) {
            dates <- runoff$date
          } else {
            dates <- as.Date(paste(runoff$yyyy, runoff$mm, runoff$dd, sep = "-"))
          }
          n_time <- length(dates)

          # Auto-extract QOBS only if we are looking at simulated discharge
          if (show_observed && is.null(observed) && output_variable == "QSIM") {
            qobs_col <- paste0("QOBS_", subbasin_padded)
            if (qobs_col %in% colnames(runoff)) {
              qobs_values <- runoff[[qobs_col]]
              
              # Treat all negative values in QOBS as missing data (NA)
              qobs_values[qobs_values < 0] <- NA
              
              qobs_data <- data.frame(date = dates, value = qobs_values)
            }
          }
        }
      } else {
        warning(sprintf("Run %d: Column '%s' not found in runoff data", i, col_name))
        timeseries_list[[i]] <- NA
      }
    } else {
      timeseries_list[[i]] <- NA
    }
  }

  # Check for valid data
  n_valid <- sum(sapply(timeseries_list, function(x) !all(is.na(x))))
  if (n_valid == 0) {
    stop("No valid timeseries data found in ensemble output. Check subbasin_id and output_variable.",
         call. = FALSE)
  }
  if (is.null(dates)) {
    stop("Could not extract dates from ensemble output", call. = FALSE)
  }

  # Convert list to matrix
  output_matrix <- matrix(NA, nrow = n_time, ncol = n_runs)
  for (i in 1:n_runs) {
    if (!all(is.na(timeseries_list[[i]]))) {
      output_matrix[, i] <- timeseries_list[[i]]
    }
  }

  # Calculate dynamic quantiles
  quantiles <- t(apply(output_matrix, 1, quantile, 
                       probs = c(lower_quantile, 0.5, upper_quantile), 
                       na.rm = TRUE))
  stats <- data.frame(
    date = dates,
    q_low = quantiles[, 1],
    median = quantiles[, 2],
    q_high = quantiles[, 3]
  )

  # Prep observed data upfront so we can filter it easily
  obs_plot_data <- NULL
  if (show_observed) {
    if (!is.null(observed)) {
      if (is.data.frame(observed) && "date" %in% colnames(observed) && "value" %in% colnames(observed)) {
        observed$value[observed$value < 0] <- NA
        obs_plot_data <- observed
      } else {
        warning("Observed data must be a data frame with 'date' and 'value' columns")
      }
    } else if (!is.null(qobs_data)) {
      obs_plot_data <- qobs_data
    }
  }

  # Apply date_range filter if provided
  if (!is.null(date_range) && length(date_range) == 2) {
    start_date <- as.Date(date_range[1])
    end_date <- as.Date(date_range[2])
    
    stats <- stats[stats$date >= start_date & stats$date <= end_date, ]
    
    if (!is.null(obs_plot_data)) {
      obs_plot_data <- obs_plot_data[obs_plot_data$date >= start_date & obs_plot_data$date <= end_date, ]
    }
  }

  # Generate a dynamic label for the legend based on the user's quantiles
  interval_percent <- round((upper_quantile - lower_quantile) * 100)
  ci_label <- paste0(interval_percent, "% Interval")

  # Create plot
  p <- ggplot(stats, aes(x = date)) +
    geom_ribbon(aes(ymin = q_low, ymax = q_high, fill = ci_label), alpha = 0.4) +
    geom_line(aes(y = median, color = "Simulated Median"), linewidth = 0.5) +
    theme_bw() +
    labs(x = "Date",
         y = y_label,
         title = paste0("Ensemble Uncertainty - Subbasin ", subbasin_num),
         fill = "Uncertainty",
         color = "Timeseries") +
    scale_fill_manual(values = setNames("orange", ci_label)) +
    scale_color_manual(values = c("Simulated Median" = "red", "Observed" = "black")) +
    theme(legend.position = "bottom")

  # Add observed data 
  if (!is.null(obs_plot_data)) {
    p <- p + geom_line(data = obs_plot_data, aes(x = date, y = value, color = "Observed"), 
                       linewidth = 0.4, alpha = 0.8)
  }

  # Limit Y-axis if q_max is specified as a numeric value
  if (is.numeric(q_max)) {
    p <- p + coord_cartesian(ylim = c(NA, q_max))
  }

  return(p)
}

#' Plot Distribution of Performance Metrics
#'
#' Visualizes the distribution of a performance metric (e.g., NSE, KGE)
#' across ensemble runs as a histogram with optional reference lines.
#' The red dashed vertical line shows the median of the distribution.
#'
#' @param metric_values Numeric vector of metric values from
#'   \code{\link{extract_ensemble_metrics}} or \code{\link{calculate_ensemble_metrics}}
#' @param metric_name Name of the metric for axis labels (e.g., "NSE", "KGE").
#'   Default is "Metric"
#' @param reference_value Optional numeric value to show as a reference line
#'   (e.g., baseline model performance). Shown as a blue dashed line.
#' @param bins Number of histogram bins. Default is 30
#' @param show_median Logical. If TRUE (default), show a red dashed vertical
#'   line at the median of the distribution
#' @param show_mean Logical. If TRUE, show a green dotted vertical line
#'   at the mean of the distribution. Default is FALSE
#'
#' @return ggplot object showing histogram with density overlay and reference lines
#'
#' @details
#' The plot includes:
#' \itemize{
#'   \item Gray histogram of metric values
#'   \item Red dashed vertical line: median of the distribution
#'   \item Green dotted vertical line: mean of the distribution (if \code{show_mean = TRUE})
#'   \item Blue dashed vertical line: reference value (if provided)
#' }
#'
#' @export
#' @seealso \code{\link{extract_ensemble_metrics}} to extract pre-calculated metrics,
#'   \code{\link{calculate_ensemble_metrics}} to calculate metrics from QSIM/QOBS,
#'   \code{\link{plot_ensemble_uncertainty}} for ensemble discharge visualization
#' @examples
#' \dontrun{
#' # After running ensemble
#' results <- run_cosero_ensemble_parallel(...)
#'
#' # Extract NSE values
#' nse <- extract_ensemble_metrics(results, subbasin_id = "001", metric = "NSE")
#'
#' # Plot distribution (red line = median)
#' p <- plot_metric_distribution(nse, metric_name = "NSE")
#' print(p)
#'
#' # With baseline reference and mean
#' p <- plot_metric_distribution(nse, metric_name = "NSE",
#'                                reference_value = 0.75,
#'                                show_mean = TRUE)
#' print(p)
#'
#' # Compare multiple metrics
#' kge <- extract_ensemble_metrics(results, subbasin_id = "001", metric = "KGE")
#' p1 <- plot_metric_distribution(nse, metric_name = "NSE")
#' p2 <- plot_metric_distribution(kge, metric_name = "KGE")
#' # patchwork::wrap_plots(p1, p2, ncol = 2)
#' }
plot_metric_distribution <- function(metric_values, metric_name = "Metric",
                                      reference_value = NULL, bins = 30,
                                      show_median = TRUE, show_mean = FALSE) {

  # Remove NA values
  valid_values <- metric_values[!is.na(metric_values)]
  n_valid <- length(valid_values)
  n_total <- length(metric_values)
  n_na <- n_total - n_valid

  if (n_valid == 0) {
    stop("No valid (non-NA) metric values provided", call. = FALSE)
  }

  if (n_na > 0) {
    message(sprintf("Note: %d of %d runs had NA metric values (excluded from plot)",
                    n_na, n_total))
  }

  df <- data.frame(value = valid_values)

  # Build subtitle with summary statistics
  subtitle_parts <- sprintf("n = %d | range: [%.3f, %.3f]",
                            n_valid, min(valid_values), max(valid_values))

  # Create base plot
  p <- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)), bins = bins,
                   fill = "gray70", color = "gray40", alpha = 0.7) +
    geom_density(color = "black", linewidth = 0.8) +
    theme_bw() +
    labs(x = metric_name,
         y = "Density",
         title = paste0("Distribution of ", metric_name, " Across Ensemble Runs"),
         subtitle = subtitle_parts)

  # Add median line (red dashed)
  if (show_median) {
    med_val <- median(valid_values)
    p <- p + geom_vline(xintercept = med_val, color = "red",
                        linetype = "dashed", linewidth = 1) +
      annotate("text", x = med_val, y = Inf, label = sprintf("Median: %.3f", med_val),
               color = "red", hjust = -0.1, vjust = 2, size = 3.5)
  }

  # Add mean line (green dotted)
  if (show_mean) {
    mean_val <- mean(valid_values)
    p <- p + geom_vline(xintercept = mean_val, color = "darkgreen",
                        linetype = "dotted", linewidth = 1) +
      annotate("text", x = mean_val, y = Inf, label = sprintf("Mean: %.3f", mean_val),
               color = "darkgreen", hjust = -0.1, vjust = 3.5, size = 3.5)
  }

  # Add reference line (blue dashed)
  if (!is.null(reference_value)) {
    p <- p + geom_vline(xintercept = reference_value, color = "blue",
                        linetype = "dashed", linewidth = 1) +
      annotate("text", x = reference_value, y = Inf, label = sprintf("Reference: %.3f", reference_value),
               color = "blue", hjust = -0.1, vjust = 5, size = 3.5)
  }

  return(p)
}

# 9 Behavioral Analysis #####

#' Extract Behavioral Runs from Ensemble Output
#'
#' Evaluates an ensemble of COSERO model runs against three performance
#' criteria simultaneously (NSE, KGE, and percent bias). Generates a
#' multi-objective scatter plot that shows which runs pass all criteria
#' ("Behavioral"), pass NSE/KGE but not bias ("Fails pBias Only"), or fail
#' entirely ("Fails Box"). Optionally generates an ensemble uncertainty
#' hydrograph showing only the behavioral runs.
#'
#' Performance metrics are computed internally via
#' \code{\link{extract_ensemble_metrics}} unless a pre-computed
#' \code{metrics_df} is supplied.  When computed internally, \code{run_id}
#' values are sequential (1 to \code{n_runs}), and failed runs receive
#' \code{NA} for all metrics — they are excluded from behavioral selection
#' automatically.
#'
#' @param ensemble_output List. Output from
#'   \code{\link{run_cosero_ensemble_parallel}} or
#'   \code{\link{run_cosero_ensemble}}.
#' @param subbasin_id Character or numeric. Subbasin to evaluate and plot
#'   (e.g., \code{"003"} or \code{3}). Auto-padded to 4 digits internally.
#'   Default is \code{"0001"}.
#' @param nse_thresh Numeric. Minimum acceptable NSE. Default is 0.6.
#' @param kge_thresh Numeric. Minimum acceptable KGE. Default is 0.6.
#' @param pbias_thresh Numeric vector of length 2. Acceptable percent bias
#'   range in percent (e.g., \code{c(-20, 20)} means ±20\%). Default is
#'   \code{c(-20, 20)}. Computed internally as \code{(BETA - 1) * 100} where
#'   BETA is the KGE bias ratio term from COSERO's \code{statistics.txt}.
#' @param metrics_df Optional data frame of pre-computed metrics with columns
#'   \code{run_id} (integer, 1-based positional index into
#'   \code{ensemble_output$results}), \code{NSE}, \code{KGE}, and
#'   \code{pBias}. If \code{NULL} (default), metrics are computed internally
#'   via \code{\link{extract_ensemble_metrics}}.
#' @param xlim Optional numeric vector of length 2 for KGE x-axis limits.
#' @param ylim Optional numeric vector of length 2 for NSE y-axis limits.
#' @param plot_uncertainty Logical. If \code{TRUE}, also generates an ensemble
#'   uncertainty hydrograph for the behavioral runs. Default is \code{FALSE}.
#' @param ... Additional arguments passed to
#'   \code{\link{plot_ensemble_uncertainty}} (e.g., \code{date_range},
#'   \code{q_max}, \code{lower_quantile}).
#'
#' @return A named list with:
#' \describe{
#'   \item{\code{scatter_plot}}{ggplot object: multi-objective KGE vs NSE scatter.}
#'   \item{\code{uncertainty_plot}}{ggplot object or \code{NULL}: behavioral
#'     ensemble hydrograph (only if \code{plot_uncertainty = TRUE}).}
#'   \item{\code{filtered_ensemble}}{The input \code{ensemble_output} with
#'     \code{$results} and \code{$parameter_sets} subsetted to behavioral runs only.}
#'   \item{\code{behavioral_run_ids}}{Integer vector of run indices that passed
#'     all criteria (1-based positions into \code{ensemble_output$results}).}
#'   \item{\code{metrics_df}}{Data frame of all run metrics with \code{category}
#'     column added. Useful for downstream analysis.}
#' }
#'
#' @details
#' The scatter plot uses three visual layers drawn bottom-to-top:
#' \enumerate{
#'   \item Grey points: fail NSE and/or KGE threshold.
#'   \item Red points: pass NSE/KGE box but fail percent bias.
#'   \item Filled circles: behavioral runs, colored by pBias
#'     (brown = wet bias, green = dry bias, white = unbiased).
#' }
#' Threshold boundaries are shown as dashed reference lines.
#'
#' The filtered ensemble object can be used directly in
#' \code{\link{plot_dotty}} or \code{\link{calculate_sobol_indices}} to
#' restrict downstream analysis to behavioral runs only.
#'
#' @seealso
#' \code{\link{extract_ensemble_metrics}},
#' \code{\link{plot_ensemble_uncertainty}},
#' \code{\link{plot_dotty}},
#' \code{\link{calculate_sobol_indices}}
#'
#' @import ggplot2
#' @importFrom dplyr mutate case_when
#' @export
#'
#' @examples
#' \dontrun{
#' # Run sensitivity ensemble first
#' ensemble <- run_cosero_ensemble_parallel(
#'   project_path    = "D:\\COSERO_project",
#'   parameter_sets  = samples$parameter_sets,
#'   par_bounds      = par_bounds,
#'   base_settings   = list(SPINUP = 365, OUTPUTTYPE = 1),
#'   n_cores         = 4
#' )
#'
#' # Extract behavioral runs — metrics computed automatically for subbasin 3
#' # pBias is derived internally as (BETA - 1) * 100 from statistics.txt
#' eval_out <- extract_behavioral_runs(
#'   ensemble_output  = ensemble,
#'   subbasin_id      = "003",
#'   nse_thresh       = 0.65,
#'   kge_thresh       = 0.60,
#'   pbias_thresh     = c(-15, 15),   # percent bias range
#'   plot_uncertainty = TRUE,
#'   date_range       = c("2010-01-01", "2012-12-31"),  # zoom hydrograph
#'   q_max            = 200,                             # cap y-axis at 200 m3/s
#'   lower_quantile   = 0.10,                            # 80% uncertainty band
#'   upper_quantile   = 0.90,
#'   xlim             = c(-0.5, 1),   # zoom KGE axis on scatter plot
#'   ylim             = c(-1, 1)      # zoom NSE axis on scatter plot
#' )
#'
#' # Inspect plots
#' print(eval_out$scatter_plot)
#' print(eval_out$uncertainty_plot)
#'
#' # Use filtered ensemble for dotty plots restricted to behavioral runs
#' behav_nse <- eval_out$metrics_df$NSE[
#'   eval_out$metrics_df$category == "Behavioral"
#' ]
#' plot_dotty(eval_out$filtered_ensemble$parameter_sets, Y = behav_nse,
#'            y_label = "NSE", reference_line = 0.65)
#'
#' # Supply pre-computed metrics_df to skip internal metric extraction.
#' # pBias must already be in percent: derive from BETA as (BETA - 1) * 100
#' beta_values  <- extract_ensemble_metrics(ensemble, subbasin_id = "003", metric = "BETA")
#' my_metrics <- data.frame(
#'   run_id = seq_along(nse_values),
#'   NSE    = nse_values,
#'   KGE    = kge_values,
#'   pBias  = (beta_values - 1) * 100
#' )
#' eval_out2 <- extract_behavioral_runs(
#'   ensemble_output = ensemble,
#'   subbasin_id     = "003",
#'   metrics_df      = my_metrics
#' )
#' }
extract_behavioral_runs <- function(ensemble_output,
                                    subbasin_id   = "0001",
                                    nse_thresh    = 0.6,
                                    kge_thresh    = 0.6,
                                    pbias_thresh  = c(-20, 20),
                                    metrics_df    = NULL,
                                    xlim          = NULL,
                                    ylim          = NULL,
                                    plot_uncertainty = FALSE,
                                    ...) {

  # Fix for R CMD check: "no visible binding for global variable"
  NSE <- KGE <- pBias <- category <- pass_nse_kge <- pass_pbias <- NULL

  # --- Input validation ---
  if (!is.list(ensemble_output) || is.null(ensemble_output$results)) {
    stop("'ensemble_output' must be the list returned by run_cosero_ensemble_parallel() or run_cosero_ensemble().",
         call. = FALSE)
  }
  if (length(pbias_thresh) != 2 || pbias_thresh[1] >= pbias_thresh[2]) {
    stop("'pbias_thresh' must be a numeric vector of length 2 with pbias_thresh[1] < pbias_thresh[2].",
         call. = FALSE)
  }

  # Pad subbasin ID to 4 digits for column matching
  subbasin_num    <- suppressWarnings(as.numeric(subbasin_id))
  subbasin_padded <- sprintf("%04d", subbasin_num)

  # --- Build metrics_df internally if not supplied ---
  if (is.null(metrics_df)) {
    nse_vec   <- extract_ensemble_metrics(ensemble_output, subbasin_id = subbasin_padded, metric = "NSE")
    kge_vec   <- extract_ensemble_metrics(ensemble_output, subbasin_id = subbasin_padded, metric = "KGE")
    # BETA = mean(QSIM)/mean(QOBS) from statistics.txt (KGE bias ratio term).
    # Convert to percent bias: pBias = (BETA - 1) * 100
    # COSERO's BIAS column is in m³/s (absolute), so it cannot be used here.
    beta_vec  <- extract_ensemble_metrics(ensemble_output, subbasin_id = subbasin_padded, metric = "BETA")

    metrics_df <- data.frame(
      run_id = seq_along(nse_vec),
      NSE    = nse_vec,
      KGE    = kge_vec,
      pBias  = (beta_vec - 1) * 100
    )
  } else {
    # Validate user-supplied metrics_df
    required_cols <- c("run_id", "NSE", "KGE", "pBias")
    missing_cols  <- setdiff(required_cols, colnames(metrics_df))
    if (length(missing_cols) > 0) {
      stop(sprintf(
        "Supplied 'metrics_df' is missing required columns: %s",
        paste(missing_cols, collapse = ", ")
      ), call. = FALSE)
    }
  }

  # --- Categorise runs ---
  # NA metrics (failed runs) evaluate to FALSE in comparisons -> "Outside Box"
  metrics_df <- metrics_df %>%
    mutate(
      pass_nse_kge = (!is.na(NSE) & !is.na(KGE) & NSE >= nse_thresh & KGE >= kge_thresh),
      pass_pbias   = (!is.na(pBias) & pBias >= pbias_thresh[1] & pBias <= pbias_thresh[2]),
      category     = case_when(
        pass_nse_kge & pass_pbias  ~ "Behavioral",
        pass_nse_kge & !pass_pbias ~ "Fails pBias Only",
        TRUE                       ~ "Outside Box"
      )
    )

  behavioral_ids <- metrics_df$run_id[metrics_df$category == "Behavioral"]
  n_behavioral   <- length(behavioral_ids)
  n_total        <- nrow(metrics_df)

  if (n_behavioral == 0) {
    warning(sprintf(
      "No runs met all behavioral criteria (NSE >= %s, KGE >= %s, pBias in [%s, %s]). Returning empty filtered ensemble.",
      nse_thresh, kge_thresh, pbias_thresh[1], pbias_thresh[2]
    ), call. = FALSE)
  } else {
    message(sprintf("%d / %d runs are behavioral (%.1f%%)",
                    n_behavioral, n_total, 100 * n_behavioral / n_total))
  }

  # --- Filter ensemble to behavioral runs ---
  filtered_ensemble          <- ensemble_output
  filtered_ensemble$results  <- ensemble_output$results[behavioral_ids]

  if (!is.null(filtered_ensemble$parameter_sets) && n_behavioral > 0) {
    filtered_ensemble$parameter_sets <-
      filtered_ensemble$parameter_sets[behavioral_ids, , drop = FALSE]
  }

  # --- Scatter plot ---
  df_outside    <- metrics_df[metrics_df$category == "Outside Box",    ]
  df_alarming   <- metrics_df[metrics_df$category == "Fails pBias Only", ]
  df_behavioral <- metrics_df[metrics_df$category == "Behavioral",     ]

  p_scatter <- ggplot() +
    # Threshold reference lines (avoids scalar-in-aes() issue)
    geom_vline(xintercept = kge_thresh,  linetype = "dashed", color = "grey30", linewidth = 0.5) +
    geom_hline(yintercept = nse_thresh,  linetype = "dashed", color = "grey30", linewidth = 0.5) +
    # Layer 1: fails NSE/KGE box
    geom_point(data = df_outside,
               aes(x = KGE, y = NSE, color = "Fails Box"),
               size = 1.2, alpha = 0.45) +
    # Layer 2: passes box but fails pBias
    geom_point(data = df_alarming,
               aes(x = KGE, y = NSE, color = "Fails pBias Only"),
               size = 2.0, alpha = 0.80) +
    # Layer 3: behavioral (filled circle, colored by pBias value)
    geom_point(data = df_behavioral,
               aes(x = KGE, y = NSE, fill = pBias, color = "Behavioral"),
               shape = 21, size = 3.0, alpha = 0.90, stroke = 0.5) +
    scale_color_manual(
      name   = "Category",
      values = c("Fails Box" = "grey70", "Fails pBias Only" = "indianred", "Behavioral" = "black"),
      guide  = guide_legend(override.aes = list(size = 2.5, shape = c(16, 16, 21)))
    ) +
    scale_fill_gradient2(
      low      = "#8c510a",
      mid      = "white",
      high     = "#01665e",
      midpoint = 0,
      name     = "pBias (%)"
    ) +
    theme_bw() +
    labs(
      title    = paste0("Multi-Objective Evaluation \u2014 Subbasin ", subbasin_num),
      subtitle = sprintf("NSE \u2265 %s  |  KGE \u2265 %s  |  pBias: [%s, %s%%]  |  %d / %d behavioral",
                         nse_thresh, kge_thresh, pbias_thresh[1], pbias_thresh[2],
                         n_behavioral, n_total),
      x = "KGE", y = "NSE"
    ) +
    theme(legend.position = "right")

  if (!is.null(xlim) || !is.null(ylim)) {
    p_scatter <- p_scatter + coord_cartesian(xlim = xlim, ylim = ylim)
  }

  # --- Uncertainty plot (optional) ---
  p_uncert <- NULL
  if (plot_uncertainty) {
    if (n_behavioral > 0) {
      p_uncert <- plot_ensemble_uncertainty(
        filtered_ensemble,
        subbasin_id = subbasin_padded,
        ...
      ) +
        labs(
          title    = paste0("Behavioral Ensemble Uncertainty \u2014 Subbasin ", subbasin_num),
          subtitle = sprintf("Selected: NSE \u2265 %s | KGE \u2265 %s | pBias: [%s, %s%%]  (%d runs)",
                             nse_thresh, kge_thresh, pbias_thresh[1], pbias_thresh[2],
                             n_behavioral)
        )
    } else {
      warning("Skipping uncertainty plot: no behavioral runs to plot.", call. = FALSE)
    }
  }

  return(list(
    scatter_plot      = p_scatter,
    uncertainty_plot  = p_uncert,
    filtered_ensemble = filtered_ensemble,
    behavioral_run_ids = behavioral_ids,
    metrics_df        = metrics_df
  ))
}

# 10 Export Functions #####

#' Export Sensitivity Analysis Results
#'
#' @param output_dir Output directory
#' @param sobol_indices Sobol indices object
#' @param parameter_sets Parameter sets
#' @param metrics Performance metrics
#' @param prefix File prefix
#' @export
export_sensitivity_results <- function(output_dir,
                                       sobol_indices,
                                       parameter_sets,
                                       metrics = NULL,
                                       prefix = "cosero_sensitivity") {

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Save Sobol indices
  saveRDS(sobol_indices, file.path(output_dir, paste0(prefix, "_sobol.rds")))

  # Save parameter sets
  write.csv(parameter_sets,
            file.path(output_dir, paste0(prefix, "_parameters.csv")),
            row.names = FALSE)

  # Save metrics
  if (!is.null(metrics)) {
    write.csv(data.frame(run = 1:length(metrics), metric = metrics),
              file.path(output_dir, paste0(prefix, "_metrics.csv")),
              row.names = FALSE)
  }

  # Export summary table
  results_table <- sobol_indices$results %>%
    filter(sensitivity == "Si") %>%
    arrange(desc(original)) %>%
    select(parameters, original, std.error, low.ci, high.ci)

  write.csv(results_table,
            file.path(output_dir, paste0(prefix, "_summary.csv")),
            row.names = FALSE)

  cat("Results exported to:", output_dir, "\n")
}

