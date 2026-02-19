# COSERO Sensitivity Analysis
# Sobol-based global sensitivity analysis for COSERO parameters
# Date: 2025-11-04

#' @importFrom dplyr filter mutate select arrange group_by summarise bind_rows %>% desc
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom purrr map map_dbl map_dfr map2_df
#' @importFrom tibble as_tibble
#' @importFrom ggplot2 ggplot aes geom_bar geom_point geom_line geom_ribbon geom_hline
#' @importFrom ggplot2 coord_flip coord_cartesian facet_wrap
#' @importFrom ggplot2 theme_minimal theme_bw theme element_text element_rect
#' @importFrom ggplot2 labs ggtitle
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
  "q05", "q25", "q75", "q95", ".", "batch_position"
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
    sobol_matrix = sobol_mat,
    parameter_sets = par_scaled,
    n = n,
    par_names = par_names
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
  # KEY FIX: Ensure each run in a batch uses a UNIQUE worker (no conflicts)
  if (!quiet) cat(sprintf("Starting %d runs in %d batches...\n\n", n_runs, n_batches))

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
      cat(sprintf("  Completed %d/%d runs (batch %d/%d)\n",
                  completed_count, n_runs, batch_idx, n_batches))
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
    cat(sprintf("  Average per run: %s\n", format_time_duration(runtime * 60 / n_runs)))

    # Calculate efficiency
    avg_run_time <- runtime * 60 / n_runs
    est_sequential_time <- avg_run_time * n_runs
    actual_speedup <- est_sequential_time / parallel_time
    efficiency <- (actual_speedup / n_cores) * 100

    cat(sprintf("  Parallel efficiency: %.0f%% (speedup: %.2fx / ideal: %dx)\n",
                efficiency, actual_speedup, n_cores))
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
#' Calculates first-order and total-order Sobol sensitivity indices from
#' model outputs and parameter samples. Uses bootstrap for uncertainty estimation.
#'
#' @param Y Numeric vector of model outputs (length = nrow(parameter_sets))
#' @param sobol_samples List from generate_sobol_samples() containing parameter sets
#' @param boot Logical. If TRUE, performs bootstrap for confidence intervals.
#' @param R Integer. Number of bootstrap resamples (default: 100)
#'
#' @return Sobol indices object from sensobol package containing:
#'   \item{results}{Data frame with sensitivity indices (Si, Ti) and confidence intervals}
#'   \item{...}{Other sensobol output fields}
#'
#' @export
#' @examples
#' \dontrun{
#' # After running ensemble and extracting metrics
#' nse <- extract_ensemble_metrics(results, subbasin_id = "0001", metric = "NSE")
#'
#' # Calculate sensitivity indices
#' sobol_ind <- calculate_sobol_indices(
#'   Y = nse,
#'   sobol_samples = samples,
#'   boot = TRUE,
#'   R = 100
#' )
#'
#' # Plot results
#' plot_sobol(sobol_ind)
#' }
calculate_sobol_indices <- function(Y, sobol_samples, boot = TRUE, R = 100) {

  # Remove NA values
  valid_idx <- !is.na(Y)
  Y_clean <- Y[valid_idx]

  sobol_result <- sobol_indices(
    Y = Y_clean,
    N = sobol_samples$n,
    params = sobol_samples$par_names,
    first = "jansen",
    total = "jansen",
    boot = boot,
    R = R
  )

  return(sobol_result)
}

# 8 Visualization #####

#' Plot Sobol Indices
#'
#' @param sobol_indices Result from calculate_sobol_indices()
#' @param title Plot title
#' @return ggplot object
plot_sobol <- function(sobol_indices, title = "Sobol Sensitivity Indices") {
  p <- plot(sobol_indices) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9)) +
    ggtitle(title)

  return(p)
}

#' Create Dotty Plots
#'
#' @param parameter_sets Parameter sets tibble
#' @param Y Output vector
#' @param y_label Y-axis label
#' @param n_col Number of columns in facet
#' @param reference_line Reference line value (optional)
#' @param y_min Minimum Y-axis value (e.g., 0 to start from zero)
#' @return ggplot object
plot_dotty <- function(parameter_sets, Y, y_label = "Output",
                       n_col = 3, reference_line = NULL, y_min = NULL) {

  dotty_data <- parameter_sets %>%
    mutate(output = Y) %>%
    pivot_longer(cols = -output, names_to = "parameter", values_to = "value")

  p <- ggplot(dotty_data, aes(x = value, y = output)) +
    geom_point(alpha = 0.6, size = 1.2, color = "darkblue") +
    facet_wrap(~ parameter, scales = "free_x", ncol = n_col) +
    theme_bw() +
    labs(x = "Parameter Value", y = y_label) +
    theme(
      strip.text = element_text(size = 10, face = "bold"),
      strip.background = element_rect(fill = "lightgray")
    )

  # Set Y-axis minimum if specified
  if (!is.null(y_min)) {
    y_max <- max(Y, na.rm = TRUE)
    p <- p + coord_cartesian(ylim = c(y_min, y_max))
  }

  if (!is.null(reference_line)) {
    p <- p + geom_hline(yintercept = reference_line,
                       linetype = "dashed", color = "red", linewidth = 1)
  }

  return(p)
}

#' Plot Ensemble Uncertainty
#'
#' Visualizes ensemble uncertainty by plotting median and quantile ranges
#' across multiple model runs. Works directly with output from
#' run_cosero_ensemble() or run_cosero_ensemble_parallel().
#'
#' @param ensemble_output Output from run_cosero_ensemble_parallel() or
#'   run_cosero_ensemble(), or a list of individual run results
#' @param observed Optional data frame with observed data containing 'date'
#'   and 'value' columns
#' @param subbasin_id Subbasin identifier (e.g., "0001"). Default is "0001"
#' @param variable Variable to plot (e.g., "QSIM", "QOBS"). Default is "QSIM"
#'
#' @return ggplot object showing median (blue line), 50% confidence interval
#'   (dark gray ribbon), and 90% confidence interval (light gray ribbon)
#'
#' @export
#' @examples
#' \dontrun{
#' # After running ensemble
#' results <- run_cosero_ensemble_parallel(...)
#'
#' # Plot uncertainty for subbasin 0001
#' p <- plot_ensemble_uncertainty(results, subbasin_id = "0001")
#' print(p)
#'
#' # With observed data
#' obs <- data.frame(date = dates, value = observed_discharge)
#' p <- plot_ensemble_uncertainty(results, observed = obs, subbasin_id = "0001")
#' }
plot_ensemble_uncertainty <- function(ensemble_output, observed = NULL,
                                      subbasin_id = "0001", variable = "QSIM") {

  # Check if ensemble_output is the output from run_cosero_ensemble_parallel/run_cosero_ensemble
  if (!is.null(ensemble_output$results)) {
    results_list <- ensemble_output$results
  } else {
    # Assume it's already a list of results
    results_list <- ensemble_output
  }

  n_runs <- length(results_list)

  # Extract timeseries data from each run
  timeseries_list <- vector("list", n_runs)
  dates <- NULL
  n_time <- NULL

  for (i in 1:n_runs) {
    result <- results_list[[i]]

    if (result$success && !is.null(result$output_data) &&
        !is.null(result$output_data$runoff)) {

      runoff <- result$output_data$runoff

      # Construct column name based on variable and subbasin_id
      col_name <- paste0(variable, "_", subbasin_id)

      if (col_name %in% colnames(runoff)) {
        timeseries_list[[i]] <- runoff[[col_name]]

        # Get dates from first successful run
        if (is.null(dates)) {
          if ("date" %in% colnames(runoff)) {
            dates <- runoff$date
          } else {
            # Construct dates from yyyy, mm, dd columns
            dates <- as.Date(paste(runoff$yyyy, runoff$mm, runoff$dd, sep = "-"))
          }
          n_time <- length(dates)
        }
      } else {
        warning(sprintf("Run %d: Column '%s' not found in runoff data", i, col_name))
        timeseries_list[[i]] <- NA
      }
    } else {
      timeseries_list[[i]] <- NA
    }
  }

  # Check if we got any valid data
  n_valid <- sum(sapply(timeseries_list, function(x) !all(is.na(x))))
  if (n_valid == 0) {
    stop("No valid timeseries data found in ensemble output. Check subbasin_id and variable.",
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

  # Calculate quantiles
  stats <- data.frame(
    date = dates,
    median = apply(output_matrix, 1, median, na.rm = TRUE),
    q25 = apply(output_matrix, 1, quantile, 0.25, na.rm = TRUE),
    q75 = apply(output_matrix, 1, quantile, 0.75, na.rm = TRUE),
    q05 = apply(output_matrix, 1, quantile, 0.05, na.rm = TRUE),
    q95 = apply(output_matrix, 1, quantile, 0.95, na.rm = TRUE)
  )

  # Create plot
  p <- ggplot(stats, aes(x = date)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "gray80", alpha = 0.5) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "gray60", alpha = 0.5) +
    geom_line(aes(y = median), color = "blue", linewidth = 1) +
    theme_bw() +
    labs(x = "Date",
         y = paste0(variable, " (Subbasin ", subbasin_id, ")"),
         title = paste0("Ensemble Uncertainty (", n_valid, "/", n_runs, " runs)"))

  # Add observed data if provided
  if (!is.null(observed)) {
    if (is.data.frame(observed) && "date" %in% colnames(observed) && "value" %in% colnames(observed)) {
      p <- p + geom_line(data = observed, aes(x = date, y = value),
                        color = "black", linewidth = 1.2)
    } else {
      warning("Observed data must be a data frame with 'date' and 'value' columns")
    }
  }

  return(p)
}

# 9 Export Functions #####

#' Export Sensitivity Analysis Results
#'
#' @param output_dir Output directory
#' @param sobol_indices Sobol indices object
#' @param parameter_sets Parameter sets
#' @param metrics Performance metrics
#' @param prefix File prefix
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

