# COSERO Sensitivity Analysis
# Sobol-based global sensitivity analysis for COSERO parameters
# Author: COSERO R Interface
# Date: 2025-11-04

# 1 Load Libraries #####
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(sensobol)
library(hydroGOF)
library(parallel)
library(doSNOW)    # For progress updates in parallel
library(foreach)

# Note: COSERO functions loaded from package
# (run_cosero, read_cosero_output, read_defaults, etc.)

# Helper Functions #####

#' Display Progress for Ensemble Runs
#'
#' @param current Current run number
#' @param total Total number of runs
#' @param start_time Start time of ensemble
#' @param run_type Type of run ("Simulation" or other label)
#' @param quiet Suppress output
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
#'     \item "relchg" - Relative change (multiply original by factor, preserves spatial patterns)
#'     \item "abschg" - Absolute change (add value to original, preserves spatial differences)
#'   }
#' @param default Vector of default values (optional, same length as parameters)
#' @param description Vector of parameter descriptions (optional)
#' @param category Vector of parameter categories (optional)
#' @param sample_min Vector of minimum sampling values for Sobol (optional)
#' @param sample_max Vector of maximum sampling values for Sobol (optional)
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
    stop("Length of 'min' must match length of 'parameters'")
  }
  if (length(max) != n_params) {
    stop("Length of 'max' must match length of 'parameters'")
  }
  if (length(modification_type) != n_params) {
    stop("Length of 'modification_type' must match length of 'parameters'")
  }

  # Validate modification types
  valid_types <- c("relchg", "abschg")
  invalid_types <- modification_type[!modification_type %in% valid_types]
  if (length(invalid_types) > 0) {
    stop("Invalid modification_type: ", paste(invalid_types, collapse = ", "),
         "\nValid types are: ", paste(valid_types, collapse = ", "))
  }

  # Validate min < max
  if (any(min >= max)) {
    invalid_idx <- which(min >= max)
    stop("min must be less than max for parameters: ",
         paste(parameters[invalid_idx], collapse = ", "))
  }

  # Set sampling ranges (for Sobol)
  if (is.null(sample_min)) {
    sample_min <- ifelse(modification_type == "relchg", 0.5, min)
  }
  if (is.null(sample_max)) {
    sample_max <- ifelse(modification_type == "relchg", 2.0, max)
  }

  # Set defaults for optional parameters
  if (is.null(default)) {
    default <- ifelse(modification_type == "relchg", 1.0, (min + max) / 2)
  } else if (length(default) != n_params) {
    stop("Length of 'default' must match length of 'parameters'")
  }

  if (is.null(description)) {
    description <- paste("Parameter:", parameters)
  } else if (length(description) != n_params) {
    stop("Length of 'description' must match length of 'parameters'")
  }

  if (is.null(category)) {
    category <- rep("custom", n_params)
  } else if (length(category) != n_params) {
    stop("Length of 'category' must match length of 'parameters'")
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
      stop("'parameters' must be specified when using custom_min, custom_max, and custom_modification_type")
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
    bounds_file <- system.file("extdata", "parameter_bounds.csv", package = "COSERO")
    if (bounds_file == "") {
      stop("Could not find parameter_bounds.csv in package installation.\n",
           "Either install the package data or provide a custom bounds_file path.",
           call. = FALSE)
    }
  } else if (!file.exists(bounds_file)) {
    stop("Parameter bounds file not found: ", bounds_file,
         "\nEither provide a valid CSV file path or use custom bounds parameters")
  }

  bounds <- read.csv(bounds_file, stringsAsFactors = FALSE)

  if (!is.null(parameters)) {
    bounds <- bounds %>% filter(parameter %in% parameters)
  }

  # Add sample_min/sample_max if missing
  if (!"sample_min" %in% colnames(bounds)) {
    bounds$sample_min <- ifelse(bounds$modification_type == "relchg", 0.5, bounds$min)
  }
  if (!"sample_max" %in% colnames(bounds)) {
    bounds$sample_max <- ifelse(bounds$modification_type == "relchg", 2.0, bounds$max)
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
#'
#' @return List containing:
#'   \item{results}{List of COSERO output data for each run}
#'   \item{parameter_sets}{Original parameter sets used}
#'   \item{runtime_minutes}{Total runtime in minutes}
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
#' nse <- extract_cosero_metrics(results, subbasin_id = "0001", metric = "NSE")
#' }
run_cosero_ensemble <- function(project_path,
                                parameter_sets,
                                par_bounds,
                                base_settings = NULL,
                                par_file = NULL,
                                quiet = FALSE) {

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
    stop("Parameter file not found: ", par_file)
  }

  if (!quiet) cat("Using parameter file:", par_file, "\n")

  n_runs <- nrow(parameter_sets)
  results <- vector("list", n_runs)

  # Create backup folder for parameter files
  backup_dir <- file.path(dirname(par_file), "parameterfile_backup")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)
  backup_file <- file.path(backup_dir, paste0(basename(par_file), ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S")))
  file.copy(par_file, backup_file)

  # Read original parameter values once (detect format) - always quiet
  original_values <- tryCatch({
    read_parameter_table(par_file, names(parameter_sets), zone_id = NULL, quiet = TRUE)
  }, error = function(e) {
    read_parameter_file(par_file, names(parameter_sets))
  })

  # Check if using tabular format (used for modify_parameter_table fallback)
  # Don't pre-load - just detect format
  param_file_structure <- tryCatch({
    # Just check if it's tabular format, don't load the data
    test_read <- readLines(par_file, n = 3)
    if (length(test_read) >= 2 && grepl("\t", test_read[2])) {
      list(is_tabular = TRUE)
    } else {
      list(is_tabular = FALSE)
    }
  }, error = function(e) {
    list(is_tabular = FALSE)
  })

  start_time <- Sys.time()

  # Report initial message once
  if (!quiet) {
    cat(sprintf("\nStarting %d COSERO simulations (sequential)\n", n_runs))
    param_names_str <- paste(names(parameter_sets), collapse = ", ")
    cat(sprintf("Parameters: %s\n\n", param_names_str))
  }

  for (i in 1:n_runs) {
    # Restore original file for each run
    file.copy(backup_file, par_file, overwrite = TRUE)

    # Modify parameters (try tabular format first, fall back to simple format)
    if (param_file_structure$is_tabular) {
      tryCatch({
        modify_parameter_table(par_file, parameter_sets[i, ], par_bounds, original_values, quiet = TRUE)
      }, error = function(e) {
        modify_parameter_file(par_file, parameter_sets[i, ], par_bounds, original_values)
      })
    } else {
      modify_parameter_file(par_file, parameter_sets[i, ], par_bounds, original_values)
    }

    # Run COSERO
    tryCatch({
      result <- run_cosero(
        project_path = project_path,
        defaults_settings = base_settings,
        quiet = TRUE,
        read_outputs = TRUE
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
  }

  # Restore original parameter file
  file.copy(backup_file, par_file, overwrite = TRUE)

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  if (!quiet) {
    cat(sprintf("\n✓ Ensemble completed in %s\n", format_time_duration(as.numeric(runtime) * 60)))
    cat(sprintf("  Average per run: %s\n", format_time_duration(runtime * 60 / n_runs)))
  }

  return(list(
    results = results,
    parameter_sets = parameter_sets,
    runtime_minutes = as.numeric(runtime)
  ))
}

#' Read Parameter Values from File
#'
#' @param par_file Path to para.txt
#' @param param_names Vector of parameter names to read
#' @return Named list of parameter values
read_parameter_file <- function(par_file, param_names) {
  lines <- readLines(par_file)
  values <- list()

  for (param_name in param_names) {
    # Case-insensitive search
    param_idx <- grep(paste0("^", param_name, "\\b"), lines, ignore.case = TRUE)

    if (length(param_idx) > 0) {
      value_idx <- param_idx[1] + 1
      if (value_idx <= length(lines)) {
        values[[param_name]] <- as.numeric(lines[value_idx])
      }
    }
  }

  return(values)
}

#' Modify COSERO Parameter File
#'
#' @param par_file Path to para.txt
#' @param params Named vector/list of sampled parameter values
#' @param par_bounds Parameter bounds table with modification types
#' @param original_values Original parameter values from file
modify_parameter_file <- function(par_file, params, par_bounds, original_values) {
  lines <- readLines(par_file)

  for (param_name in names(params)) {
    sampled_value <- params[[param_name]]

    # Get modification type (case-insensitive)
    mod_type_idx <- which(par_bounds$parameter == param_name)
    if (length(mod_type_idx) == 0) {
      mod_type_idx <- which(tolower(par_bounds$parameter) == tolower(param_name))
    }

    if (length(mod_type_idx) == 0) {
      warning("Modification type not found for ", param_name)
      next
    }

    mod_type <- par_bounds$modification_type[mod_type_idx[1]]

    # Calculate final value based on modification type
    if (mod_type == "relchg") {
      # Multiply original by sampled_value (preserves spatial pattern)
      original <- original_values[[param_name]]
      if (!is.null(original)) {
        final_value <- original * sampled_value
      } else {
        stop("Original value not found for ", param_name)
      }
    } else if (mod_type == "abschg") {
      # Add sampled_value to original value (preserves spatial differences)
      original <- original_values[[param_name]]
      if (!is.null(original)) {
        final_value <- original + sampled_value
      } else {
        stop("Original value not found for ", param_name)
      }
    } else {
      stop("Unknown modification type: ", mod_type, ". Use 'relchg' or 'abschg'.")
    }

    # Apply physical bounds as safeguards
    param_min <- par_bounds$min[mod_type_idx[1]]
    param_max <- par_bounds$max[mod_type_idx[1]]

    if (length(param_min) > 0 && length(param_max) > 0) {
      # Clip to physical bounds
      final_value_orig <- final_value
      final_value <- max(param_min, min(param_max, final_value))

      # Warn if clipping occurred
      if (abs(final_value - final_value_orig) > 1e-10) {
        warning(sprintf("Parameter %s: value %.3f clipped to physical bounds [%.3f, %.3f] -> %.3f",
                        param_name, final_value_orig, param_min, param_max, final_value))
      }
    }

    # Find parameter line and update (case-insensitive)
    param_idx <- grep(paste0("^", param_name, "\\b"), lines, ignore.case = TRUE)

    if (length(param_idx) > 0) {
      value_idx <- param_idx[1] + 1
      if (value_idx <= length(lines)) {
        lines[value_idx] <- as.character(final_value)
      }
    }
  }

  writeLines(lines, par_file)
}

#' Read Parameter Values from Tabular File
#'
#' @param par_file Path to parameter file (tabular format with zones)
#' @param param_names Vector of parameter names to read
#' @param zone_id Zone ID to extract parameters from (default: first zone). Use "all" to read all zones.
#' @return If zone_id is specified: Named list of parameter values.
#'         If zone_id = "all": Data frame with columns NZ_ (zone ID) and one column per parameter
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
    stop("Zone ", zone_id, " not found in parameter file")
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
      if (mod_type == "relchg") {
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] * sampled_value
      } else if (mod_type == "abschg") {
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] + sampled_value
      } else {
        stop("Unknown modification type: ", mod_type, ". Use 'relchg' or 'abschg'.")
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

  # Write back to file (only write operation per run!)
  writeLines(first_line, par_file)
  suppressWarnings({
    write.table(
      param_data,
      file = par_file,
      append = TRUE,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      na = ""
    )
  })
}

#' Modify COSERO Parameter File (Tabular Format)
#'
#' @param par_file Path to parameter file
#' @param params Named vector/list of sampled parameter values
#' @param par_bounds Parameter bounds table with modification types
#' @param original_values Original parameter values from file
#' @param zones Vector of zone IDs to modify (NULL = all zones)
#' @param quiet Suppress messages
modify_parameter_table <- function(par_file, params, par_bounds, original_values, zones = NULL, quiet = FALSE) {
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
      # Calculate final value based on modification type
      if (mod_type == "relchg") {
        # Multiply original by sampled_value (preserves spatial pattern)
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] * sampled_value
      } else if (mod_type == "abschg") {
        # Add sampled_value to original value (preserves spatial differences)
        param_data[zone_mask, param_col] <- param_data[zone_mask, param_col] + sampled_value
      } else {
        stop("Unknown modification type: ", mod_type, ". Use 'relchg' or 'abschg'.")
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

  # Write back to file
  # Write first line
  writeLines(first_line, par_file)

  # Append parameter table with column names
  # Suppress expected warning about appending column names
  suppressWarnings({
    write.table(
      param_data,
      file = par_file,
      append = TRUE,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE,
      na = ""
    )
  })
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
#' @return List with results and parameters
run_cosero_ensemble_parallel <- function(project_path,
                                         parameter_sets,
                                         par_bounds,
                                         base_settings = NULL,
                                         n_cores = NULL,
                                         temp_dir = NULL,
                                         quiet = FALSE) {

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
    stop("Parameter file not found: ", par_file)
  }

  # Read original parameter values (detect format) - always quiet
  original_values <- tryCatch({
    read_parameter_table(par_file, names(parameter_sets), zone_id = NULL, quiet = TRUE)
  }, error = function(e) {
    read_parameter_file(par_file, names(parameter_sets))
  })

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
                      "modify_parameter_file", "modify_parameter_table",
                      "read_parameter_file", "read_parameter_table", "find_parameter_column"),
                envir = environment())

  # Load required libraries and package on each worker
  clusterEvalQ(cl, {
    library(dplyr)
    library(readr)
    library(tibble)
    library(stringr)
    library(lubridate)
    library(data.table)

    # Try to load COSERO package (works for installed package)
    if (requireNamespace("COSERO", quietly = TRUE)) {
      library(COSERO)
    } else {
      # If not installed, try devtools::load_all() for development
      if (requireNamespace("devtools", quietly = TRUE)) {
        # Assume package is in working directory or parent
        pkg_path <- getwd()
        if (!file.exists(file.path(pkg_path, "DESCRIPTION"))) {
          pkg_path <- dirname(pkg_path)
        }
        devtools::load_all(pkg_path, quiet = TRUE)
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

  # Pre-create worker directories (once per core, not per run)
  # Only create as many as needed (don't create 4 workers for 3 runs)
  n_workers <- min(n_cores, n_runs)
  worker_dirs <- file.path(temp_dir, paste0("worker_", seq_len(n_workers)))

  if (!quiet) cat("Preparing", n_workers, "worker directories...\n")
  prep_start <- Sys.time()

  # Parallelize the worker directory setup itself!
  clusterExport(cl, c("worker_dirs", "project_path"), envir = environment())
  parLapply(cl, worker_dirs, function(wdir) {
    if (dir.exists(wdir)) unlink(wdir, recursive = TRUE)
    dir.create(wdir, recursive = TRUE, showWarnings = FALSE)

    # Copy project once per worker
    project_files <- list.files(project_path, full.names = TRUE, all.files = FALSE, recursive = FALSE)
    file.copy(from = project_files, to = wdir, recursive = TRUE, overwrite = TRUE)
    return(TRUE)
  })

  prep_time <- difftime(Sys.time(), prep_start, units = "secs")
  if (!quiet) cat(sprintf("Worker setup completed in %.1fs\n\n", prep_time))

  # Export worker_dirs to workers
  clusterExport(cl, c("worker_dirs"), envir = environment())

  # Run in parallel with foreach and live progress
  results <- foreach(
    i = 1:n_runs,
    .packages = c("dplyr", "readr", "tibble", "stringr", "lubridate", "data.table"),
    .options.snow = opts
  ) %dopar% {
    # Identify which worker this is (1-based index)
    worker_idx <- (i %% length(worker_dirs)) + 1
    if (worker_idx > length(worker_dirs)) worker_idx <- 1

    worker_project_full <- worker_dirs[worker_idx]
    worker_par_file <- file.path(worker_project_full, "input", par_filename)

    # Check if parameter file exists
    if (!file.exists(worker_par_file)) {
      stop("Parameter file not found in worker project: ", worker_par_file)
    }

    tryCatch({
      # Modify parameters (try tabular format first, fall back to simple format)
      tryCatch({
        modify_parameter_table(worker_par_file, parameter_sets[i, ],
                              par_bounds, original_values, quiet = TRUE)
      }, error = function(e) {
        modify_parameter_file(worker_par_file, parameter_sets[i, ],
                             par_bounds, original_values)
      })

      # Run COSERO
      result <- run_cosero(
        project_path = worker_project_full,
        defaults_settings = base_settings,
        quiet = TRUE,
        read_outputs = TRUE
      )

      return(result)

    }, error = function(e) {
      # Provide detailed error information
      error_msg <- paste0(
        "Run ", i, " failed: ", e$message,
        "\nWorker project: ", worker_project_full,
        "\nParameter file: ", worker_par_file,
        "\nFile exists: ", file.exists(worker_par_file)
      )
      warning(error_msg)
      return(list(success = FALSE, error = error_msg, run_id = i))
    })
  }

  # Cleanup worker directories after all runs complete
  for (wdir in worker_dirs) {
    if (dir.exists(wdir)) unlink(wdir, recursive = TRUE)
  }

  parallel_time <- as.numeric(difftime(Sys.time(), parallel_start, units = "secs"))

  # Cleanup
  stopCluster(cl)
  on.exit()

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  if (!quiet) {
    cat(sprintf("\n✓ Ensemble completed in %s\n", format_time_duration(as.numeric(runtime) * 60)))
    cat(sprintf("  Average per run: %s\n", format_time_duration(runtime * 60 / n_runs)))

    # Calculate efficiency
    avg_run_time <- runtime * 60 / n_runs
    est_sequential_time <- avg_run_time * n_runs
    actual_speedup <- est_sequential_time / parallel_time
    efficiency <- (actual_speedup / n_cores) * 100

    cat(sprintf("  Parallel efficiency: %.0f%% (speedup: %.2fx / ideal: %dx)\n",
                efficiency, actual_speedup, n_cores))
  }

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
        stop("Unknown variable: ", variable)
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

#' Extract Pre-calculated Metrics from COSERO Statistics
#'
#' Extracts performance metrics (NSE, KGE, etc.) that were pre-calculated by COSERO
#' and stored in statistics.txt. This is the recommended approach for standard metrics
#' as it is faster and guaranteed to match COSERO's internal evaluation (including
#' proper spin-up handling).
#'
#' @param ensemble_results Results from run_cosero_ensemble() or run_cosero_ensemble_parallel()
#' @param subbasin_id Subbasin ID to extract metrics for (e.g., "001" or 1)
#' @param metric Performance metric to extract ("NSE", "KGE", "BIAS", "RMSE", etc.)
#'               Must be a column name in the statistics data frame
#' @param warn_nan If TRUE, warns when metrics are NaN (no observed data for subbasin)
#' @return Numeric vector of metric values for each ensemble run
#' @export
#' @examples
#' \dontrun{
#' # Run ensemble
#' results <- run_cosero_ensemble_parallel(project_path, parameter_sets, n_cores = 4)
#'
#' # Extract NSE for subbasin 001
#' nse_values <- extract_cosero_metrics(results, subbasin_id = "001", metric = "NSE")
#'
#' # Use in sensitivity analysis
#' sobol_indices <- calculate_sobol_indices(nse_values, sobol_samples)
#' plot_sobol(sobol_indices)
#' }
extract_cosero_metrics <- function(ensemble_results, subbasin_id = "001",
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

#' Calculate Performance Metrics for Ensemble (Custom Calculation)
#'
#' Calculates performance metrics by comparing simulated (QSIM) against observed (QOBS) discharge
#' from COSERO results. Automatically applies spin-up period from defaults_settings.
#'
#' For standard metrics (NSE, KGE) already calculated by COSERO, use extract_cosero_metrics()
#' instead - it's faster and guaranteed to match COSERO's evaluation.
#'
#' @param ensemble_results Results from run_cosero_ensemble() or run_cosero_ensemble_parallel()
#' @param subbasin_id Subbasin ID to calculate metrics for (e.g., "001" or 1)
#' @param metric Performance metric ("NSE", "KGE", "RMSE", "PBIAS")
#' @param spinup Spin-up period in timesteps (optional). If NULL, reads from defaults_settings$SPINUP
#' @return Vector of metric values
#' @export
#' @examples
#' \dontrun{
#' # Calculate PBIAS (auto-detects spin-up from settings)
#' pbias <- calculate_ensemble_metrics(results, subbasin_id = "001", metric = "PBIAS")
#'
#' # Override spin-up period
#' pbias <- calculate_ensemble_metrics(results, subbasin_id = "001", metric = "PBIAS", spinup = NULL)
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
            stop("Unknown metric: ", metric)
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
#' nse <- extract_cosero_metrics(results, subbasin_id = "0001", metric = "NSE")
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
#' @return ggplot object
plot_dotty <- function(parameter_sets, Y, y_label = "Output",
                       n_col = 3, reference_line = NULL) {

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

  if (!is.null(reference_line)) {
    p <- p + geom_hline(yintercept = reference_line,
                       linetype = "dashed", color = "red", size = 1)
  }

  return(p)
}

#' Plot Ensemble Uncertainty
#'
#' @param ensemble_output List of time series from extract_ensemble_output()
#' @param observed Observed data frame with date and value
#' @return ggplot object
plot_ensemble_uncertainty <- function(ensemble_output, observed = NULL) {

  # Convert list to matrix
  n_time <- length(ensemble_output[[1]]$value)
  n_runs <- length(ensemble_output)

  output_matrix <- matrix(NA, nrow = n_time, ncol = n_runs)
  for (i in 1:n_runs) {
    if (!is.na(ensemble_output[[i]])[1]) {
      output_matrix[, i] <- ensemble_output[[i]]$value
    }
  }

  # Calculate quantiles
  dates <- ensemble_output[[1]]$date
  stats <- data.frame(
    date = dates,
    median = apply(output_matrix, 1, median, na.rm = TRUE),
    q25 = apply(output_matrix, 1, quantile, 0.25, na.rm = TRUE),
    q75 = apply(output_matrix, 1, quantile, 0.75, na.rm = TRUE),
    q05 = apply(output_matrix, 1, quantile, 0.05, na.rm = TRUE),
    q95 = apply(output_matrix, 1, quantile, 0.95, na.rm = TRUE)
  )

  p <- ggplot(stats, aes(x = date)) +
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "gray80", alpha = 0.5) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "gray60", alpha = 0.5) +
    geom_line(aes(y = median), color = "blue", size = 1) +
    theme_bw() +
    labs(x = "Date", y = "Value", title = "Ensemble Uncertainty")

  if (!is.null(observed)) {
    p <- p + geom_line(data = observed, aes(x = date, y = value),
                      color = "black", size = 1.2)
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

cat("COSERO Sensitivity Analysis functions loaded\n")
