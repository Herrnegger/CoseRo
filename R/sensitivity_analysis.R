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
library(pbapply)  # For progress bars in parallel

# Note: COSERO functions loaded from package
# (run_cosero, read_cosero_output, read_defaults, etc.)

# 2 Parameter Setup #####

#' Create Custom Parameter Bounds
#'
#' Create parameter bounds manually without using CSV file
#'
#' @param parameters Vector of parameter names
#' @param min Vector of minimum values (same length as parameters)
#' @param max Vector of maximum values (same length as parameters)
#' @param modification_type Vector of modification types: "absval", "relchg", or "abschg" (same length as parameters)
#' @param default Vector of default values (optional, same length as parameters)
#' @param description Vector of parameter descriptions (optional)
#' @param category Vector of parameter categories (optional)
#' @return Tibble with parameter bounds in the same format as CSV file
#' @examples
#' # Create custom bounds for 3 parameters
#' custom_bounds <- create_custom_bounds(
#'   parameters = c("BETA", "CTMAX", "TAB1"),
#'   min = c(0.1, 2, 0.1),
#'   max = c(10, 12, 5),
#'   modification_type = c("absval", "absval", "relchg"),
#'   default = c(4.5, 5, 1)
#' )
create_custom_bounds <- function(parameters,
                                  min,
                                  max,
                                  modification_type,
                                  default = NULL,
                                  description = NULL,
                                  category = NULL) {

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
  valid_types <- c("absval", "relchg", "abschg")
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

  # Set defaults for optional parameters
  if (is.null(default)) {
    default <- (min + max) / 2  # Use midpoint as default
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
    category = category
  )

  return(bounds)
}

#' Load COSERO Parameter Bounds
#'
#' Load parameter bounds from CSV file or use custom bounds.
#' Custom bounds take precedence over CSV file.
#'
#' @param bounds_file Path to parameter bounds CSV file (NULL = use package default)
#' @param parameters Vector of parameter names to include (NULL = all)
#' @param custom_bounds Custom parameter bounds tibble from create_custom_bounds() (optional)
#' @param custom_min Vector of minimum values for custom bounds (optional)
#' @param custom_max Vector of maximum values for custom bounds (optional)
#' @param custom_modification_type Vector of modification types for custom bounds (optional)
#' @return Tibble with parameter bounds
#' @examples
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
#'   modification_type = c("absval", "absval")
#' )
#' bounds <- load_parameter_bounds(custom_bounds = custom)
#'
#' # Use custom bounds with direct parameters
#' bounds <- load_parameter_bounds(
#'   parameters = c("BETA", "CTMAX"),
#'   custom_min = c(0.5, 3),
#'   custom_max = c(8, 10),
#'   custom_modification_type = c("absval", "absval")
#' )
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

  cat("Loading parameter bounds from CSV (", nrow(bounds), " parameters)\n")
  return(bounds)
}

#' Create Parameter Bounds Matrix for Sobol Sampling
#'
#' @param par_bounds Tibble from load_parameter_bounds()
#' @return Named tibble with min/max rows for sobol_matrices()
create_sobol_bounds <- function(par_bounds) {
  bounds_matrix <- tibble::tibble(
    min = par_bounds$min,
    max = par_bounds$max
  )

  # Transpose to have parameters as columns
  bounds_t <- as.data.frame(t(bounds_matrix))
  colnames(bounds_t) <- par_bounds$parameter

  return(as_tibble(bounds_t))
}

# 3 Sobol Sampling #####

#' Generate Sobol Parameter Sets
#'
#' @param par_bounds Parameter bounds from create_sobol_bounds()
#' @param n Sample size (total runs = n * (2 + n_params))
#' @param order Sobol order ("first" or "second")
#' @return List with sobol_matrix (raw) and parameter_sets (scaled)
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

#' Run COSERO Ensemble with Parameter Sets
#'
#' @param project_path Path to COSERO project
#' @param parameter_sets Tibble with parameter combinations (from generate_sobol_samples)
#' @param par_bounds Parameter bounds table with modification types
#' @param base_settings List of base COSERO settings
#' @param par_file Path to parameter file to modify (NULL = read from defaults.txt)
#' @param quiet Suppress output
#' @return List with results and parameters
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

  if (!quiet) cat("Running", n_runs, "COSERO simulations...\n")

  # Backup original parameter file
  backup_file <- paste0(par_file, ".backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  file.copy(par_file, backup_file)

  # Read original parameter values once
  original_values <- read_parameter_file(par_file, names(parameter_sets))

  start_time <- Sys.time()

  for (i in 1:n_runs) {
    if (!quiet && i %% 10 == 0) {
      cat("  Run", i, "of", n_runs, "\n")
    }

    # Restore original file for each run
    file.copy(backup_file, par_file, overwrite = TRUE)

    # Modify parameters in para.txt
    modify_parameter_file(par_file, parameter_sets[i, ], par_bounds, original_values)

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
  }

  # Restore original parameter file
  file.copy(backup_file, par_file, overwrite = TRUE)

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  if (!quiet) cat("Ensemble completed in", round(runtime, 2), "minutes\n")

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
    param_idx <- grep(paste0("^", param_name, "\\b"), lines)

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

    # Get modification type
    mod_type <- par_bounds$modification_type[par_bounds$parameter == param_name]

    # Calculate final value based on modification type
    if (mod_type == "absval") {
      # Replace with absolute value
      final_value <- sampled_value
    } else if (mod_type == "relchg") {
      # Multiply original by sampled_value (direct multiplier)
      original <- original_values[[param_name]]
      if (!is.null(original)) {
        final_value <- original * sampled_value
      } else {
        warning("Original value not found for ", param_name, ", using sampled value")
        final_value <- sampled_value
      }
    } else if (mod_type == "abschg") {
      # Add sampled_value to original value
      original <- original_values[[param_name]]
      if (!is.null(original)) {
        final_value <- original + sampled_value
      } else {
        warning("Original value not found for ", param_name, ", using sampled value")
        final_value <- sampled_value
      }
    } else {
      # Default: use sampled value
      final_value <- sampled_value
    }

    # Find parameter line and update
    param_idx <- grep(paste0("^", param_name, "\\b"), lines)

    if (length(param_idx) > 0) {
      value_idx <- param_idx[1] + 1
      if (value_idx <= length(lines)) {
        lines[value_idx] <- as.character(final_value)
      }
    }
  }

  writeLines(lines, par_file)
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

  if (!quiet) {
    cat("Running", n_runs, "COSERO simulations in parallel\n")
    cat("Using", n_cores, "cores\n")
  }

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

  # Read original parameter values
  original_values <- read_parameter_file(par_file, names(parameter_sets))

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
                      "modify_parameter_file", "read_parameter_file"),
                envir = environment())

  # Load required libraries and package on each worker
  clusterEvalQ(cl, {
    library(dplyr)
    library(readr)
    library(tibble)
    library(stringr)
    library(lubridate)
    library(data.table)
    library(COSERO)  # Load package functions
  })

  # Parallel worker function
  worker_function <- function(run_ids) {
    worker_results <- vector("list", length(run_ids))

    # Create unique temp project for this worker
    worker_id <- Sys.getpid()
    worker_project_full <- file.path(temp_dir, paste0("worker_", worker_id))

    # Copy entire project to temp location
    tryCatch({
      # Create the temp worker directory
      dir.create(worker_project_full, recursive = TRUE, showWarnings = FALSE)

      # Copy all contents from project_path to worker_project_full
      project_files <- list.files(project_path, full.names = TRUE, all.files = FALSE, recursive = FALSE)
      file.copy(from = project_files, to = worker_project_full, recursive = TRUE, overwrite = TRUE)
    }, error = function(e) {
      stop("Failed to copy project to temp location: ", e$message)
    })

    # Get worker parameter file path
    worker_par_file <- file.path(worker_project_full, "input", par_filename)

    # Check if parameter file exists
    if (!file.exists(worker_par_file)) {
      stop("Parameter file not found in worker project: ", worker_par_file)
    }

    # Backup original parameter file
    backup_file <- paste0(worker_par_file, ".backup")
    file.copy(worker_par_file, backup_file, overwrite = TRUE)

    for (idx in seq_along(run_ids)) {
      run_i <- run_ids[idx]

      tryCatch({
        # Restore original parameter file
        file.copy(backup_file, worker_par_file, overwrite = TRUE)

        # Modify parameters
        modify_parameter_file(worker_par_file, parameter_sets[run_i, ],
                             par_bounds, original_values)

        # Run COSERO
        result <- run_cosero(
          project_path = worker_project_full,
          defaults_settings = base_settings,
          quiet = TRUE,
          read_outputs = TRUE
        )
        worker_results[[idx]] <- result

      }, error = function(e) {
        # Provide detailed error information
        error_msg <- paste0(
          "Run ", run_i, " failed: ", e$message,
          "\nWorker project: ", worker_project_full,
          "\nParameter file: ", worker_par_file,
          "\nFile exists: ", file.exists(worker_par_file)
        )
        warning(error_msg)
        worker_results[[idx]] <- list(success = FALSE, error = error_msg, run_id = run_i)
      })
    }

    # Cleanup temp project
    unlink(worker_project_full, recursive = TRUE)

    return(worker_results)
  }

  # Run parallel jobs with progress bar using pbapply
  if (!quiet) {
    cat(sprintf("\n[%s] Starting %d simulations on %d cores\n",
                format(Sys.time(), "%H:%M:%S"), n_runs, n_cores))
    cat("Progress: [==>] indicates completed chunks\n\n")

    # Use pblapply for progress bar
    results_chunks <- pbapply::pblapply(chunks, worker_function, cl = cl)

    cat("\n")
  } else {
    results_chunks <- parallel::parLapplyLB(cl, chunks, worker_function)
  }

  # Flatten results
  results <- unlist(results_chunks, recursive = FALSE)

  # Cleanup
  stopCluster(cl)
  on.exit()
  unlink(temp_dir, recursive = TRUE)

  runtime <- difftime(Sys.time(), start_time, units = "mins")
  if (!quiet) {
    cat("\nEnsemble completed in", round(runtime, 2), "minutes\n")
    cat("Average time per run:", round(runtime * 60 / n_runs, 1), "seconds\n")
  }

  return(list(
    results = results,
    parameter_sets = parameter_sets,
    runtime_minutes = as.numeric(runtime),
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

#' Calculate Performance Metrics for Ensemble
#'
#' @param ensemble_results Results from run_cosero_ensemble()
#' @param observed Observed data vector
#' @param metric Performance metric ("NSE", "KGE", "RMSE")
#' @return Vector of metric values
calculate_ensemble_metrics <- function(ensemble_results, observed, metric = "KGE") {

  n_runs <- length(ensemble_results$results)
  metrics <- numeric(n_runs)

  for (i in 1:n_runs) {
    result <- ensemble_results$results[[i]]

    if (result$success && !is.null(result$output_data)) {
      simulated <- result$output_data$runoff$value

      # Match length with observed
      n_obs <- length(observed)
      if (length(simulated) > n_obs) {
        simulated <- simulated[1:n_obs]
      }

      # Calculate metric
      metrics[i] <- tryCatch({
        if (metric == "KGE") {
          hydroGOF::KGE(simulated, observed)
        } else if (metric == "NSE") {
          hydroGOF::NSE(simulated, observed)
        } else if (metric == "RMSE") {
          hydroGOF::rmse(simulated, observed)
        } else {
          stop("Unknown metric: ", metric)
        }
      }, error = function(e) {
        NA
      })
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
#' @param Y Output vector (length = n_runs)
#' @param sobol_samples Sobol samples from generate_sobol_samples()
#' @param boot Bootstrap resamples (default: 100)
#' @return Sobol indices object
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
