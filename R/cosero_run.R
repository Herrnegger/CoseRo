# COSERO Core Run Interface
# Main COSERO execution and configuration management
# Author: COSERO R Interface
# Date: 2025-09-25

# 1 Load Libraries #####
library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(lubridate)

# 2 Configuration Management #####

# 2.1 COSERO Default Parameters #####
cosero_defaults <- data.frame(
  parameter = c(
    "PROJECTINFO", "DATAFILE", "PARAFILE", "IKL", "NCLASS", "OUTPUTTYPE",
    "STARTDATE", "ENDDATE", "SPINUP", "SC_FLAG", "RUNOFFFILE", "STATSFILE",
    "OPTFILE", "WRITERASTERS", "OUTCONTROL", "ADDFLUXCONT", "ADDFLUXFILE"
  ),
  type = c(
    "character", "character", "character", "integer", "integer", "integer",
    "date", "date", "integer", "flag", "character", "character",
    "character", "character", "integer", "flag", "character"
  ),
  required = rep(TRUE, 17),
  stringsAsFactors = FALSE
)

# 2.2 Configuration Display #####

#' Show COSERO Default Configuration Parameters
#'
#' Displays available COSERO configuration parameters and their types.
#' Useful for understanding what settings can be passed to run_cosero().
#'
#' @param parameter_name Optional. If specified, shows details for a single parameter.
#'   If NULL (default), shows all available parameters.
#'
#' @return Invisibly returns the cosero_defaults data frame
#' @export
#' @examples
#' \dontrun{
#' # Show all parameters
#' show_cosero_defaults()
#'
#' # Show details for specific parameter
#' show_cosero_defaults("STARTDATE")
#' show_cosero_defaults("OUTPUTTYPE")
#' }
show_cosero_defaults <- function(parameter_name = NULL) {
  if (!is.null(parameter_name)) {
    param_info <- cosero_defaults[cosero_defaults$parameter == parameter_name, ]
    if (nrow(param_info) == 0) {
      cat("Parameter not found. Available:", paste(cosero_defaults$parameter, collapse = ", "), "\n")
      return(invisible(NULL))
    }
    cat("Parameter:", param_info$parameter, "Type:", param_info$type, "\n")
  } else {
    cat("COSERO Configuration Parameters (", nrow(cosero_defaults), " total):\n")
    for (i in 1:nrow(cosero_defaults)) {
      param <- cosero_defaults[i, ]
      cat(sprintf("%-15s [%s]\n", param$parameter, param$type))
    }
  }
  invisible(cosero_defaults)
}

# 2.3 Configuration Validation #####
validate_cosero_defaults <- function(settings) {
  validation_results <- list(valid = TRUE, messages = character(0), settings = settings)

  for (param_name in names(settings)) {
    param_value <- settings[[param_name]]

    if (!param_name %in% cosero_defaults$parameter) {
      validation_results$messages <- c(validation_results$messages, paste("Unknown parameter:", param_name))
      next
    }

    param_info <- cosero_defaults[cosero_defaults$parameter == param_name, ]
    param_type <- param_info$type

    # Validate by type
    if (param_type == "integer") {
      if (!is.numeric(param_value) || param_value != as.integer(param_value)) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(validation_results$messages, paste(param_name, "must be integer"))
      }
      if (param_name == "OUTPUTTYPE" && !param_value %in% 0:3) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(validation_results$messages, "OUTPUTTYPE must be 0, 1, 2, or 3")
      }
    } else if (param_type == "flag") {
      if (!param_value %in% c(0, 1)) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(validation_results$messages, paste(param_name, "must be 0 or 1"))
      }
    } else if (param_type == "date") {
      # Handle both string format "2015 1 1 0 0" and vector format c(2015, 1, 1, 0, 0)
      if (is.character(param_value) && length(param_value) == 1) {
        # Convert string to vector
        date_parts <- as.numeric(strsplit(param_value, "\\s+")[[1]])
        if (length(date_parts) != 5) {
          validation_results$valid <- FALSE
          validation_results$messages <- c(validation_results$messages, paste(param_name, "must have 5 elements: year month day hour minute"))
        } else {
          # Update the settings with the converted vector for later processing
          validation_results$settings[[param_name]] <- date_parts
        }
      } else if (length(param_value) != 5) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(validation_results$messages, paste(param_name, "must have 5 elements: year month day hour minute"))
      }
    } else if (param_type == "character") {
      if (!is.character(param_value) || nchar(param_value) == 0) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(validation_results$messages, paste(param_name, "must be non-empty character"))
      }
    }
  }

  # Cross-parameter validation
  if ("STARTDATE" %in% names(validation_results$settings) && "ENDDATE" %in% names(validation_results$settings)) {
    start_date <- validation_results$settings$STARTDATE
    end_date <- validation_results$settings$ENDDATE
    if (length(start_date) == 5 && length(end_date) == 5) {
      start_num <- start_date[1] * 10000 + start_date[2] * 100 + start_date[3]
      end_num <- end_date[1] * 10000 + end_date[2] * 100 + end_date[3]
      if (start_num >= end_num) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(validation_results$messages, "ENDDATE must be after STARTDATE")
      }
    }
  }

  return(validation_results)
}

# 3 File Management #####

# 3.1 Defaults File Operations #####
get_default_cosero_values <- function() {
  list(
    PROJECTINFO = "COSERO_Project",
    DATAFILE = "data.txt",
    PARAFILE = "para.txt",
    IKL = 1,
    NCLASS = 1,
    OUTPUTTYPE = 1,
    STARTDATE = "2010 1 1 0 0",
    ENDDATE = "2015 12 31 23 59",
    SPINUP = 365,
    SC_FLAG = 0,
    RUNOFFFILE = "runoff.txt",
    STATSFILE = "statistics.txt",
    OPTFILE = "opt.txt",
    OUTCONTROL = 1,
    ADDFLUXCONT = 0,
    ADDFLUXFILE = "addflux.txt"
  )
}

modify_defaults <- function(defaults_file, settings, quiet = FALSE) {
  if (!file.exists(defaults_file)) {
    create_default_defaults(defaults_file, quiet)
  }

  lines <- readLines(defaults_file)

  # Filter out NULL values (user wants to keep existing defaults)
  settings <- settings[!sapply(settings, is.null)]

  for (param_name in names(settings)) {
    param_value <- settings[[param_name]]

    # Find lines that start with the parameter name (may have description after)
    param_line <- grep(paste0("^", param_name, "\\b"), lines)

    if (length(param_line) > 0) {
      # Parameter exists - update the value on the next line
      value_line <- param_line[1] + 1
      if (value_line <= length(lines)) {
        if (length(param_value) == 1) {
          lines[value_line] <- as.character(param_value)
        } else {
          lines[value_line] <- paste(param_value, collapse = " ")
        }
      } else {
        # Parameter name is last line - append value
        lines <- c(lines, as.character(param_value))
      }
    } else {
      # Parameter doesn't exist - add it
      if (length(param_value) == 1) {
        lines <- c(lines, param_name, as.character(param_value))
      } else {
        lines <- c(lines, param_name, paste(param_value, collapse = " "))
      }
    }
  }

  writeLines(lines, defaults_file)
  if (!quiet) cat("Updated defaults.txt with", length(settings), "parameters\n")
}

create_default_defaults <- function(defaults_file, quiet = FALSE) {
  default_values <- get_default_cosero_values()
  content <- character(0)
  for (param_name in names(default_values)) {
    content <- c(content, param_name, as.character(default_values[[param_name]]))
  }
  writeLines(content, defaults_file)
  if (!quiet) cat("Created default defaults.txt at:", defaults_file, "\n")
}

#' Read COSERO Defaults File
#'
#' Reads configuration settings from a COSERO defaults.txt file.
#' Parses the file format and returns settings as a named list.
#'
#' @param defaults_file Path to defaults.txt file
#'
#' @return Named list with configuration settings. Common items include:
#'   \item{PROJECTINFO}{Project name/description}
#'   \item{DATAFILE}{Input data filename}
#'   \item{PARAFILE}{Parameter filename}
#'   \item{STARTDATE}{Start date as character vector c(YYYY, MM, DD, HH, MM)}
#'   \item{ENDDATE}{End date as character vector c(YYYY, MM, DD, HH, MM)}
#'   \item{SPINUP}{Spin-up period in timesteps}
#'   \item{OUTPUTTYPE}{Output detail level (0-3)}
#'   \item{...}{Other COSERO settings}
#'
#' @export
#' @examples
#' \dontrun{
#' # Read defaults file
#' settings <- read_defaults("path/to/input/defaults.txt")
#'
#' # Access specific settings
#' start_date <- settings$STARTDATE
#' spinup <- settings$SPINUP
#'
#' # Use in run_cosero
#' result <- run_cosero("path/to/project",
#'                      defaults_settings = settings)
#' }
read_defaults <- function(defaults_file) {
  if (!file.exists(defaults_file)) {
    stop("defaults.txt file not found: ", defaults_file)
  }

  lines <- readLines(defaults_file)
  settings <- list()

  # Get valid parameter names from cosero_defaults
  valid_params <- cosero_defaults$parameter

  i <- 1
  while (i <= length(lines)) {
    line <- trimws(lines[i])

    # Skip empty lines
    if (nchar(line) == 0) {
      i <- i + 1
      next
    }

    # Extract potential parameter name (first word, before any spaces or parentheses)
    potential_param <- regmatches(line, regexpr("^[A-Z_]+", line))

    # Check if this line contains a valid parameter keyword
    if (length(potential_param) > 0 && potential_param[1] %in% valid_params) {
      param_name <- potential_param[1]

      # Look for the value in the next non-empty line
      j <- i + 1
      while (j <= length(lines)) {
        next_line <- trimws(lines[j])

        # Skip empty lines
        if (nchar(next_line) == 0) {
          j <- j + 1
          next
        }

        # Check if we've hit another valid parameter keyword
        next_potential_param <- regmatches(next_line, regexpr("^[A-Z_]+", next_line))
        if (length(next_potential_param) > 0 && next_potential_param[1] %in% valid_params) {
          break
        }

        # This should be our value line
        if (grepl("\\s", next_line)) {
          # Multiple space-separated values
          param_value <- strsplit(next_line, "\\s+")[[1]]
        } else {
          # Single value
          param_value <- next_line
        }

        settings[[param_name]] <- param_value
        i <- j  # Continue from this line
        break
      }

      if (j > length(lines)) {
        # No value found, continue to next line
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  return(settings)
}

# 3.2 Project Setup #####
setup_cosero_project <- function(project_path, defaults_settings = NULL, timestamp, quiet = FALSE) {
  input_dir <- file.path(project_path, "input")
  output_dir <- file.path(project_path, "output")

  if (!dir.exists(input_dir)) dir.create(input_dir, recursive = TRUE)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Create backup folder for parameter files
  backup_dir <- file.path(input_dir, "parameterfile_backup")
  if (!dir.exists(backup_dir)) dir.create(backup_dir, recursive = TRUE)

  defaults_file <- file.path(input_dir, "defaults.txt")
  current_settings <- NULL

  if (file.exists(defaults_file)) {
    current_settings <- read_defaults(defaults_file)
    if (!is.null(defaults_settings)) {
      backup_file <- file.path(backup_dir, paste0("defaults_backup_", timestamp, ".txt"))
      file.copy(defaults_file, backup_file)
      if (!quiet) cat("Backed up defaults.txt to:", backup_file, "\n")
      modify_defaults(defaults_file, defaults_settings, quiet)
      # Save the settings used for this run
      used_file <- file.path(backup_dir, paste0("defaults_used_", timestamp, ".txt"))
      file.copy(defaults_file, used_file)
      if (!quiet) cat("Saved used settings to:", used_file, "\n")
    }
  } else {
    if (!is.null(defaults_settings)) {
      modify_defaults(defaults_file, defaults_settings, quiet)
    } else {
      create_default_defaults(defaults_file, quiet)
    }
  }

  return(list(
    input_dir = input_dir,
    output_dir = output_dir,
    defaults_file = defaults_file,
    current_settings = current_settings,
    backup_dir = backup_dir
  ))
}

# 4 COSERO Execution #####

# 4.1 Commands File Creation #####
create_commands_file <- function(commands_file, auto_answers, quiet = FALSE) {
  commands <- c(
    auto_answers$simulate_option,
    auto_answers$period_correct,
    auto_answers$run_type,
    auto_answers$statevar_source,
    auto_answers$tmmon_option,
    auto_answers$stats_option,
    "Pause"
  )

  writeLines(as.character(commands), commands_file)
  if (!quiet) cat("Created commands file with", length(commands), "commands\n")
}

# 4.2 Model Execution #####
execute_cosero <- function(project_path, exe_name, commands_file, capture_output, show_output) {
  old_wd <- getwd()
  setwd(project_path)

  tryCatch({
    if (file.exists(commands_file)) {
      batch_file <- "run_cosero_temp.bat"
      batch_content <- paste0(exe_name, " < ", basename(commands_file))
      writeLines(batch_content, batch_file)

      # Capture output to check for errors
      output <- system(paste0('"', batch_file, '"'),
                      intern = TRUE,
                      ignore.stderr = FALSE,
                      wait = TRUE)

      result_code <- attr(output, "status")
      if (is.null(result_code)) result_code <- 0

      # Check for error messages in output
      error_patterns <- c("ERROR:", "Error:", "error:", "could not find", "terminated")
      has_error <- any(sapply(error_patterns, function(p) {
        tryCatch(any(grepl(p, output, ignore.case = TRUE, useBytes = TRUE)),
                 error = function(e) FALSE)
      }))

      if (show_output) {
        cat(paste(output, collapse = "\n"), "\n")
      }

      return(list(exit_code = result_code, output = output, has_error = has_error))
    } else {
      output <- system2(exe_name,
                       stdout = TRUE,
                       stderr = TRUE,
                       wait = TRUE)

      result_code <- attr(output, "status")
      if (is.null(result_code)) result_code <- 0

      error_patterns <- c("ERROR:", "Error:", "error:", "could not find", "terminated")
      has_error <- any(sapply(error_patterns, function(p) {
        tryCatch(any(grepl(p, output, ignore.case = TRUE, useBytes = TRUE)),
                 error = function(e) FALSE)
      }))

      if (show_output) {
        cat(paste(output, collapse = "\n"), "\n")
      }

      return(list(exit_code = result_code, output = output, has_error = has_error))
    }
  }, finally = {
    setwd(old_wd)
  })
}

# 5 Main COSERO Interface #####

#' Run COSERO Hydrological Model
#'
#' Main interface for executing COSERO simulation with custom settings
#'
#' @param project_path Path to COSERO project directory (must contain COSERO.exe)
#' @param defaults_settings Named list of configuration settings to override in defaults.txt
#'        (e.g., STARTDATE, ENDDATE, SPINUP, OUTPUTTYPE, etc.)
#' @param exe_name Name of COSERO executable (default: "COSERO.exe")
#' @param timeout Maximum execution time in seconds (legacy parameter, currently not used)
#' @param capture_output Whether to capture and parse model output (default: TRUE)
#' @param read_outputs Whether to read output files after successful execution (default: TRUE)
#' @param quiet Suppress console output except for runtime and errors (default: FALSE)
#' @param statevar_source State variable source:
#'        1 = read from parameter file (default)
#'        2 = read from statevar.dmp file (warm start from previous run)
#' @param tmmon_option Monthly temperature option:
#'        1 = use TMMon from parameter file (default)
#'        2 = calculate TMMon from input data
#' @param auto_answers Custom automation answers for COSERO prompts (advanced users only)
#'        Overrides statevar_source and tmmon_option if provided
#'
#' @return List containing:
#'   \item{exit_code}{Integer exit code (0 = success)}
#'   \item{success}{Logical indicating simulation success}
#'   \item{has_error}{Logical indicating if error messages were detected}
#'   \item{error_message}{String with error message (NULL if no error)}
#'   \item{output_data}{Parsed output data (if read_outputs = TRUE and successful)}
#'   \item{project_path}{Original project path}
#'   \item{execution_time}{POSIXct timestamp of execution}
#'   \item{runtime_seconds}{Numeric runtime in seconds}
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic run with defaults
#' result <- run_cosero("path/to/project")
#'
#' # Run with custom date range
#' result <- run_cosero("path/to/project",
#'                     defaults_settings = list(
#'                       STARTDATE = "2015 1 1 0 0",
#'                       ENDDATE = "2015 12 31 23 59",
#'                       SPINUP = 365
#'                     ))
#'
#' # Run using existing state variables from previous run (warm start)
#' result <- run_cosero("path/to/project",
#'                     statevar_source = 2)
#'
#' # Run with calculated monthly temperature
#' result <- run_cosero("path/to/project",
#'                     tmmon_option = 2)
#'
#' # Combine both options
#' result <- run_cosero("path/to/project",
#'                     statevar_source = 2,
#'                     tmmon_option = 2)
#' }
#'
run_cosero <- function(project_path,
                      defaults_settings = NULL,
                      exe_name = "COSERO.exe",
                      timeout = 3600,
                      capture_output = TRUE,
                      read_outputs = TRUE,
                      quiet = FALSE,
                      statevar_source = 1,
                      tmmon_option = 1,
                      auto_answers = NULL) {

  # Validate inputs
  if (!dir.exists(project_path)) stop("Project path does not exist: ", project_path)
  if (!file.exists(file.path(project_path, exe_name))) stop("COSERO executable not found: ", exe_name)

  # Validate statevar_source parameter
  if (!statevar_source %in% c(1, 2)) {
    stop("statevar_source must be 1 (parameter file) or 2 (statevar.dmp file)")
  }

  # Validate tmmon_option parameter
  if (!tmmon_option %in% c(1, 2)) {
    stop("tmmon_option must be 1 (from parameter file) or 2 (calculate from data)")
  }

  # Check if statevar.dmp exists when needed
  if (statevar_source == 2) {
    statevar_file <- file.path(project_path, "input", "statevar.dmp")
    if (!file.exists(statevar_file)) {
      warning("statevar_source=2 specified but statevar.dmp not found at: ", statevar_file)
      if (!quiet) cat("Note: COSERO will use default state variables if statevar.dmp is missing\n")
    } else {
      if (!quiet) cat("Using state variables from:", statevar_file, "\n")
    }
  }

  # Start runtime tracking
  start_time <- Sys.time()

  # Extract run parameters from defaults_settings if provided
  if (!is.null(defaults_settings)) {
    if ("statevar_source" %in% names(defaults_settings)) {
      statevar_source <- defaults_settings$statevar_source
      defaults_settings$statevar_source <- NULL  # Remove from settings list
      if (!quiet) cat("Using statevar_source from settings:", statevar_source, "\n")
    }
    if ("tmmon_option" %in% names(defaults_settings)) {
      tmmon_option <- defaults_settings$tmmon_option
      defaults_settings$tmmon_option <- NULL  # Remove from settings list
      if (!quiet) cat("Using tmmon_option from settings:", tmmon_option, "\n")
    }
  }

  # Process settings (convert string dates to vectors if needed)
  if (!is.null(defaults_settings) && length(defaults_settings) > 0) {
    validation <- validate_cosero_defaults(defaults_settings)
    if (length(validation$messages) > 0) sapply(validation$messages, cat, "\n")
    if (!validation$valid) {
      stop("Invalid configuration settings provided")
    }
    # Use the processed settings (with converted dates)
    defaults_settings <- validation$settings
  }

  # Setup project
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  project_setup <- setup_cosero_project(project_path, defaults_settings, timestamp, quiet)

  # Build auto_answers from parameters
  if (is.null(auto_answers)) {
    # Use function parameters to build auto_answers
    auto_answers <- list(
      simulate_option = 1,
      period_correct = "y",
      run_type = 1,
      statevar_source = statevar_source,  # Use parameter value
      tmmon_option = tmmon_option,        # Use parameter value
      stats_option = 0
    )
    if (!quiet) cat("Run options: statevar_source =", statevar_source, ", tmmon_option =", tmmon_option, "\n")
  } else {
    # User provided custom auto_answers - override with explicit parameters
    if (!is.null(auto_answers$statevar_source) && auto_answers$statevar_source != statevar_source) {
      cat("Note: Overriding auto_answers$statevar_source with function parameter value:", statevar_source, "\n")
    }
    if (!is.null(auto_answers$tmmon_option) && auto_answers$tmmon_option != tmmon_option) {
      cat("Note: Overriding auto_answers$tmmon_option with function parameter value:", tmmon_option, "\n")
    }
    auto_answers$statevar_source <- statevar_source
    auto_answers$tmmon_option <- tmmon_option
  }

  # Execute COSERO
  commands_file <- file.path(project_path, "cosero_commands.txt")
  create_commands_file(commands_file, auto_answers, quiet)

  if (!quiet) cat("Starting COSERO simulation...\n")

  tryCatch({
    show_output <- !quiet && capture_output
    exec_result <- execute_cosero(project_path, exe_name, commands_file, capture_output, show_output)

    # Calculate runtime
    end_time <- Sys.time()
    runtime_seconds <- as.numeric(difftime(end_time, start_time, units = "secs"))

    # Check for errors
    if (exec_result$has_error || exec_result$exit_code != 0) {
      cat("\n!!! COSERO execution failed !!!\n")
      cat(sprintf("Runtime: %.2f seconds (%.2f minutes)\n", runtime_seconds, runtime_seconds/60))
      if (exec_result$has_error) {
        cat("Error detected in output:\n")
        error_lines <- exec_result$output[sapply(exec_result$output, function(line) {
          tryCatch(grepl("ERROR:|Error:|error:|could not find|terminated", line,
                        ignore.case = TRUE, useBytes = TRUE),
                  error = function(e) FALSE)
        })]
        cat(paste(error_lines, collapse = "\n"), "\n")
      }

      return(list(
        exit_code = exec_result$exit_code,
        success = FALSE,
        has_error = exec_result$has_error,
        error_message = if(exec_result$has_error) paste(error_lines, collapse = "; ") else NULL,
        output_data = NULL,
        project_path = project_path,
        execution_time = end_time,
        runtime_seconds = runtime_seconds
      ))
    }

    # Read outputs if successful and requested
    output_data <- NULL
    if (read_outputs && exec_result$exit_code == 0 && !exec_result$has_error) {
      if (!quiet) cat("Reading output files...\n")
      # Load readers if not already loaded
      if (!exists("read_cosero_output")) {
        source("02_cosero_readers.R")
      }
      tryCatch({
        output_data <- read_cosero_output(project_setup$output_dir, project_setup$defaults_file, quiet = quiet)
      }, error = function(e) {
        warning("Could not read output files: ", e$message)
      })
    } else if (!read_outputs && !quiet) {
      cat("Skipping output file reading (read_outputs = FALSE)\n")
    }

    if (!quiet) {
      cat("\nCOSERO simulation completed successfully!\n")
      cat(sprintf("Runtime: %.2f seconds (%.2f minutes)\n", runtime_seconds, runtime_seconds/60))
    }

    return(list(
      exit_code = exec_result$exit_code,
      success = TRUE,
      has_error = FALSE,
      error_message = NULL,
      output_data = output_data,
      defaults_settings = defaults_settings,
      project_path = project_path,
      execution_time = end_time,
      runtime_seconds = runtime_seconds
    ))

  }, error = function(e) {
    stop("Error running COSERO: ", e$message)
  })
}

# 6 Utility Functions #####
# Note: Direct list format is now preferred for settings
# Example: list(STARTDATE = "2015 1 1 0 0", ENDDATE = "2015 12 31 23 59", SPINUP = 365)

cat("COSERO Core Run Interface loaded\n")
