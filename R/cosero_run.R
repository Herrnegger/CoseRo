# COSERO Model Execution
# Main interface for running COSERO simulations
#
# Configuration management: see cosero_config.R
# Metrics extraction: see cosero_metrics.R

#' @importFrom dplyr filter mutate select arrange group_by
#'   summarise bind_rows
#' @importFrom lubridate ymd_hm year month day hour minute
#' @importFrom tibble tibble as_tibble
NULL

# ============================================================================
# Commands File Creation
# ============================================================================

#' @keywords internal
create_commands_file <- function(commands_file,
                                 statevar_source,
                                 tmmon_option) {
  commands <- c(
    1,                  # simulate_option
    "y",                # period_correct
    1,                  # run_type
    statevar_source,    # 1=cold start, 2=warm start
    tmmon_option,       # 1=from file, 2=calculate
    0,                  # stats_option
    "Pause"
  )
  writeLines(as.character(commands), commands_file)
}

# ============================================================================
# Model Execution
# ============================================================================

#' @keywords internal
execute_cosero <- function(project_path, exe_name,
                           commands_file, capture_output,
                           show_output) {
  old_wd <- getwd()
  setwd(project_path)

  tryCatch({
    if (file.exists(commands_file)) {
      batch_file <- "run_cosero_temp.bat"
      batch_content <- paste0(
        exe_name, " < ", basename(commands_file)
      )
      writeLines(batch_content, batch_file)

      output <- system(
        paste0('"', batch_file, '"'),
        intern = TRUE,
        ignore.stderr = FALSE,
        wait = TRUE
      )

      result_code <- attr(output, "status")
      if (is.null(result_code)) result_code <- 0

      has_error <- detect_cosero_errors(output)

      if (show_output) {
        cat(paste(output, collapse = "\n"), "\n")
      }

      list(
        exit_code = result_code,
        output = output,
        has_error = has_error
      )
    } else {
      output <- system2(
        exe_name,
        stdout = TRUE,
        stderr = TRUE,
        wait = TRUE
      )

      result_code <- attr(output, "status")
      if (is.null(result_code)) result_code <- 0

      has_error <- detect_cosero_errors(output)

      if (show_output) {
        cat(paste(output, collapse = "\n"), "\n")
      }

      list(
        exit_code = result_code,
        output = output,
        has_error = has_error
      )
    }
  }, finally = {
    setwd(old_wd)
  })
}

#' Check COSERO output for error patterns
#' @keywords internal
detect_cosero_errors <- function(output) {
  error_patterns <- c(
    "ERROR:", "Error:", "error:",
    "could not find", "terminated"
  )
  any(sapply(error_patterns, function(p) {
    tryCatch(
      any(grepl(p, output,
                ignore.case = TRUE, useBytes = TRUE)),
      error = function(e) FALSE
    )
  }))
}

#' Find error lines in COSERO output
#' @keywords internal
extract_error_lines <- function(output) {
  output[sapply(output, function(line) {
    tryCatch(
      grepl("ERROR:|Error:|error:|could not find|terminated",
            line, ignore.case = TRUE, useBytes = TRUE),
      error = function(e) FALSE
    )
  })]
}

# ============================================================================
# Console Output Helpers
# ============================================================================

#' Print a separator line
#' @keywords internal
print_header <- function(title) {
  line <- paste0(
    strrep("\u2500", 2), " ", title, " ",
    strrep("\u2500", max(1, 50 - nchar(title) - 4))
  )
  cat("\n", line, "\n", sep = "")
}

#' Format COSERO date vector as readable string
#' @keywords internal
format_period_string <- function(settings) {
  format_one <- function(date_val) {
    if (is.numeric(date_val) && length(date_val) == 5) {
      sprintf("%04d-%02d-%02d",
              date_val[1], date_val[2], date_val[3])
    } else if (is.character(date_val)) {
      parts <- as.numeric(strsplit(date_val, "\\s+")[[1]])
      if (length(parts) >= 3) {
        sprintf("%04d-%02d-%02d",
                parts[1], parts[2], parts[3])
      } else {
        date_val
      }
    } else {
      as.character(date_val)
    }
  }

  start <- if (!is.null(settings$STARTDATE)) {
    format_one(settings$STARTDATE)
  } else {
    "?"
  }
  end <- if (!is.null(settings$ENDDATE)) {
    format_one(settings$ENDDATE)
  } else {
    "?"
  }
  spinup <- if (!is.null(settings$SPINUP)) {
    settings$SPINUP
  } else {
    "?"
  }

  sprintf("%s to %s | Spinup: %s", start, end, spinup)
}

#' Print the run header block
#' @keywords internal
print_run_header <- function(project_path,
                             defaults_settings,
                             statevar_source,
                             tmmon_option) {
  print_header("run_cosero")

  cat("  Project: ", project_path, "\n", sep = "")

  # Period info
  if (!is.null(defaults_settings)) {
    cat("  Period:  ",
        format_period_string(defaults_settings),
        "\n", sep = "")
  }

  # Start mode
  start_mode <- if (statevar_source == 1) {
    "cold start"
  } else {
    "warm start (statevar.dmp)"
  }
  tmmon_mode <- if (tmmon_option == 1) {
    "TMMon from file"
  } else {
    "TMMon calculated"
  }
  cat("  Start:   ", start_mode, " | ", tmmon_mode,
      "\n", sep = "")

  # Changed settings
  if (!is.null(defaults_settings) &&
      length(defaults_settings) > 0) {
    param_names <- paste(names(defaults_settings),
                         collapse = ", ")
    cat("  Changed: ", param_names, "\n", sep = "")
  }
}

#' Print results summary with NSE/KGE table
#' @keywords internal
print_run_results <- function(output_data,
                              runtime_seconds) {
  print_header("Results")

  if (!is.null(output_data)) {
    # Compact file summary
    files_read <- character(0)

    if (!is.null(output_data$runoff)) {
      n_sb <- attr(output_data$runoff, "n_subbasins")
      if (is.null(n_sb)) {
        cols <- grep("QOBS_", colnames(output_data$runoff))
        n_sb <- length(cols)
      }
      files_read <- c(files_read,
                       sprintf("runoff (%s sb)", n_sb))
    }
    for (f in c("precipitation", "runoff_components",
                "water_balance")) {
      if (!is.null(output_data[[f]])) {
        short <- switch(f,
          precipitation = "prec",
          runoff_components = "plus",
          water_balance = "plus1"
        )
        files_read <- c(files_read, short)
      }
    }
    if (!is.null(output_data$topology)) {
      files_read <- c(files_read, "topology")
    }

    if (length(files_read) > 0) {
      cat("  Read: ",
          paste(files_read, collapse = ", "),
          "\n", sep = "")
    }

    # Second line: statistics and parameters
    files2 <- character(0)
    if (!is.null(output_data$statistics)) {
      n_sb <- nrow(output_data$statistics)
      files2 <- c(files2,
                   sprintf("statistics (%s sb)", n_sb))
    }
    if (!is.null(output_data$parameters)) {
      n_zones <- nrow(output_data$parameters)
      files2 <- c(files2,
                   sprintf("parameters (%s zones)", n_zones))
    }
    if (length(files2) > 0) {
      cat("  Read: ",
          paste(files2, collapse = ", "),
          "\n", sep = "")
    }

    # NSE/KGE/BETA table from statistics
    stats <- output_data$statistics
    if (!is.null(stats) && "NSE" %in% colnames(stats) &&
        "KGE" %in% colnames(stats) &&
        "BETA" %in% colnames(stats)) {
      cat("\n  Performance (NSE / KGE / BETA):\n")
      for (i in seq_len(nrow(stats))) {
        cat(sprintf(
          "    Subbasin %s:  NSE = %.4f  |  KGE = %.4f  |  BETA = %.4f\n",
          stats$sb[i],
          as.numeric(stats$NSE[i]),
          as.numeric(stats$KGE[i]),
          as.numeric(stats$BETA[i])))
      }
    } else if (!is.null(stats) &&
               "NSE" %in% colnames(stats) &&
               "KGE" %in% colnames(stats)) {
      cat("\n  Performance (NSE / KGE):\n")
      for (i in seq_len(nrow(stats))) {
        cat(sprintf(
          "    Subbasin %s:  NSE = %.4f  |  KGE = %.4f\n",
          stats$sb[i],
          as.numeric(stats$NSE[i]),
          as.numeric(stats$KGE[i])))
      }
    }
  }

  cat(sprintf("\n  Completed in %.2f seconds\n",
              runtime_seconds))
}

# ============================================================================
# Main COSERO Interface
# ============================================================================

#' Run COSERO Hydrological Model
#'
#' Main interface for executing COSERO simulation with custom
#' settings. Automatically manages configuration files, executes
#' the model, and optionally reads output files.
#'
#' @param project_path Path to COSERO project directory
#'   (must contain COSERO.exe)
#' @param defaults_settings Named list of configuration settings
#'   to override in defaults.txt. Available settings include:
#'   \itemize{
#'     \item \strong{PROJECTINFO} - Project name/description
#'     \item \strong{DATAFILE} - Observed discharge filename
#'     \item \strong{PARAFILE} - Parameter filename
#'     \item \strong{IKL} - Number of snow classes
#'     \item \strong{NCLASS} - Number of landuse classes
#'     \item \strong{STARTDATE} - Simulation start date as
#'       "YYYY MM DD HH MM" (e.g., "2015 1 1 0 0")
#'     \item \strong{ENDDATE} - Simulation end date as
#'       "YYYY MM DD HH MM" (e.g., "2015 12 31 23 59")
#'     \item \strong{SPINUP} - Spin-up period in timesteps
#'       (must be >= 1)
#'     \item \strong{OUTPUTTYPE} - Output detail level:
#'       1 (QSIM only), 2 (ZRVIEW compatible), 3 (full evaluation)
#'     \item \strong{SC_FLAG} - Runoff depth area basis:
#'       0 = local subbasin (EZFL_B), 1 = total upstream (EZFL_T)
#'     \item \strong{OUTCONTROL} - Write zonal time series flag
#'       (0 or 1; requires OUTPUTTYPE=3)
#'     \item \strong{RUNOFFFILE} - Runoff output filename
#'     \item \strong{STATSFILE} - Statistics output filename
#'     \item \strong{OPTFILE} - Optimization progress filename
#'     \item \strong{ADDFLUXCONT} - Enable additional inflow from
#'       outside the study area (0 or 1)
#'     \item \strong{ADDFLUXFILE} - Additional inflow filename
#'       (NB-TONZ header + time series, read from "input")
#'     \item \strong{ADDREGCONT} - Enable regression-based inflow
#'       estimation from predictor subbasins (0 or 1)
#'     \item \strong{ADDREGFILE} - Regression parameter filename
#'       (NB, intercept, num_predictors, slope/predictor pairs,
#'       read from "input")
#'     \item \strong{WRITERASTERS} - Raster output control filename
#'   }
#'   Use \code{\link{show_cosero_defaults}} to see all
#'   available parameters.
#' @param exe_name Name of COSERO executable
#'   (default: "COSERO.exe")
#' @param capture_output Whether to capture and parse model
#'   output (default: TRUE)
#' @param read_outputs Whether to read output files after
#'   successful execution (default: TRUE)
#' @param quiet Suppress console output except for runtime
#'   and errors (default: FALSE)
#' @param statevar_source State variable source:
#'   \itemize{
#'     \item 1 = read from parameter file (cold start, default)
#'     \item 2 = read from statevar.dmp file (warm start)
#'   }
#' @param tmmon_option Monthly temperature option:
#'   \itemize{
#'     \item 1 = use TMMon values from parameter file (default)
#'     \item 2 = calculate TMMon from input data
#'   }
#' @param create_backup If TRUE (default), creates backup
#'   of defaults.txt in parameterfile_backup folder.
#'   Set to FALSE during optimization runs.
#'
#' @return List containing:
#'   \item{exit_code}{Integer exit code (0 = success)}
#'   \item{success}{Logical indicating simulation success}
#'   \item{has_error}{Logical indicating if error messages
#'     were detected}
#'   \item{error_message}{String with error message
#'     (NULL if no error)}
#'   \item{output_data}{Parsed output data (if
#'     read_outputs = TRUE and successful).
#'     See \code{\link{read_cosero_output}} for structure.}
#'   \item{defaults_settings}{Configuration settings used}
#'   \item{project_path}{Original project path}
#'   \item{execution_time}{POSIXct timestamp of execution}
#'   \item{runtime_seconds}{Numeric runtime in seconds}
#'
#' @export
#' @examples
#' \dontrun{
#' # Basic run with existing defaults
#' result <- run_cosero("path/to/project")
#'
#' # Standard run with date range and spin-up
#' result <- run_cosero(
#'   "path/to/project",
#'   defaults_settings = list(
#'     STARTDATE = "1991 1 1 0 0",
#'     ENDDATE = "2020 12 31 0 0",
#'     SPINUP = 365,
#'     OUTPUTTYPE = 1
#'   )
#' )
#'
#' # Warm start with calculated monthly temperature
#' result <- run_cosero(
#'   "path/to/project",
#'   defaults_settings = list(
#'     STARTDATE = "1991 1 1 0 0",
#'     ENDDATE = "2020 12 31 0 0",
#'     SPINUP = 1,
#'     OUTPUTTYPE = 1
#'   ),
#'   statevar_source = 2,
#'   tmmon_option = 2,
#'   read_outputs = FALSE,
#'   quiet = TRUE
#' )
#' }
#'
run_cosero <- function(project_path,
                       defaults_settings = NULL,
                       exe_name = "COSERO.exe",
                       capture_output = TRUE,
                       read_outputs = TRUE,
                       quiet = FALSE,
                       statevar_source = 1,
                       tmmon_option = 1,
                       create_backup = TRUE) {

  # -- Input Validation --
  if (!dir.exists(project_path)) {
    stop("Project path does not exist: ",
         project_path, call. = FALSE)
  }
  if (!file.exists(file.path(project_path, exe_name))) {
    stop("COSERO executable not found: ",
         exe_name, call. = FALSE)
  }
  if (!statevar_source %in% c(1, 2)) {
    stop("statevar_source must be 1 (parameter file) ",
         "or 2 (statevar.dmp file)", call. = FALSE)
  }
  if (!tmmon_option %in% c(1, 2)) {
    stop("tmmon_option must be 1 (from parameter file) ",
         "or 2 (calculate from data)", call. = FALSE)
  }

  # -- State Variable Handling (silent) --
  if (statevar_source == 2) {
    handle_statevar_dmp(project_path)
  }

  # Start runtime tracking
  start_time <- Sys.time()

  # -- Extract Run Parameters from Settings --
  if (!is.null(defaults_settings)) {
    if ("statevar_source" %in% names(defaults_settings)) {
      statevar_source <- defaults_settings$statevar_source
      defaults_settings$statevar_source <- NULL
    }
    if ("tmmon_option" %in% names(defaults_settings)) {
      tmmon_option <- defaults_settings$tmmon_option
      defaults_settings$tmmon_option <- NULL
    }
  }

  # -- Validate and Process Settings --
  if (!is.null(defaults_settings) &&
      length(defaults_settings) > 0) {
    validation <- validate_cosero_defaults(defaults_settings)
    if (length(validation$messages) > 0) {
      sapply(validation$messages, cat, "\n")
    }
    if (!validation$valid) {
      stop("Invalid configuration settings provided",
           call. = FALSE)
    }
    defaults_settings <- validation$settings
  }

  # -- Setup Project (silent) --
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  project_setup <- setup_project_directories(
    project_path, defaults_settings, timestamp,
    quiet = TRUE, create_backup
  )

  # -- Print Header --
  if (!quiet) {
    print_run_header(
      project_path, defaults_settings,
      statevar_source, tmmon_option
    )
    print_header("COSERO output")
  }

  # -- Build Commands File & Execute --
  commands_file <- file.path(
    project_path, "cosero_commands.txt"
  )
  create_commands_file(
    commands_file, statevar_source, tmmon_option
  )

  tryCatch({
    show_output <- !quiet && capture_output
    exec_result <- execute_cosero(
      project_path, exe_name, commands_file,
      capture_output, show_output
    )

    end_time <- Sys.time()
    runtime_seconds <- as.numeric(
      difftime(end_time, start_time, units = "secs")
    )

    # -- Handle Errors --
    if (exec_result$has_error ||
        exec_result$exit_code != 0) {
      return(handle_execution_failure(
        exec_result, runtime_seconds, project_path
      ))
    }

    # -- Read Outputs (silently) --
    output_data <- NULL
    if (read_outputs && exec_result$exit_code == 0 &&
        !exec_result$has_error) {
      tryCatch({
        output_data <- read_cosero_output(
          project_setup$output_dir,
          project_setup$defaults_file,
          quiet = TRUE
        )
      }, error = function(e) {
        warning("Could not read output files: ",
                e$message)
      })
    }

    # -- Print Results Summary --
    if (!quiet) {
      print_run_results(output_data, runtime_seconds)
    }

    list(
      exit_code = exec_result$exit_code,
      success = TRUE,
      has_error = FALSE,
      error_message = NULL,
      output_data = output_data,
      defaults_settings = defaults_settings,
      project_path = project_path,
      execution_time = end_time,
      runtime_seconds = runtime_seconds
    )

  }, error = function(e) {
    stop("Error running COSERO: ", e$message,
         call. = FALSE)
  })
}

# ============================================================================
# Internal Helpers
# ============================================================================

#' Handle statevar.dmp file location (silent)
#' @keywords internal
handle_statevar_dmp <- function(project_path) {
  statevar_input <- file.path(
    project_path, "input", "statevar.dmp"
  )
  statevar_output <- file.path(
    project_path, "output", "statevar.dmp"
  )

  if (file.exists(statevar_input)) {
    # Already in input folder, nothing to do
  } else if (file.exists(statevar_output)) {
    file.copy(statevar_output, statevar_input,
              overwrite = TRUE)
  } else {
    warning(
      "statevar_source=2 specified but statevar.dmp ",
      "not found in input or output folder"
    )
  }
}

#' Handle execution failure reporting
#' @keywords internal
handle_execution_failure <- function(exec_result,
                                     runtime_seconds,
                                     project_path) {
  print_header("FAILED")
  cat(sprintf(
    "  Runtime: %.2f seconds\n",
    runtime_seconds
  ))

  error_lines <- NULL
  if (exec_result$has_error) {
    cat("  Error detected in output:\n")
    error_lines <- extract_error_lines(exec_result$output)
    cat("  ", paste(error_lines, collapse = "\n  "), "\n")
  }

  list(
    exit_code = exec_result$exit_code,
    success = FALSE,
    has_error = exec_result$has_error,
    error_message = if (exec_result$has_error) {
      paste(error_lines, collapse = "; ")
    } else {
      NULL
    },
    output_data = NULL,
    project_path = project_path,
    execution_time = Sys.time(),
    runtime_seconds = runtime_seconds
  )
}
