# COSERO Configuration Management
# Configuration parameters, validation, file I/O, and project setup
# Extracted from cosero_run.R for clarity

#' @importFrom data.table fread
NULL

# ============================================================================
# COSERO Default Parameters
# ============================================================================

cosero_defaults <- data.frame(
  parameter = c(
    # Project identity
    "PROJECTINFO",
    # Input files
    "DATAFILE", "PARAFILE",
    # Model structure
    "IKL", "NCLASS",
    # Simulation period
    "STARTDATE", "ENDDATE", "SPINUP",
    # Output control
    "OUTPUTTYPE", "SC_FLAG", "OUTCONTROL",
    "RUNOFFFILE", "STATSFILE", "OPTFILE",
    # Optional boundary inputs
    "ADDFLUXCONT", "ADDFLUXFILE", "ADDREGCONT", "ADDREGFILE",
    # Rarely changed
    "WRITERASTERS"
  ),
  type = c(
    "character",
    "character", "character",
    "integer", "integer",
    "date", "date", "integer",
    "integer", "flag", "integer",
    "character", "character", "character",
    "flag", "character", "flag", "character",
    "character"
  ),
  required = rep(TRUE, 19),
  stringsAsFactors = FALSE
)

# ============================================================================
# Configuration Display
# ============================================================================

#' Show COSERO Default Configuration Parameters
#'
#' Displays available COSERO configuration parameters and their types.
#' Useful for understanding what settings can be passed to run_cosero().
#'
#' @param parameter_name Optional. If specified, shows details for a
#'   single parameter. If NULL (default), shows all available parameters.
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
    param_info <- cosero_defaults[
      cosero_defaults$parameter == parameter_name,
    ]
    if (nrow(param_info) == 0) {
      cat(
        "Parameter not found. Available:",
        paste(cosero_defaults$parameter, collapse = ", "), "\n"
      )
      return(invisible(NULL))
    }
    cat(
      "Parameter:", param_info$parameter,
      "Type:", param_info$type, "\n"
    )
  } else {
    cat(
      "COSERO Configuration Parameters (",
      nrow(cosero_defaults), " total):\n"
    )
    for (i in seq_len(nrow(cosero_defaults))) {
      param <- cosero_defaults[i, ]
      cat(sprintf("%-15s [%s]\n", param$parameter, param$type))
    }
  }
  invisible(cosero_defaults)
}

# ============================================================================
# Configuration Validation
# ============================================================================

#' @keywords internal
validate_cosero_defaults <- function(settings) {
  validation_results <- list(
    valid = TRUE,
    messages = character(0),
    settings = settings
  )

  for (param_name in names(settings)) {
    param_value <- settings[[param_name]]

    if (!param_name %in% cosero_defaults$parameter) {
      validation_results$messages <- c(
        validation_results$messages,
        paste("Unknown parameter:", param_name)
      )
      next
    }

    param_info <- cosero_defaults[
      cosero_defaults$parameter == param_name,
    ]
    param_type <- param_info$type

    # Validate by type
    if (param_type == "integer") {
      if (!is.numeric(param_value) ||
          param_value != as.integer(param_value)) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          paste(param_name, "must be integer")
        )
      }
      if (param_name == "OUTPUTTYPE" &&
          !param_value %in% 0:3) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          "OUTPUTTYPE must be 0, 1, 2, or 3"
        )
      }
      if (param_name == "SPINUP" && param_value < 1) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          "SPINUP must be >= 1 (COSERO requires at least 1 timestep)"
        )
      }
    } else if (param_type == "flag") {
      if (!param_value %in% c(0, 1)) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          paste(param_name, "must be 0 or 1")
        )
      }
    } else if (param_type == "date") {
      # Handle both string "2015 1 1 0 0" and vector c(2015, 1, 1, 0, 0)
      if (is.character(param_value) && length(param_value) == 1) {
        date_parts <- as.numeric(strsplit(param_value, "\\s+")[[1]])
        if (length(date_parts) != 5) {
          validation_results$valid <- FALSE
          validation_results$messages <- c(
            validation_results$messages,
            paste(param_name, "must have 5 elements:",
                  "year month day hour minute")
          )
        } else {
          validation_results$settings[[param_name]] <- date_parts
        }
      } else if (length(param_value) != 5) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          paste(param_name, "must have 5 elements:",
                "year month day hour minute")
        )
      }
    } else if (param_type == "character") {
      if (!is.character(param_value) || nchar(param_value) == 0) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          paste(param_name, "must be non-empty character")
        )
      }
    }
  }

  # Cross-parameter validation
  has_start <- "STARTDATE" %in% names(validation_results$settings)
  has_end <- "ENDDATE" %in% names(validation_results$settings)

  if (has_start && has_end) {
    start_date <- validation_results$settings$STARTDATE
    end_date <- validation_results$settings$ENDDATE
    if (length(start_date) == 5 && length(end_date) == 5) {
      start_num <- start_date[1] * 10000 +
        start_date[2] * 100 + start_date[3]
      end_num <- end_date[1] * 10000 +
        end_date[2] * 100 + end_date[3]
      if (start_num >= end_num) {
        validation_results$valid <- FALSE
        validation_results$messages <- c(
          validation_results$messages,
          "ENDDATE must be after STARTDATE"
        )
      }
    }
  }

  validation_results
}

# ============================================================================
# Defaults File Operations
# ============================================================================

#' @keywords internal
get_default_cosero_values <- function() {
  # Kept for backward compatibility with modify_defaults(); not used by
  # create_default_defaults() anymore.
  list(
    PROJECTINFO = "COSERO_Project",
    DATAFILE    = "Qobs.txt",
    BINDATAFILE = "not_used.dat",
    PARAFILE    = "para_ini.txt",
    IKL         = 5,
    NCLASS      = 10,
    OUTPUTTYPE  = 1,
    SC_FLAG     = 1,
    OUTCONTROL  = 0,
    STARTDATE   = "2010 1 1 0 0",
    ENDDATE     = "2015 12 31 0 0",
    SPINUP      = 365,
    RUNOFFFILE  = "COSERO.runoff",
    STATSFILE   = "statistics.txt",
    OPTFILE     = "optprogress.txt",
    WRITERASTERS = "raster_write.txt",
    ADDFLUXCONT = 0,
    ADDFLUXFILE = "Qadd.txt",
    ADDREGCONT  = 0,
    ADDREGFILE  = "reg_para.txt"
  )
}

#' @keywords internal
modify_defaults <- function(defaults_file, settings, quiet = FALSE) {
  if (!file.exists(defaults_file)) {
    create_default_defaults(defaults_file, quiet)
  }

  lines <- readLines(defaults_file)

  # Filter out NULL values (user wants to keep existing defaults)
  settings <- settings[!sapply(settings, is.null)]

  for (param_name in names(settings)) {
    param_value <- settings[[param_name]]

    # Find lines that start with the parameter name
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
        lines <- c(lines, param_name,
                    paste(param_value, collapse = " "))
      }
    }
  }

  writeLines(lines, defaults_file)
  if (!quiet) {
    cat("Updated defaults.txt with", length(settings), "parameters\n")
  }
}

#' @keywords internal
create_default_defaults <- function(defaults_file, quiet = FALSE) {
  lines <- c(
    "This file contains default settings for COSERO",
    "",
    # --- Project identity ---
    "PROJECTINFO (default project info, written into first line of each output file)",
    "COSERO_Project",
    "",
    # --- Input files ---
    "DATAFILE (default input data file containing observed discharge, read in from directory \"input\")",
    "Qobs.txt",
    "",
    "PARAFILE (default input parameter file, read in from directory \"input\")",
    "para_ini.txt",
    "",
    # --- Model structure ---
    "IKL (# of snow classes)",
    "5",
    "",
    "NCLASS (# of Landuse classes)",
    "10",
    "",
    # --- Simulation period ---
    "STARTDATE (start date of simulation period in format yyyy mm dd hh mm)",
    "2010 1 1 0 0",
    "",
    "ENDDATE (end date of simulation period in format yyyy mm dd hh mm)",
    "2015 12 31 0 0",
    "",
    "SPINUP (length of spin-up period without evaluation [time-steps])",
    "365",
    "",
    # --- Output control ---
    paste0(
      "OUTPUTTYPE (Sets the output evaluation extent:",
      " 1 - only QSIM; 2 - ZRVIEW compatible; 3 - full evaluation)"
    ),
    "1",
    "",
    paste0(
      "SC_FLAG (Use local subbasin area (EZFL_B, \"0\") or total upstream",
      " catchment area (EZFL_T, \"1\") for runoff depth / flux calculations)"
    ),
    "1",
    "",
    paste0(
      "OUTCONTROL (if set to \"1\", zonal values will be written for every time step:",
      " folder cdr/output is needed; very slow; outputtype must be \"3\"; otherwise set to \"0\")"
    ),
    "0",
    "",
    "RUNOFFFILE (default output file for simulated runoff of a single run, written to directory \"output\")",
    "COSERO.runoff",
    "",
    "STATSFILE (default output file for performance statistics of a single run, written to directory \"output\")",
    "statistics.txt",
    "",
    "OPTFILE (default output file for progress of optimization, written to directory \"output\")",
    "optprogress.txt",
    "",
    # --- Optional boundary inputs ---
    paste0(
      "ADDFLUXCONT (if set to \"1\", file Qadd with additional inflow from outside the",
      " study area is read in; otherwise set to \"0\")"
    ),
    "0",
    "",
    paste0(
      "ADDFLUXFILE (additional inflow in m\u00b3/s added to specified subbasins/zones.",
      " Starts with an NB-TONZ mapping header, followed by the time series.",
      " Read in from directory \"input\")"
    ),
    "Qadd.txt",
    "",
    paste0(
      "ADDREGCONT (if set to \"1\", file Qreg with regression parameters for estimating",
      " inflow from outside the study area is read in; otherwise set to \"0\")"
    ),
    "0",
    "",
    paste0(
      "ADDREGFILE (regression parameters (up to 3 predictors) to calculate subbasin",
      " inflow based on discharge in other (predictor) subbasins.",
      " Read in from directory \"input\")"
    ),
    "reg_para.txt",
    "",
    # --- Rarely changed ---
    "WRITERASTERS (write state/flux-rasters for use in FEWS)",
    "raster_write.txt",
    "",
    "BINDATAFILE (default input binary data file, read in from directory \"cdr/input\", not used)",
    "not_used.dat",
    ""
  )
  writeLines(lines, defaults_file)
  if (!quiet) {
    cat("Created default defaults.txt at:", defaults_file, "\n")
  }
}

#' Read COSERO Defaults File
#'
#' Reads configuration settings from a COSERO defaults.txt file.
#' Parses the file format and returns settings as a named list.
#'
#' @param defaults_file Path to defaults.txt file
#'
#' @return Named list with configuration settings. Common items:
#'   \item{PROJECTINFO}{Project name/description}
#'   \item{DATAFILE}{Input data filename}
#'   \item{PARAFILE}{Parameter filename}
#'   \item{STARTDATE}{Start date as character vector
#'     c(YYYY, MM, DD, HH, MM)}
#'   \item{ENDDATE}{End date as character vector
#'     c(YYYY, MM, DD, HH, MM)}
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
    stop("defaults.txt file not found: ", defaults_file,
         call. = FALSE)
  }

  lines <- readLines(defaults_file)
  settings <- list()
  valid_params <- cosero_defaults$parameter

  i <- 1
  while (i <= length(lines)) {
    line <- trimws(lines[i])

    # Skip empty lines
    if (nchar(line) == 0) {
      i <- i + 1
      next
    }

    # Extract potential parameter name
    potential_param <- regmatches(
      line, regexpr("^[A-Z_]+", line)
    )

    # Check if this line contains a valid parameter keyword
    if (length(potential_param) > 0 &&
        potential_param[1] %in% valid_params) {
      param_name <- potential_param[1]

      # Look for the value in the next non-empty line
      j <- i + 1
      while (j <= length(lines)) {
        next_line <- trimws(lines[j])

        if (nchar(next_line) == 0) {
          j <- j + 1
          next
        }

        # Check if we've hit another parameter keyword
        next_potential <- regmatches(
          next_line, regexpr("^[A-Z_]+", next_line)
        )
        if (length(next_potential) > 0 &&
            next_potential[1] %in% valid_params) {
          break
        }

        # This should be our value line
        if (grepl("\\s", next_line)) {
          param_value <- strsplit(next_line, "\\s+")[[1]]
        } else {
          param_value <- next_line
        }

        settings[[param_name]] <- param_value
        i <- j
        break
      }

      if (j > length(lines)) {
        i <- i + 1
      }
    } else {
      i <- i + 1
    }
  }

  settings
}

# ============================================================================
# Project Setup
# ============================================================================

#' @keywords internal
setup_project_directories <- function(project_path,
                                      defaults_settings = NULL,
                                      timestamp,
                                      quiet = FALSE,
                                      create_backup = TRUE) {
  input_dir <- file.path(project_path, "input")
  output_dir <- file.path(project_path, "output")

  if (!dir.exists(input_dir)) {
    dir.create(input_dir, recursive = TRUE)
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  backup_dir <- NULL
  if (create_backup) {
    backup_dir <- file.path(input_dir, "parameterfile_backup")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
    }
  }

  defaults_file <- file.path(input_dir, "defaults.txt")
  current_settings <- NULL

  if (file.exists(defaults_file)) {
    current_settings <- read_defaults(defaults_file)
    if (!is.null(defaults_settings)) {
      if (create_backup) {
        backup_file <- file.path(
          backup_dir,
          paste0("defaults.txt.backup_", timestamp)
        )
        file.copy(defaults_file, backup_file)
        if (!quiet) {
          cat("Backed up defaults.txt to:", backup_file, "\n")
        }
      }
      modify_defaults(defaults_file, defaults_settings, quiet)
      if (create_backup) {
        used_file <- file.path(
          backup_dir,
          paste0("defaults.txt.used_", timestamp)
        )
        file.copy(defaults_file, used_file)
        if (!quiet) {
          cat("Saved used settings to:", used_file, "\n")
        }
      }
    }
  } else {
    if (!is.null(defaults_settings)) {
      modify_defaults(defaults_file, defaults_settings, quiet)
    } else {
      create_default_defaults(defaults_file, quiet)
    }
  }

  list(
    input_dir = input_dir,
    output_dir = output_dir,
    defaults_file = defaults_file,
    current_settings = current_settings,
    backup_dir = backup_dir
  )
}
