# COSERO Data Readers Interface
# File reading and data processing functions for COSERO outputs
# Author: COSERO R Interface
# Date: 2025-09-25

#' @importFrom data.table fread
#' @importFrom dplyr filter mutate select arrange group_by summarise %>% any_of
#' @importFrom lubridate ymd_hm year month day hour minute
#' @importFrom utils write.table
NULL

# Main Output Reader #####

#' Read COSERO Model Output Files
#'
#' Main function to read all COSERO output files from a simulation directory.
#' Automatically detects the OUTPUTTYPE and reads available files accordingly.
#'
#' @param output_dir Path to COSERO output directory
#' @param defaults_file Path to defaults.txt file (optional). If provided, reads parameter file and settings.
#' @param quiet Logical. If TRUE, suppresses progress messages.
#'
#' @return A list containing:
#'   \item{runoff}{Discharge data (QOBS, QSIM) from COSERO.runoff}
#'   \item{precipitation}{Precipitation data from COSERO.prec}
#'   \item{runoff_components}{Runoff components from COSERO.plus}
#'   \item{water_balance}{Water balance variables from COSERO.plus1}
#'   \item{statistics}{Performance statistics from statistics.txt}
#'   \item{topology}{Basin topology from topology.txt}
#'   \item{glacier}{Glacier variables (if OUTPUTTYPE >= 2)}
#'   \item{meteorology}{Meteorological variables (if OUTPUTTYPE >= 2)}
#'   \item{monitor}{Monitoring data (if OUTPUTTYPE >= 3)}
#'   \item{monitor_subbasins}{Per-subbasin monitoring data (if OUTPUTTYPE >= 3)}
#'   \item{rundepth}{Runoff depth data (if OUTPUTTYPE >= 3)}
#'   \item{longterm_annual}{Annual means (if OUTPUTTYPE >= 3)}
#'   \item{longterm_seasonal}{Seasonal means (if OUTPUTTYPE >= 3)}
#'   \item{parameters}{Parameter values (if defaults_file provided)}
#'   \item{defaults_settings}{Settings from defaults.txt (if provided)}
#'   \item{metadata}{Metadata including outputtype and subbasin list}
#'
#' @export
#' @examples
#' \dontrun{
#' # Read all outputs from a simulation
#' outputs <- read_cosero_output("path/to/output")
#'
#' # Read with parameter file
#' outputs <- read_cosero_output("path/to/output",
#'                               defaults_file = "path/to/input/defaults.txt")
#'
#' # Access specific outputs
#' discharge <- outputs$runoff
#' stats <- outputs$statistics
#' }
read_cosero_output <- function(output_dir, defaults_file = NULL, quiet = FALSE) {
  output_data <- list()

  # Detect OUTPUTTYPE
  outputtype <- detect_outputtype(output_dir)
  if (!quiet) cat("Detected OUTPUTTYPE:", outputtype, "\n")

  # OUTPUTTYPE 1 - Core files
  output_data$runoff <- read_runoff(output_dir, quiet)
  output_data$precipitation <- read_precipitation(output_dir, quiet)
  output_data$runoff_components <- read_plus(output_dir, quiet)
  output_data$water_balance <- read_plus1(output_dir, quiet)
  output_data$statistics <- read_statistics(output_dir, quiet)
  output_data$topology <- read_topology(output_dir, quiet)

  # OUTPUTTYPE 2 - Additional files
  if (outputtype >= 2) {
    output_data$glacier <- read_var_glac(output_dir, quiet)
    output_data$meteorology <- read_var_met(output_dir, quiet)
  }

  # OUTPUTTYPE 3 - Monitor and long-term files
  if (outputtype >= 3) {
    output_data$monitor <- read_monitor(output_dir, quiet)
    output_data$monitor_subbasins <- read_monitor_subbasins(output_dir, quiet)
    output_data$rundepth <- read_rundepth(output_dir, quiet)
    output_data$longterm_annual <- read_longterm_annual(output_dir, quiet)
    output_data$longterm_seasonal <- read_longterm_seasonal(output_dir, quiet)
  }

  # Read parameters if defaults file provided
  if (!is.null(defaults_file) && file.exists(defaults_file)) {
    settings <- read_defaults(defaults_file)
    if ("PARAFILE" %in% names(settings)) {
      param_file <- file.path(dirname(defaults_file), settings$PARAFILE)
      if (file.exists(param_file)) {
        if (!quiet) cat("Reading parameter file:", settings$PARAFILE, "\n")
        output_data$parameters <- read_cosero_parameters(param_file, skip_lines = 1, quiet = quiet)
      }
    }

    # Store defaults settings including SPINUP
    output_data$defaults_settings <- settings
  }

  # Add metadata
  output_data$metadata <- list(
    outputtype = outputtype,
    output_dir = output_dir,
    subbasins = detect_subbasins(output_dir)
  )

  return(output_data)
}

# 2.1 OUTPUTTYPE Detection #####
#' Detect COSERO output type
#' @param output_dir Path to COSERO output directory
#' @return Integer: 1, 2, or 3 corresponding to OUTPUTTYPE
#' @export
detect_outputtype <- function(output_dir) {
  has_monitor <- file.exists(file.path(output_dir, "monitor.txt"))
  has_var_met <- file.exists(file.path(output_dir, "var_MET.txt"))

  if (has_monitor) return(3)
  if (has_var_met) return(2)
  return(1)
}

#' Detect available subbasins from COSERO output
#' @param output_dir Path to COSERO output directory
#' @return Character vector of subbasin IDs found in output files
#' @export
detect_subbasins <- function(output_dir) {
  monitor_files <- list.files(output_dir, pattern = "^monitor_sb\\d+\\.txt$")
  if (length(monitor_files) > 0) {
    sbs <- gsub("monitor_sb(\\d+)\\.txt", "\\1", monitor_files)
    return(sort(sbs))
  }

  runoff_file <- file.path(output_dir, "COSERO.runoff")
  if (file.exists(runoff_file)) {
    lines <- readLines(runoff_file, n = 20)
    header_line <- grep("yyyy\\s+mm\\s+dd", lines, value = TRUE)[1]
    if (!is.na(header_line)) {
      qobs_cols <- gregexpr("QOBS_\\d+", header_line)[[1]]
      if (qobs_cols[1] > 0) {
        matches <- regmatches(header_line, gregexpr("QOBS_(\\d+)", header_line))[[1]]
        sbs <- gsub("QOBS_(\\d+)", "\\1", matches)
        return(sort(unique(sbs)))
      }
    }
  }
  return(character(0))
}

# 3 Runoff Data Reader #####

# 3.1 Main Runoff Reader #####

#' Read COSERO Runoff File
#'
#' Reads discharge data (observed and simulated) from COSERO.runoff file.
#'
#' @param runoff_file Path to COSERO.runoff file
#' @param missing_value Numeric value representing missing data (default: -999.00)
#' @param quiet Logical. If TRUE, suppresses progress messages.
#'
#' @return Data frame with columns:
#'   \item{yyyy, mm, dd, hh}{Date and time components}
#'   \item{DateTime}{POSIXct datetime object}
#'   \item{Date}{Date object}
#'   \item{QOBS_XXXX}{Observed discharge for subbasin XXXX (m³/s)}
#'   \item{QSIM_XXXX}{Simulated discharge for subbasin XXXX (m³/s)}
#'   \item{Qloc_XXXX}{Local discharge for subbasin XXXX (m³/s)}
#'
#' @export
#' @examples
#' \dontrun{
#' # Read runoff file
#' runoff <- read_cosero_runoff("path/to/COSERO.runoff")
#'
#' # Access discharge for subbasin 0001
#' qobs <- runoff$QOBS_0001
#' qsim <- runoff$QSIM_0001
#' }
read_cosero_runoff <- function(runoff_file, missing_value = -999.00, quiet = FALSE) {
  if (!file.exists(runoff_file)) {
    stop("Runoff file not found: ", runoff_file, call. = FALSE)
  }

  if (!quiet) cat("Reading COSERO runoff file:", runoff_file, "\n")
  lines <- readLines(runoff_file)

  # Find data section
  runoff_start <- grep("#### Runoff ####", lines)
  if (length(runoff_start) == 0) {
    stop("Could not find '#### Runoff ####' section in file", call. = FALSE)
  }

  # Find header line
  header_pattern <- grep("yyyy\\s+mm\\s+dd", lines)
  if (length(header_pattern) == 0) {
    stop("Could not find header line with 'yyyy mm dd' pattern", call. = FALSE)
  }

  header_line_idx <- header_pattern[header_pattern > runoff_start[1]][1]
  if (is.na(header_line_idx)) {
    stop("Could not find header line after runoff section", call. = FALSE)
  }

  # Parse header
  header <- trimws(lines[header_line_idx])
  col_names <- unlist(strsplit(header, "\\s+"))

  # Get data lines
  data_start_idx <- header_line_idx + 1
  data_lines <- lines[data_start_idx:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  if (!quiet) cat("Found", length(col_names), "columns,", length(data_lines), "data lines\n")

  # Parse data
  runoff_data <- parse_cosero_data_lines(data_lines, col_names)
  runoff_data <- create_cosero_dataframe(runoff_data, col_names)

  # Process data
  runoff_data <- add_datetime_columns(runoff_data)
  runoff_data <- handle_missing_values(runoff_data, missing_value, quiet)
  runoff_data <- add_subbasin_metadata(runoff_data)

  # Report results
  subbasin_cols <- grep("QOBS_|QSIM_", colnames(runoff_data), value = TRUE)
  n_subbasins <- length(unique(gsub(".*_(\\d+)", "\\1", subbasin_cols)))
  if (!quiet) cat("Successfully read", nrow(runoff_data), "rows for", n_subbasins, "subbasins\n")

  return(runoff_data)
}

# Data Processing Functions #####

#' @keywords internal
parse_cosero_data_lines <- function(data_lines, col_names) {
  data_matrix <- matrix(nrow = length(data_lines), ncol = length(col_names))

  for (i in seq_along(data_lines)) {
    values <- unlist(strsplit(trimws(data_lines[i]), "\\s+"))
    if (length(values) >= length(col_names)) {
      data_matrix[i, ] <- values[1:length(col_names)]
    } else if (length(values) > 0) {
      # Pad with NA if fewer columns
      padded_values <- c(values, rep(NA, length(col_names) - length(values)))
      data_matrix[i, ] <- padded_values[1:length(col_names)]
    }
  }

  return(data_matrix)
}

#' @keywords internal
create_cosero_dataframe <- function(data_matrix, col_names) {
  runoff_data <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(runoff_data) <- col_names

  # Convert numeric columns
  date_time_cols <- c("yyyy", "mm", "dd", "hh", "mm")
  numeric_cols <- !col_names %in% date_time_cols

  for (col in col_names[numeric_cols]) {
    runoff_data[[col]] <- as.numeric(runoff_data[[col]])
  }

  return(runoff_data)
}

add_datetime_columns <- function(data) {
  tryCatch({
    if ("yyyy" %in% colnames(data) && "mm" %in% colnames(data)) {
      # Handle duplicate 'mm' columns (month and minute)
      year_col <- data$yyyy
      month_col <- data[[2]]  # Second column should be month
      day_col <- data[[3]]    # Third column should be day
      hour_col <- if (ncol(data) >= 4) data[[4]] else 0
      minute_col <- if (ncol(data) >= 5) data[[5]] else 0

      data$DateTime <- as.POSIXct(
        paste(year_col, month_col, day_col, hour_col, minute_col),
        format = "%Y %m %d %H %M"
      )
      # Create Date directly from year/month/day to avoid timezone issues
      data$Date <- as.Date(paste(year_col, month_col, day_col, sep = "-"))
    }
  }, error = function(e) {
    warning("Could not create DateTime columns: ", e$message)
  })

  return(data)
}

handle_missing_values <- function(data, missing_value, quiet = FALSE) {
  discharge_cols <- grep("QOBS_|QSIM_|Qloc_", colnames(data))

  if (length(discharge_cols) > 0) {
    for (col_idx in discharge_cols) {
      data[[col_idx]][data[[col_idx]] == missing_value] <- NA
    }
    if (!quiet) cat("Replaced", missing_value, "with NA in", length(discharge_cols), "discharge columns\n")
  }

  return(data)
}

add_subbasin_metadata <- function(data) {
  discharge_cols <- grep("QOBS_|QSIM_|Qloc_", colnames(data), value = TRUE)
  subbasin_ids <- unique(gsub(".*_(\\d+)", "\\1", discharge_cols))

  attr(data, "subbasin_ids") <- subbasin_ids
  attr(data, "n_subbasins") <- length(subbasin_ids)

  return(data)
}

# 4 Statistics Reader #####

#' Read COSERO Statistics File
#'
#' Reads model performance statistics from statistics.txt file.
#'
#' @param stats_file Path to statistics.txt file
#' @param quiet Logical. If TRUE, suppresses progress messages.
#'
#' @return Data frame with columns:
#'   \item{sb}{Subbasin ID}
#'   \item{NSE}{Nash-Sutcliffe Efficiency}
#'   \item{KGE}{Kling-Gupta Efficiency}
#'   \item{BIAS}{Bias (percent)}
#'   \item{RMSE}{Root Mean Square Error}
#'   \item{...}{Other performance metrics}
#' The data frame has an attribute "spinup_timestep" indicating the start of evaluation period.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read statistics
#' stats <- read_cosero_statistics("path/to/statistics.txt")
#'
#' # Get NSE for all subbasins
#' nse_values <- stats$NSE
#'
#' # Get spinup period
#' spinup <- attr(stats, "spinup_timestep")
#' }
read_cosero_statistics <- function(stats_file, quiet = FALSE) {
  lines <- readLines(stats_file)

  # Extract spin-up timestep (start time-step of evaluation)
  spinup_line <- grep("start time-step of evaluation:", lines, value = TRUE)
  spinup_timestep <- NA
  if (length(spinup_line) > 0) {
    spinup_timestep <- as.integer(sub(".*:\\s*(\\d+).*", "\\1", spinup_line[1]))
  }

  # Find data section
  header_line <- grep("sb\\s+NSE\\s+KGE", lines)
  if (length(header_line) == 0) {
    stop("Could not find statistics header in file", call. = FALSE)
  }

  # Get header and data
  header <- trimws(lines[header_line[1]])
  data_lines <- lines[(header_line[1] + 1):length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  col_names <- unlist(strsplit(header, "\\s+"))

  # Parse data
  stats_list <- list()
  for (i in seq_along(data_lines)) {
    line <- trimws(data_lines[i])
    values <- unlist(strsplit(line, "\\s+"))

    if (length(values) >= length(col_names)) {
      values <- values[1:length(col_names)]
      stats_list[[i]] <- values
    }
  }

  # Create dataframe
  stats_data <- do.call(rbind, stats_list)
  stats_data <- as.data.frame(stats_data, stringsAsFactors = FALSE)
  colnames(stats_data) <- col_names

  # Convert numeric columns
  numeric_cols <- col_names[col_names != "sb"]
  for (col in numeric_cols) {
    stats_data[[col]] <- as.numeric(stats_data[[col]])
  }

  # Add spin-up as attribute
  attr(stats_data, "spinup_timestep") <- spinup_timestep

  if (!quiet) {
    cat("Successfully read statistics for", nrow(stats_data), "subbasins\n")
    if (!is.na(spinup_timestep)) {
      cat("Spin-up period:", spinup_timestep, "timesteps\n")
    }
  }
  return(stats_data)
}

# 5 Parameters Reader #####

#' Read COSERO Parameter File
#'
#' Reads model parameters from para.txt or similar parameter files.
#' Supports both simple and tabular parameter file formats.
#'
#' @param param_file Path to parameter file
#' @param skip_lines Number of header lines to skip (default: 1)
#' @param quiet Logical. If TRUE, suppresses progress messages.
#'
#' @return Data frame with parameter values. Columns include:
#'   \item{NZ_}{Zone ID}
#'   \item{NB_}{Subbasin ID}
#'   \item{IZ_}{Zone index}
#'   \item{BETA_, CTMAX_, TAB1_, ...}{Model parameters}
#' The data frame has attributes "subbasin_ids", "n_subbasins", "zone_ids", and "n_zones".
#'
#' @export
#' @examples
#' \dontrun{
#' # Read parameter file
#' params <- read_cosero_parameters("path/to/para.txt")
#'
#' # Get parameters for first zone
#' zone1_params <- params[params$NZ_ == 1, ]
#'
#' # Get all BETA values
#' beta_values <- params$BETA_
#' }
read_cosero_parameters <- function(param_file, skip_lines = 1, quiet = FALSE) {
  if (!file.exists(param_file)) {
    stop("Parameter file not found: ", param_file, call. = FALSE)
  }

  lines <- readLines(param_file, warn = FALSE)
  if (length(lines) < 3) {
    stop("Parameter file appears to be too short or empty", call. = FALSE)
  }

  # Get header
  header_start <- skip_lines + 1
  header_line <- lines[header_start]
  col_names <- unlist(strsplit(trimws(header_line), "\\s+"))
  col_names <- col_names[nchar(col_names) > 0]

  # Get data lines
  data_start <- header_start + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  # Parse data
  param_data <- parse_parameter_data(data_lines, col_names)
  param_data <- convert_parameter_types(param_data)
  param_data <- add_parameter_metadata(param_data)

  if (!quiet) cat("Successfully read parameters for", nrow(param_data), "zones/subbasins\n")
  return(param_data)
}

#' Write COSERO Parameter File
#'
#' Writes a parameter data frame to a COSERO parameter file in tabular format.
#' Automatically adds a timestamp to track when the file was last modified.
#'
#' @param par_file Path to the output parameter file
#' @param param_data Data frame with parameter values (from read_cosero_parameters or modified)
#' @param project_info Character string for the first line of the file. If NULL (default),
#'   reads the existing first line from par_file (if it exists) or uses a generic header.
#' @param add_timestamp Logical. If TRUE (default), appends a timestamp to the project_info.
#' @param quiet Suppress messages
#'
#' @return Invisible NULL (called for side effects)
#' @export
#' @examples
#' \dontrun{
#' # Read, modify, and write
#' params <- read_cosero_parameters("para.txt")
#' params$BETA_ <- params$BETA_ * 1.1
#' write_cosero_parameters("para_modified.txt", params)
#'
#' # Write with custom project info
#' write_cosero_parameters("para.txt", params,
#'                         project_info = "Wildalpen optimized")
#'
#' # Write without timestamp
#' write_cosero_parameters("para.txt", params, add_timestamp = FALSE)
#' }
write_cosero_parameters <- function(par_file, param_data,
                                    project_info = NULL,
                                    add_timestamp = TRUE,
                                    quiet = FALSE) {

  # Get project_info from existing file if not provided
  if (is.null(project_info)) {
    if (file.exists(par_file)) {
      project_info <- readLines(par_file, n = 1, warn = FALSE)
      # Remove any existing timestamp pattern to avoid duplication
      project_info <- sub("\\s*\\[Modified:.*\\]\\s*$", "", project_info)
    } else {
      project_info <- "COSERO Parameter File"
    }
  }

  # Add timestamp if requested
  if (add_timestamp) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    project_info <- paste0(project_info, " [Modified: ", timestamp, "]")
  }

  # Write first line (project info)
  writeLines(project_info, par_file)

  # Append parameter table with headers
  suppressWarnings({
    write.table(
      param_data,
      file = par_file,
      append = TRUE,
      sep = "\t",
      row.names = FALSE,
      col.names = TRUE,
      quote = FALSE
    )
  })

  if (!quiet) {
    cat("Wrote", nrow(param_data), "zones to:", par_file, "\n")
  }

  invisible(NULL)
}

parse_parameter_data <- function(data_lines, col_names) {
  n_rows <- length(data_lines)
  n_cols <- length(col_names)
  parsed_rows <- list()

  for (i in 1:n_rows) {
    line <- trimws(data_lines[i])
    if (nchar(line) == 0) next

    values_raw <- unlist(strsplit(line, "\\s+"))
    values <- values_raw[nchar(values_raw) > 0]

    if (length(values) >= n_cols) {
      values <- values[1:n_cols]
      parsed_rows[[i]] <- values
    } else if (length(values) > 0) {
      padded_values <- c(values, rep(NA, n_cols - length(values)))
      parsed_rows[[i]] <- padded_values[1:n_cols]
    }
  }

  # Convert to dataframe
  param_matrix <- do.call(rbind, parsed_rows[!sapply(parsed_rows, is.null)])
  param_data <- as.data.frame(param_matrix, stringsAsFactors = FALSE)
  colnames(param_data) <- col_names

  return(param_data)
}

convert_parameter_types <- function(param_data) {
  id_columns <- c("NB_", "IZ_", "NZ_", "WATERBODY_", "SOILTYPE_")
  coordinate_columns <- c("X_COORD", "Y_COORD")

  for (col_name in colnames(param_data)) {
    if (col_name %in% id_columns) {
      param_data[[col_name]] <- as.integer(param_data[[col_name]])
    } else if (col_name %in% coordinate_columns) {
      numeric_vals <- as.numeric(param_data[[col_name]])
      numeric_vals[numeric_vals == -999] <- NA
      param_data[[col_name]] <- numeric_vals
    } else {
      tryCatch({
        numeric_vals <- as.numeric(param_data[[col_name]])
        numeric_vals[numeric_vals == -999.0000] <- NA
        param_data[[col_name]] <- numeric_vals
      }, error = function(e) {
        warning("Could not convert column ", col_name, " to numeric")
      })
    }
  }

  return(param_data)
}

add_parameter_metadata <- function(param_data) {
  if ("NB_" %in% colnames(param_data)) {
    subbasin_ids <- unique(param_data$NB_)
    attr(param_data, "subbasin_ids") <- subbasin_ids
    attr(param_data, "n_subbasins") <- length(subbasin_ids)
  }

  if ("IZ_" %in% colnames(param_data)) {
    zone_ids <- unique(param_data$IZ_)
    attr(param_data, "zone_ids") <- zone_ids
    attr(param_data, "n_zones") <- length(zone_ids)
  }

  return(param_data)
}

# 6 Additional Timeseries Readers #####
read_cosero_timeseries_file <- function(file_path, data_type = "timeseries", quiet = FALSE) {
  lines <- readLines(file_path)

  header <- trimws(lines[2])
  data_lines <- lines[3:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  col_names <- unlist(strsplit(header, "\\s+"))
  data_matrix <- parse_cosero_data_lines(data_lines, col_names)

  timeseries_data <- create_cosero_dataframe(data_matrix, col_names)
  timeseries_data <- add_datetime_columns(timeseries_data)

  if (!quiet) cat("Successfully read", nrow(timeseries_data), "rows of", data_type, "\n")
  return(timeseries_data)
}

read_cosero_precipitation <- function(file_path, quiet = FALSE) {
  lines <- readLines(file_path)

  header <- trimws(lines[1])
  data_lines <- lines[2:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  col_names <- unlist(strsplit(header, "\\s+"))
  data_matrix <- parse_cosero_data_lines(data_lines, col_names)

  prec_data <- create_cosero_dataframe(data_matrix, col_names)
  prec_data <- add_datetime_columns(prec_data)

  if (!quiet) cat("Successfully read", nrow(prec_data), "rows of precipitation data\n")
  return(prec_data)
}

# 7 Data Access Functions #####

# 7.1 Subbasin Data Access #####

#' Extract Discharge Data for Specific Subbasin
#'
#' Extracts observed and simulated discharge data for a single subbasin
#' from the full runoff dataset.
#'
#' @param runoff_data Data frame from read_cosero_runoff() or read_cosero_output()$runoff
#' @param subbasin_id Subbasin ID as character (e.g., "0001") or numeric (e.g., 1)
#'
#' @return Data frame with columns:
#'   \item{DateTime, Date}{Time information}
#'   \item{Q_obs}{Observed discharge (m³/s)}
#'   \item{Q_sim}{Simulated discharge (m³/s)}
#'   \item{Q_local}{Local discharge (m³/s), if available}
#' The data frame has an attribute "subbasin_id" with the subbasin ID.
#'
#' @export
#' @examples
#' \dontrun{
#' # Read outputs
#' outputs <- read_cosero_output("path/to/output")
#'
#' # Get data for subbasin 1
#' sb1 <- get_subbasin_data(outputs$runoff, 1)
#' sb1 <- get_subbasin_data(outputs$runoff, "0001")
#'
#' # Plot comparison
#' plot(sb1$Date, sb1$Q_obs, type = "l", col = "blue")
#' lines(sb1$Date, sb1$Q_sim, col = "red")
#' }
get_subbasin_data <- function(runoff_data, subbasin_id) {
  if (is.numeric(subbasin_id)) {
    subbasin_id <- sprintf("%04d", subbasin_id)
  }

  qobs_col <- paste0("QOBS_", subbasin_id)
  qsim_col <- paste0("QSIM_", subbasin_id)
  qloc_col <- paste0("Qloc_", subbasin_id)

  available_cols <- colnames(runoff_data)
  if (!qobs_col %in% available_cols || !qsim_col %in% available_cols) {
    available_sbs <- unique(gsub(".*_(\\d+)", "\\1", available_cols[grep("QOBS_", available_cols)]))
    stop("Subbasin ", subbasin_id, " not found. Available: ", paste(available_sbs, collapse = ", "),
         call. = FALSE)
  }

  result <- runoff_data %>%
    select(
      any_of(c("yyyy", "mm", "dd", "hh", "DateTime", "Date")),
      Q_obs = !!qobs_col,
      Q_sim = !!qsim_col
    )

  if (qloc_col %in% available_cols) {
    result$Q_local <- runoff_data[[qloc_col]]
  }

  attr(result, "subbasin_id") <- subbasin_id
  return(result)
}

#' List Available Subbasins in Dataset
#'
#' Returns a vector of subbasin IDs available in the runoff dataset.
#'
#' @param runoff_data Data frame from read_cosero_runoff()
#' @param quiet Logical. If TRUE, suppresses output message.
#'
#' @return Character vector of subbasin IDs
#'
#' @export
#' @examples
#' \dontrun{
#' # Read outputs
#' outputs <- read_cosero_output("path/to/output")
#'
#' # List available subbasins
#' subbasins <- list_subbasins(outputs$runoff)
#' # Returns: c("0001", "0002", "0003", ...)
#' }
list_subbasins <- function(runoff_data, quiet = FALSE) {
  discharge_cols <- grep("QOBS_", colnames(runoff_data), value = TRUE)
  subbasin_ids <- sort(gsub("QOBS_(\\d+)", "\\1", discharge_cols))

  if (!quiet) cat("Available subbasins:", paste(subbasin_ids, collapse = ", "), "\n")
  return(subbasin_ids)
}

#' Extract Parameters for Specific Subbasin
#'
#' Extracts all parameter values for a specific subbasin from parameter dataset.
#'
#' @param param_data Data frame from read_cosero_parameters()
#' @param subbasin_id Subbasin ID as character or numeric
#'
#' @return Data frame with all zones/parameters for the specified subbasin
#'
#' @export
#' @examples
#' \dontrun{
#' # Read parameter file
#' params <- read_cosero_parameters("path/to/para.txt")
#'
#' # Get parameters for subbasin 1
#' sb1_params <- get_subbasin_parameters(params, 1)
#'
#' # Access specific parameter
#' beta_values <- sb1_params$BETA_
#' }
get_subbasin_parameters <- function(param_data, subbasin_id) {
  if (!"NB_" %in% colnames(param_data)) {
    stop("No subbasin ID column (NB_) found in parameter data", call. = FALSE)
  }

  if (is.character(subbasin_id)) {
    subbasin_id <- as.integer(subbasin_id)
  }

  sb_params <- param_data[param_data$NB_ == subbasin_id, ]

  if (nrow(sb_params) == 0) {
    available_sbs <- unique(param_data$NB_)
    stop("Subbasin ", subbasin_id, " not found. Available: ", paste(available_sbs, collapse = ", "),
         call. = FALSE)
  }

  return(sb_params)
}

# 8 New File Readers #####

# Helper: read simple timeseries with header
read_simple_timeseries <- function(file_path, skip = 1, quiet = FALSE) {
  if (!file.exists(file_path)) return(NULL)
  if (!quiet) cat("Reading:", basename(file_path), "\n")

  df <- fread(file_path, skip = skip, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)
  return(df)
}

# 8.1 Wrappers for existing readers
#' Read COSERO.runoff from output directory
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_runoff <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.runoff")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.runoff\n")
  read_cosero_runoff(file, quiet = quiet)
}

#' Read COSERO.prec from output directory
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_precipitation <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.prec")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.prec\n")

  # Use read_cosero_precipitation for consistency
  read_cosero_precipitation(file, quiet = quiet)
}

#' Read COSERO.plus from output directory
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_plus <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.plus")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.plus\n")

  df <- fread(file, skip = 1, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)
  return(df)
}

#' Read COSERO.plus1 from output directory (converts cumulative to timestep values)
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_plus1 <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.plus1")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.plus1\n")

  df <- fread(file, skip = 1, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)

  # Convert cumulative sums to timestep values
  sum_cols <- grep("_SUM_", colnames(df), value = TRUE)

  # Get month and day columns by index to avoid duplicate "mm" column issue
  # (COSERO.plus1 has: yyyy mm dd hh mm where mm appears twice)
  month_col <- if (ncol(df) >= 2) df[[2]] else NULL
  day_col <- if (ncol(df) >= 3) df[[3]] else NULL

  for (col in sum_cols) {
    new_col <- gsub("_SUM_", "_", col)
    vals <- df[[col]]

    # Calculate differences (vectorized: today - yesterday)
    timestep_vals <- c(vals[1], diff(vals))

    # Handle water year resets (September 1)
    # When COSERO resets on Sept 1, diff() gives negative values
    # In that case, use the cumulative value directly (it's the current day's value after reset)
    if (!is.null(month_col) && !is.null(day_col)) {
      sept_1_indices <- which(month_col == 9 & day_col == 1)
      if (length(sept_1_indices) > 0) {
        # For Sept 1: if diff is negative (reset occurred), use cumulative value
        for (idx in sept_1_indices) {
          if (idx > 1 && timestep_vals[idx] < 0) {
            timestep_vals[idx] <- vals[idx]
          }
        }
      }
    }

    df[[new_col]] <- timestep_vals
  }

  return(df)
}

#' Read statistics.txt from output directory
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_statistics <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "statistics.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: statistics.txt\n")
  read_cosero_statistics(file, quiet = quiet)
}

#' Read topology.txt from output directory
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_topology <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "topology.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: topology.txt\n")

  lines <- readLines(file)
  header_idx <- grep("^\\s*NZ\\s+NB\\s+IZ", lines)[1]
  if (is.na(header_idx)) return(NULL)

  blank_idx <- which(nchar(trimws(lines[(header_idx+1):length(lines)])) == 0)[1]
  nrows <- if (!is.na(blank_idx)) blank_idx - 1 else -1

  df <- fread(file, skip = header_idx - 1, nrows = nrows, header = TRUE, data.table = FALSE)
  return(df)
}

# 8.2 OUTPUTTYPE 2 readers
#' Read var_glac.txt from output directory (OUTPUTTYPE >= 2)
#' @param output_dir Path to COSERO output directory
#' @param quiet Suppress messages
#' @return Data frame or NULL if file not found
#' @export
read_var_glac <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "var_glac.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: var_glac.txt\n")

  df <- fread(file, skip = 0, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)
  return(df)
}

read_var_met <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "var_MET.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: var_MET.txt\n")

  df <- fread(file, skip = 0, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)
  return(df)
}

# 8.3 OUTPUTTYPE 3 readers
read_monitor <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "monitor.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: monitor.txt\n")

  df <- fread(file, header = TRUE, data.table = FALSE)
  return(df)
}

read_monitor_subbasins <- function(output_dir, quiet = FALSE) {
  pattern <- "^monitor_sb\\d+\\.txt$"
  files <- list.files(output_dir, pattern = pattern, full.names = TRUE)

  if (length(files) == 0) return(NULL)
  if (!quiet) cat("Reading:", length(files), "monitor_sb files\n")

  result <- list()
  for (f in files) {
    sb_id <- gsub(".*monitor_sb(\\d+)\\.txt", "\\1", basename(f))
    df <- fread(f, skip = 0, header = TRUE, data.table = FALSE)
    df <- add_datetime_columns(df)

    # Remove subbasin suffix from column names
    colnames(df) <- gsub(paste0("_", sb_id), "", colnames(df))
    result[[sb_id]] <- df
  }

  return(result)
}

read_rundepth <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "rundepth.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: rundepth.txt\n")

  lines <- readLines(file)

  # Find the line with time columns and first basin (line 3: yyyy mm dd hh mm QOBS_0001 QSIM_0001)
  time_header_idx <- grep("^\\s*yyyy\\s+mm\\s+dd\\s+hh\\s+mm", lines)[1]
  if (is.na(time_header_idx)) return(NULL)

  # Find first line with numeric data (starts with year)
  data_start <- grep("^\\s*20\\d\\d", lines)[1]
  if (is.na(data_start)) return(NULL)

  # Reconstruct single-line header from multi-line header
  # Line 3 has: yyyy mm dd hh mm QOBS_0001 QSIM_0001
  # Lines 4+ have: QOBS_XXXX QSIM_XXXX (one pair per line)
  header_lines <- lines[time_header_idx:(data_start-1)]
  # Remove empty lines
  header_lines <- header_lines[nchar(trimws(header_lines)) > 0]

  # Extract all column names by splitting each header line and combining
  col_names <- c()
  for (line in header_lines) {
    parts <- unlist(strsplit(trimws(line), "\\s+"))
    col_names <- c(col_names, parts)
  }

  # Parse data
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  data_matrix <- parse_cosero_data_lines(data_lines, col_names)
  df <- create_cosero_dataframe(data_matrix, col_names)
  df <- add_datetime_columns(df)

  return(df)
}

read_longterm_annual <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "long-term_annual_means.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: long-term_annual_means.txt\n")

  lines <- readLines(file)

  # Find the header line (contains "SC_FLAG:")
  header_idx <- grep("SC_FLAG:", lines)[1]
  if (is.na(header_idx)) return(NULL)

  # Parse header line - split by whitespace
  header_line <- trimws(lines[header_idx])
  all_parts <- unlist(strsplit(header_line, "\\s+"))

  # Skip "SC_FLAG:" and "1" at the beginning, keep rest as column names
  # Format is: "SC_FLAG: 1 subbasin TQOBS Qobs ..."
  # We want: "project" "subbasin" "TQOBS" "Qobs" ...
  col_names <- c("project", all_parts[3:length(all_parts)])

  # Data starts on next line
  data_start <- header_idx + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  # Parse data using existing function
  data_matrix <- parse_cosero_data_lines(data_lines, col_names)
  df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(df) <- col_names

  # Remove project column (first column)
  df <- df[, -1, drop = FALSE]

  # Convert all remaining columns to numeric
  for (i in 1:ncol(df)) {
    df[[i]] <- as.numeric(df[[i]])
  }

  return(df)
}

read_longterm_seasonal <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "long-term_seasonal_means.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: long-term_seasonal_means.txt\n")

  lines <- readLines(file)

  # Find the header line (contains "SC_FLAG:")
  header_idx <- grep("SC_FLAG:", lines)[1]
  if (is.na(header_idx)) return(NULL)

  # Parse header line - split by whitespace
  header_line <- trimws(lines[header_idx])
  all_parts <- unlist(strsplit(header_line, "\\s+"))

  # Skip "SC_FLAG:" and "1" at the beginning, keep rest as column names
  # Format is: "SC_FLAG: 1 subbasin season TQOBS Qobs ..."
  # We want: "project" "subbasin" "season" "TQOBS" "Qobs" ...
  col_names <- c("project", all_parts[3:length(all_parts)])

  # Data starts on next line
  data_start <- header_idx + 1
  data_lines <- lines[data_start:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  # Parse data using existing function
  data_matrix <- parse_cosero_data_lines(data_lines, col_names)
  df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
  colnames(df) <- col_names

  # Remove project column (first column)
  df <- df[, -1, drop = FALSE]

  # Convert all remaining columns to numeric
  for (i in 1:ncol(df)) {
    df[[i]] <- as.numeric(df[[i]])
  }

  return(df)
}
