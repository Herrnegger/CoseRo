# COSERO Data Readers Interface
# File reading and data processing functions for COSERO outputs
# Author: COSERO R Interface
# Date: 2025-09-25

# 1 Load Libraries #####
library(data.table)
library(dplyr)
library(lubridate)

# 2 Main Output Reader #####
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
        output_data$parameters <- read_cosero_parameters(param_file)
      }
    }
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
detect_outputtype <- function(output_dir) {
  has_monitor <- file.exists(file.path(output_dir, "monitor.txt"))
  has_var_met <- file.exists(file.path(output_dir, "var_MET.txt"))

  if (has_monitor) return(3)
  if (has_var_met) return(2)
  return(1)
}

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
read_cosero_runoff <- function(runoff_file, missing_value = -999.00) {
  if (!file.exists(runoff_file)) {
    stop("Runoff file not found: ", runoff_file)
  }

  cat("Reading COSERO runoff file:", runoff_file, "\n")
  lines <- readLines(runoff_file)

  # Find data section
  runoff_start <- grep("#### Runoff ####", lines)
  if (length(runoff_start) == 0) {
    stop("Could not find '#### Runoff ####' section in file")
  }

  # Find header line
  header_pattern <- grep("yyyy\\s+mm\\s+dd", lines)
  if (length(header_pattern) == 0) {
    stop("Could not find header line with 'yyyy mm dd' pattern")
  }

  header_line_idx <- header_pattern[header_pattern > runoff_start[1]][1]
  if (is.na(header_line_idx)) {
    stop("Could not find header line after runoff section")
  }

  # Parse header
  header <- trimws(lines[header_line_idx])
  col_names <- unlist(strsplit(header, "\\s+"))

  # Get data lines
  data_start_idx <- header_line_idx + 1
  data_lines <- lines[data_start_idx:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  cat("Found", length(col_names), "columns,", length(data_lines), "data lines\n")

  # Parse data
  runoff_data <- parse_cosero_data_lines(data_lines, col_names)
  runoff_data <- create_cosero_dataframe(runoff_data, col_names)

  # Process data
  runoff_data <- add_datetime_columns(runoff_data)
  runoff_data <- handle_missing_values(runoff_data, missing_value)
  runoff_data <- add_subbasin_metadata(runoff_data)

  # Report results
  subbasin_cols <- grep("QOBS_|QSIM_", colnames(runoff_data), value = TRUE)
  n_subbasins <- length(unique(gsub(".*_(\\d+)", "\\1", subbasin_cols)))
  cat("Successfully read", nrow(runoff_data), "rows for", n_subbasins, "subbasins\n")

  return(runoff_data)
}

# 3.2 Data Processing Functions #####
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

handle_missing_values <- function(data, missing_value) {
  discharge_cols <- grep("QOBS_|QSIM_|Qloc_", colnames(data))

  if (length(discharge_cols) > 0) {
    for (col_idx in discharge_cols) {
      data[[col_idx]][data[[col_idx]] == missing_value] <- NA
    }
    cat("Replaced", missing_value, "with NA in", length(discharge_cols), "discharge columns\n")
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
read_cosero_statistics <- function(stats_file) {
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
    stop("Could not find statistics header in file")
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

  cat("Successfully read statistics for", nrow(stats_data), "subbasins\n")
  if (!is.na(spinup_timestep)) {
    cat("Spin-up period:", spinup_timestep, "timesteps\n")
  }
  return(stats_data)
}

# 5 Parameters Reader #####
read_cosero_parameters <- function(param_file, skip_lines = 1) {
  if (!file.exists(param_file)) {
    stop("Parameter file not found: ", param_file)
  }

  lines <- readLines(param_file, warn = FALSE)
  if (length(lines) < 3) {
    stop("Parameter file appears to be too short or empty")
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

  cat("Successfully read parameters for", nrow(param_data), "zones/subbasins\n")
  return(param_data)
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
        numeric_vals[numeric_vals %in% c(-999.0000, 9999.0000, 99999.0000)] <- NA
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
read_cosero_timeseries_file <- function(file_path, data_type = "timeseries") {
  lines <- readLines(file_path)

  header <- trimws(lines[2])
  data_lines <- lines[3:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  col_names <- unlist(strsplit(header, "\\s+"))
  data_matrix <- parse_cosero_data_lines(data_lines, col_names)

  timeseries_data <- create_cosero_dataframe(data_matrix, col_names)
  timeseries_data <- add_datetime_columns(timeseries_data)

  cat("Successfully read", nrow(timeseries_data), "rows of", data_type, "\n")
  return(timeseries_data)
}

read_cosero_precipitation <- function(file_path) {
  lines <- readLines(file_path)

  header <- trimws(lines[1])
  data_lines <- lines[2:length(lines)]
  data_lines <- data_lines[nchar(trimws(data_lines)) > 0]

  col_names <- unlist(strsplit(header, "\\s+"))
  data_matrix <- parse_cosero_data_lines(data_lines, col_names)

  prec_data <- create_cosero_dataframe(data_matrix, col_names)
  prec_data <- add_datetime_columns(prec_data)

  cat("Successfully read", nrow(prec_data), "rows of precipitation data\n")
  return(prec_data)
}

# 7 Data Access Functions #####

# 7.1 Subbasin Data Access #####
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
    stop("Subbasin ", subbasin_id, " not found. Available: ", paste(available_sbs, collapse = ", "))
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

list_subbasins <- function(runoff_data) {
  discharge_cols <- grep("QOBS_", colnames(runoff_data), value = TRUE)
  subbasin_ids <- sort(gsub("QOBS_(\\d+)", "\\1", discharge_cols))

  cat("Available subbasins:", paste(subbasin_ids, collapse = ", "), "\n")
  return(subbasin_ids)
}

get_subbasin_parameters <- function(param_data, subbasin_id) {
  if (!"NB_" %in% colnames(param_data)) {
    stop("No subbasin ID column (NB_) found in parameter data")
  }

  if (is.character(subbasin_id)) {
    subbasin_id <- as.integer(subbasin_id)
  }

  sb_params <- param_data[param_data$NB_ == subbasin_id, ]

  if (nrow(sb_params) == 0) {
    available_sbs <- unique(param_data$NB_)
    stop("Subbasin ", subbasin_id, " not found. Available: ", paste(available_sbs, collapse = ", "))
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
read_runoff <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.runoff")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.runoff\n")
  read_cosero_runoff(file)
}

read_precipitation <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.prec")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.prec\n")

  df <- fread(file, skip = 0, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)
  return(df)
}

read_plus <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "COSERO.plus")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: COSERO.plus\n")

  df <- fread(file, skip = 1, header = TRUE, data.table = FALSE)
  df <- add_datetime_columns(df)
  return(df)
}

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

read_statistics <- function(output_dir, quiet = FALSE) {
  file <- file.path(output_dir, "statistics.txt")
  if (!file.exists(file)) return(NULL)
  if (!quiet) cat("Reading: statistics.txt\n")
  read_cosero_statistics(file)
}

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

cat("COSERO Data Readers Interface loaded\n")
