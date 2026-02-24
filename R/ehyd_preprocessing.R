#' eHYD Discharge Data Preprocessing for COSERO
#'
#' @description
#' Extraction of eHYD daily discharge data (Q-Tagesmittel) for 
#' COSERO hydrological zones. This function parses raw CSV files downloaded from 
#' the Austrian Hydrographic Service (eHYD) and converts them into the standard 
#' COSERO QOBS format.
#'
#' @details
#' The Austrian Hydrographic Service (eHYD) provides daily discharge data in CSV 
#' format, typically named \code{Q-Tagesmittel-\{HZB-Nummer\}.csv}. This function
#' maps these files to specific COSERO subbasins using the \code{gauge_to_nb} argument.
#'
#' The ``Gauge ID'' in this function maps directly to the \code{HZB-Nummer}. 
#' The function extracts this number directly from the filenames using regular 
#' expressions, allowing you to simply drop the downloaded files into a folder 
#' and provide the mapping vector.
#'
#' **Missing Data:** If a gauge has "Luecke" (the German keyword for gap) or any 
#' missing days within the combined period across all files, it is automatically 
#' filled with the \code{na_value} (defaulting to \code{-0.01} as required by COSERO).
#'
#' **Date Period:** If \code{start_date} and \code{end_date} are not explicitly 
#' provided, the function will determine the maximum possible overlapping period by 
#' taking the absolute earliest start date and absolute latest end date across all 
#' processed files. Shorter records will be padded with \code{na_value}.
#'
#' @param input_dir Directory containing raw eHYD \code{Q-Tagesmittel-*.csv} files.
#' @param output_file Path to the resulting COSERO QOBS text file.
#' @param gauge_to_nb A named numeric or integer vector mapping the eHYD Gauge ID 
#'   (\code{HZB-Nummer}) to the COSERO subbasin ID (\code{NB}). The function automatically extracts
#'   the \code{HZB-Nummer} from the raw filenames (e.g., \code{Q-Tagesmittel-210864.csv} -> \code{210864}).
#'   Example: \code{c("210864" = 1, "210880" = 2)}.
#' @param catchment_name String for the output file's first line (Title). Default is 
#'   \code{"COSERO"}.
#' @param start_date Optional start date in \code{YYYY-MM-DD} format or Date object. 
#'   If \code{NULL}, calculated as the absolute minimum start date across all files.
#' @param end_date Optional end date in \code{YYYY-MM-DD} format or Date object. 
#'   If \code{NULL}, calculated as the absolute maximum end date across all files.
#' @param na_value Missing value replacement code for COSERO. Default is \code{-0.01}.
#'
#' @return Invisibly returns the generated QOBS data frame in wide format.
#'
#' @examples
#' \dontrun{
#' # Map eHYD HZB-Nummer to COSERO Subbasins
#' gauge_mapping <- c(
#'   "210864" = 1,  # Gusswerk / Salza
#'   "210880" = 2,  # Weichselboden / Radmerbach
#'   "210898" = 3   # Wildalpen / Salza
#' )
#'
#' # Full period (auto-calculated)
#' write_ehyd_qobs(
#'   input_dir = "D:/Data/eHYD/raw",
#'   output_file = "D:/COSERO_project/input/Qobs.txt",
#'   gauge_to_nb = gauge_mapping,
#'   catchment_name = "COSERO-Wildalpen"
#' )
#'
#' # Specific time period
#' write_ehyd_qobs(
#'   input_dir = "D:/Data/eHYD/raw",
#'   output_file = "D:/COSERO_project/input/Qobs.txt",
#'   gauge_to_nb = gauge_mapping,
#'   start_date = "1991-01-01",
#'   end_date = "2024-12-31"
#' )
#' }
#'
#' @references
#' BML (Federal Ministry of Agriculture, Forestry, Regions and Water Management)
#' eHYD - WebGIS Application of the Hydrographic Service in Austria.
#' \href{https://ehyd.gv.at/}{eHYD Portal}
#'
#' @importFrom data.table fread rbindlist setcolorder
#' @importFrom utils read.table write.table
#' @export
write_ehyd_qobs <- function(
  input_dir, 
  output_file, 
  gauge_to_nb,
  catchment_name = "COSERO", 
  start_date = NULL, 
  end_date = NULL, 
  na_value = -0.01
) {
  
  # Validate directories and inputs
  if (!dir.exists(input_dir)) stop("input_dir does not exist: ", input_dir, call. = FALSE)
  if (!is.numeric(gauge_to_nb) && !is.integer(gauge_to_nb)) {
    stop("gauge_to_nb must be a named numeric/integer vector (e.g., c('210864' = 1))", call. = FALSE)
  }
  if (is.null(names(gauge_to_nb))) {
    stop("gauge_to_nb must have names corresponding to specific HZB-Nummern.", call. = FALSE)
  }
  
  if (!is.null(start_date)) start_date <- as.Date(start_date)
  if (!is.null(end_date)) end_date <- as.Date(end_date)
  if (!is.null(start_date) && !is.null(end_date) && start_date > end_date) {
    stop("start_date cannot be after end_date", call. = FALSE)
  }
  
  # Find eHYD Q files
  raw_files <- list.files(input_dir, pattern = "Q-Tagesmittel-.*\\.csv$", full.names = TRUE)
  if (length(raw_files) == 0) {
    stop("No eHYD files (Q-Tagesmittel-*.csv) found in: ", input_dir, call. = FALSE)
  }
  
  # Internal function to read a single eHYD file robustly
  read_ehyd_file <- function(filepath, gauge_id) {
    # eHYD uses latin1 and specific formatting (values start after 'Werte:')
    lines <- readLines(filepath, encoding = "latin1", warn = FALSE)
    data_start <- which(grepl("^Werte:", lines)) + 1
    
    if (length(data_start) == 0 || is.na(data_start) || data_start > length(lines)) {
      warning("Could not find 'Werte:' marker in ", basename(filepath), call. = FALSE)
      return(NULL)
    }
    
    data_lines <- lines[data_start:length(lines)]
    data_lines <- data_lines[nchar(trimws(data_lines)) > 0]
    
    # Parse date and value
    parsed <- lapply(data_lines, function(line) {
      parts <- strsplit(line, ";")[[1]]
      if (length(parts) >= 2) {
        datetime_str <- trimws(parts[1])
        value_str <- trimws(parts[2])
        
        # Parse datetime (DD.MM.YYYY HH:MM:SS)
        date <- as.Date(datetime_str, format = "%d.%m.%Y")
        if (is.na(date)) return(NULL)
        
        # Check for gap keywords ("Luecke")
        if (grepl("^L", value_str) || grepl("L\\u00fccke", value_str, ignore.case = TRUE)) {
          value <- NA_real_
        } else {
          # European format: comma as decimal
          value <- as.numeric(gsub(",", ".", value_str))
        }
        
        return(data.frame(date = date, Q = value, gauge_id = gauge_id, stringsAsFactors = FALSE))
      }
      return(NULL)
    })
    
    do.call(rbind, parsed)
  }
  
  # Process files and collect data
  all_data_list <- list()
  cat("Reading eHYD raw files...\n")
  
  for (f in raw_files) {
    gauge_id <- gsub(".*Q-Tagesmittel-([0-9]+)\\.csv", "\\1", basename(f))
    
    # Only read if this gauge is in the mapping target
    if (gauge_id %in% names(gauge_to_nb)) {
      df <- read_ehyd_file(f, gauge_id)
      if (!is.null(df) && nrow(df) > 0) {
        all_data_list[[gauge_id]] <- df
      }
    }
  }
  
  if (length(all_data_list) == 0) {
    stop("None of the specified gauges in gauge_to_nb were found in the parsed eHYD files.", call. = FALSE)
  }
  
  # Combine to a single data frame
  all_data <- data.table::rbindlist(all_data_list)
  
  # Determine sequence dates
  if (is.null(start_date)) start_date <- min(all_data$date, na.rm = TRUE)
  if (is.null(end_date)) end_date <- max(all_data$date, na.rm = TRUE)
  
  cat(sprintf("Date range established: %s to %s\n", start_date, end_date))
  
  # Create master date skeleton
  dates_seq <- seq(start_date, end_date, by = "day")
  qobs <- data.frame(
    date = dates_seq,
    YYYY = as.integer(format(dates_seq, "%Y")),
    MM   = as.integer(format(dates_seq, "%m")),
    DD   = as.integer(format(dates_seq, "%d")),
    hh   = 0L,
    mm   = 0L
  )
  
  n_nb <- max(gauge_to_nb)
  
  # Initialize QOBS columns with NA values
  for (nb in 1:n_nb) {
    qobs[[paste0("QOBS_", nb)]] <- na_value
  }
  
  # Populate columns
  for (gauge_id in names(gauge_to_nb)) {
    if (gauge_id %in% names(all_data_list)) {
      nb <- gauge_to_nb[[gauge_id]]
      col_name <- paste0("QOBS_", nb)
      
      df <- all_data_list[[gauge_id]]
      
      # Match dates and assign
      idx <- match(df$date, qobs$date)
      valid <- !is.na(idx)
      
      # For values that are within the skeleton period, apply the values (and NA handling)
      target_rows <- idx[valid]
      available_Q <- df$Q[valid]
      
      # Replace explicit eHYD NAs with na_value
      available_Q[is.na(available_Q)] <- na_value
      
      qobs[[col_name]][target_rows] <- round(available_Q, 2)
    }
  }
  
  # Prepare output formatting
  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  header_lines <- paste0("QOBS_", 1:n_nb)
  separator <- paste(rep("#", 80), collapse = "")
  
  data_cols <- c("YYYY", "MM", "DD", "hh", "mm", paste0("QOBS_", 1:n_nb))
  data_df <- qobs[, data_cols]
  
  # Write file
  cat("Writing COSERO QOBS file...\n")
  con <- file(output_file, "w")
  on.exit(close(con), add = TRUE)
  
  writeLines(catchment_name, con)
  writeLines(header_lines, con)
  writeLines(separator, con)
  
  write.table(data_df, con, sep = "    ", row.names = FALSE, col.names = FALSE, quote = FALSE)
  
  cat(sprintf("Success: Written %d subbasins and %d time steps to %s\n", n_nb, nrow(data_df), output_file))
  
  # Summary output
  for (nb in 1:n_nb) {
    col_name <- paste0("QOBS_", nb)
    n_valid <- sum(qobs[[col_name]] != na_value)
    n_missing <- sum(qobs[[col_name]] == na_value)
    cat(sprintf("  Subbasin NB %d: %d valid, %d missing\n", nb, n_valid, n_missing))
  }
  
  invisible(data_df)
}
