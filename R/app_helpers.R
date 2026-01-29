# COSERO Visualization App - Helper Functions
# Data processing and plotting functions for Shiny app
# Author: COSERO R Interface
# Date: 2025-10-28

#' @importFrom dplyr filter mutate select arrange group_by summarise
#' @importFrom lubridate ymd_hm year month day hour minute
#' @importFrom plotly plot_ly add_trace layout
NULL

library(dplyr)
library(lubridate)
library(plotly)

# Color Configuration ####
# Centralized color definitions for all plots
# Colors can be specified as named colors or HEX codes (e.g., "#FF5733")

# Discharge plot colors
COLORS_DISCHARGE <- list(
  Q_obs = "#0000FF",    # blue
  Q_sim = "#FF0000"     # red
)

# Precipitation plot colors (includes ET for seasonality panel)
COLORS_PRECIPITATION <- list(
  PRAIN = "#0066CC",    # blue
  PSNOW = "#000000",    # black
  ETA = "#FF8C00"       # orange (same as ETAGEB_cum for consistency)
)

# Runoff components plot colors
COLORS_RUNOFF <- list(
  QAB123 = "#669D31",      # green
  QAB23 = "#FF8C00",       # orange
  QAB3 = "#1F0322",        # dark purple
  QAB = "#800080",         # purple
  glacmelt = "#F0BCD4"     # pink
)

# Water balance plot colors
COLORS_WATER_BALANCE <- list(
  BW0 = "#e0afa0",         # tan
  BW3 = "#1F0322",         # dark purple
  SWW = "#00FFFF",         # cyan
  P_cum = "#247BA0",       # blue
  ETAGEB_cum = "#FF8C00",  # orange
  QABGEB_cum = "#669D31"   # green
)

# Display names for precipitation variables
NAMES_PRECIPITATION <- list(
  PRAIN = "Rain",
  PSNOW = "Snow",
  ETA = "Evapotranspiration"
)

# Display names for runoff components
NAMES_RUNOFF <- list(
  QAB123 = "Total Runoff (QAB123)",
  QAB23 = "Subsurface Runoff (QAB23)",
  QAB3 = "Baseflow (QAB3)",
  QAB = "Total Runoff",
  glacmelt = "Glacier Melt"
)

# Display names for water balance variables
NAMES_WATER_BALANCE <- list(
  BW0 = "Soil Moisture Top (BW0)",
  BW3 = "Groundwater state (BW3)",
  SWW = "Snow Water Equivalent",
  P_cum = "Cumulative Precipitation",
  ETAGEB_cum = "Cumulative ET",
  QABGEB_cum = "Cumulative Runoff"
)

# 1. Data Processing Functions ####

#' Prepare Subbasin Data for Plotting
#'
#' Extracts and filters all relevant data for a specific subbasin from COSERO outputs.
#' Prepares data for discharge, precipitation, runoff components, water balance, and glacier panels.
#'
#' @param result Output list from read_cosero_output()
#' @param subbasin_id Subbasin ID as character (e.g., "0001") or numeric (e.g., 1)
#' @param date_range Optional vector of c(start_date, end_date) as Date objects for filtering
#'
#' @return List containing:
#'   \item{discharge}{Discharge data (Q_obs, Q_sim) for the subbasin}
#'   \item{precipitation}{Precipitation data (PRAIN, PSNOW) for the subbasin}
#'   \item{runoff_components}{Runoff components (QAB*) for the subbasin}
#'   \item{water_balance}{Water balance variables for the subbasin}
#'   \item{glacier}{Glacier melt data for the subbasin}
#'   \item{statistics}{Performance statistics for the subbasin}
#'   \item{subbasin_id}{Formatted subbasin ID}
#'
#' @export
#' @examples
#' \dontrun{
#' # Read COSERO outputs
#' outputs <- read_cosero_output("path/to/output")
#'
#' # Prepare data for subbasin 1
#' sb_data <- prepare_subbasin_data(outputs, 1)
#'
#' # Prepare with date filtering
#' sb_data <- prepare_subbasin_data(outputs, "0001",
#'                                   date_range = c(as.Date("2015-01-01"),
#'                                                  as.Date("2015-12-31")))
#' }
prepare_subbasin_data <- function(result, subbasin_id, date_range = NULL) {
  # Format subbasin ID
  if (is.numeric(subbasin_id)) {
    sb_id <- sprintf("%04d", subbasin_id)
  } else {
    sb_id <- subbasin_id
  }

  # Extract discharge data
  discharge <- get_subbasin_data(result$runoff, sb_id)

  # Filter by date range if provided (use bracket notation to avoid duplicate names issue)
  if (!is.null(date_range) && !is.null(discharge) && "Date" %in% colnames(discharge)) {
    discharge <- discharge[discharge$Date >= date_range[1] & discharge$Date <= date_range[2], ]
  }

  # Extract precipitation data for selected subbasin
  precipitation <- result$precipitation
  if (!is.null(precipitation)) {
    # Find columns for this subbasin
    prain_col <- grep(paste0("PRAIN.*_", sb_id), colnames(precipitation), value = TRUE)[1]
    psnow_col <- grep(paste0("PSNOW.*_", sb_id), colnames(precipitation), value = TRUE)[1]

    if (!is.na(prain_col) && !is.na(psnow_col)) {
      # Select time columns and precipitation columns for this subbasin
      time_cols <- c("yyyy", "mm", "dd", "hh", "DateTime", "Date")
      keep_cols <- c(time_cols[time_cols %in% colnames(precipitation)], prain_col, psnow_col)
      precipitation <- precipitation[, keep_cols, drop = FALSE]

      # Rename to standard names
      names(precipitation)[names(precipitation) == prain_col] <- "PRAIN"
      names(precipitation)[names(precipitation) == psnow_col] <- "PSNOW"
    }

    # Filter by date range
    if (!is.null(date_range) && "Date" %in% colnames(precipitation)) {
      precipitation <- precipitation[precipitation$Date >= date_range[1] & precipitation$Date <= date_range[2], ]
    }
  }

  # Extract runoff components for selected subbasin
  runoff_comp <- result$runoff_components
  if (!is.null(runoff_comp)) {
    # Find QAB columns for this subbasin
    qab_cols <- grep(paste0("QAB.*_", sb_id), colnames(runoff_comp), value = TRUE)

    if (length(qab_cols) > 0) {
      # Select time columns and QAB columns for this subbasin
      time_cols <- c("yyyy", "mm", "dd", "hh", "DateTime", "Date")
      keep_cols <- c(time_cols[time_cols %in% colnames(runoff_comp)], qab_cols)
      runoff_comp <- runoff_comp[, keep_cols, drop = FALSE]

      # Rename to standard names (remove GEB and subbasin suffix)
      for (col in qab_cols) {
        new_name <- gsub("GEB.*", "", col)
        names(runoff_comp)[names(runoff_comp) == col] <- new_name
      }
    }

    # Filter by date range
    if (!is.null(date_range) && "Date" %in% colnames(runoff_comp)) {
      runoff_comp <- runoff_comp[runoff_comp$Date >= date_range[1] & runoff_comp$Date <= date_range[2], ]
    }
  }

  # Extract water balance for selected subbasin
  water_balance <- result$water_balance
  if (!is.null(water_balance)) {
    # Find columns for this subbasin (look for _0001 pattern)
    wb_cols <- grep(paste0("_", sb_id, "$"), colnames(water_balance), value = TRUE)

    if (length(wb_cols) > 0) {
      # Select time columns and water balance columns for this subbasin
      time_cols <- c("yyyy", "mm", "dd", "hh", "DateTime", "Date")
      keep_cols <- c(time_cols[time_cols %in% colnames(water_balance)], wb_cols)
      water_balance <- water_balance[, keep_cols, drop = FALSE]

      # Rename to standard names (remove GEB and subbasin suffix)
      # Keep _SUM_ indicator to distinguish cumulative from timestep values
      for (col in wb_cols) {
        # Check if this is a cumulative column
        if (grepl("_SUM_", col)) {
          # For cumulative: PGEB_SUM_0001 -> P_SUM
          # Remove GEB prefix, preserve _SUM, remove subbasin suffix
          new_name <- sub("GEB_SUM_", "_SUM_", col)  # PGEB_SUM_0001 -> P_SUM_0001
          new_name <- sub(paste0("_", sb_id, "$"), "", new_name)  # P_SUM_0001 -> P_SUM
        } else {
          # For timestep: PGEB_0001 -> P, ETAGEB_0001 -> ETAGEB, BW0GEB_0001 -> BW0
          # Remove GEB and subbasin suffix
          new_name <- sub("GEB_", "_", col)  # BW0GEB_0001 -> BW0_0001, PGEB_0001 -> P_0001
          new_name <- sub(paste0("_", sb_id, "$"), "", new_name)  # BW0_0001 -> BW0, P_0001 -> P
        }
        names(water_balance)[names(water_balance) == col] <- new_name
      }
    }

    # Filter by date range
    if (!is.null(date_range) && "Date" %in% colnames(water_balance)) {
      water_balance <- water_balance[water_balance$Date >= date_range[1] & water_balance$Date <= date_range[2], ]
    }
  }

  # Extract glacier data for selected subbasin
  glacier <- result$glacier
  if (!is.null(glacier)) {
    # Find glacmelt column for this subbasin
    glacmelt_col <- grep(paste0("glacmelt.*_", sb_id), colnames(glacier), value = TRUE)[1]

    if (!is.na(glacmelt_col)) {
      # Select time columns and glacmelt column for this subbasin
      time_cols <- c("yyyy", "mm", "dd", "hh", "DateTime", "Date")
      keep_cols <- c(time_cols[time_cols %in% colnames(glacier)], glacmelt_col)
      glacier <- glacier[, keep_cols, drop = FALSE]

      # Rename to standard name
      names(glacier)[names(glacier) == glacmelt_col] <- "glacmelt"
    }

    # Filter by date range
    if (!is.null(date_range) && "Date" %in% colnames(glacier)) {
      glacier <- glacier[glacier$Date >= date_range[1] & glacier$Date <= date_range[2], ]
    }
  }

  # Get statistics for this subbasin
  stats <- NULL
  if (!is.null(result$statistics)) {
    stats <- result$statistics %>%
      filter(sb == sb_id | sb == as.numeric(sb_id))
  }

  return(list(
    discharge = discharge,
    precipitation = precipitation,
    runoff_components = runoff_comp,
    water_balance = water_balance,
    glacier = glacier,
    statistics = stats,
    subbasin_id = sb_id
  ))
}

#' Calculate cumulative sums for water balance variables
#' @param data Data frame with timestep values
#' @param vars Character vector of variable names to accumulate
#' @return Data frame with added cumulative columns (suffix _cum)
#' @details Cumulative sums reset on October 1st of each year (water year)
calculate_cumulative <- function(data, vars) {
  if (is.null(data) || nrow(data) == 0) return(data)

  # Check if Date column exists
  if (!"Date" %in% colnames(data)) {
    warning("Date column not found, calculating simple cumsum without water year reset")
    for (var in vars) {
      if (var %in% colnames(data)) {
        cum_var <- paste0(var, "_cum")
        data[[cum_var]] <- cumsum(replace(data[[var]], is.na(data[[var]]), 0))
      }
    }
    return(data)
  }

  for (var in vars) {
    if (var %in% colnames(data)) {
      cum_var <- paste0(var, "_cum")

      # Diagnostic: Check if variable values look suspiciously high (might already be cumulative)
      # Use different thresholds for different variables
      var_mean <- mean(data[[var]], na.rm = TRUE)
      var_max <- max(data[[var]], na.rm = TRUE)

      # Set threshold based on variable type
      threshold <- if (var %in% c("P", "PRAIN", "PSNOW")) {
        500  # Precipitation: 500mm/day is extreme but possible
      } else if (var %in% c("ET", "ETA", "ETP", "ETVEG", "ETSOIL")) {
        20   # Evapotranspiration: >20mm/day is suspicious
      } else if (var %in% c("Q", "QAB", "QGES")) {
        200  # Runoff in mm: 200mm/day would be extreme
      } else {
        100  # Default threshold for other variables
      }

      if (var_max > threshold) {
        warning(paste0("Variable ", var, " has suspiciously high values (mean: ",
                      round(var_mean, 2), ", max: ", round(var_max, 2),
                      "). These might already be cumulative values!"),
                call. = FALSE)
      }

      # Extract month and year from Date column
      data$month <- month(data$Date)
      data$year <- year(data$Date)

      # Create water year: Oct 1 - Sep 30
      # If month >= 10 (Oct-Dec), water year = calendar year + 1
      # If month < 10 (Jan-Sep), water year = calendar year
      data$water_year <- ifelse(data$month >= 10, data$year + 1, data$year)

      # Calculate cumulative sum with reset for each water year
      data[[cum_var]] <- ave(
        replace(data[[var]], is.na(data[[var]]), 0),
        data$water_year,
        FUN = cumsum
      )

      # Clean up temporary columns
      data$month <- NULL
      data$year <- NULL
      data$water_year <- NULL
    }
  }

  return(data)
}

# 2. Plotting Functions ####

#' Create Discharge Time Series Plot
#'
#' Creates an interactive plotly time series showing observed and simulated discharge.
#'
#' @param data Data frame with Date, Q_obs, and Q_sim columns
#'
#' @return Plotly object
#' @export
#' @examples
#' \dontrun{
#' # Prepare subbasin data
#' sb_data <- prepare_subbasin_data(outputs, 1)
#'
#' # Create discharge plot
#' p <- plot_discharge(sb_data$discharge)
#' p
#' }
plot_discharge <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty(text = "No discharge data available"))
  }

  p <- plot_ly(data, x = ~Date) %>%
    add_trace(
      y = ~Q_obs,
      name = "Q Obs",
      type = "scatter",
      mode = "lines",
      line = list(color = COLORS_DISCHARGE$Q_obs, width = 1.5),
      hovertemplate = paste0(
        "<b>Date:</b> %{x|%Y-%m-%d}<br>",
        "<b>Q Obs:</b> %{y:.2f} m³/s<br>",
        "<extra></extra>"
      )
    ) %>%
    add_trace(
      y = ~Q_sim,
      name = "Q Sim",
      type = "scatter",
      mode = "lines",
      line = list(color = COLORS_DISCHARGE$Q_sim, width = 1.5),
      hovertemplate = paste0(
        "<b>Date:</b> %{x|%Y-%m-%d}<br>",
        "<b>Q Sim:</b> %{y:.2f} m³/s<br>",
        "<extra></extra>"
      )
    ) %>%
    layout(
      title = list(text = "Discharge", font = list(size = 14)),
      xaxis = list(title = list(text = "", font = list(size = 12)), tickfont = list(size = 11)),
      yaxis = list(title = list(text = "Discharge (m³/s)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Create Precipitation Time Series Plot
#'
#' Creates an interactive plotly chart showing rainfall and snowfall.
#' Can show as stacked bars (combined) or separate bars.
#'
#' @param data Data frame with Date, PRAIN, and PSNOW columns
#' @param separate Logical. If TRUE, shows rain and snow as separate bars; if FALSE, stacked.
#'
#' @return Plotly object
#' @export
#' @examples
#' \dontrun{
#' # Create precipitation plot (stacked)
#' p <- plot_precipitation(sb_data$precipitation)
#'
#' # Create precipitation plot (separate)
#' p <- plot_precipitation(sb_data$precipitation, separate = TRUE)
#' }
plot_precipitation <- function(data, separate = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty(text = "No precipitation data available"))
  }

  if (separate) {
    # Separate PRAIN and PSNOW
    p <- plot_ly(data, x = ~Date) %>%
      add_trace(
        y = ~PRAIN,
        name = "Rain",
        type = "bar",
        marker = list(color = COLORS_PRECIPITATION$PRAIN),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Rain:</b> %{y:.1f} mm<br>",
          "<extra></extra>"
        )
      ) %>%
      add_trace(
        y = ~PSNOW,
        name = "Snow",
        type = "bar",
        marker = list(color = COLORS_PRECIPITATION$PSNOW),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Snow:</b> %{y:.1f} mm<br>",
          "<extra></extra>"
        )
      )
  } else {
    # Combined stacked bars
    p <- plot_ly(data, x = ~Date) %>%
      add_trace(
        y = ~PRAIN,
        name = "Rain",
        type = "bar",
        marker = list(color = COLORS_PRECIPITATION$PRAIN),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Rain:</b> %{y:.1f} mm<br>",
          "<extra></extra>"
        )
      ) %>%
      add_trace(
        y = ~PSNOW,
        name = "Snow",
        type = "bar",
        marker = list(color = COLORS_PRECIPITATION$PSNOW),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>Snow:</b> %{y:.1f} mm<br>",
          "<extra></extra>"
        )
      ) %>%
      layout(barmode = "stack")
  }

  p <- p %>%
    layout(
      title = list(text = "Precipitation", font = list(size = 14)),
      xaxis = list(title = list(text = "", font = list(size = 12)), tickfont = list(size = 11)),
      yaxis = list(title = list(text = "Precipitation (mm)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Create Runoff Components Time Series Plot
#'
#' Creates an interactive plotly chart showing different runoff components (surface, subsurface, baseflow).
#' Optionally includes glacier melt contribution.
#'
#' @param data Data frame with Date and QAB* columns (runoff components)
#' @param glacier_data Optional data frame with glacmelt column
#'
#' @return Plotly object
#' @export
#' @examples
#' \dontrun{
#' # Create runoff components plot
#' p <- plot_runoff_components(sb_data$runoff_components)
#'
#' # Include glacier melt
#' p <- plot_runoff_components(sb_data$runoff_components, sb_data$glacier)
#' }
plot_runoff_components <- function(data, glacier_data = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty(text = "No runoff components data available"))
  }

  # Check which QAB columns are available
  qab_cols <- grep("^QAB", colnames(data), value = TRUE)

  if (length(qab_cols) == 0) {
    return(plotly_empty(text = "No QAB columns found"))
  }

  p <- plot_ly(data, x = ~Date)

  # Add QAB traces using centralized color configuration
  for (col in qab_cols) {
    display_name <- if (col %in% names(NAMES_RUNOFF)) NAMES_RUNOFF[[col]] else col
    color <- if (col %in% names(COLORS_RUNOFF)) COLORS_RUNOFF[[col]] else "gray"

    p <- p %>%
      add_trace(
        y = data[[col]],
        name = display_name,
        type = "scatter",
        mode = "lines",
        line = list(color = color, width = 1.5),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>", display_name, ":</b> %{y:.2f} mm<br>",
          "<extra></extra>"
        )
      )
  }

  # Add glacmelt if glacier data is provided
  if (!is.null(glacier_data) && "glacmelt" %in% colnames(glacier_data)) {
    display_name <- NAMES_RUNOFF$glacmelt
    color <- COLORS_RUNOFF$glacmelt

    p <- p %>%
      add_trace(
        data = glacier_data,
        x = ~Date,
        y = ~glacmelt,
        name = display_name,
        type = "scatter",
        mode = "lines",
        line = list(color = color, width = 1.5),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>", display_name, ":</b> %{y:.2f} mm<br>",
          "<extra></extra>"
        )
      )
  }

  p <- p %>%
    layout(
      title = list(text = "Runoff Components", font = list(size = 14)),
      xaxis = list(title = list(text = "", font = list(size = 12)), tickfont = list(size = 11)),
      yaxis = list(title = list(text = "Runoff (mm)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Create Water Balance Time Series Plot
#'
#' Creates an interactive plotly chart showing water balance components including
#' storage states (soil moisture, groundwater, snow) and cumulative fluxes (P, ET, Q).
#'
#' @param data Data frame with Date and water balance columns
#' @param selected_vars Character vector of variable names to plot (default: c("BW0", "BW3", "SWW"))
#' @param show_cumulative Logical. If TRUE, calculates and shows cumulative flux variables.
#'
#' @return Plotly object
#' @export
#' @examples
#' \dontrun{
#' # Create water balance plot with default variables
#' p <- plot_water_balance(sb_data$water_balance)
#'
#' # Plot specific variables
#' p <- plot_water_balance(sb_data$water_balance,
#'                          selected_vars = c("BW0", "SWW", "P_cum"))
#'
#' # Without cumulative variables
#' p <- plot_water_balance(sb_data$water_balance, show_cumulative = FALSE)
#' }
plot_water_balance <- function(data, selected_vars = NULL, show_cumulative = TRUE) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty(text = "No water balance data available"))
  }

  # Default variables if none selected
  if (is.null(selected_vars) || length(selected_vars) == 0) {
    selected_vars <- c("BW0", "BW3", "SWW")
  }

  # Calculate cumulative if requested
  if (show_cumulative) {
    # Note: After renaming in prepare_subbasin_data, columns are:
    # ETA (not ETAGEB) and QAB (not QABGEB)
    flux_vars <- c("P", "ETA", "QAB")
    data <- calculate_cumulative(data, flux_vars)

    # Rename cumulative columns to match UI expectations
    if ("P_cum" %in% colnames(data)) {
      # P_cum is already correct
    }
    if ("ETA_cum" %in% colnames(data)) {
      names(data)[names(data) == "ETA_cum"] <- "ETAGEB_cum"
    }
    if ("QAB_cum" %in% colnames(data)) {
      names(data)[names(data) == "QAB_cum"] <- "QABGEB_cum"
    }
  }

  # Filter to available variables
  available_vars <- selected_vars[selected_vars %in% colnames(data)]

  if (length(available_vars) == 0) {
    return(plotly_empty(text = "Selected variables not available in data"))
  }

  # Use centralized color and name configuration
  p <- plot_ly(data, x = ~Date)

  for (var in available_vars) {
    color <- if (var %in% names(COLORS_WATER_BALANCE)) COLORS_WATER_BALANCE[[var]] else "gray"
    display_name <- if (var %in% names(NAMES_WATER_BALANCE)) NAMES_WATER_BALANCE[[var]] else var

    p <- p %>%
      add_trace(
        y = data[[var]],
        name = display_name,
        type = "scatter",
        mode = "lines",
        line = list(color = color, width = 1.5),
        hovertemplate = paste0(
          "<b>Date:</b> %{x|%Y-%m-%d}<br>",
          "<b>", display_name, ":</b> %{y:.1f} mm<br>",
          "<extra></extra>"
        )
      )
  }

  p <- p %>%
    layout(
      title = list(text = "Water Balance / States", font = list(size = 14)),
      xaxis = list(title = list(text = "", font = list(size = 12)), tickfont = list(size = 11)),
      yaxis = list(title = list(text = "Storage / Cumulative Flux (mm)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Create empty plotly with message
#' @param text Message to display
#' @return Plotly object
plotly_empty <- function(text = "No data available") {
  plot_ly() %>%
    layout(
      title = list(text = text, x = 0.5, xanchor = "center"),
      xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
      yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
    )
}

# 3. Seasonality Functions ####

#' Detect time step of input data
#' @param data Data frame with Date column
#' @return Character: "hourly", "daily", or "monthly"
detect_timestep <- function(data) {
  if (is.null(data) || nrow(data) < 2) {
    warning("Not enough data to detect timestep, assuming daily")
    return("daily")
  }

  if (!"Date" %in% colnames(data)) {
    warning("Date column not found, assuming daily")
    return("daily")
  }

  # Calculate time differences between consecutive rows
  time_diffs <- as.numeric(diff(data$Date), units = "days")

  # Remove any zero or negative differences (data errors)
  time_diffs <- time_diffs[time_diffs > 0]

  if (length(time_diffs) == 0) {
    warning("Could not calculate time differences, assuming daily")
    return("daily")
  }

  # Calculate median difference to handle irregular data
  median_diff <- median(time_diffs, na.rm = TRUE)

  # Determine timestep based on median difference
  if (median_diff < 0.5) {
    # Less than 12 hours = hourly data
    return("hourly")
  } else if (median_diff >= 0.5 && median_diff < 2) {
    # Between 12 hours and 2 days = daily data
    return("daily")
  } else {
    # Greater than 2 days = monthly data
    return("monthly")
  }
}

#' Prepare seasonality data (monthly aggregation)
#' @param subbasin_data Output from prepare_subbasin_data()
#' @param spinup_timesteps Number of initial timesteps to exclude
#' @return List with monthly aggregated data for each panel
#' @details Fluxes (P, ET, Q) are summed, states (BW0, BW3, SWW) are averaged
#' @export
prepare_seasonality_data <- function(subbasin_data, spinup_timesteps = 0) {
  result <- list()

  # Helper function to aggregate timestep data to daily sums (for hourly data)
  # This is needed because hourly precipitation/ET needs to be summed to daily first
  aggregate_to_daily <- function(data, value_cols, method = "sum") {
    if (is.null(data) || nrow(data) == 0) return(NULL)
    if (!"Date" %in% colnames(data)) return(NULL)

    # Group by Date and aggregate
    if (method == "sum") {
      daily <- data %>%
        group_by(Date) %>%
        summarise(across(all_of(value_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop")
    } else {  # mean
      daily <- data %>%
        group_by(Date) %>%
        summarise(across(all_of(value_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop")
    }

    return(as.data.frame(daily))
  }

  # Helper function to calculate monthly aggregation from daily/hourly data
  # For fluxes: daily values -> monthly SUMS (mm/month)
  # For states: daily values -> monthly MEANS (mm)
  calc_monthly_aggregation <- function(data, value_cols, spinup = 0, method = "sum") {
    if (is.null(data) || nrow(data) == 0) return(NULL)
    if (!"Date" %in% colnames(data)) return(NULL)

    # Detect timestep
    timestep <- detect_timestep(data)

    # Remove spin-up period
    if (spinup > 0 && nrow(data) > spinup) {
      data <- data[(spinup + 1):nrow(data), ]
    }

    # For hourly data with fluxes, first aggregate to daily sums
    if (timestep == "hourly" && method == "sum") {
      data <- aggregate_to_daily(data, value_cols, method = "sum")
      if (is.null(data)) return(NULL)
      timestep <- "daily"  # Now we have daily data
    }

    # Add month and year columns for grouping
    data$month <- month(data$Date)
    data$year <- year(data$Date)

    # Calculate monthly aggregation
    if (method == "sum") {
      # For fluxes: sum daily values to get monthly total (mm/month)
      # First group by year and month to get monthly sums for each year
      monthly_by_year <- data %>%
        group_by(year, month) %>%
        summarise(across(all_of(value_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop")

      # Then calculate mean across years for each month (mean monthly sum)
      monthly <- monthly_by_year %>%
        group_by(month) %>%
        summarise(across(all_of(value_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        arrange(month)
    } else {  # method == "mean"
      # For states: average all values within each month across all years
      monthly <- data %>%
        group_by(month) %>%
        summarise(across(all_of(value_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
        arrange(month)
    }

    return(as.data.frame(monthly))
  }

  # Panel 1: Discharge - use mean (already in m³/s)
  if (!is.null(subbasin_data$discharge)) {
    result$discharge <- calc_monthly_aggregation(
      subbasin_data$discharge,
      c("Q_obs", "Q_sim"),
      spinup_timesteps,
      method = "mean"
    )
  }

  # Panel 2: Precipitation + ET - use SUM to get mm/month
  # First handle precipitation
  if (!is.null(subbasin_data$precipitation)) {
    result$precipitation <- calc_monthly_aggregation(
      subbasin_data$precipitation,
      c("PRAIN", "PSNOW"),
      spinup_timesteps,
      method = "sum"
    )
  }

  # Add ET to precipitation panel
  if (!is.null(subbasin_data$water_balance) && "ETA" %in% colnames(subbasin_data$water_balance)) {
    et_monthly <- calc_monthly_aggregation(
      subbasin_data$water_balance,
      "ETA",
      spinup_timesteps,
      method = "sum"
    )
    # Merge ET with precipitation data
    if (!is.null(result$precipitation) && !is.null(et_monthly)) {
      result$precipitation <- merge(result$precipitation, et_monthly, by = "month")
    } else if (!is.null(et_monthly)) {
      # If no precipitation data, create precipitation result with just ET
      result$precipitation <- et_monthly
    }
  }

  # Panel 3: Runoff components - use SUM to get mm/month
  if (!is.null(subbasin_data$runoff_components)) {
    qab_cols <- grep("^QAB", colnames(subbasin_data$runoff_components), value = TRUE)
    if (length(qab_cols) > 0) {
      result$runoff_components <- calc_monthly_aggregation(
        subbasin_data$runoff_components,
        qab_cols,
        spinup_timesteps,
        method = "sum"
      )
    }
  }

  # Add glacmelt to runoff components if available
  if (!is.null(subbasin_data$glacier) && "glacmelt" %in% colnames(subbasin_data$glacier)) {
    glacmelt_monthly <- calc_monthly_aggregation(
      subbasin_data$glacier,
      "glacmelt",
      spinup_timesteps,
      method = "sum"
    )
    if (!is.null(result$runoff_components) && !is.null(glacmelt_monthly)) {
      result$runoff_components <- merge(result$runoff_components, glacmelt_monthly, by = "month")
    }
  }

  # Panel 4: Water balance STATES only (BW0, BW3, SWW) - use MEAN
  # States represent storage at a point in time, so we average them
  if (!is.null(subbasin_data$water_balance)) {
    state_cols <- c("BW0", "BW3", "SWW")
    available_cols <- state_cols[state_cols %in% colnames(subbasin_data$water_balance)]
    if (length(available_cols) > 0) {
      result$water_balance <- calc_monthly_aggregation(
        subbasin_data$water_balance,
        available_cols,
        spinup_timesteps,
        method = "mean"
      )
    }
  }

  return(result)
}

#' Plot seasonality - Discharge
#' @param monthly_data Monthly aggregated discharge data
#' @return A plotly object
#' @export
plot_seasonality_discharge <- function(monthly_data) {
  if (is.null(monthly_data) || nrow(monthly_data) == 0) {
    return(plotly_empty(text = "No discharge data available"))
  }

  p <- plot_ly(monthly_data, x = ~month) %>%
    add_trace(
      y = ~Q_obs,
      name = "Q Obs",
      type = "scatter",
      mode = "lines+markers",
      line = list(color = COLORS_DISCHARGE$Q_obs, width = 2),
      marker = list(size = 6, color = COLORS_DISCHARGE$Q_obs)
    ) %>%
    add_trace(
      y = ~Q_sim,
      name = "Q Sim",
      type = "scatter",
      mode = "lines+markers",
      line = list(color = COLORS_DISCHARGE$Q_sim, width = 2),
      marker = list(size = 6, color = COLORS_DISCHARGE$Q_sim)
    ) %>%
    layout(
      title = list(text = "Mean Monthly Discharge", font = list(size = 14)),
      xaxis = list(
        title = list(text = "", font = list(size = 12)),
        tickmode = "array",
        tickvals = 1:12,
        ticktext = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        tickfont = list(size = 11)
      ),
      yaxis = list(title = list(text = "Discharge (m³/s)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Plot seasonality - Precipitation + ET
#' @param monthly_data Monthly aggregated precipitation data
#' @return A plotly object
#' @export
plot_seasonality_precipitation <- function(monthly_data) {
  if (is.null(monthly_data) || nrow(monthly_data) == 0) {
    return(plotly_empty(text = "No precipitation data available"))
  }

  # Determine which variables are available
  available_vars <- colnames(monthly_data)[colnames(monthly_data) != "month"]

  p <- plot_ly(monthly_data, x = ~month)

  # Add traces for each available variable
  for (var in available_vars) {
    if (var %in% names(COLORS_PRECIPITATION)) {
      color <- COLORS_PRECIPITATION[[var]]
      display_name <- if (var %in% names(NAMES_PRECIPITATION)) NAMES_PRECIPITATION[[var]] else var

      p <- p %>%
        add_trace(
          y = monthly_data[[var]],
          name = display_name,
          type = "scatter",
          mode = "lines+markers",
          line = list(color = color, width = 2),
          marker = list(size = 6, color = color),
          hovertemplate = paste0(
            "<b>", display_name, ":</b> %{y:.1f} mm/month<br>",
            "<extra></extra>"
          )
        )
    }
  }

  p <- p %>%
    layout(
      title = list(text = "Mean Monthly Precipitation + ET", font = list(size = 14)),
      xaxis = list(
        title = list(text = "", font = list(size = 12)),
        tickmode = "array",
        tickvals = 1:12,
        ticktext = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        tickfont = list(size = 11)
      ),
      yaxis = list(title = list(text = "Flux (mm/month)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Plot seasonality - Runoff Components
#' @param monthly_data Monthly aggregated runoff components data
#' @return A plotly object
#' @export
plot_seasonality_runoff <- function(monthly_data) {
  if (is.null(monthly_data) || nrow(monthly_data) == 0) {
    return(plotly_empty(text = "No runoff components data available"))
  }

  qab_cols <- grep("^QAB|glacmelt", colnames(monthly_data), value = TRUE)
  if (length(qab_cols) == 0) {
    return(plotly_empty(text = "No QAB columns found"))
  }

  p <- plot_ly(monthly_data, x = ~month)

  for (col in qab_cols) {
    display_name <- if (col %in% names(NAMES_RUNOFF)) NAMES_RUNOFF[[col]] else col
    color <- if (col %in% names(COLORS_RUNOFF)) COLORS_RUNOFF[[col]] else "gray"

    p <- p %>%
      add_trace(
        y = monthly_data[[col]],
        name = display_name,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = color, width = 2),
        marker = list(size = 6, color = color),
        hovertemplate = paste0(
          "<b>", display_name, ":</b> %{y:.1f} mm/month<br>",
          "<extra></extra>"
        )
      )
  }

  p <- p %>%
    layout(
      title = list(text = "Mean Monthly Runoff Components", font = list(size = 14)),
      xaxis = list(
        title = list(text = "", font = list(size = 12)),
        tickmode = "array",
        tickvals = 1:12,
        ticktext = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        tickfont = list(size = 11)
      ),
      yaxis = list(title = list(text = "Runoff (mm/month)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

#' Plot seasonality - Water Balance States
#' @param monthly_data Monthly aggregated data with state variables
#' @param selected_vars Optional vector of variables to plot (default: all available states)
#' @details Shows only state variables (BW0, BW3, SWW) as monthly means
#' @return A plotly object
#' @export
plot_seasonality_water_balance <- function(monthly_data, selected_vars = NULL) {
  if (is.null(monthly_data) || nrow(monthly_data) == 0) {
    return(plotly_empty(text = "No water balance data available"))
  }

  # Default variables: show all available state variables
  if (is.null(selected_vars) || length(selected_vars) == 0) {
    # Only include state variables (BW0, BW3, SWW)
    state_vars <- c("BW0", "BW3", "SWW")
    selected_vars <- state_vars[state_vars %in% colnames(monthly_data)]
  }

  # Filter to available variables
  available_vars <- selected_vars[selected_vars %in% colnames(monthly_data)]

  if (length(available_vars) == 0) {
    return(plotly_empty(text = "No state variables available"))
  }

  p <- plot_ly(monthly_data, x = ~month)

  for (var in available_vars) {
    color <- if (var %in% names(COLORS_WATER_BALANCE)) COLORS_WATER_BALANCE[[var]] else "gray"
    display_name <- if (var %in% names(NAMES_WATER_BALANCE)) NAMES_WATER_BALANCE[[var]] else var

    p <- p %>%
      add_trace(
        y = monthly_data[[var]],
        name = display_name,
        type = "scatter",
        mode = "lines+markers",
        line = list(color = color, width = 2),
        marker = list(size = 6, color = color),
        hovertemplate = paste0(
          "<b>", display_name, ":</b> %{y:.1f} mm<br>",
          "<extra></extra>"
        )
      )
  }

  p <- p %>%
    layout(
      title = list(text = "Mean Monthly Water Balance States", font = list(size = 14)),
      xaxis = list(
        title = list(text = "", font = list(size = 12)),
        tickmode = "array",
        tickvals = 1:12,
        ticktext = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                     "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
        tickfont = list(size = 11)
      ),
      yaxis = list(title = list(text = "Storage (mm)", font = list(size = 12)), tickfont = list(size = 11)),
      hovermode = "x unified",
      font = list(size = 11),
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2, font = list(size = 10))
    )

  return(p)
}

# 4. Export Functions ####

#' Export plot as PNG
#' @param plot Plotly object
#' @param filename Output filename
#' @param width Width in pixels
#' @param height Height in pixels
#' @return Logical indicating success
#' @export
export_plot_png <- function(plot, filename, width = 1200, height = 400) {
  tryCatch({
    # Try kaleido first (preferred method for plotly)
    if (requireNamespace("kaleido", quietly = TRUE)) {
      plotly::save_image(plot, filename, width = width, height = height)
      return(TRUE)
    }

    # Fallback to webshot (requires webshot package and phantomjs)
    if (requireNamespace("webshot", quietly = TRUE)) {
      # Use temporary directory to avoid permission issues
      temp_dir <- tempdir()
      temp_html <- file.path(temp_dir, paste0("temp_", Sys.getpid(), ".html"))

      htmlwidgets::saveWidget(plot, temp_html, selfcontained = TRUE)
      webshot::webshot(temp_html, filename, vwidth = width, vheight = height)
      if (file.exists(temp_html)) file.remove(temp_html)

      return(TRUE)
    }

    # If none of the above work, throw an informative error
    stop("PNG export requires: kaleido package (recommended) or webshot + phantomjs.\n",
         "Install with: install.packages('kaleido')")

  }, error = function(e) {
    warning("Could not export PNG: ", e$message,
            "\nPlease install kaleido: install.packages('kaleido')")
    return(FALSE)
  })
}

# COSERO Run Configuration Helpers ####

#' Normalize file path from user input
#' Handles both forward and backslashes, removes quotes
#'
#' This function is designed to handle common copy-paste scenarios from Windows Explorer:
#' - Paths with forward slashes: "D:/Projects/COSERO"
#' - Paths with backslashes: "D:\Projects\COSERO"
#' - Paths with quotes: '"D:\Projects\COSERO"' (from Shift+Right-click -> Copy as path)
#' - Mixed slashes: "D:/Projects\COSERO"
#' - Leading/trailing whitespace
#'
#' @param path Raw path string from user input
#' @return Normalized path string with consistent separators
#' @export
#' @examples
#' normalize_path_input('"D:\\Projects\\COSERO"')  # Returns: D:/Projects/COSERO
#' normalize_path_input("D:/Projects/COSERO")      # Returns: D:/Projects/COSERO
normalize_path_input <- function(path) {
  if (is.null(path) || !is.character(path) || length(path) == 0) {
    return("")
  }

  # Remove leading/trailing whitespace
  path <- trimws(path)

  # Remove surrounding quotes (single or double) that might come from copy-paste
  # Windows "Copy as path" adds quotes like: "D:\Projects\COSERO"
  path <- gsub('^["\']|["\']$', '', path)

  # Normalize slashes to the system's preferred separator
  # This converts both / and \ to the correct separator for the OS
  # On Windows, this standardizes to forward slashes for R compatibility
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  return(path)
}

#' Read COSERO defaults.txt with safe fallbacks
#' @param project_path Path to COSERO project directory
#' @return List of defaults with fallback values
#' @export
read_cosero_defaults_safe <- function(project_path) {
  # Default fallback values
  defaults <- list(
    PROJECTINFO = "COSERO_Project",
    DATAFILE = "data.txt",
    PARAFILE = "para.txt",
    IKL = 1,
    NCLASS = 1,
    OUTPUTTYPE = 3,
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

  # Try to read from defaults.txt if it exists
  if (!is.null(project_path) && nzchar(project_path)) {
    defaults_file <- file.path(project_path, "input", "defaults.txt")

    if (file.exists(defaults_file)) {
      tryCatch({
        # Source the core run file to get read_defaults function
        if (exists("read_defaults")) {
          file_defaults <- read_defaults(defaults_file)
          # Merge with fallback defaults (file values override fallbacks)
          defaults <- modifyList(defaults, file_defaults)
        }
      }, error = function(e) {
        # Silently use fallback defaults if reading fails
      })
    }
  }

  return(defaults)
}

#' Parse COSERO date string to components
#' @param date_string String in format "YYYY MM DD HH MM" or vector c(YYYY, MM, DD, HH, MM)
#' @return List with date, hour, minute
#' @export
parse_cosero_date <- function(date_string) {
  # Handle both string and vector formats
  if (is.character(date_string) && length(date_string) == 1) {
    # String format: "2010 1 1 0 0"
    parts <- strsplit(as.character(date_string), "\\s+")[[1]]
  } else if (is.numeric(date_string) || is.character(date_string)) {
    # Vector format: c("2010", "1", "1", "0", "0") or c(2010, 1, 1, 0, 0)
    parts <- as.character(date_string)
  } else {
    parts <- NULL
  }

  if (!is.null(parts) && length(parts) == 5) {
    # Ensure parts are numeric
    parts_num <- as.integer(parts)

    # Create date with proper zero-padding for month and day
    date_str <- sprintf("%d-%02d-%02d", parts_num[1], parts_num[2], parts_num[3])

    list(
      date = as.Date(date_str),
      hour = parts_num[4],
      minute = parts_num[5]
    )
  } else {
    # Fallback to current date
    list(
      date = Sys.Date(),
      hour = 0,
      minute = 0
    )
  }
}

#' Format date components to COSERO date string
#' @param date Date object
#' @param hour Hour (0-23)
#' @param minute Minute (0-59)
#' @return String in format "YYYY MM DD HH MM"
#' @export
format_cosero_date <- function(date, hour = 0, minute = 0) {
  sprintf("%d %d %d %d %d",
          year(date), month(date), day(date),
          as.integer(hour), as.integer(minute))
}
