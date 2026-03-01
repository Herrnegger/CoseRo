# Title: Time Series Tab Module
# Description: Sidebar controls, value boxes, combined hydrograph, and diagnostic tabs
# Author/Architect: Mathew Herrnegger
# Coding: Claude/PI
# Date: 2026-02-28

# ── UI ────────────────────────────────────────────────────────────────────────
timeseries_ui <- function(id) {
  ns <- NS(id)

  tagList(
  tags$style(HTML("
    .ts-compact .card { margin-bottom: 0.4rem; }
    .ts-compact .card-header { padding: 0.25rem 0.5rem; font-size: 0.85rem; }
    .ts-compact .card-body { padding: 0.4rem 0.5rem; }
    .ts-compact .form-group { margin-bottom: 0.35rem; }
    .ts-compact .form-label, .ts-compact label { font-size: 0.82rem; margin-bottom: 0.1rem; }
    .ts-compact .form-control, .ts-compact .form-select { padding: 0.2rem 0.4rem; font-size: 0.82rem; min-height: unset; }
    .ts-compact .bslib-value-box { min-height: unset; padding: 0.2rem 0; }
    .ts-compact .value-box-title { font-size: 0.75rem; }
    .ts-compact .value-box-value { font-size: 1.15rem; line-height: 1.2; }
    .ts-compact .value-box-showcase { padding: 0.2rem; max-width: 45px; }
    .ts-compact .value-box-area { padding: 0.3rem 0.6rem; }
    .ts-compact .bslib-grid { gap: 0.3rem; }
  ")),
  div(class = "ts-compact",
  layout_sidebar(
    # ── Sidebar: Controls ───────────────────────────────────────────────────
    sidebar = sidebar(
      title = "Time Series Controls",
      width = 300,
      accordion(
        open = c("Data Source", "View Settings"),

        accordion_panel(
          "Data Source",
          icon = icon("database"),
          textInput(ns("output_dir"), "Output Directory", value = ""),
          div(
            class = "d-flex justify-content-between align-items-center mb-2",
            checkboxInput(ns("use_cache"), "Use cache", value = TRUE, width = "auto"),
            actionButton(ns("load_data"), "Load Data",
                         class = "btn-primary btn-sm", icon = icon("download"))
          ),
          verbatimTextOutput(ns("status_text"))
        ),

        accordion_panel(
          "View Settings",
          icon = icon("sliders-h"),
          uiOutput(ns("subbasin_selector_ui")),
          div(
            class = "d-flex justify-content-between mb-3",
            actionButton(ns("prev_sb"), "< Prev", class = "btn-secondary btn-sm"),
            actionButton(ns("next_sb"), "Next >", class = "btn-secondary btn-sm")
          ),
          uiOutput(ns("date_range_ui")),
          actionButton(ns("reset_zoom"), "Reset Zoom",
                       class = "btn-light btn-sm w-100 mt-2", icon = icon("search-minus"))
        )
      )
    ),

    # ── Main Panel ──────────────────────────────────────────────────────────

    # Tier 1: Value Boxes
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      value_box(
        title = "Subbasin NSE",
        value = textOutput(ns("vb_nse")),
        showcase = icon("chart-line"),
        theme = "secondary",
        class = "py-1"
      ),
      value_box(
        title = "KGE (Kling-Gupta)",
        value = textOutput(ns("vb_kge")),
        showcase = icon("droplet"),
        theme = "success",
        class = "py-1"
      ),
      value_box(
        title = "BETA (Volume Bias)",
        value = textOutput(ns("vb_beta")),
        showcase = icon("balance-scale"),
        theme = "info",
        class = "py-1"
      )
    ),

    # Tier 2: Combined Hydrograph (Precip + Discharge)
    card(
      full_screen = TRUE,
      card_header("Hydrograph & Precipitation", class = "bg-light py-1"),
      card_body(
        class = "p-0",
        plotlyOutput(ns("hydrograph_plot"), height = "420px")
      )
    ),

    # Tier 3: Diagnostic Tabs
    navset_card_underline(
      full_screen = TRUE,
      id = ns("diagnostic_tabs"),
      nav_panel(
        title = "Runoff Components",
        icon = icon("water"),
        plotlyOutput(ns("components_plot"), height = "350px")
      ),
      nav_panel(
        title = "Water Balance / States",
        icon = icon("chart-area"),
        plotlyOutput(ns("water_balance_plot"), height = "350px")
      ),
      nav_panel(
        title = "Temperature & ET",
        icon = icon("temperature-half"),
        plotlyOutput(ns("temperature_plot"), height = "350px")
      )
    )
  ) # end layout_sidebar
  ) # end div.ts-compact
  ) # end tagList
}

# ── Server ────────────────────────────────────────────────────────────────────
timeseries_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Local reactive values ─────────────────────────────────────────────
    rv <- reactiveValues(
      subbasin_data   = NULL,
      date_range_full = NULL
    )

    # ── Auto-populate output dir from shared project ──────────────────────
    observe({
      if (!is.null(shared$output_dir)) {
        updateTextInput(session, "output_dir", value = shared$output_dir)
      }
    })

    # Auto-trigger load when project is set via launch_cosero_app()
    observe({
      req(shared$project_dir)
      # Small delay to let the UI render
      shinyjs::delay(1200, {
        shinyjs::click(ns("load_data"))
      })
    }) |> bindEvent(shared$project_dir, once = TRUE)

    # ── Cache validation helper ───────────────────────────────────────────
    is_cache_valid <- function(output_dir, cache_file) {
      if (!file.exists(cache_file)) return(FALSE)
      cache_time <- file.info(cache_file)$mtime
      data_files <- c("COSERO.runoff", "COSERO.prec", "COSERO.plus",
                       "COSERO.plus1", "statistics.txt")
      for (f in data_files) {
        fpath <- file.path(output_dir, f)
        if (file.exists(fpath) && file.info(fpath)$mtime > cache_time) {
          return(FALSE)
        }
      }
      TRUE
    }

    # ── Load Data ─────────────────────────────────────────────────────────
    observeEvent(input$load_data, {
      req(input$output_dir)
      out_dir <- input$output_dir

      withProgress(message = "Loading COSERO data...", value = 0, {
        tryCatch({
          if (!dir.exists(out_dir)) {
            shared$status_message <- paste("Error: Directory not found:", out_dir)
            showNotification("Directory does not exist", type = "error", duration = 6)
            return()
          }

          # Check essential files
          if (!file.exists(file.path(out_dir, "COSERO.runoff"))) {
            shared$status_message <- "Error: COSERO.runoff not found"
            showNotification("COSERO.runoff not found — has the model been run?",
                             type = "error", duration = 8)
            return()
          }

          cache_file <- file.path(out_dir, ".cosero_cache.rds")

          if (input$use_cache && is_cache_valid(out_dir, cache_file)) {
            incProgress(0.5, detail = "Loading from cache...")
            shared$cosero_data <- readRDS(cache_file)
          } else {
            incProgress(0.1, detail = "Detecting output type")
            outputtype <- detect_outputtype(out_dir)

            incProgress(0.1, detail = "Reading runoff")
            runoff <- read_runoff(out_dir, quiet = TRUE)

            incProgress(0.1, detail = "Reading precipitation")
            precipitation <- read_precipitation(out_dir, quiet = TRUE)

            incProgress(0.1, detail = "Reading runoff components")
            runoff_comp <- read_plus(out_dir, quiet = TRUE)

            incProgress(0.1, detail = "Reading water balance")
            water_balance <- read_plus1(out_dir, quiet = TRUE)

            incProgress(0.05, detail = "Reading statistics")
            statistics  <- read_statistics(out_dir, quiet = TRUE)
            topology    <- read_topology(out_dir, quiet = TRUE)

            incProgress(0.05, detail = "Reading additional files")
            glacier     <- NULL
            meteorology <- NULL
            if (outputtype >= 2) {
              glacier     <- read_var_glac(out_dir, quiet = TRUE)
              meteorology <- read_var_met(out_dir, quiet = TRUE)
            }

            shared$cosero_data <- list(
              runoff             = runoff,
              precipitation      = precipitation,
              runoff_components  = runoff_comp,
              water_balance      = water_balance,
              statistics         = statistics,
              topology           = topology,
              glacier            = glacier,
              meteorology        = meteorology,
              metadata = list(
                outputtype = outputtype,
                output_dir = out_dir,
                subbasins  = detect_subbasins(out_dir)
              )
            )

            # Save cache
            if (input$use_cache) {
              incProgress(0.1, detail = "Saving cache...")
              tryCatch(
                saveRDS(shared$cosero_data, cache_file, compress = "xz"),
                error = function(e) warning("Could not save cache: ", e$message)
              )
            }
          }

          # Update shared output_dir
          shared$output_dir <- out_dir

          # Set date range
          if (!is.null(shared$cosero_data$runoff) &&
              "Date" %in% colnames(shared$cosero_data$runoff)) {
            rv$date_range_full <- range(shared$cosero_data$runoff$Date, na.rm = TRUE)
          }

          n_sb <- length(shared$cosero_data$metadata$subbasins)
          otype <- shared$cosero_data$metadata$outputtype
          shared$status_message <- paste0(
            "Loaded! OUTPUTTYPE ", otype, ", ", n_sb, " subbasins."
          )
          showNotification(
            paste0("Data loaded: ", n_sb, " subbasins (type ", otype, ")"),
            type = "message", duration = 3
          )

        }, error = function(e) {
          shared$status_message <- paste("Error:", e$message)
          showNotification(paste("Load error:", e$message), type = "error", duration = 8)
        })
      })
    })

    # ── Subbasin selector UI ──────────────────────────────────────────────
    output$subbasin_selector_ui <- renderUI({
      req(shared$cosero_data)
      sbs <- shared$cosero_data$metadata$subbasins
      if (length(sbs) == 0) return(tags$p("No subbasins found"))
      sel <- shared$selected_subbasin %||% sbs[1]
      selectInput(ns("selected_subbasin"), "Select Subbasin:", choices = sbs, selected = sel)
    })

    # Sync subbasin selection to shared state
    observeEvent(input$selected_subbasin, {
      shared$selected_subbasin <- input$selected_subbasin
    })

    # Update local selector when shared changes from another tab
    observe({
      req(shared$selected_subbasin, input$selected_subbasin)
      if (shared$selected_subbasin != input$selected_subbasin) {
        updateSelectInput(session, "selected_subbasin", selected = shared$selected_subbasin)
      }
    })

    # Prev / Next navigation
    observeEvent(input$prev_sb, {
      req(shared$cosero_data, input$selected_subbasin)
      sbs <- shared$cosero_data$metadata$subbasins
      idx <- which(sbs == input$selected_subbasin)
      if (length(idx) > 0 && idx > 1) {
        updateSelectInput(session, "selected_subbasin", selected = sbs[idx - 1])
      }
    })
    observeEvent(input$next_sb, {
      req(shared$cosero_data, input$selected_subbasin)
      sbs <- shared$cosero_data$metadata$subbasins
      idx <- which(sbs == input$selected_subbasin)
      if (length(idx) > 0 && idx < length(sbs)) {
        updateSelectInput(session, "selected_subbasin", selected = sbs[idx + 1])
      }
    })

    # ── Date range ────────────────────────────────────────────────────────
    output$date_range_ui <- renderUI({
      req(rv$date_range_full)
      dateRangeInput(
        ns("date_range"), "Date Range:",
        start = rv$date_range_full[1],
        end   = rv$date_range_full[2],
        min   = rv$date_range_full[1],
        max   = rv$date_range_full[2]
      )
    })

    observeEvent(input$reset_zoom, {
      req(rv$date_range_full)
      updateDateRangeInput(session, "date_range",
                           start = rv$date_range_full[1],
                           end   = rv$date_range_full[2])
      # Also reset plotly zoom on all charts via proxy
      for (plot_id in c("hydrograph_plot", "components_plot",
                        "water_balance_plot", "temperature_plot")) {
        plotlyProxy(plot_id, session) |>
          plotlyProxyInvoke("relayout", list(
            "xaxis.autorange"  = TRUE, "yaxis.autorange"  = TRUE,
            "xaxis2.autorange" = TRUE, "yaxis2.autorange" = TRUE
          ))
      }
    })

    date_range_debounced <- debounce(reactive(input$date_range), 500)

    # ── Prepare subbasin data ─────────────────────────────────────────────
    subbasin_data <- reactive({
      req(shared$cosero_data, input$selected_subbasin)
      dr <- date_range_debounced()
      sb_data <- prepare_subbasin_data(shared$cosero_data, input$selected_subbasin, dr)

      # Augment with meteorology (temperature)
      sb_data$meteorology <- extract_met_subbasin(
        shared$cosero_data$meteorology, input$selected_subbasin, dr
      )
      sb_data
    })

    observe({ rv$subbasin_data <- subbasin_data() })

    # ── Status text ───────────────────────────────────────────────────────
    output$status_text <- renderText({ shared$status_message })

    # ── Value Boxes ───────────────────────────────────────────────────────
    output$vb_nse <- renderText({
      stats <- subbasin_data()$statistics
      if (is.null(stats) || nrow(stats) == 0 || !"NSE" %in% colnames(stats)) return("—")
      sprintf("%.3f", stats$NSE[1])
    })

    output$vb_kge <- renderText({
      stats <- subbasin_data()$statistics
      if (is.null(stats) || nrow(stats) == 0 || !"KGE" %in% colnames(stats)) return("—")
      sprintf("%.3f", stats$KGE[1])
    })

    output$vb_beta <- renderText({
      stats <- subbasin_data()$statistics
      if (is.null(stats) || nrow(stats) == 0) return("—")
      # BETA = ratio of means (sim/obs)
      beta_col <- intersect(c("BETA", "beta", "Bias"), colnames(stats))
      if (length(beta_col) == 0) {
        # Calculate from discharge data if not in stats
        d <- subbasin_data()$discharge
        if (!is.null(d) && all(c("Q_obs", "Q_sim") %in% colnames(d))) {
          obs_mean <- mean(d$Q_obs, na.rm = TRUE)
          if (!is.na(obs_mean) && obs_mean > 0) {
            return(sprintf("%.3f", mean(d$Q_sim, na.rm = TRUE) / obs_mean))
          }
        }
        return("—")
      }
      sprintf("%.3f", stats[[beta_col[1]]][1])
    })

    # ── Combined Hydrograph (Precip inverted + Discharge) ─────────────────
    output$hydrograph_plot <- renderPlotly({
      sb <- subbasin_data()

      # Build precipitation subplot (inverted, bars pointing down)
      p_precip <- build_precip_subplot(sb$precipitation)

      # Build discharge subplot
      p_discharge <- build_discharge_subplot(sb$discharge)

      # Combine with shared x-axis
      subplot(p_precip, p_discharge,
              nrows = 2, shareX = TRUE,
              heights = c(0.25, 0.75),
              titleY = TRUE) |>
        layout(
          hovermode  = "x unified",
          xaxis  = list(matches = "x2"),
          xaxis2 = list(matches = "x"),
          legend     = list(orientation = "h", x = 0.5, xanchor = "center",
                            y = -0.05, font = list(size = 10)),
          margin     = list(t = 10, b = 40)
        ) |>
        config(scrollZoom = TRUE)
    })

    # ── Runoff Components ─────────────────────────────────────────────────
    output$components_plot <- renderPlotly({
      sb <- subbasin_data()
      p <- plot_runoff_components(sb$runoff_components, glacier_data = sb$glacier)
      p |> layout(title = list(text = ""), margin = list(t = 5)) |>
        config(scrollZoom = TRUE)
    })

    # ── Water Balance / States ────────────────────────────────────────────
    output$water_balance_plot <- renderPlotly({
      sb <- subbasin_data()
      selected_vars <- c("BW0", "BW3", "SWW", "P_cum", "ETAGEB_cum", "QABGEB_cum")
      p <- plot_water_balance(sb$water_balance, selected_vars = selected_vars,
                              show_cumulative = TRUE)
      p |> layout(title = list(text = ""), margin = list(t = 5)) |>
        config(scrollZoom = TRUE)
    })

    # ── Temperature & ET ─────────────────────────────────────────────────
    output$temperature_plot <- renderPlotly({
      sb <- subbasin_data()
      met <- sb$meteorology
      wb  <- sb$water_balance

      p <- build_temperature_et_plot(met, wb)
      p |> layout(title = list(text = ""), margin = list(t = 5)) |>
        config(scrollZoom = TRUE)
    })
  })
}

# ── Helper: Extract meteorology for one subbasin ─────────────────────────────
#' @keywords internal
extract_met_subbasin <- function(meteorology, subbasin_id, date_range = NULL) {
  if (is.null(meteorology)) return(NULL)

  sb_id <- if (is.numeric(subbasin_id)) sprintf("%04d", subbasin_id) else subbasin_id

  # Find temperature column (patterns: TGEB_XXXX, T_XXXX, Temp_XXXX)
  # Anchor with ^ to avoid matching ETPGEB (potential ET) which contains "TGEB"
  temp_col <- grep(
    paste0("^(TGEB|T_|Temp).*_?", sb_id), colnames(meteorology), value = TRUE
  )[1]

  if (is.na(temp_col)) {
    # Fallback: try any column with the subbasin ID that looks like temperature
    temp_col <- grep(paste0("_", sb_id, "$"), colnames(meteorology), value = TRUE)
    # Pick first that isn't a known precip/ET variable
    temp_col <- temp_col[!grepl("^(P|ET|PRAIN|PSNOW|ETP)", temp_col)]
    temp_col <- temp_col[1]
  }
  if (is.na(temp_col) || is.null(temp_col)) return(NULL)

  time_cols <- c("yyyy", "mm", "dd", "hh", "DateTime", "Date")
  keep <- c(time_cols[time_cols %in% colnames(meteorology)], temp_col)
  met <- meteorology[, keep, drop = FALSE]
  names(met)[names(met) == temp_col] <- "Temperature"

  if (!is.null(date_range) && "Date" %in% colnames(met)) {
    met <- met[met$Date >= date_range[1] & met$Date <= date_range[2], ]
  }

  met
}

# ── Helper: Build inverted precipitation sub-plot ────────────────────────────
#' @keywords internal
build_precip_subplot <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(
      plotly_empty(text = "No precipitation data") |>
        layout(yaxis = list(title = "Precip (mm)", autorange = "reversed"))
    )
  }

  p <- plot_ly(data, x = ~Date)

  if ("PRAIN" %in% colnames(data)) {
    p <- p |> add_trace(
      y = ~PRAIN, name = "Rain", type = "bar",
      marker = list(color = "#0066CC"),
      hovertemplate = "<b>Rain:</b> %{y:.1f} mm<extra></extra>"
    )
  }
  if ("PSNOW" %in% colnames(data)) {
    p <- p |> add_trace(
      y = ~PSNOW, name = "Snow", type = "bar",
      marker = list(color = "#333333"),
      hovertemplate = "<b>Snow:</b> %{y:.1f} mm<extra></extra>"
    )
  }

  p |> layout(
    barmode = "stack",
    yaxis = list(
      title = list(text = "Precip (mm)", font = list(size = 11)),
      autorange = "reversed",
      tickfont = list(size = 10)
    ),
    xaxis = list(title = ""),
    showlegend = TRUE
  )
}

# ── Helper: Build discharge sub-plot ─────────────────────────────────────────
#' @keywords internal
build_discharge_subplot <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(plotly_empty(text = "No discharge data"))
  }

  p <- plot_ly(data, x = ~Date)

  if ("Q_obs" %in% colnames(data)) {
    p <- p |> add_trace(
      y = ~Q_obs, name = "Observed", type = "scatter", mode = "lines",
      line = list(color = "#e74c3c", width = 1.2),
      hovertemplate = "<b>Q Obs:</b> %{y:.2f} m\u00b3/s<extra></extra>"
    )
  }
  if ("Q_sim" %in% colnames(data)) {
    p <- p |> add_trace(
      y = ~Q_sim, name = "Simulated", type = "scatter", mode = "lines",
      line = list(color = "#3498db", width = 1.2),
      hovertemplate = "<b>Q Sim:</b> %{y:.2f} m\u00b3/s<extra></extra>"
    )
  }

  p |> layout(
    yaxis = list(
      title = list(text = "Discharge (m\u00b3/s)", font = list(size = 11)),
      tickfont = list(size = 10)
    ),
    xaxis = list(
      title = "",
      tickfont = list(size = 10)
    )
  )
}

# ── Helper: Build temperature + ET subplot ───────────────────────────────────
#' @keywords internal
build_temperature_et_plot <- function(met, wb) {
  has_temp <- !is.null(met) && nrow(met) > 0 && "Temperature" %in% colnames(met)
  has_et   <- !is.null(wb) && nrow(wb) > 0 && "ETA" %in% colnames(wb) && "Date" %in% colnames(wb)

  if (!has_temp && !has_et) {
    return(plotly_empty(text = "No temperature/ET data (requires OUTPUTTYPE \u2265 2)"))
  }

  # Temperature subplot
  p_temp <- if (has_temp) {
    plot_ly(met, x = ~Date) |>
      add_trace(
        y = ~Temperature, name = "Temperature", type = "scatter", mode = "lines",
        line = list(color = "#e67e22", width = 1.2),
        hovertemplate = "<b>Temp:</b> %{y:.1f} \u00b0C<extra></extra>"
      ) |>
      add_trace(
        y = rep(0, nrow(met)), name = "0 \u00b0C", type = "scatter", mode = "lines",
        line = list(color = "#aaaaaa", width = 0.8, dash = "dash"),
        showlegend = FALSE, hoverinfo = "skip"
      ) |>
      layout(
        yaxis = list(
          title = list(text = "Temperature (\u00b0C)", font = list(size = 11)),
          tickfont = list(size = 10)
        ),
        xaxis = list(title = "")
      )
  } else {
    plotly_empty(text = "No temperature data")
  }

  # ET subplot
  p_et <- if (has_et) {
    plot_ly(wb, x = ~Date) |>
      add_trace(
        y = ~ETA, name = "Actual ET", type = "scatter", mode = "lines",
        line = list(color = COLORS_PRECIPITATION$ETA, width = 1.2),
        hovertemplate = "<b>ET:</b> %{y:.1f} mm<extra></extra>"
      ) |>
      layout(
        yaxis = list(
          title = list(text = "ET (mm)", font = list(size = 11)),
          tickfont = list(size = 10)
        ),
        xaxis = list(
          title = "",
          tickfont = list(size = 10)
        )
      )
  } else {
    plotly_empty(text = "No ET data")
  }

  subplot(p_temp, p_et, nrows = 2, shareX = TRUE,
          heights = c(0.5, 0.5), titleY = TRUE) |>
    layout(
      hovermode = "x unified",
      legend = list(orientation = "h", x = 0.5, xanchor = "center",
                    y = -0.08, font = list(size = 10)),
      margin = list(t = 10, b = 40)
    )
}
