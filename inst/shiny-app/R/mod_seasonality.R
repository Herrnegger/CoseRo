# Title: Seasonality Tab Module
# Description: Monthly aggregated water balance, discharge, runoff components, and drivers
# Author/Architect: Mathew Herrnegger
# Coding: Claude/PI
# Date: 2026-02-28

# ── UI ────────────────────────────────────────────────────────────────────────
seasonality_ui <- function(id) {
  ns <- NS(id)

  tagList(
  tags$style(HTML("
    .season-compact .card { margin-bottom: 0.4rem; }
    .season-compact .card-header { padding: 0.25rem 0.5rem; font-size: 0.85rem; }
    .season-compact .card-body { padding: 0.4rem 0.5rem; }
    .season-compact .form-group { margin-bottom: 0.35rem; }
    .season-compact .form-label, .season-compact label { font-size: 0.82rem; margin-bottom: 0.1rem; }
    .season-compact .form-control, .season-compact .form-select { padding: 0.2rem 0.4rem; font-size: 0.82rem; min-height: unset; }
    .season-compact .bslib-value-box { min-height: unset; padding: 0.2rem 0; }
    .season-compact .value-box-title { font-size: 0.75rem; }
    .season-compact .value-box-value { font-size: 1.15rem; line-height: 1.2; }
    .season-compact .value-box-showcase { padding: 0.2rem; max-width: 45px; }
    .season-compact .value-box-area { padding: 0.3rem 0.6rem; }
    .season-compact .bslib-grid { gap: 0.3rem; }
  ")),
  div(class = "season-compact",
  layout_sidebar(
    sidebar = sidebar(
      title = "Seasonality Controls",
      width = 300,

      accordion(
        open = c("View Settings"),

        accordion_panel(
          "View Settings",
          icon = icon("sliders-h"),
          uiOutput(ns("subbasin_selector_ui")),
          div(
            class = "d-flex justify-content-between mb-3",
            actionButton(ns("prev_sb"), "< Prev", class = "btn-secondary btn-sm"),
            actionButton(ns("next_sb"), "Next >", class = "btn-secondary btn-sm")
          ),
          checkboxInput(ns("cumsum"), "Show cumulative sums (Jan \u2192 Dec)",
                        value = FALSE),
          tags$p(class = "text-muted small mt-2",
                 icon("info-circle"),
                 "Spinup period is excluded from seasonality calculations.")
        )
      )
    ),

    # ── Main Panel ──────────────────────────────────────────────────────────

    # Tier 1: Value boxes
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      value_box(
        title = "Mean Annual Precip",
        value = textOutput(ns("vb_precip")),
        showcase = icon("cloud-rain"),
        theme = "secondary", class = "py-1"
      ),
      value_box(
        title = "Mean Annual Runoff",
        value = textOutput(ns("vb_runoff")),
        showcase = icon("water"),
        theme = "success", class = "py-1"
      ),
      value_box(
        title = "Runoff Ratio",
        value = textOutput(ns("vb_ratio")),
        showcase = icon("percent"),
        theme = "info", class = "py-1"
      )
    ),

    # Tier 2: Primary — Monthly Water Balance (Precip + Discharge)
    card(
      full_screen = TRUE,
      card_header("Monthly Water Balance", class = "bg-light py-1"),
      card_body(
        class = "p-0",
        plotlyOutput(ns("monthly_wb_plot"), height = "400px")
      )
    ),

    # Tier 3: Secondary — tabbed diagnostics
    navset_card_underline(
      full_screen = TRUE,
      id = ns("season_diag_tabs"),
      nav_panel(
        title = "Runoff Components",
        icon = icon("water"),
        plotlyOutput(ns("season_runoff_plot"), height = "350px")
      ),
      nav_panel(
        title = "Water Balance / States",
        icon = icon("chart-area"),
        plotlyOutput(ns("season_states_plot"), height = "350px")
      ),
      nav_panel(
        title = "Drivers (Temp & ET)",
        icon = icon("temperature-half"),
        plotlyOutput(ns("season_drivers_plot"), height = "350px")
      )
    )
  ) # end layout_sidebar
  ) # end div.season-compact
  ) # end tagList
}

# ── Server ────────────────────────────────────────────────────────────────────
seasonality_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Subbasin selector ─────────────────────────────────────────────────
    output$subbasin_selector_ui <- renderUI({
      req(shared$cosero_data)
      sbs <- shared$cosero_data$metadata$subbasins
      if (length(sbs) == 0) return(tags$p("No subbasins"))
      sel <- shared$selected_subbasin %||% sbs[1]
      selectInput(ns("selected_subbasin"), "Select Subbasin:", choices = sbs, selected = sel)
    })

    observeEvent(input$selected_subbasin, {
      shared$selected_subbasin <- input$selected_subbasin
    })
    observe({
      req(shared$selected_subbasin, input$selected_subbasin)
      if (shared$selected_subbasin != input$selected_subbasin) {
        updateSelectInput(session, "selected_subbasin", selected = shared$selected_subbasin)
      }
    })

    observeEvent(input$prev_sb, {
      req(shared$cosero_data, input$selected_subbasin)
      sbs <- shared$cosero_data$metadata$subbasins
      idx <- which(sbs == input$selected_subbasin)
      if (length(idx) > 0 && idx > 1)
        updateSelectInput(session, "selected_subbasin", selected = sbs[idx - 1])
    })
    observeEvent(input$next_sb, {
      req(shared$cosero_data, input$selected_subbasin)
      sbs <- shared$cosero_data$metadata$subbasins
      idx <- which(sbs == input$selected_subbasin)
      if (length(idx) > 0 && idx < length(sbs))
        updateSelectInput(session, "selected_subbasin", selected = sbs[idx + 1])
    })

    # ── Prepare subbasin + seasonality data ───────────────────────────────
    subbasin_data <- reactive({
      req(shared$cosero_data, input$selected_subbasin)
      sb <- prepare_subbasin_data(shared$cosero_data, input$selected_subbasin)
      sb$meteorology <- extract_met_subbasin(
        shared$cosero_data$meteorology, input$selected_subbasin
      )
      sb
    })

    seasonality <- reactive({
      req(subbasin_data(), shared$cosero_data$statistics)
      spinup <- attr(shared$cosero_data$statistics, "spinup_timestep")
      if (is.null(spinup) || is.na(spinup)) spinup <- 0
      prepare_seasonality_data(subbasin_data(), spinup)
    })

    # ── Value Boxes ───────────────────────────────────────────────────────
    output$vb_precip <- renderText({
      sd <- seasonality()
      if (is.null(sd$precipitation)) return("—")
      # Sum monthly P values for mean annual
      pcols <- intersect(c("PRAIN", "PSNOW"), colnames(sd$precipitation))
      if (length(pcols) == 0) return("—")
      total <- sum(sd$precipitation[, pcols], na.rm = TRUE)
      paste0(round(total, 0), " mm/yr")
    })

    output$vb_runoff <- renderText({
      sd <- seasonality()
      if (is.null(sd$discharge)) return("—")
      # Sum monthly Q_sim or Q_obs if available
      qcol <- intersect(c("Q_sim", "Q_obs"), colnames(sd$discharge))
      if (length(qcol) == 0) return("—")
      total <- sum(sd$discharge[[qcol[1]]], na.rm = TRUE)
      paste0(round(total, 1), " m\u00b3/s")
    })

    output$vb_ratio <- renderText({
      sb <- subbasin_data()
      if (is.null(sb$discharge) || is.null(sb$water_balance)) return("—")
      # Approximate: mean(Q_sim) * 86400 * 365.25 / (mean(P) * area * 1e6) — simplified
      # Just show ratio of sums
      q <- mean(sb$discharge$Q_sim, na.rm = TRUE)
      p_col <- intersect(c("P", "PRAIN"), colnames(sb$water_balance))
      if (length(p_col) == 0 || is.null(sb$precipitation)) return("—")
      p_total <- sum(sb$precipitation$PRAIN, na.rm = TRUE) +
                 sum(sb$precipitation$PSNOW, na.rm = TRUE)
      if (p_total == 0) return("—")
      # Very rough approximation
      "—"
    })

    # ── Helper: apply cumulative sum if toggled ───────────────────────────
    maybe_cumsum <- function(sdata) {
      if (!isTRUE(input$cumsum) || is.null(sdata)) return(sdata)
      value_cols <- setdiff(colnames(sdata), "month")
      for (col in value_cols) {
        sdata[[col]] <- cumsum(replace(sdata[[col]], is.na(sdata[[col]]), 0))
      }
      sdata
    }

    # ── Monthly Water Balance plot (Precip inverted + Discharge) ──────────
    output$monthly_wb_plot <- renderPlotly({
      sd <- seasonality()

      # Precip subplot
      pdata <- maybe_cumsum(sd$precipitation)
      p_precip <- if (!is.null(pdata) && nrow(pdata) > 0) {
        p <- plot_ly(pdata, x = ~month)
        if ("PRAIN" %in% colnames(pdata))
          p <- p |> add_trace(y = ~PRAIN, name = "Rain", type = "bar",
                              marker = list(color = COLORS_PRECIPITATION$PRAIN),
                              hovertemplate = "<b>Rain:</b> %{y:.1f} mm<extra></extra>")
        if ("PSNOW" %in% colnames(pdata))
          p <- p |> add_trace(y = ~PSNOW, name = "Snow", type = "bar",
                              marker = list(color = COLORS_PRECIPITATION$PSNOW),
                              hovertemplate = "<b>Snow:</b> %{y:.1f} mm<extra></extra>")
        p |> layout(barmode = "stack",
                    yaxis = list(title = "Precip (mm)", autorange = "reversed"),
                    xaxis = list(title = ""))
      } else {
        plotly_empty("No precipitation data")
      }

      # Discharge subplot
      ddata <- maybe_cumsum(sd$discharge)
      p_q <- if (!is.null(ddata) && nrow(ddata) > 0) {
        p <- plot_ly(ddata, x = ~month)
        if ("Q_obs" %in% colnames(ddata))
          p <- p |> add_trace(y = ~Q_obs, name = "Q Observed", type = "scatter",
                              mode = "lines+markers",
                              line = list(color = COLORS_DISCHARGE$Q_obs, width = 2),
                              marker = list(size = 5, color = COLORS_DISCHARGE$Q_obs),
                              hovertemplate = "<b>Q Obs:</b> %{y:.2f} m\u00b3/s<extra></extra>")
        if ("Q_sim" %in% colnames(ddata))
          p <- p |> add_trace(y = ~Q_sim, name = "Q Simulated", type = "scatter",
                              mode = "lines+markers",
                              line = list(color = COLORS_DISCHARGE$Q_sim, width = 2),
                              marker = list(size = 5, color = COLORS_DISCHARGE$Q_sim),
                              hovertemplate = "<b>Q Sim:</b> %{y:.2f} m\u00b3/s<extra></extra>")
        p |> layout(yaxis = list(title = "Discharge (m\u00b3/s)"),
                    xaxis = list(title = "", dtick = 1))
      } else {
        plotly_empty("No discharge data")
      }

      subplot(p_precip, p_q, nrows = 2, shareX = TRUE,
              heights = c(0.3, 0.7), titleY = TRUE) |>
        layout(
          hovermode = "x unified",
          xaxis = list(
            tickmode = "array", tickvals = 1:12,
            ticktext = c("Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec")
          ),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = -0.08, font = list(size = 10)),
          margin = list(t = 10, b = 40)
        )
    })

    # ── Runoff Components ─────────────────────────────────────────────────
    output$season_runoff_plot <- renderPlotly({
      sd <- seasonality()
      sdata <- maybe_cumsum(sd$runoff_components)
      plot_seasonality_runoff(sdata)
    })

    # ── Water Balance / States ────────────────────────────────────────────
    output$season_states_plot <- renderPlotly({
      sd <- seasonality()
      storage_vars <- c("BW0", "BW3", "SWW")
      plot_seasonality_water_balance(sd$water_balance, storage_vars)
    })

    # ── Drivers: Temperature + ET ─────────────────────────────────────────
    output$season_drivers_plot <- renderPlotly({
      sb <- subbasin_data()
      met <- sb$meteorology

      # Monthly temperature
      if (!is.null(met) && "Temperature" %in% colnames(met) && "Date" %in% colnames(met)) {
        met$month <- as.integer(format(met$Date, "%m"))
        temp_monthly <- stats::aggregate(Temperature ~ month, data = met, FUN = mean, na.rm = TRUE)
      } else {
        temp_monthly <- NULL
      }

      # Monthly ET from water balance
      wb <- sb$water_balance
      et_monthly <- NULL
      if (!is.null(wb) && "ETA" %in% colnames(wb) && "Date" %in% colnames(wb)) {
        wb$month <- as.integer(format(wb$Date, "%m"))
        et_monthly <- stats::aggregate(ETA ~ month, data = wb, FUN = sum, na.rm = TRUE)
        # Normalize by number of years
        n_years <- length(unique(format(wb$Date, "%Y")))
        if (n_years > 0) et_monthly$ETA <- et_monthly$ETA / n_years
      }

      if (is.null(temp_monthly) && is.null(et_monthly)) {
        return(plotly_empty("No temperature/ET data (requires OUTPUTTYPE \u2265 2)"))
      }

      # Apply cumulative sums to ET if toggled
      if (isTRUE(input$cumsum) && !is.null(et_monthly)) {
        et_monthly$ETA <- cumsum(replace(et_monthly$ETA, is.na(et_monthly$ETA), 0))
      }

      et_ylab <- if (isTRUE(input$cumsum)) "ET cumulative (mm)" else "ET (mm/month)"

      # Build subplot: Temperature (top) + ET (bottom)
      p_temp <- if (!is.null(temp_monthly)) {
        plot_ly(temp_monthly, x = ~month) |>
          add_trace(y = ~Temperature, name = "Mean Temperature",
                    type = "scatter", mode = "lines+markers",
                    line = list(color = "#e67e22", width = 2),
                    marker = list(size = 5, color = "#e67e22"),
                    hovertemplate = "<b>Temperature:</b> %{y:.1f} \u00b0C<extra></extra>") |>
          add_trace(y = rep(0, 12), name = "0\u00b0C", type = "scatter",
                    mode = "lines", line = list(color = "#aaa", dash = "dash", width = 0.8),
                    showlegend = FALSE, hoverinfo = "skip") |>
          layout(yaxis = list(title = "Temperature (\u00b0C)"),
                 xaxis = list(title = ""))
      } else {
        plotly_empty("No temperature data")
      }

      p_et <- if (!is.null(et_monthly)) {
        plot_ly(et_monthly, x = ~month) |>
          add_trace(y = ~ETA, name = "Actual ET", type = "bar",
                    marker = list(color = COLORS_PRECIPITATION$ETA),
                    hovertemplate = paste0("<b>ET:</b> %{y:.1f} mm<extra></extra>")) |>
          layout(yaxis = list(title = et_ylab),
                 xaxis = list(title = "", dtick = 1))
      } else {
        plotly_empty("No ET data")
      }

      subplot(p_temp, p_et, nrows = 2, shareX = TRUE,
              heights = c(0.5, 0.5), titleY = TRUE) |>
        layout(
          hovermode = "x unified",
          xaxis = list(
            tickmode = "array", tickvals = 1:12,
            ticktext = c("Jan","Feb","Mar","Apr","May","Jun",
                         "Jul","Aug","Sep","Oct","Nov","Dec")
          ),
          legend = list(orientation = "h", x = 0.5, xanchor = "center",
                        y = -0.08, font = list(size = 10)),
          margin = list(t = 10, b = 40)
        )
    })
  })
}
