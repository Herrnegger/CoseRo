# Title:       Export Tab Module
# Description: Download filtered data (CSV) and interactive plots (HTML)
# Author:      Mathew Herrnegger
# Coding:      Claude/PI
# Date:        2026-03-03

# ── UI ────────────────────────────────────────────────────────────────────────
export_ui <- function(id) {
  ns <- NS(id)

  tagList(
  tags$style(HTML("
    .export-compact .card { margin-bottom: 0.4rem; }
    .export-compact .card-header { padding: 0.25rem 0.5rem; font-size: 0.85rem; }
    .export-compact .card-body { padding: 0.4rem 0.5rem; }
    .export-compact .form-group { margin-bottom: 0.35rem; }
    .export-compact .form-label, .export-compact label { font-size: 0.82rem; margin-bottom: 0.1rem; }
    .export-compact .form-control, .export-compact .form-select { padding: 0.2rem 0.4rem; font-size: 0.82rem; min-height: unset; }
    .export-compact .bslib-value-box { min-height: unset; padding: 0.2rem 0; }
    .export-compact .value-box-title { font-size: 0.75rem; }
    .export-compact .value-box-value { font-size: 1.15rem; line-height: 1.2; }
    .export-compact .value-box-showcase { padding: 0.2rem; max-width: 45px; }
    .export-compact .value-box-area { padding: 0.3rem 0.6rem; }
    .export-compact .bslib-grid { gap: 0.3rem; }
  ")),
  div(class = "export-compact",
  layout_sidebar(
    sidebar = sidebar(
      title = "Export Controls",
      width = 300,

      accordion(
        open = c("Data Scope"),

        accordion_panel(
          "Data Scope",
          icon = icon("filter"),
          uiOutput(ns("subbasin_selector_ui")),
          uiOutput(ns("date_range_ui"))
        )
      )
    ),

    # ── Main Panel ──────────────────────────────────────────────────────────

    # Tier 1: Value boxes
    layout_columns(
      fill = FALSE,
      col_widths = c(6, 6),
      value_box(
        title = "Selected Period",
        value = textOutput(ns("vb_period")),
        showcase = icon("calendar"),
        theme = "secondary", class = "py-1"
      ),
      value_box(
        title = "Target Subbasin",
        value = textOutput(ns("vb_subbasin")),
        showcase = icon("crosshairs"),
        theme = "info", class = "py-1"
      )
    ),

    # Tier 2: Export cards — two columns
    layout_columns(
      col_widths = c(6, 6),

      # ── Column 1: CSV ──
      card(
        card_header(
          class = "py-1",
          icon("table", class = "me-1"), "Data (CSV)"
        ),
        card_body(
          class = "p-3",
          div(class = "d-grid gap-2",
              downloadButton(ns("dl_discharge_csv"), "Discharge",
                             class = "btn-success btn-sm"),
              downloadButton(ns("dl_precip_csv"), "Precipitation",
                             class = "btn-success btn-sm"),
              downloadButton(ns("dl_runoff_csv"), "Runoff Components",
                             class = "btn-success btn-sm"),
              downloadButton(ns("dl_wb_csv"), "Water Balance",
                             class = "btn-success btn-sm"),
              downloadButton(ns("dl_stats_csv"), "Statistics (All)",
                             class = "btn-success btn-sm")
          )
        )
      ),

      # ── Column 2: HTML (always works) ──
      card(
        card_header(
          class = "py-1",
          icon("globe", class = "me-1"), "Plots (HTML)"
        ),
        card_body(
          class = "p-3",
          div(class = "d-grid gap-2",
              downloadButton(ns("dl_hydrograph_html"), "Hydrograph + Precip",
                             class = "btn-primary btn-sm"),
              downloadButton(ns("dl_runoff_html"), "Runoff Components",
                             class = "btn-primary btn-sm"),
              downloadButton(ns("dl_wb_html"), "Water Balance",
                             class = "btn-primary btn-sm"),
              downloadButton(ns("dl_temp_html"), "Temperature & ET",
                             class = "btn-primary btn-sm")
          ),
          tags$p(class = "text-muted small mt-2 mb-0",
                 icon("info-circle"),
                 "Interactive files \u2014 open in any browser.",
                 "Use the", icon("camera"), "toolbar icon to save as PNG.")
        )
      )
    )
  ) # end layout_sidebar
  ) # end div.export-compact
  ) # end tagList
}

# ── Server ────────────────────────────────────────────────────────────────────
export_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Subbasin selector ─────────────────────────────────────────────────
    output$subbasin_selector_ui <- renderUI({
      req(shared$cosero_data)
      sbs <- shared$cosero_data$metadata$subbasins
      if (length(sbs) == 0) return(NULL)
      sel <- shared$selected_subbasin %||% sbs[1]
      selectInput(ns("selected_subbasin"), "Subbasin:", choices = sbs, selected = sel)
    })

    output$date_range_ui <- renderUI({
      req(shared$cosero_data$runoff)
      dr <- range(shared$cosero_data$runoff$Date, na.rm = TRUE)
      dateRangeInput(ns("date_range"), "Date Range:", start = dr[1], end = dr[2],
                     min = dr[1], max = dr[2])
    })

    # ── Prepare data for export ───────────────────────────────────────────
    export_data <- reactive({
      req(shared$cosero_data, input$selected_subbasin)
      CoseRo::prepare_subbasin_data(shared$cosero_data, input$selected_subbasin,
                            input$date_range)
    })

    # ── Value Boxes ───────────────────────────────────────────────────────
    output$vb_period <- renderText({
      req(input$date_range)
      d <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days"))
      paste0(d, " days (", round(d / 365.25, 1), " yrs)")
    })

    output$vb_subbasin <- renderText({
      input$selected_subbasin %||% "\u2014"
    })

    # ── Plot builders (reused by HTML handlers) ───────────────────────────
    build_hydrograph <- function() {
      d <- export_data()
      p_precip <- build_precip_subplot(d$precipitation)
      p_q <- build_discharge_subplot(d$discharge)
      subplot(p_precip, p_q, nrows = 2, shareX = TRUE,
              heights = c(0.25, 0.75), titleY = TRUE)
    }

    build_runoff <- function() {
      d <- export_data()
      CoseRo::plot_runoff_components(d$runoff_components, glacier_data = d$glacier)
    }

    build_wb <- function() {
      d <- export_data()
      CoseRo::plot_water_balance(d$water_balance,
                                 selected_vars = c("BW0", "BW3", "SWW",
                                                   "P_cum", "ETAGEB_cum", "QABGEB_cum"),
                                 show_cumulative = TRUE)
    }

    build_temp_et <- function() {
      sb_data <- export_data()
      build_temperature_et_plot(sb_data$temperature, sb_data$water_balance)
    }

    # ── CSV Downloads ─────────────────────────────────────────────────────
    output$dl_discharge_csv <- downloadHandler(
      filename = function() paste0("discharge_", input$selected_subbasin, "_", Sys.Date(), ".csv"),
      content = function(file) {
        d <- export_data()$discharge
        if (!is.null(d)) write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_precip_csv <- downloadHandler(
      filename = function() paste0("precipitation_", input$selected_subbasin, "_", Sys.Date(), ".csv"),
      content = function(file) {
        d <- export_data()$precipitation
        if (!is.null(d)) write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_runoff_csv <- downloadHandler(
      filename = function() paste0("runoff_components_", input$selected_subbasin, "_", Sys.Date(), ".csv"),
      content = function(file) {
        d <- export_data()$runoff_components
        if (!is.null(d)) write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_wb_csv <- downloadHandler(
      filename = function() paste0("water_balance_", input$selected_subbasin, "_", Sys.Date(), ".csv"),
      content = function(file) {
        d <- export_data()$water_balance
        if (!is.null(d)) write.csv(d, file, row.names = FALSE)
      }
    )
    output$dl_stats_csv <- downloadHandler(
      filename = function() paste0("statistics_all_", Sys.Date(), ".csv"),
      content = function(file) {
        s <- shared$cosero_data$statistics
        if (!is.null(s)) write.csv(s, file, row.names = FALSE)
      }
    )

    # ── HTML Downloads (always works, zero extra deps) ────────────────────
    safe_export_html <- function(plot, file) {
      tryCatch({
        htmlwidgets::saveWidget(
          plotly::partial_bundle(plot),
          file = file,
          selfcontained = TRUE
        )
      }, error = function(e) {
        # If partial_bundle fails, try full widget
        htmlwidgets::saveWidget(plot, file = file, selfcontained = TRUE)
      })
    }

    output$dl_hydrograph_html <- downloadHandler(
      filename = function() paste0("hydrograph_", input$selected_subbasin, "_", Sys.Date(), ".html"),
      content = function(file) safe_export_html(build_hydrograph(), file)
    )
    output$dl_runoff_html <- downloadHandler(
      filename = function() paste0("runoff_components_", input$selected_subbasin, "_", Sys.Date(), ".html"),
      content = function(file) safe_export_html(build_runoff(), file)
    )
    output$dl_wb_html <- downloadHandler(
      filename = function() paste0("water_balance_", input$selected_subbasin, "_", Sys.Date(), ".html"),
      content = function(file) safe_export_html(build_wb(), file)
    )
    output$dl_temp_html <- downloadHandler(
      filename = function() paste0("temperature_et_", input$selected_subbasin, "_", Sys.Date(), ".html"),
      content = function(file) safe_export_html(build_temp_et(), file)
    )
  })
}
