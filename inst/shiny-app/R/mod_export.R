# Title: Export Tab Module
# Description: Download filtered data (CSV) and plots (PNG) with export controls
# Author/Architect: Mathew Herrnegger
# Coding: Claude/PI
# Date: 2026-02-28

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
        ),

        accordion_panel(
          "Plot Formatting",
          icon = icon("image"),
          selectInput(ns("format"), "Format:", choices = c("PNG", "SVG"),
                      selected = "PNG", width = "100%"),
          layout_column_wrap(
            width = 1 / 2, gap = "0.5rem",
            numericInput(ns("width"), "Width (px):", value = 1200, width = "100%"),
            numericInput(ns("height"), "Height (px):", value = 400, width = "100%")
          )
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

    # Tier 2: Export buttons
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header("Export Data (CSV)", class = "bg-light py-1"),
        card_body(
          class = "p-3",
          div(class = "d-grid gap-2",
              downloadButton(ns("dl_discharge_csv"), "Discharge Data",
                             class = "btn-success"),
              downloadButton(ns("dl_precip_csv"), "Precipitation Data",
                             class = "btn-success"),
              downloadButton(ns("dl_runoff_csv"), "Runoff Components",
                             class = "btn-success"),
              downloadButton(ns("dl_wb_csv"), "Water Balance / States",
                             class = "btn-success"),
              downloadButton(ns("dl_stats_csv"), "Statistics (All Subbasins)",
                             class = "btn-success")
          )
        )
      ),

      card(
        card_header("Export Plots (PNG)", class = "bg-light py-1"),
        card_body(
          class = "p-3",
          div(class = "d-grid gap-2",
              downloadButton(ns("dl_hydrograph_png"), "Hydrograph + Precip",
                             class = "btn-primary"),
              downloadButton(ns("dl_runoff_png"), "Runoff Components",
                             class = "btn-primary"),
              downloadButton(ns("dl_wb_png"), "Water Balance / States",
                             class = "btn-primary"),
              downloadButton(ns("dl_temp_png"), "Temperature & ET",
                             class = "btn-primary")
          ),
          tags$p(class = "text-muted small mt-2",
                 icon("info-circle"),
                 "First export may take a few seconds (engine startup).",
                 "Subsequent exports are fast.")
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
      prepare_subbasin_data(shared$cosero_data, input$selected_subbasin,
                            input$date_range)
    })

    # ── Value Boxes ───────────────────────────────────────────────────────
    output$vb_period <- renderText({
      req(input$date_range)
      d <- as.numeric(difftime(input$date_range[2], input$date_range[1], units = "days"))
      paste0(d, " days (", round(d / 365.25, 1), " yrs)")
    })

    output$vb_subbasin <- renderText({
      input$selected_subbasin %||% "—"
    })

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

    # ── PNG Downloads ─────────────────────────────────────────────────────
    output$dl_hydrograph_png <- downloadHandler(
      filename = function() paste0("hydrograph_", input$selected_subbasin, "_", Sys.Date(), ".png"),
      content = function(file) {
        d <- export_data()
        p_precip <- build_precip_subplot(d$precipitation)
        p_q <- build_discharge_subplot(d$discharge)
        p <- subplot(p_precip, p_q, nrows = 2, shareX = TRUE,
                     heights = c(0.25, 0.75), titleY = TRUE)
        export_plot_png(p, file, width = input$width, height = input$height)
      }
    )
    output$dl_runoff_png <- downloadHandler(
      filename = function() paste0("runoff_components_", input$selected_subbasin, "_", Sys.Date(), ".png"),
      content = function(file) {
        d <- export_data()
        p <- plot_runoff_components(d$runoff_components, glacier_data = d$glacier)
        export_plot_png(p, file, width = input$width, height = input$height)
      }
    )
    output$dl_wb_png <- downloadHandler(
      filename = function() paste0("water_balance_", input$selected_subbasin, "_", Sys.Date(), ".png"),
      content = function(file) {
        d <- export_data()
        p <- plot_water_balance(d$water_balance,
                                selected_vars = c("BW0", "BW3", "SWW", "P_cum", "ETAGEB_cum", "QABGEB_cum"),
                                show_cumulative = TRUE)
        export_plot_png(p, file, width = input$width, height = input$height)
      }
    )
    output$dl_temp_png <- downloadHandler(
      filename = function() paste0("temperature_et_", input$selected_subbasin, "_", Sys.Date(), ".png"),
      content = function(file) {
        met <- extract_met_subbasin(
          shared$cosero_data$meteorology, input$selected_subbasin, input$date_range
        )
        sb_data <- prepare_subbasin_data(shared$cosero_data, input$selected_subbasin, input$date_range)
        p <- build_temperature_et_plot(met, sb_data$water_balance)
        export_plot_png(p, file, width = input$width, height = input$height)
      }
    )
  })
}
