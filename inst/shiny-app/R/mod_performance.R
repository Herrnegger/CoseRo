# Title: Model Performance Tab Module
# Description: Metric bar charts, threshold-based coloring, multi-subbasin comparison table
# Author/Architect: Mathew Herrnegger
# Coding: Claude/PI
# Date: 2026-02-28

# Supported metrics and their slider configs
# BETA uses a tolerance slider (symmetric around 1); others use a simple threshold
METRIC_CONFIGS <- list(
  NSE    = list(slider_min = -1, slider_max = 1,   slider_default = 0.5,  step = 0.05, type = "threshold"),
  KGE    = list(slider_min = -1, slider_max = 1,   slider_default = 0.5,  step = 0.05, type = "threshold"),
  KGEadj = list(slider_min = -1, slider_max = 1,   slider_default = 0.5,  step = 0.05, type = "threshold"),
  BETA   = list(slider_min = 0,  slider_max = 0.5, slider_default = 0.1,  step = 0.01, type = "tolerance")
)

# ── UI ────────────────────────────────────────────────────────────────────────
performance_ui <- function(id) {
  ns <- NS(id)

  tagList(
  tags$style(HTML("
    .perf-compact .card { margin-bottom: 0.4rem; }
    .perf-compact .card-header { padding: 0.25rem 0.5rem; font-size: 0.85rem; }
    .perf-compact .card-body { padding: 0.4rem 0.5rem; }
    .perf-compact .form-group { margin-bottom: 0.35rem; }
    .perf-compact .form-label, .perf-compact label { font-size: 0.82rem; margin-bottom: 0.1rem; }
    .perf-compact .form-control, .perf-compact .form-select { padding: 0.2rem 0.4rem; font-size: 0.82rem; min-height: unset; }
    .perf-compact .bslib-value-box { min-height: unset; padding: 0.2rem 0; }
    .perf-compact .value-box-title { font-size: 0.75rem; }
    .perf-compact .value-box-value { font-size: 1.15rem; line-height: 1.2; }
    .perf-compact .value-box-showcase { padding: 0.2rem; max-width: 45px; }
    .perf-compact .value-box-area { padding: 0.3rem 0.6rem; }
    .perf-compact .bslib-grid { gap: 0.3rem; }
  ")),
  div(class = "perf-compact",
  layout_sidebar(
    sidebar = sidebar(
      title = "Performance Controls",
      width = 300,

      accordion(
        open = c("Metric Selection"),

        accordion_panel(
          "Metric Selection",
          icon = icon("chart-bar"),
          uiOutput(ns("metric_selector_ui")),
          uiOutput(ns("threshold_ui"))
        )
      )
    ),

    # ── Main Panel ──────────────────────────────────────────────────────────

    # Tier 1: Value Boxes
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      value_box(
        title = "Basin Average",
        value = textOutput(ns("vb_avg")),
        showcase = icon("chart-line"),
        theme = "secondary", class = "py-1"
      ),
      value_box(
        title = "Worst Subbasin",
        value = textOutput(ns("vb_worst")),
        showcase = icon("triangle-exclamation"),
        theme = "danger", class = "py-1",
        p(class = "small mb-0", style = "opacity:0.85;", textOutput(ns("vb_worst_id")))
      ),
      value_box(
        title = "Best Subbasin",
        value = textOutput(ns("vb_best")),
        showcase = icon("circle-check"),
        theme = "success", class = "py-1",
        p(class = "small mb-0", style = "opacity:0.85;", textOutput(ns("vb_best_id")))
      )
    ),

    # Tier 2: Bar chart
    card(
      full_screen = TRUE,
      card_header(textOutput(ns("bar_chart_title")), class = "bg-light py-1"),
      card_body(
        class = "p-0",
        plotlyOutput(ns("metric_bar_chart"), height = "400px")
      )
    ),

    # Tier 3: Table
    card(
      full_screen = TRUE,
      card_header("All Metrics \u00d7 All Subbasins", class = "bg-light py-1"),
      card_body(
        class = "p-2",
        DTOutput(ns("stats_table"))
      )
    )
  ) # end layout_sidebar
  ) # end div.perf-compact
  ) # end tagList
}

# ── Server ────────────────────────────────────────────────────────────────────
performance_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Helper: get config for current metric ─────────────────────────────
    metric_cfg <- reactive({
      m <- input$metric
      if (!is.null(m) && m %in% names(METRIC_CONFIGS)) METRIC_CONFIGS[[m]] else NULL
    })

    # ── Helper: "worst" = farthest from ideal ─────────────────────────────
    find_worst_idx <- function(values, metric_name) {
      if (metric_name == "BETA") {
        which.max(abs(values - 1))
      } else {
        which.min(values)
      }
    }

    find_best_idx <- function(values, metric_name) {
      if (metric_name == "BETA") {
        which.min(abs(values - 1))
      } else {
        which.max(values)
      }
    }

    # ── Metric selector (only supported metrics) ─────────────────────────
    output$metric_selector_ui <- renderUI({
      req(shared$cosero_data$statistics)
      stats <- shared$cosero_data$statistics
      # Only show metrics we support AND that exist in the data
      supported <- names(METRIC_CONFIGS)
      available <- intersect(supported, colnames(stats))
      if (length(available) == 0) {
        return(tags$p(class = "text-muted small", "No supported metrics found (NSE, KGE, KGEadj, BETA)."))
      }
      sel <- if ("NSE" %in% available) "NSE" else available[1]
      selectInput(ns("metric"), "Metric:", choices = available, selected = sel)
    })

    # ── Dynamic threshold/tolerance slider ────────────────────────────────
    output$threshold_ui <- renderUI({
      cfg <- metric_cfg()
      if (is.null(cfg)) return(NULL)

      if (cfg$type == "tolerance") {
        sliderInput(ns("threshold"), "Tolerance around 1:",
                    min = cfg$slider_min, max = cfg$slider_max,
                    value = cfg$slider_default, step = cfg$step)
      } else {
        sliderInput(ns("threshold"), "Reference Threshold:",
                    min = cfg$slider_min, max = cfg$slider_max,
                    value = cfg$slider_default, step = cfg$step)
      }
    })

    # ── Reactive: metric values ───────────────────────────────────────────
    metric_vals <- reactive({
      req(shared$cosero_data$statistics, input$metric)
      stats <- shared$cosero_data$statistics
      if (!input$metric %in% colnames(stats)) return(NULL)
      data.frame(
        sb = stats$sb,
        value = stats[[input$metric]],
        stringsAsFactors = FALSE
      )
    })

    # ── Value Boxes ───────────────────────────────────────────────────────
    output$vb_avg <- renderText({
      mv <- metric_vals()
      if (is.null(mv)) return("\u2014")
      sprintf("%.3f", mean(mv$value, na.rm = TRUE))
    })

    output$vb_worst <- renderText({
      mv <- metric_vals()
      if (is.null(mv) || !is.character(input$metric)) return("\u2014")
      idx <- find_worst_idx(mv$value, input$metric)
      sprintf("%.3f", mv$value[idx])
    })

    output$vb_worst_id <- renderText({
      mv <- metric_vals()
      if (is.null(mv) || !is.character(input$metric)) return("")
      idx <- find_worst_idx(mv$value, input$metric)
      paste("Subbasin", mv$sb[idx])
    })

    output$vb_best <- renderText({
      mv <- metric_vals()
      if (is.null(mv) || !is.character(input$metric)) return("\u2014")
      idx <- find_best_idx(mv$value, input$metric)
      sprintf("%.3f", mv$value[idx])
    })

    output$vb_best_id <- renderText({
      mv <- metric_vals()
      if (is.null(mv) || !is.character(input$metric)) return("")
      idx <- find_best_idx(mv$value, input$metric)
      paste("Subbasin", mv$sb[idx])
    })

    # ── Bar chart title ───────────────────────────────────────────────────
    output$bar_chart_title <- renderText({
      paste(input$metric %||% "Metric", "across All Subbasins")
    })

    # ── Horizontal bar chart ──────────────────────────────────────────────
    output$metric_bar_chart <- renderPlotly({
      mv <- metric_vals()
      if (is.null(mv) || is.null(input$threshold) || !is.character(input$metric)) {
        return(plotly_empty("No statistics loaded"))
      }

      cfg    <- metric_cfg()
      thresh <- input$threshold

      # Neutral bar color (steel blue)
      bar_color <- "#4682B4"

      p <- plot_ly(mv, y = ~reorder(sb, value), x = ~value, type = "bar",
              orientation = "h",
              marker = list(color = bar_color),
              text = ~sprintf("%.3f", value),
              hovertemplate = paste0(
                "<b>Subbasin:</b> %{y}<br>",
                "<b>", input$metric, ":</b> %{x:.3f}<extra></extra>"
              ))

      # Threshold lines
      shapes <- list()
      annotations <- list()

      if (!is.null(cfg) && cfg$type == "tolerance") {
        # BETA: two symmetric lines around 1
        upper <- 1 + thresh
        lower <- 1 - thresh
        shapes <- list(
          list(type = "line", x0 = upper, x1 = upper,
               y0 = -0.5, y1 = nrow(mv) - 0.5,
               line = list(color = "#333", width = 2, dash = "dash")),
          list(type = "line", x0 = lower, x1 = lower,
               y0 = -0.5, y1 = nrow(mv) - 0.5,
               line = list(color = "#333", width = 2, dash = "dash")),
          # Ideal line at 1
          list(type = "line", x0 = 1, x1 = 1,
               y0 = -0.5, y1 = nrow(mv) - 0.5,
               line = list(color = "#28a745", width = 1.5, dash = "dot"))
        )
        annotations <- list(
          list(x = upper, y = nrow(mv) - 0.5,
               text = round(upper, 2), showarrow = FALSE,
               xanchor = "left", xshift = 3, font = list(size = 10)),
          list(x = lower, y = nrow(mv) - 0.5,
               text = round(lower, 2), showarrow = FALSE,
               xanchor = "right", xshift = -3, font = list(size = 10))
        )
      } else {
        # NSE, KGE, KGEadj: single reference line
        shapes <- list(
          list(type = "line", x0 = thresh, x1 = thresh,
               y0 = -0.5, y1 = nrow(mv) - 0.5,
               line = list(color = "#333", width = 2, dash = "dash"))
        )
        annotations <- list(
          list(x = thresh, y = nrow(mv) - 0.5,
               text = paste("Threshold:", thresh), showarrow = FALSE,
               xanchor = "left", xshift = 3, font = list(size = 10))
        )
      }

      p |> layout(
        xaxis = list(title = input$metric),
        yaxis = list(title = "", type = "category"),
        shapes = shapes,
        annotations = annotations,
        margin = list(l = 80)
      )
    })

    # ── Statistics table ──────────────────────────────────────────────────
    output$stats_table <- renderDT({
      req(shared$cosero_data$statistics)
      stats <- shared$cosero_data$statistics
      numeric_cols <- names(stats)[sapply(stats, is.numeric)]

      dt <- datatable(
        stats,
        options = list(
          pageLength = 20, scrollX = TRUE, scrollY = "400px",
          dom = "ft"
        ),
        rownames = FALSE
      )

      if (length(numeric_cols) > 0) {
        dt <- formatRound(dt, columns = numeric_cols, digits = 3)
      }
      dt
    })
  })
}
