# COSERO Workbench
# Interactive Shiny app for running COSERO model and visualizing outputs
# Author: COSERO R Interface
# Date: 2025-11-03
#
# Features:
# - Run COSERO model directly from GUI
# - Load and visualize model outputs
# - Time series and seasonality analysis
# - Performance statistics
# - Export plots and data

# Load required libraries
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(shinyFiles)
library(shinyjs)

# Note: Functions loaded from COSERO package
# (run_cosero, read_cosero_output, plotting functions, etc.)

# Load COSERO package functions
# Try to load installed package first, otherwise source from R/ directory
if (requireNamespace("COSERO", quietly = TRUE)) {
  library(COSERO)
} else {
  # Fallback: source from package directory
  # Get the app directory and navigate to package R/ folder
  app_dir <- getwd()
  pkg_dir <- normalizePath(file.path(app_dir, "../.."), mustWork = FALSE)

  # Source all R files needed by the app
  source(file.path(pkg_dir, "R/app_helpers.R"))
  source(file.path(pkg_dir, "R/cosero_readers.R"))
  source(file.path(pkg_dir, "R/cosero_run.R"))
}

# Default output directory (current working directory's output folder)
default_output_dir <- normalizePath(file.path(getwd(), "output"), winslash = "/", mustWork = FALSE)

# Define bslib theme ####
app_theme <- bs_theme(
  version = 5,
  bootswatch = "united",
  primary = "#2C5F2D",
  secondary = "#97BC62",
  success = "#2C5F2D",
  info = "#3498db",
  warning = "#f39c12",
  danger = "#e74c3c",
  base_font = font_google("Roboto"),
  heading_font = font_google("Raleway", wght = c(400, 500)),
  code_font = font_google("Fira Code"),
  font_scale = 0.95,                # Global font size control (0.8-1.2 recommended)
  bg = "#f8f9fa",                   # Main background color (white by default)
  fg = "#212529",                   # Main text color
  "body-bg" = "#f8f9fa",            # Body background
  "card-border-color" = "#dee2e6",
  "card-cap-bg" = "#f8f9fa",
  "well-bg" = "#f8f9fa"
)

# UI Definition ####
ui <- fluidPage(
  theme = app_theme,
  useShinyjs(),  # Enable shinyjs

  # Custom CSS for enhanced visuals
  tags$head(
    tags$style(HTML("
      .drag-drop-zone {
        border: 2px solid #97BC62;
        border-radius: 8px;
        padding: 25px;
        text-align: center;
        background-color: #f8f9fa;
        transition: all 0.3s ease;
        cursor: pointer;
        margin-bottom: 15px;
      }
      .drag-drop-zone:hover {
        background-color: #e9ecef;
        border-color: #2C5F2D;
        box-shadow: 0 2px 6px rgba(44, 95, 45, 0.15);
      }
      .section-header {
        font-size: 1.2em;
        font-weight: 600;
        color: #2C5F2D;
        margin-bottom: 20px;
        margin-top: 15px;
        padding-bottom: 10px;
        border-bottom: 2px solid #97BC62;
      }
      .subsection-header {
        font-size: 1.05em;
        font-weight: 600;
        color: #495057;
        margin-top: 15px;
        margin-bottom: 12px;
      }
      .input-hint {
        font-size: 0.85em;
        color: #6c757d;
        margin-top: -8px;
        margin-bottom: 12px;
      }
      .card-header {
        font-weight: 600;
        background-color: #f8f9fa;
      }
      .validation-success {
        color: #28a745;
        font-weight: 500;
        padding: 8px 12px;
        background-color: #d4edda;
        border-radius: 4px;
        margin: 10px 0;
      }
      .validation-warning {
        color: #856404;
        font-weight: 500;
        padding: 8px 12px;
        background-color: #fff3cd;
        border-radius: 4px;
        margin: 10px 0;
      }
      .validation-error {
        color: #721c24;
        font-weight: 500;
        padding: 8px 12px;
        background-color: #f8d7da;
        border-radius: 4px;
        margin: 10px 0;
      }
    "))
  ),

  # Custom header
  tags$div(
    style = "display: flex; align-items: center; padding: 15px 0px; margin-bottom: 20px; border-bottom: 2px solid #dee2e6; box-shadow: 0 2px 4px rgba(0,0,0,0.05);",
    tags$img(
      src = "logo.svg",
      height = "75px",
      style = "margin-right: 20px;",
      onerror = "this.style.display='none'; console.error('Logo not found: logo.svg');"
    ),
    tags$h2("COSERO Workbench", style = "margin: 0; font-weight: 600;")
  ),

  # Full width tabset panel without sidebar
  tabsetPanel(
    id = "main_tabs",

        # COSERO Run Tab ####
        tabPanel(
          "COSERO Run",
          icon = icon("play-circle"),

          tags$div(class = "section-header", "Configure & Run COSERO Model"),

          fluidRow(
            # Single column layout
            column(8,
              card(
                card_header("Project Setup"),
                card_body(
                # Drag-drop zone for project directory
                tags$div(
                  id = "project_drag_zone",
                  class = "drag-drop-zone",
                  tags$p(
                    tags$strong("Click Here to Select Project Folder", style = "font-size: 1.1em;"),
                    tags$br(),
                    tags$span("Opens file browser to navigate to your COSERO project", style = "font-size: 0.9em; color: #6c757d;")
                  )
                ),

                # Hidden shinyDirButton (triggered by clicking the zone)
                shinyjs::hidden(
                  shinyDirButton(
                    "run_project_dir_btn",
                    "Browse",
                    "Select COSERO project directory",
                    icon = icon("folder-open")
                  )
                ),

                # Project path input
                textInput(
                  "run_project_path",
                  "Project Directory:",
                  value = normalizePath(getwd(), winslash = "/", mustWork = FALSE),
                  placeholder = "Path to COSERO project folder"
                ),
                tags$p(
                  class = "input-hint",
                  "Tip: You can also copy-paste the path directly - quotes and slashes (/ or \\) are handled automatically"
                ),
                uiOutput("project_validation_ui"),
                actionButton(
                  "load_defaults_btn",
                  "Load Defaults from Project",
                  icon = icon("sync"),
                  class = "btn-sm"
                ),
                tags$hr(),

                tags$div(class = "subsection-header", "Custom Parameter File (Optional)"),
                tags$p(
                  class = "input-hint",
                  "Leave empty to use default para.txt from project. Or specify a custom parameter file to temporarily use instead."
                ),

                # Drag-drop zone for parameter file
                tags$div(
                  id = "param_file_drag_zone",
                  class = "drag-drop-zone",
                  style = "padding: 15px;",
                  tags$p(
                    tags$strong("Click to Select Custom Parameter File", style = "font-size: 0.95em;"),
                    tags$br(),
                    tags$span("Optional: Select a .txt parameter file", style = "font-size: 0.85em; color: #6c757d;")
                  )
                ),

                # Hidden shinyFilesButton
                shinyjs::hidden(
                  shinyFilesButton(
                    "param_file_btn",
                    "Browse",
                    "Select parameter file",
                    icon = icon("file"),
                    multiple = FALSE
                  )
                ),

                # Parameter file path input
                textInput(
                  "param_file_path",
                  "Parameter File Path:",
                  value = "",
                  placeholder = "Optional: Path to custom parameter file (leave empty for default)"
                ),
                actionButton(
                  "clear_param_file",
                  "Clear Selection",
                  icon = icon("times"),
                  class = "btn-sm btn-warning",
                  style = "margin-top: -10px; margin-bottom: 10px;"
                ),
                uiOutput("param_file_validation_ui"),
                tags$hr(),

                tags$div(class = "subsection-header", "Simulation Period"),
                fluidRow(
                  column(6,
                    dateInput(
                      "run_start_date",
                      "Start Date:",
                      value = "2010-01-01"
                    )
                  ),
                  column(3,
                    selectInput(
                      "run_start_hour",
                      "Hour:",
                      choices = 0:23,
                      selected = 0
                    )
                  ),
                  column(3,
                    selectInput(
                      "run_start_minute",
                      "Min:",
                      choices = c(0, 15, 30, 45),
                      selected = 0
                    )
                  )
                ),
                fluidRow(
                  column(6,
                    dateInput(
                      "run_end_date",
                      "End Date:",
                      value = "2015-12-31"
                    )
                  ),
                  column(3,
                    selectInput(
                      "run_end_hour",
                      "Hour:",
                      choices = 0:23,
                      selected = 23
                    )
                  ),
                  column(3,
                    selectInput(
                      "run_end_minute",
                      "Min:",
                      choices = c(0, 15, 30, 45, 59),
                      selected = 59
                    )
                  )
                ),
                numericInput(
                  "run_spinup",
                  "Spinup Period (days):",
                  value = 365,
                  min = 0,
                  max = 10000
                ),
                tags$hr(),

                tags$div(class = "subsection-header", "Run Configuration"),
                radioButtons(
                  "run_mode",
                  "Run Mode:",
                  choices = list(
                    "Cold Start (from parameter file)" = 1,
                    "Warm Start (from previous run)" = 2
                  ),
                  selected = 1
                ),
                uiOutput("warm_start_status_ui"),
                selectInput(
                  "run_output_type",
                  "Output Level:",
                  choices = list(
                    "Basic (runoff + statistics)" = 1,
                    "Standard (+ glacier/meteorology)" = 2,
                    "Comprehensive (+ long-term means)" = 3
                  ),
                  selected = 3
                ),
                tags$hr(),

                tags$div(class = "subsection-header", "Advanced Options"),
                actionButton(
                  "toggle_advanced_params",
                  "Show Advanced Parameters",
                  icon = icon("chevron-down"),
                  class = "btn-default btn-sm",
                  style = "margin-bottom: 10px;"
                ),
                conditionalPanel(
                  condition = "input.toggle_advanced_params % 2 == 1",
                  tags$br(),
                  radioButtons(
                    "run_sc_flag",
                    "Flux Calculation Area:",
                    choices = list(
                      "Local area (EZFL_B)" = 0,
                      "Upstream area (EZFL_T)" = 1
                    ),
                    selected = 0,
                    inline = TRUE
                  ),
                  checkboxInput(
                    "run_outcontrol",
                    "Enable Zonal Outputs (requires Comprehensive output level)",
                    value = TRUE
                  ),
                  radioButtons(
                    "run_tmmon_option",
                    "Temperature Calculation:",
                    choices = list(
                      "From monthly temperature file" = 1,
                      "Calculate from input data" = 2
                    ),
                    selected = 1,
                    inline = TRUE
                  ),
                  numericInput(
                    "run_ikl",
                    "Snow Classes (IKL):",
                    value = 1,
                    min = 1,
                    max = 20
                  ),
                  numericInput(
                    "run_nclass",
                    "Land Use Classes (NCLASS):",
                    value = 1,
                    min = 1,
                    max = 20
                  ),
                  textInput(
                    "run_project_info",
                    "Project Name:",
                    value = "COSERO_Project"
                  )
                ),
                tags$hr(),

                tags$div(class = "subsection-header", "Execution"),
                actionButton(
                  "run_cosero_btn",
                  "Run COSERO Model",
                  icon = icon("play"),
                  class = "btn-primary btn-lg",
                  style = "width: 100%; margin-bottom: 10px;"
                ),
                uiOutput("run_progress_ui"),
                tags$hr(),

                tags$div(class = "subsection-header", "Results Summary"),
                verbatimTextOutput("run_results_summary")
                )
              )
            )
          )
        ),

        # Time Series Tab ####
        tabPanel(
          "Time Series",
          icon = icon("chart-line"),

          tags$div(class = "section-header", "Time Series Analysis"),

          # Collapsible Control Panel
          tags$div(
            style = "margin: 15px 0;",
            tags$button(
              id = "toggle_ts_controls",
              class = "btn btn-default btn-sm",
              style = "margin-bottom: 10px;",
              onclick = "$(this).find('i').toggleClass('fa-chevron-down fa-chevron-up'); $('#ts_controls_panel').slideToggle();",
              icon("chevron-up"),
              " Show/Hide Controls"
            ),
            tags$div(
              id = "ts_controls_panel",
              style = "display: block;",
              wellPanel(
                style = "background-color: #f8f9fa; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                fluidRow(
                  column(3,
                    tags$div(class = "subsection-header", "Data Selection"),
                    textInput(
                      "output_dir",
                      "Output Directory:",
                      value = default_output_dir
                    ),
                    actionButton(
                      "load_data",
                      "Load Data",
                      icon = icon("folder-open"),
                      class = "btn-primary"
                    ),
                    checkboxInput(
                      "use_cache",
                      "Use cache (faster)",
                      value = TRUE
                    ),
                    tags$hr(),
                    verbatimTextOutput("status_text")
                  ),
                  column(3,
                    tags$div(class = "subsection-header", "Display Options"),
                    uiOutput("subbasin_selector"),
                    fluidRow(
                      column(6, actionButton("prev_subbasin", "< Prev", class = "btn-sm", style = "width: 100%;")),
                      column(6, actionButton("next_subbasin", "Next >", class = "btn-sm", style = "width: 100%;"))
                    ),
                    tags$br(),
                    uiOutput("date_range_slider"),
                    actionButton("reset_zoom", "Reset Zoom", icon = icon("refresh"))
                  ),
                  column(3,
                    tags$div(class = "subsection-header", "Storage Variables"),
                    checkboxGroupInput(
                      "wb_storage_vars",
                      NULL,
                      choices = c(
                        "Soil Moisture Top (BW0)" = "BW0",
                        "Groundwater state (BW3)" = "BW3",
                        "Snow Water Equiv. (SWW)" = "SWW"
                      ),
                      selected = c("BW0", "BW3", "SWW")
                    )
                  ),
                  column(3,
                    tags$div(class = "subsection-header", "Cumulative Fluxes"),
                    checkboxGroupInput(
                      "wb_cumulative_vars",
                      NULL,
                      choices = c(
                        "Cumulative Precipitation" = "P_cum",
                        "Cumulative ET" = "ETAGEB_cum",
                        "Cumulative Runoff" = "QABGEB_cum"
                      ),
                      selected = c("P_cum", "ETAGEB_cum", "QABGEB_cum")
                    )
                  )
                )
              )
            )
          ),

          fluidRow(
            column(12, plotlyOutput("plot_discharge", height = "300px"))
          ),
          tags$hr(),

          fluidRow(
            column(12, plotlyOutput("plot_precipitation", height = "200px"))
          ),
          tags$hr(),

          fluidRow(
            column(12, plotlyOutput("plot_runoff_components", height = "250px"))
          ),
          tags$hr(),

          fluidRow(
            column(12, plotlyOutput("plot_water_balance", height = "300px"))
          )
        ),

        # Seasonality Tab ####
        tabPanel(
          "Seasonality",
          icon = icon("calendar"),

          tags$div(class = "section-header", "Seasonality Analysis"),

          # Collapsible Control Panel
          tags$div(
            style = "margin: 15px;",
            tags$button(
              id = "toggle_seasonality_controls",
              class = "btn btn-default btn-sm",
              style = "margin-bottom: 10px;",
              onclick = "$(this).find('i').toggleClass('fa-chevron-down fa-chevron-up'); $('#seasonality_controls_panel').slideToggle();",
              icon("chevron-up"),
              " Show/Hide Controls"
            ),
            tags$div(
              id = "seasonality_controls_panel",
              style = "display: block;",
              wellPanel(
                style = "background-color: #f8f9fa; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                fluidRow(
                  column(4,
                    tags$div(class = "subsection-header", "Display Options"),
                    uiOutput("subbasin_selector_seasonality"),
                    fluidRow(
                      column(6, actionButton("prev_subbasin_seasonality", "< Prev", class = "btn-sm", style = "width: 100%;")),
                      column(6, actionButton("next_subbasin_seasonality", "Next >", class = "btn-sm", style = "width: 100%;"))
                    )
                  ),
                  column(4,
                    tags$div(class = "subsection-header", "Storage Variables"),
                    checkboxGroupInput(
                      "wb_storage_vars_seasonality",
                      NULL,
                      choices = c(
                        "Soil Moisture Top (BW0)" = "BW0",
                        "Groundwater state (BW3)" = "BW3",
                        "Snow Water Equiv. (SWW)" = "SWW"
                      ),
                      selected = c("BW0", "BW3", "SWW")
                    )
                  ),
                  column(4,
                    tags$div(class = "subsection-header", "Status"),
                    verbatimTextOutput("status_text_seasonality")
                  )
                )
              )
            )
          ),

          fluidRow(
            column(12, plotlyOutput("plot_seasonality_discharge", height = "300px"))
          ),
          tags$hr(),

          fluidRow(
            column(12, plotlyOutput("plot_seasonality_precipitation", height = "200px"))
          ),
          tags$hr(),

          fluidRow(
            column(12, plotlyOutput("plot_seasonality_runoff", height = "250px"))
          ),
          tags$hr(),

          fluidRow(
            column(12, plotlyOutput("plot_seasonality_water_balance", height = "300px"))
          )
        ),

        # Statistics Tab ####
        tabPanel(
          "Statistics",
          icon = icon("table"),

          tags$div(class = "section-header", "Performance Statistics"),

          # Collapsible Control Panel
          tags$div(
            style = "margin: 15px;",
            tags$button(
              id = "toggle_stats_controls",
              class = "btn btn-default btn-sm",
              style = "margin-bottom: 10px;",
              onclick = "$(this).find('i').toggleClass('fa-chevron-down fa-chevron-up'); $('#stats_controls_panel').slideToggle();",
              icon("chevron-up"),
              " Show/Hide Controls"
            ),
            tags$div(
              id = "stats_controls_panel",
              style = "display: block;",
              wellPanel(
                style = "background-color: #f8f9fa; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);",
                fluidRow(
                  column(4,
                    tags$div(class = "subsection-header", "Display Options"),
                    radioButtons(
                      "stats_view_mode",
                      "View Mode:",
                      choices = list(
                        "All Subbasins (by Objective Function)" = "all",
                        "Single Subbasin (all metrics)" = "single"
                      ),
                      selected = "all"
                    )
                  ),
                  column(4,
                    tags$div(class = "subsection-header", "Selection"),
                    # Shown when view_mode = "all"
                    conditionalPanel(
                      condition = "input.stats_view_mode == 'all'",
                      uiOutput("objective_function_selector")
                    ),
                    # Shown when view_mode = "single"
                    conditionalPanel(
                      condition = "input.stats_view_mode == 'single'",
                      uiOutput("subbasin_selector_stats"),
                      fluidRow(
                        column(6, actionButton("prev_subbasin_stats", "< Prev", class = "btn-sm", style = "width: 100%;")),
                        column(6, actionButton("next_subbasin_stats", "Next >", class = "btn-sm", style = "width: 100%;"))
                      )
                    )
                  ),
                  column(4,
                    tags$div(class = "subsection-header", "Status"),
                    verbatimTextOutput("status_text_stats")
                  )
                )
              )
            )
          ),

          # Show plot when in "all" mode
          conditionalPanel(
            condition = "input.stats_view_mode == 'all'",
            plotlyOutput("stats_comparison_plot", height = "400px"),
            tags$hr()
          ),

          # Always show table
          DTOutput("stats_table"),

          # Show additional info only in "single" mode
          conditionalPanel(
            condition = "input.stats_view_mode == 'single'",
            tags$hr(),
            tags$div(class = "subsection-header", "Additional Information"),
            verbatimTextOutput("stats_summary")
          )
        ),

        # Export Tab ####
        tabPanel(
          "Export & Download",
          icon = icon("download"),

          tags$div(class = "section-header", "Export Options"),

          fluidRow(
            column(6,
              card(
                card_header("Export Plots"),
                card_body(
                  p("Download plots as image files (requires kaleido package)"),
                  downloadButton("download_discharge_png", "Discharge Plot (PNG)", class = "btn-primary mb-2", style = "width: 100%;"),
                  downloadButton("download_precip_png", "Precipitation Plot (PNG)", class = "btn-primary mb-2", style = "width: 100%;"),
                  downloadButton("download_runoff_png", "Runoff Components (PNG)", class = "btn-primary mb-2", style = "width: 100%;"),
                  downloadButton("download_wb_png", "Water Balance / States (PNG)", class = "btn-primary", style = "width: 100%;")
                )
              )
            ),
            column(6,
              card(
                card_header("Export Data"),
                card_body(
                  p("Download filtered data as CSV files"),
                  downloadButton("download_discharge_csv", "Discharge Data (CSV)", class = "btn-success mb-2", style = "width: 100%;"),
                  downloadButton("download_precip_csv", "Precipitation Data (CSV)", class = "btn-success mb-2", style = "width: 100%;"),
                  downloadButton("download_runoff_csv", "Runoff Components (CSV)", class = "btn-success mb-2", style = "width: 100%;"),
                  downloadButton("download_wb_csv", "Water Balance / States (CSV)", class = "btn-success", style = "width: 100%;")
                )
              )
            )
          )
        )
    )
)

# Server Logic ####
server <- function(input, output, session) {

  # Reactive Values ####
  rv <- reactiveValues(
    cosero_data = NULL,
    subbasin_data = NULL,
    date_range_full = NULL,
    status_message = "No data loaded. Click 'Load Data' to begin.",
    run_result = NULL,
    run_in_progress = FALSE
  )

  # COSERO Run Tab Logic ####

  # Directory browser setup ####
  # Set up convenient starting locations for the file browser
  volumes <- c(
    "Working Directory" = normalizePath(getwd(), winslash = "/"),
    "Home" = fs::path_home(),
    getVolumes()()  # This adds all drive letters (C:, D:, etc.)
  )
  shinyDirChoose(input, "run_project_dir_btn", roots = volumes, session = session)

  # Parameter file browser setup ####
  shinyFileChoose(input, "param_file_btn", roots = volumes, session = session,
                  filetypes = c("txt"))

  # Update text input when directory is selected via browser ####
  observeEvent(input$run_project_dir_btn, {
    if (!is.null(input$run_project_dir_btn) && !is.integer(input$run_project_dir_btn)) {
      dir_path <- parseDirPath(volumes, input$run_project_dir_btn)
      if (length(dir_path) > 0) {
        updateTextInput(session, "run_project_path", value = dir_path)
      }
    }
  })

  # Update text input when parameter file is selected via browser ####
  observeEvent(input$param_file_btn, {
    if (!is.null(input$param_file_btn) && !is.integer(input$param_file_btn)) {
      file_path <- parseFilePaths(volumes, input$param_file_btn)
      if (nrow(file_path) > 0) {
        updateTextInput(session, "param_file_path", value = as.character(file_path$datapath))
      }
    }
  })

  # Clear parameter file selection ####
  observeEvent(input$clear_param_file, {
    updateTextInput(session, "param_file_path", value = "")
  })

  # Auto-update Output Directory when Project Directory changes ####
  observeEvent(input$run_project_path, {
    if (!is.null(input$run_project_path) && nchar(input$run_project_path) > 0) {
      project_path <- normalized_project_path()
      if (dir.exists(project_path)) {
        output_dir <- file.path(project_path, "output")
        updateTextInput(session, "output_dir", value = normalizePath(output_dir, winslash = "/", mustWork = FALSE))
      }
    }
  })

  # Toggle button icon for Advanced Parameters ####
  observeEvent(input$toggle_advanced_params, {
    if (input$toggle_advanced_params %% 2 == 1) {
      # Expanded
      updateActionButton(session, "toggle_advanced_params",
                        label = "Hide Advanced Parameters",
                        icon = icon("chevron-up"))
    } else {
      # Collapsed
      updateActionButton(session, "toggle_advanced_params",
                        label = "Show Advanced Parameters",
                        icon = icon("chevron-down"))
    }
  })

  # Normalized project path (handles / and \ and removes quotes) ####
  normalized_project_path <- reactive({
    req(input$run_project_path)
    normalize_path_input(input$run_project_path)
  })

  # Click zone triggers file browser ####
  observe({
    shinyjs::runjs("
      var dropZone = document.getElementById('project_drag_zone');
      if (dropZone) {
        // Click to open file browser
        dropZone.addEventListener('click', function(e) {
          document.getElementById('run_project_dir_btn').click();
        });
      }

      var paramDropZone = document.getElementById('param_file_drag_zone');
      if (paramDropZone) {
        // Click to open parameter file browser
        paramDropZone.addEventListener('click', function(e) {
          document.getElementById('param_file_btn').click();
        });
      }
    ")
  })

  # Project validation ####
  output$project_validation_ui <- renderUI({
    req(input$run_project_path)

    if (nchar(input$run_project_path) == 0) {
      return(tags$p(class = "input-hint", "Enter project path to validate"))
    }

    project_path <- normalized_project_path()

    if (!dir.exists(project_path)) {
      return(tags$div(class = "validation-error", "Directory does not exist"))
    }

    exe_path <- file.path(project_path, "COSERO.exe")
    if (!file.exists(exe_path)) {
      return(tags$div(class = "validation-warning", "COSERO.exe not found in directory"))
    }

    return(tags$div(class = "validation-success", "Valid COSERO project"))
  })

  # Parameter file validation ####
  output$param_file_validation_ui <- renderUI({
    # If no parameter file specified, that's OK (will use default)
    if (is.null(input$param_file_path) || nchar(input$param_file_path) == 0) {
      return(tags$p(
        class = "input-hint",
        style = "font-size: 0.85em; margin-top: -5px;",
        "No custom parameter file selected. Will use default para.txt from project."
      ))
    }

    # Normalize the path
    param_path <- normalize_path_input(input$param_file_path)

    # Check if file exists
    if (!file.exists(param_path)) {
      return(tags$div(
        class = "validation-error",
        style = "font-size: 0.9em;",
        "Parameter file does not exist"
      ))
    }

    # Check if it's a .txt file
    if (!grepl("\\.txt$", param_path, ignore.case = TRUE)) {
      return(tags$div(
        class = "validation-warning",
        style = "font-size: 0.9em;",
        "Warning: Parameter file should have .txt extension"
      ))
    }

    # Show success with file info
    file_info <- file.info(param_path)
    file_size_kb <- round(file_info$size / 1024, 2)

    return(tags$div(
      class = "validation-success",
      style = "font-size: 0.9em;",
      sprintf("Valid parameter file (%s KB, modified: %s)",
              file_size_kb,
              format(file_info$mtime, "%Y-%m-%d %H:%M"))
    ))
  })

  # Warm start status ####
  output$warm_start_status_ui <- renderUI({
    req(input$run_project_path, input$run_mode)

    if (as.integer(input$run_mode) == 2) {  # Warm start
      project_path <- normalized_project_path()
      if (nchar(project_path) > 0 && dir.exists(project_path)) {
        # Check both possible locations for statevar.dmp (input/ first since that's where COSERO.exe looks)
        statevar_input <- file.path(project_path, "input", "statevar.dmp")
        statevar_output <- file.path(project_path, "output", "statevar.dmp")

        if (file.exists(statevar_input)) {
          file_info <- file.info(statevar_input)
          return(tags$div(
            class = "validation-success",
            style = "font-size: 0.9em;",
            sprintf("statevar.dmp found in input/ (modified: %s)", format(file_info$mtime, "%Y-%m-%d %H:%M"))
          ))
        } else if (file.exists(statevar_output)) {
          file_info <- file.info(statevar_output)
          return(tags$div(
            class = "validation-warning",
            style = "font-size: 0.9em;",
            sprintf("statevar.dmp found in output/ but should be in input/ (modified: %s)", format(file_info$mtime, "%Y-%m-%d %H:%M"))
          ))
        } else {
          return(tags$div(
            class = "validation-error",
            style = "font-size: 0.9em;",
            "statevar.dmp not found in input/ or output/ folders! Warm start will fail."
          ))
        }
      }
    }
    return(NULL)
  })

  # Load defaults from project ####
  observeEvent(input$load_defaults_btn, {
    req(input$run_project_path)

    project_path <- normalized_project_path()

    # Validate project directory
    if (!dir.exists(project_path)) {
      showNotification("Project directory does not exist", type = "error", duration = 5)
      return()
    }

    # Check if defaults.txt exists
    defaults_file <- file.path(project_path, "input", "defaults.txt")
    if (!file.exists(defaults_file)) {
      showNotification(
        paste0("defaults.txt not found at: ", defaults_file, "\nUsing default values instead."),
        type = "warning",
        duration = 8
      )
    }

    tryCatch({
      defaults <- read_cosero_defaults_safe(project_path)

      # Update date inputs
      start_parts <- parse_cosero_date(defaults$STARTDATE)
      end_parts <- parse_cosero_date(defaults$ENDDATE)

      updateDateInput(session, "run_start_date", value = start_parts$date)
      updateSelectInput(session, "run_start_hour", selected = start_parts$hour)
      updateSelectInput(session, "run_start_minute", selected = start_parts$minute)

      updateDateInput(session, "run_end_date", value = end_parts$date)
      updateSelectInput(session, "run_end_hour", selected = end_parts$hour)
      updateSelectInput(session, "run_end_minute", selected = end_parts$minute)

      # Update other inputs
      updateNumericInput(session, "run_spinup", value = as.integer(defaults$SPINUP))
      updateSelectInput(session, "run_output_type", selected = as.integer(defaults$OUTPUTTYPE))
      updateRadioButtons(session, "run_sc_flag", selected = as.integer(defaults$SC_FLAG))
      updateCheckboxInput(session, "run_outcontrol", value = as.integer(defaults$OUTCONTROL) == 1)
      updateNumericInput(session, "run_ikl", value = as.integer(defaults$IKL))
      updateNumericInput(session, "run_nclass", value = as.integer(defaults$NCLASS))
      updateTextInput(session, "run_project_info", value = as.character(defaults$PROJECTINFO))

      # Show appropriate success message
      if (file.exists(defaults_file)) {
        showNotification("Defaults loaded successfully from project!", type = "message", duration = 3)
      } else {
        showNotification("Using default values (defaults.txt not found)", type = "warning", duration = 5)
      }

    }, error = function(e) {
      showNotification(paste("Error loading defaults:", e$message), type = "error", duration = 5)
    })
  })

  # Run progress UI ####
  output$run_progress_ui <- renderUI({
    if (rv$run_in_progress) {
      tagList(
        tags$p(
          style = "color: blue; font-weight: bold;",
          icon("spinner", class = "fa-spin"),
          " COSERO model running..."
        )
      )
    } else {
      NULL
    }
  })

  # Run COSERO ####
  observeEvent(input$run_cosero_btn, {
    req(input$run_project_path)

    project_path <- normalized_project_path()

    # Validation
    if (!dir.exists(project_path)) {
      showNotification("Project directory does not exist", type = "error")
      return()
    }

    exe_path <- file.path(project_path, "COSERO.exe")
    if (!file.exists(exe_path)) {
      showNotification("COSERO.exe not found in project directory", type = "error")
      return()
    }

    # Date validation
    if (input$run_end_date <= input$run_start_date) {
      showNotification("End date must be after start date", type = "error")
      return()
    }

    # Warm start validation - check both possible locations
    if (as.integer(input$run_mode) == 2) {
      statevar_output <- file.path(project_path, "output", "statevar.dmp")
      statevar_input <- file.path(project_path, "input", "statevar.dmp")

      if (!file.exists(statevar_output) && !file.exists(statevar_input)) {
        showNotification(
          "Warm start selected but statevar.dmp not found in output/ or input/ folders!",
          type = "error",
          duration = 8
        )
        return()
      }
    }

    # Spinup validation for cold start
    if (as.integer(input$run_mode) == 1) {  # Cold start
      if (is.null(input$run_spinup) || input$run_spinup < 1) {
        showNotification("Spinup period must be at least 1 day for cold start", type = "error")
        return()
      }
    }

    # Set running state
    rv$run_in_progress <- TRUE
    rv$run_result <- NULL

    # Handle custom parameter file if specified
    custom_param_file <- NULL

    if (!is.null(input$param_file_path) && nchar(input$param_file_path) > 0) {
      custom_param_file <- normalize_path_input(input$param_file_path)

      # Validate custom parameter file exists
      if (!file.exists(custom_param_file)) {
        rv$run_in_progress <- FALSE
        showNotification(
          "Custom parameter file does not exist. Please check the path.",
          type = "error",
          duration = 8
        )
        return()
      }
    }

    # Build settings
    settings <- list(
      STARTDATE = format_cosero_date(
        input$run_start_date,
        as.integer(input$run_start_hour),
        as.integer(input$run_start_minute)
      ),
      ENDDATE = format_cosero_date(
        input$run_end_date,
        as.integer(input$run_end_hour),
        as.integer(input$run_end_minute)
      ),
      SPINUP = as.integer(input$run_spinup),
      OUTPUTTYPE = as.integer(input$run_output_type),
      SC_FLAG = as.integer(input$run_sc_flag),
      OUTCONTROL = as.integer(input$run_outcontrol),
      IKL = as.integer(input$run_ikl),
      NCLASS = as.integer(input$run_nclass),
      PROJECTINFO = input$run_project_info
    )

    # Run COSERO with progress
    withProgress(message = 'Running COSERO model...', value = 0, {
      tryCatch({
        incProgress(0.05, detail = "Preparing configuration...")

        # Handle custom parameter file - modify PARAFILE setting in run_cosero
        if (!is.null(custom_param_file)) {
          incProgress(0.02, detail = "Setting up custom parameter file...")

          # Simply pass the custom parameter file name to run_cosero via settings
          # This will override the PARAFILE setting from defaults.txt
          settings$PARAFILE <- basename(custom_param_file)

          # Copy the custom file to the input directory (in case it's not already there)
          input_dir <- file.path(project_path, "input")
          target_param_file <- file.path(input_dir, basename(custom_param_file))

          # Only copy if source and target are different
          if (normalizePath(custom_param_file, winslash = "/", mustWork = FALSE) !=
              normalizePath(target_param_file, winslash = "/", mustWork = FALSE)) {
            file.copy(custom_param_file, target_param_file, overwrite = TRUE)
            cat("Copied custom parameter file to input directory:", basename(custom_param_file), "\n")
          } else {
            cat("Custom parameter file already in input directory:", basename(custom_param_file), "\n")
          }

          showNotification(
            paste0("Using custom parameter file: ", basename(custom_param_file)),
            type = "message",
            duration = 5
          )
        }

        # Show execution message
        incProgress(0.03, detail = "Executing COSERO (this may take a while)...")

        result <- run_cosero(
          project_path = project_path,
          defaults_settings = settings,
          statevar_source = as.integer(input$run_mode),
          tmmon_option = as.integer(input$run_tmmon_option),
          read_outputs = FALSE,
          quiet = FALSE
        )

        incProgress(0.7, detail = "Model execution completed")
        incProgress(0.05, detail = "Processing results...")

        rv$run_result <- result
        rv$run_in_progress <- FALSE

        if (result$success) {
          # Update the output directory path for manual loading
          output_dir <- file.path(project_path, "output")
          updateTextInput(session, "output_dir", value = output_dir)

          success_msg <- paste0("COSERO run completed in ", round(result$runtime, 2), " seconds! ",
                               "Go to Time Series tab and click 'Load Data' to view results.")
          if (!is.null(custom_param_file)) {
            success_msg <- paste0(success_msg, " (Used custom parameter file)")
          }

          showNotification(
            success_msg,
            type = "message",
            duration = 10
          )
        } else {
          showNotification(
            paste("COSERO run failed:", result$error_message),
            type = "error",
            duration = 10
          )
        }

      }, error = function(e) {
        rv$run_in_progress <- FALSE
        showNotification(paste("Error running COSERO:", e$message), type = "error", duration = 10)
      })
    })
  })

  # Run results summary ####
  output$run_results_summary <- renderText({
    if (is.null(rv$run_result)) {
      return("No run completed yet.")
    }

    result <- rv$run_result

    if (result$success) {
      summary_lines <- c(
        paste("Status: SUCCESS"),
        paste("Runtime:", round(result$runtime, 2), "seconds"),
        ""
      )

      # Add statistics if available
      if (!is.null(result$output_data$statistics)) {
        stats <- result$output_data$statistics
        summary_lines <- c(summary_lines, "Performance Metrics:")

        for (i in 1:nrow(stats)) {
          subbasin <- stats$SUBBASIN[i]
          nse <- ifelse(!is.na(stats$NSE[i]), sprintf("%.3f", stats$NSE[i]), "N/A")
          kge <- ifelse(!is.na(stats$KGE[i]), sprintf("%.3f", stats$KGE[i]), "N/A")

          summary_lines <- c(
            summary_lines,
            sprintf("  Subbasin %s: NSE=%s, KGE=%s", subbasin, nse, kge)
          )
        }
      }

      return(paste(summary_lines, collapse = "\n"))

    } else {
      return(paste("Status: FAILED\n", result$error_message))
    }
  })

  # Helper function: Check if cache is valid ####
  is_cache_valid <- function(output_dir, cache_file) {
    if (!file.exists(cache_file)) return(FALSE)

    # Get cache modification time
    cache_time <- file.info(cache_file)$mtime

    # Check key data files
    data_files <- c(
      "COSERO.runoff",
      "COSERO.prec",
      "COSERO.plus",
      "COSERO.plus1",
      "statistics.txt"
    )

    for (f in data_files) {
      fpath <- file.path(output_dir, f)
      if (file.exists(fpath)) {
        if (file.info(fpath)$mtime > cache_time) {
          return(FALSE)  # Data file is newer than cache
        }
      }
    }

    return(TRUE)
  }

  # Load Data ####
  observeEvent(input$load_data, {
    req(input$output_dir)

    # Create progress indicator
    withProgress(message = 'Loading COSERO data...', value = 0, {

      tryCatch({
        # Check if directory exists
        if (!dir.exists(input$output_dir)) {
          rv$status_message <- paste("Error: Directory not found:", input$output_dir)
          showNotification(
            paste0("Directory does not exist:\n", input$output_dir),
            type = "error",
            duration = 8
          )
          return()
        }

        # Check if essential files exist
        required_files <- c("COSERO.runoff", "statistics.txt")
        missing_files <- required_files[!file.exists(file.path(input$output_dir, required_files))]

        if (length(missing_files) > 0) {
          rv$status_message <- paste("Error: Required files missing:", paste(missing_files, collapse = ", "))
          showNotification(
            paste0("Required COSERO output files not found in directory:\n",
                   paste(missing_files, collapse = ", "),
                   "\n\nPlease check that COSERO has been run and output files exist."),
            type = "error",
            duration = 10
          )
          return()
        }

        # Define cache file path
        cache_file <- file.path(input$output_dir, ".cosero_cache.rds")

        # Try to load from cache if enabled
        if (input$use_cache && is_cache_valid(input$output_dir, cache_file)) {
          incProgress(0.5, detail = "Loading from cache...")

          rv$cosero_data <- readRDS(cache_file)

          # Get date range from runoff data
          if (!is.null(rv$cosero_data$runoff) && "Date" %in% colnames(rv$cosero_data$runoff)) {
            rv$date_range_full <- range(rv$cosero_data$runoff$Date, na.rm = TRUE)
          } else {
            rv$date_range_full <- NULL
          }

          incProgress(0.5, detail = "Cache loaded successfully!")

          rv$status_message <- paste0(
            "Data loaded from cache!\n",
            "OUTPUTTYPE: ", rv$cosero_data$metadata$outputtype, "\n",
            "Subbasins: ", length(rv$cosero_data$metadata$subbasins)
          )

        } else {
          # Load from source files
          incProgress(1/8, detail = "Detecting output type")
          outputtype <- detect_outputtype(input$output_dir)

          incProgress(1/8, detail = "Reading runoff data")
          runoff <- read_runoff(input$output_dir, quiet = TRUE)

          incProgress(1/8, detail = "Reading precipitation data")
          precipitation <- read_precipitation(input$output_dir, quiet = TRUE)

          incProgress(1/8, detail = "Reading runoff components")
          runoff_comp <- read_plus(input$output_dir, quiet = TRUE)

          incProgress(1/8, detail = "Reading water balance")
          water_balance <- read_plus1(input$output_dir, quiet = TRUE)

          incProgress(1/8, detail = "Reading statistics")
          statistics <- read_statistics(input$output_dir, quiet = TRUE)
          topology <- read_topology(input$output_dir, quiet = TRUE)

          incProgress(1/8, detail = "Reading additional files")
          glacier <- NULL
          if (outputtype >= 2) {
            glacier <- read_var_glac(input$output_dir, quiet = TRUE)
          }

          # Assemble data
          rv$cosero_data <- list(
            runoff = runoff,
            precipitation = precipitation,
            runoff_components = runoff_comp,
            water_balance = water_balance,
            statistics = statistics,
            topology = topology,
            glacier = glacier,
            metadata = list(
              outputtype = outputtype,
              output_dir = input$output_dir,
              subbasins = detect_subbasins(input$output_dir)
            )
          )

          # Get date range from runoff data
          if (!is.null(rv$cosero_data$runoff) && "Date" %in% colnames(rv$cosero_data$runoff)) {
            rv$date_range_full <- range(rv$cosero_data$runoff$Date, na.rm = TRUE)
          } else {
            rv$date_range_full <- NULL
          }

          # Save to cache
          if (input$use_cache) {
            incProgress(1/8, detail = "Saving to cache...")
            tryCatch({
              saveRDS(rv$cosero_data, cache_file, compress = "xz")
            }, error = function(e) {
              warning("Could not save cache: ", e$message)
            })
          }

          rv$status_message <- paste0(
            "Data loaded successfully!\n",
            "OUTPUTTYPE: ", rv$cosero_data$metadata$outputtype, "\n",
            "Subbasins: ", length(rv$cosero_data$metadata$subbasins),
            if (input$use_cache) "\nCache saved for next time." else ""
          )

          showNotification(
            paste0("Data loaded successfully! Found ", length(rv$cosero_data$metadata$subbasins), " subbasins."),
            type = "message",
            duration = 3
          )
        }

      }, error = function(e) {
        rv$status_message <- paste("Error loading data:", e$message)
        showNotification(
          paste0("Error loading data:\n", e$message,
                 "\n\nPlease check that the output directory contains valid COSERO output files."),
          type = "error",
          duration = 10
        )
      })
    })
  })

  # Subbasin Selectors (one for each tab to maintain independence) ####
  output$subbasin_selector <- renderUI({
    req(rv$cosero_data)

    subbasins <- rv$cosero_data$metadata$subbasins
    if (length(subbasins) == 0) {
      return(p("No subbasins found in data"))
    }

    selectInput(
      "selected_subbasin",
      "Select Subbasin:",
      choices = subbasins,
      selected = subbasins[1]
    )
  })

  output$subbasin_selector_seasonality <- renderUI({
    req(rv$cosero_data)

    subbasins <- rv$cosero_data$metadata$subbasins
    if (length(subbasins) == 0) {
      return(p("No subbasins found in data"))
    }

    selectInput(
      "selected_subbasin_seasonality",
      "Select Subbasin:",
      choices = subbasins,
      selected = input$selected_subbasin %||% subbasins[1]
    )
  })

  output$subbasin_selector_stats <- renderUI({
    req(rv$cosero_data)

    subbasins <- rv$cosero_data$metadata$subbasins
    if (length(subbasins) == 0) {
      return(p("No subbasins found in data"))
    }

    selectInput(
      "selected_subbasin_stats",
      "Select Subbasin:",
      choices = subbasins,
      selected = input$selected_subbasin %||% subbasins[1]
    )
  })

  # Sync subbasin selections across tabs ####
  observeEvent(input$selected_subbasin, {
    if (!is.null(input$selected_subbasin_seasonality)) {
      updateSelectInput(session, "selected_subbasin_seasonality", selected = input$selected_subbasin)
    }
    if (!is.null(input$selected_subbasin_stats)) {
      updateSelectInput(session, "selected_subbasin_stats", selected = input$selected_subbasin)
    }
  })

  observeEvent(input$selected_subbasin_seasonality, {
    if (!is.null(input$selected_subbasin)) {
      updateSelectInput(session, "selected_subbasin", selected = input$selected_subbasin_seasonality)
    }
  })

  observeEvent(input$selected_subbasin_stats, {
    if (!is.null(input$selected_subbasin)) {
      updateSelectInput(session, "selected_subbasin", selected = input$selected_subbasin_stats)
    }
  })

  # Date Range Slider ####
  output$date_range_slider <- renderUI({
    req(rv$date_range_full)

    sliderInput(
      "date_range",
      "Date Range:",
      min = rv$date_range_full[1],
      max = rv$date_range_full[2],
      value = rv$date_range_full,
      timeFormat = "%Y-%m-%d",
      width = "100%"
    )
  })

  # Reset Zoom ####
  observeEvent(input$reset_zoom, {
    req(rv$date_range_full)
    updateSliderInput(session, "date_range", value = rv$date_range_full)
  })

  # Subbasin Navigation - Previous ####
  observeEvent(input$prev_subbasin, {
    req(rv$cosero_data, input$selected_subbasin)

    subbasins <- rv$cosero_data$metadata$subbasins
    current_idx <- which(subbasins == input$selected_subbasin)

    if (length(current_idx) > 0 && current_idx > 1) {
      updateSelectInput(session, "selected_subbasin", selected = subbasins[current_idx - 1])
    }
  })

  # Subbasin Navigation - Next ####
  observeEvent(input$next_subbasin, {
    req(rv$cosero_data, input$selected_subbasin)

    subbasins <- rv$cosero_data$metadata$subbasins
    current_idx <- which(subbasins == input$selected_subbasin)

    if (length(current_idx) > 0 && current_idx < length(subbasins)) {
      updateSelectInput(session, "selected_subbasin", selected = subbasins[current_idx + 1])
    }
  })

  # Subbasin Navigation - Seasonality Tab ####
  observeEvent(input$prev_subbasin_seasonality, {
    req(rv$cosero_data, input$selected_subbasin_seasonality)

    subbasins <- rv$cosero_data$metadata$subbasins
    current_idx <- which(subbasins == input$selected_subbasin_seasonality)

    if (length(current_idx) > 0 && current_idx > 1) {
      updateSelectInput(session, "selected_subbasin_seasonality", selected = subbasins[current_idx - 1])
    }
  })

  observeEvent(input$next_subbasin_seasonality, {
    req(rv$cosero_data, input$selected_subbasin_seasonality)

    subbasins <- rv$cosero_data$metadata$subbasins
    current_idx <- which(subbasins == input$selected_subbasin_seasonality)

    if (length(current_idx) > 0 && current_idx < length(subbasins)) {
      updateSelectInput(session, "selected_subbasin_seasonality", selected = subbasins[current_idx + 1])
    }
  })

  # Subbasin Navigation - Statistics Tab ####
  observeEvent(input$prev_subbasin_stats, {
    req(rv$cosero_data, input$selected_subbasin_stats)

    subbasins <- rv$cosero_data$metadata$subbasins
    current_idx <- which(subbasins == input$selected_subbasin_stats)

    if (length(current_idx) > 0 && current_idx > 1) {
      updateSelectInput(session, "selected_subbasin_stats", selected = subbasins[current_idx - 1])
    }
  })

  observeEvent(input$next_subbasin_stats, {
    req(rv$cosero_data, input$selected_subbasin_stats)

    subbasins <- rv$cosero_data$metadata$subbasins
    current_idx <- which(subbasins == input$selected_subbasin_stats)

    if (length(current_idx) > 0 && current_idx < length(subbasins)) {
      updateSelectInput(session, "selected_subbasin_stats", selected = subbasins[current_idx + 1])
    }
  })

  # Sync water balance variables between tabs ####
  observeEvent(input$wb_storage_vars, {
    if (!is.null(input$wb_storage_vars_seasonality)) {
      updateCheckboxGroupInput(session, "wb_storage_vars_seasonality", selected = input$wb_storage_vars)
    }
  })

  observeEvent(input$wb_storage_vars_seasonality, {
    if (!is.null(input$wb_storage_vars)) {
      updateCheckboxGroupInput(session, "wb_storage_vars", selected = input$wb_storage_vars_seasonality)
    }
  })

  # Status text outputs for each tab ####
  output$status_text_seasonality <- renderText({
    rv$status_message
  })

  output$status_text_stats <- renderText({
    rv$status_message
  })

  # Debounced date range (only updates 500ms after user stops dragging)
  date_range_debounced <- debounce(reactive(input$date_range), 500)

  # Prepare Subbasin Data with caching ####
  subbasin_data_cached <- reactive({
    req(rv$cosero_data, input$selected_subbasin)

    # Use debounced date range to avoid constant updates while dragging
    date_range <- date_range_debounced()

    prepare_subbasin_data(
      rv$cosero_data,
      input$selected_subbasin,
      date_range
    )
  }) %>% bindCache(input$selected_subbasin, date_range_debounced())

  # Update reactive value for compatibility
  observe({
    rv$subbasin_data <- subbasin_data_cached()
  })

  # Status Text ####
  output$status_text <- renderText({
    rv$status_message
  })

  # Time Series Plots ####

  # Plot 1: Discharge
  output$plot_discharge <- renderPlotly({
    req(rv$subbasin_data)
    plot_discharge(rv$subbasin_data$discharge)
  })

  # Plot 2: Precipitation
  output$plot_precipitation <- renderPlotly({
    req(rv$subbasin_data)
    plot_precipitation(rv$subbasin_data$precipitation, separate = FALSE)
  })

  # Plot 3: Runoff Components
  output$plot_runoff_components <- renderPlotly({
    req(rv$subbasin_data)
    plot_runoff_components(rv$subbasin_data$runoff_components, glacier_data = rv$subbasin_data$glacier)
  })

  # Plot 4: Water Balance (only updates when variables change)
  output$plot_water_balance <- renderPlotly({
    req(rv$subbasin_data)

    # Combine selected storage and cumulative variables
    selected_vars <- c(input$wb_storage_vars, input$wb_cumulative_vars)

    # Determine if cumulative calculation is needed
    needs_cumulative <- any(grepl("_cum$", selected_vars))

    plot_water_balance(
      rv$subbasin_data$water_balance,
      selected_vars = selected_vars,
      show_cumulative = needs_cumulative
    )
  }) %>% bindEvent(rv$subbasin_data, input$wb_storage_vars, input$wb_cumulative_vars)


  # Objective Function Selector ####
  output$objective_function_selector <- renderUI({
    req(rv$cosero_data$statistics)

    stats <- rv$cosero_data$statistics
    # Get available objective functions (exclude 'sb' column)
    available_ofs <- setdiff(colnames(stats), "sb")

    if (length(available_ofs) == 0) {
      return(p("No objective functions found"))
    }

    selectInput(
      "selected_of",
      "Objective Function:",
      choices = available_ofs,
      selected = if ("NSE" %in% available_ofs) "NSE" else available_ofs[1]
    )
  })

  # Statistics Comparison Plot (for "all" view mode) ####
  output$stats_comparison_plot <- renderPlotly({
    req(rv$cosero_data$statistics, input$selected_of, input$stats_view_mode == "all")

    stats <- rv$cosero_data$statistics
    of_col <- input$selected_of

    if (!of_col %in% colnames(stats)) {
      return(NULL)
    }

    # Create bar plot
    plot_ly(
      data = stats,
      x = ~sb,
      y = stats[[of_col]],
      type = "bar",
      marker = list(
        color = stats[[of_col]],
        colorscale = "RdYlGn",
        showscale = TRUE,
        colorbar = list(title = of_col)
      ),
      text = ~paste0("Subbasin: ", sb, "<br>", of_col, ": ", round(stats[[of_col]], 3)),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste(of_col, "across all subbasins"),
        xaxis = list(title = "Subbasin", type = "category"),
        yaxis = list(title = of_col),
        hovermode = "closest"
      )
  })

  # Statistics Table ####
  output$stats_table <- renderDT({
    req(rv$cosero_data$statistics, input$stats_view_mode)

    if (input$stats_view_mode == "all") {
      # Show all subbasins with selected OF
      req(input$selected_of)

      stats <- rv$cosero_data$statistics
      of_col <- input$selected_of

      if (!of_col %in% colnames(stats)) {
        return(NULL)
      }

      # Select only subbasin and selected OF columns
      display_data <- stats[, c("sb", of_col), drop = FALSE]

      dt <- datatable(
        display_data,
        options = list(
          pageLength = 20,
          scrollY = "400px",
          scrollX = TRUE,
          dom = 'ft'
        ),
        rownames = FALSE
      )

      # Round numeric column
      dt <- formatRound(dt, columns = of_col, digits = 3)

      return(dt)

    } else {
      # Show single subbasin with all metrics
      req(rv$subbasin_data$statistics)

      stats <- rv$subbasin_data$statistics

      # Identify numeric columns for rounding
      numeric_cols <- names(stats)[sapply(stats, is.numeric)]

      # Format for display
      dt <- datatable(
        stats,
        options = list(
          pageLength = 5,
          scrollX = TRUE,
          dom = 't'
        ),
        rownames = FALSE
      )

      # Round numeric columns
      if (length(numeric_cols) > 0) {
        dt <- formatRound(dt, columns = numeric_cols, digits = 3)
      }

      return(dt)
    }
  })

  # Seasonality Plots ####

  # Prepare seasonality data (cached - only recalculates when subbasin data changes)
  seasonality_data <- reactive({
    req(rv$subbasin_data, rv$cosero_data$statistics)

    # Get spin-up timesteps from statistics
    spinup <- attr(rv$cosero_data$statistics, "spinup_timestep")
    if (is.null(spinup) || is.na(spinup)) {
      spinup <- 0
    }

    prepare_seasonality_data(rv$subbasin_data, spinup)
  }) %>% bindCache(rv$subbasin_data, rv$cosero_data$statistics)

  # Seasonality plots
  output$plot_seasonality_discharge <- renderPlotly({
    req(seasonality_data())
    plot_seasonality_discharge(seasonality_data()$discharge)
  })

  output$plot_seasonality_precipitation <- renderPlotly({
    req(seasonality_data())
    plot_seasonality_precipitation(seasonality_data()$precipitation)
  })

  output$plot_seasonality_runoff <- renderPlotly({
    req(seasonality_data())
    plot_seasonality_runoff(seasonality_data()$runoff_components)
  })

  output$plot_seasonality_water_balance <- renderPlotly({
    req(seasonality_data())
    # Only pass storage variables to water balance seasonality plot
    # P and ET are now shown in precipitation panel, states only in water balance panel
    # Use seasonality-specific variable selector (synced with time series)
    storage_vars <- input$wb_storage_vars_seasonality %||% input$wb_storage_vars
    plot_seasonality_water_balance(seasonality_data()$water_balance, storage_vars)
  }) %>% bindEvent(seasonality_data(), input$wb_storage_vars_seasonality, input$wb_storage_vars)

  # Statistics Summary ####
  output$stats_summary <- renderText({
    req(rv$subbasin_data)

    sb_id <- rv$subbasin_data$subbasin_id
    stats <- rv$subbasin_data$statistics

    if (is.null(stats) || nrow(stats) == 0) {
      return("No statistics available for this subbasin")
    }

    # Build summary with available columns
    summary_lines <- paste0("Subbasin: ", sb_id)

    if ("NSE" %in% colnames(stats)) {
      summary_lines <- c(summary_lines, paste0("NSE: ", round(stats$NSE, 3)))
    }
    if ("KGE" %in% colnames(stats)) {
      summary_lines <- c(summary_lines, paste0("KGE: ", round(stats$KGE, 3)))
    }
    if ("RMSE" %in% colnames(stats)) {
      summary_lines <- c(summary_lines, paste0("RMSE: ", round(stats$RMSE, 3)))
    }
    if ("CORR" %in% colnames(stats)) {
      summary_lines <- c(summary_lines, paste0("Correlation: ", round(stats$CORR, 3)))
    }
    if ("BIAS" %in% colnames(stats)) {
      summary_lines <- c(summary_lines, paste0("Bias: ", round(stats$BIAS, 3)))
    }

    paste(summary_lines, collapse = "\n")
  })

  # Download Handlers - PNG ####
  output$download_discharge_png <- downloadHandler(
    filename = function() {
      paste0("discharge_", input$selected_subbasin, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$subbasin_data)
      p <- plot_discharge(rv$subbasin_data$discharge)
      success <- export_plot_png(p, file, width = 1200, height = 400)
      if (!success) {
        showNotification(
          "PNG export failed. Please install kaleido package: install.packages('kaleido')",
          type = "error",
          duration = 10
        )
      }
    }
  )

  output$download_precip_png <- downloadHandler(
    filename = function() {
      paste0("precipitation_", input$selected_subbasin, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$subbasin_data)
      p <- plot_precipitation(rv$subbasin_data$precipitation, separate = FALSE)
      success <- export_plot_png(p, file, width = 1200, height = 400)
      if (!success) {
        showNotification(
          "PNG export failed. Please install kaleido package: install.packages('kaleido')",
          type = "error",
          duration = 10
        )
      }
    }
  )

  output$download_runoff_png <- downloadHandler(
    filename = function() {
      paste0("runoff_components_", input$selected_subbasin, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$subbasin_data)
      p <- plot_runoff_components(rv$subbasin_data$runoff_components, glacier_data = rv$subbasin_data$glacier)
      success <- export_plot_png(p, file, width = 1200, height = 400)
      if (!success) {
        showNotification(
          "PNG export failed. Please install kaleido package: install.packages('kaleido')",
          type = "error",
          duration = 10
        )
      }
    }
  )

  output$download_wb_png <- downloadHandler(
    filename = function() {
      paste0("water_balance_", input$selected_subbasin, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(rv$subbasin_data)
      selected_vars <- c(input$wb_storage_vars, input$wb_cumulative_vars)
      needs_cumulative <- any(grepl("_cum$", selected_vars))
      p <- plot_water_balance(
        rv$subbasin_data$water_balance,
        selected_vars = selected_vars,
        show_cumulative = needs_cumulative
      )
      success <- export_plot_png(p, file, width = 1200, height = 400)
      if (!success) {
        showNotification(
          "PNG export failed. Please install kaleido package: install.packages('kaleido')",
          type = "error",
          duration = 10
        )
      }
    }
  )

  # Download Handlers - CSV ####
  output$download_discharge_csv <- downloadHandler(
    filename = function() {
      paste0("discharge_", input$selected_subbasin, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$subbasin_data$discharge)
      write.csv(rv$subbasin_data$discharge, file, row.names = FALSE)
    }
  )

  output$download_precip_csv <- downloadHandler(
    filename = function() {
      paste0("precipitation_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$subbasin_data$precipitation)
      write.csv(rv$subbasin_data$precipitation, file, row.names = FALSE)
    }
  )

  output$download_runoff_csv <- downloadHandler(
    filename = function() {
      paste0("runoff_components_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$subbasin_data$runoff_components)
      write.csv(rv$subbasin_data$runoff_components, file, row.names = FALSE)
    }
  )

  output$download_wb_csv <- downloadHandler(
    filename = function() {
      paste0("water_balance_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(rv$subbasin_data$water_balance)
      write.csv(rv$subbasin_data$water_balance, file, row.names = FALSE)
    }
  )
}

# Run App ####
shinyApp(ui = ui, server = server)
