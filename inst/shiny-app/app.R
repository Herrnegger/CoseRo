# Title: COSERO Workbench - Main Application Shell
# Description: Slim entry point that sources tab modules and assembles the page_navbar layout
# Author/Architect: Mathew Herrnegger
# Coding: Claude/PI
# Date: 2026-02-28

# Load required libraries
library(shiny)
library(bslib)
library(plotly)
library(dplyr)
library(lubridate)
library(DT)
library(shinyFiles)
library(shinyjs)

# Load CoseRo package
if (requireNamespace("CoseRo", quietly = TRUE)) {
  library(CoseRo)
} else {
  app_dir <- getwd()
  pkg_dir <- normalizePath(file.path(app_dir, "../.."), mustWork = FALSE)
  source(file.path(pkg_dir, "R/app_helpers.R"))
  source(file.path(pkg_dir, "R/cosero_readers.R"))
  source(file.path(pkg_dir, "R/cosero_run.R"))
  source(file.path(pkg_dir, "R/cosero_config.R"))
}

# Source tab modules
module_dir <- file.path(getwd(), "R")
if (!dir.exists(module_dir)) {
  # When running from installed package, getwd() may differ
  module_dir <- file.path(system.file("shiny-app", package = "CoseRo"), "R")
}
for (f in list.files(module_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = TRUE)
}

# ── Theme ─────────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version = 5,
  bootswatch = "yeti",
  primary = "#1e3d53",
  secondary = "#1a6b8a",
  success = "#8ba87a",
  info = "#4db8c7",
  warning = "#c5a84d",
  danger = "#c75a4d",
  base_font = font_google("Roboto"),
  heading_font = font_google("Raleway", wght = c(400, 500)),
  code_font = font_google("Fira Code"),
  font_scale = 0.95,
  bg = "#f8f9fa",
  fg = "#212529",
  "body-bg" = "#f8f9fa",
  "card-border-color" = "#e9ecef",
  "card-cap-bg" = "#f8f9fa",
  "well-bg" = "#f8f9fa",
  "nav-tabs-link-active-color" = "#1e3d53"
)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  id = "main_tabs",
  title = tags$img(
    src = "CoserRo_Logo_Transparent.svg",
    height = "36px",
    style = "vertical-align: middle;",
    onerror = "this.style.display='none';"
  ),
  window_title = "CoseRo",
  theme = app_theme,
  fillable = TRUE,

  # shinyjs must live inside a nav_item to avoid bslib warning
  nav_item(tags$span(
    useShinyjs(),
    tags$head(tags$link(rel = "icon", type = "image/svg+xml",
                        href = "CoserRo_R_Transparent.svg")),
    style = "display:none;"
  )),

  # Tab 1: COSERO Run
  nav_panel(
    title = "COSERO Run",
    icon = icon("play-circle"),
    run_ui("run")
  ),

  # Tab 2: Time Series
  nav_panel(
    title = "Time Series",
    icon = icon("chart-line"),
    timeseries_ui("ts")
  ),

  # Tab 3: Seasonality
  nav_panel(
    title = "Seasonality",
    icon = icon("calendar"),
    seasonality_ui("season")
  ),

  # Tab 4: Statistics / Performance
  nav_panel(
    title = "Statistics",
    icon = icon("table"),
    performance_ui("perf")
  ),

  # Tab 5: Export
  nav_panel(
    title = "Export",
    icon = icon("download"),
    export_ui("export")
  ),

  # Dark mode toggle (right-aligned in navbar)
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # Shared reactive values accessible by all modules
  shared <- reactiveValues(
    cosero_data     = NULL,   # Full output from read_cosero_output / manual readers
    project_dir     = NULL,   # Active project directory
    output_dir      = NULL,   # Output subdirectory
    status_message  = "No data loaded.",
    selected_subbasin = NULL  # Synced across tabs
  )

  # Auto-load project if passed from launch_cosero_app()
  observe({
    project_dir <- getOption("cosero_project_dir", default = NULL)
    if (!is.null(project_dir) && is.null(shared$project_dir)) {
      shared$project_dir <- project_dir
      shared$output_dir  <- normalizePath(
        file.path(project_dir, "output"), winslash = "/", mustWork = FALSE
      )
    }
  }) |> bindEvent(TRUE, once = TRUE)

  # Interactive theme picker (toggle via bootswatch dropdown in bottom-right)
  #bslib::bs_themer()

  # Call tab modules
  run_server("run", shared)
  timeseries_server("ts", shared)
  seasonality_server("season", shared)
  performance_server("perf", shared)
  export_server("export", shared)
}

# ── Launch ────────────────────────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
