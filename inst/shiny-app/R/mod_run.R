# Title: COSERO Run Tab Module
# Description: Project setup, model configuration (defaults.txt), and run execution
# Author/Architect: Mathew Herrnegger
# Coding: Claude/PI
# Date: 2026-02-28

# ── UI ────────────────────────────────────────────────────────────────────────
run_ui <- function(id) {
  ns <- NS(id)

  tagList(
  # Compact spacing for this tab
  tags$style(HTML("
    .run-compact .card { margin-bottom: 0.4rem; }
    .run-compact .card-header { padding: 0.25rem 0.5rem; font-size: 0.85rem; }
    .run-compact .card-body { padding: 0.4rem 0.5rem; }
    .run-compact .card-footer { padding: 0.3rem 0.5rem; }
    .run-compact .form-group { margin-bottom: 0.35rem; }
    .run-compact .form-label, .run-compact label { font-size: 0.82rem; margin-bottom: 0.1rem; }
    .run-compact .form-control, .run-compact .form-select { padding: 0.2rem 0.4rem; font-size: 0.82rem; min-height: unset; }
    .run-compact .selectize-input { font-size: 0.82rem !important; padding: 0.2rem 0.4rem !important; min-height: unset !important; }
    .run-compact .selectize-dropdown, .run-compact .selectize-dropdown-content { font-size: 0.82rem !important; }
    .run-compact .bslib-value-box { min-height: unset; padding: 0.2rem 0; }
    .run-compact .value-box-title { font-size: 0.75rem; }
    .run-compact .value-box-value { font-size: 1.15rem; line-height: 1.2; }
    .run-compact .value-box-showcase { padding: 0.2rem; max-width: 45px; }
    .run-compact .value-box-area { padding: 0.3rem 0.6rem; }
    .run-compact .time-period-grid .form-group { margin-bottom: 0.15rem; }
    .run-compact .time-period-grid .bslib-grid { margin-bottom: 0.1rem; }
    .run-compact .bslib-grid { gap: 0.3rem; }
  ")),
  div(class = "run-compact",
  layout_sidebar(
    sidebar = sidebar(
      title = "Project Control",
      width = 300,

      # Project folder selection
      shinyDirButton(ns("project_dir_btn"), "Select Project Folder",
                     "Select COSERO project directory",
                     icon = icon("folder-open"),
                     class = "btn-primary btn-sm w-100 mb-1"),
      textInput(ns("project_dir_manual"), NULL,
                placeholder = "Or paste path here\u2026",
                width = "100%"),

      uiOutput(ns("project_status_ui")),

      tags$hr(class = "my-2"),

      # Run mode
      radioButtons(ns("run_mode"), "Run Mode:",
                   choices = list("Cold Start" = 1, "Warm Start" = 2),
                   selected = 1, inline = TRUE),
      uiOutput(ns("warm_start_ui")),

      tags$hr(class = "my-2"),

      # TMMon option (used by run_cosero but not stored in defaults.txt)
      selectInput(ns("tmmon_option"),
                  "Monthly Temperature Means for Thornthwaite (TMMON):",
                  choices = list("Read from parameter file" = 1,
                                 "Calculate internally" = 2),
                  selected = 1, width = "100%"),
      uiOutput(ns("tmmon_warning_ui")),

      tags$hr(class = "my-2"),

      # Run button
      actionButton(ns("run_btn"), "Run Simulation",
                   icon = icon("play"), class = "btn-primary w-100 mb-2"),
      uiOutput(ns("run_progress_ui")),
      verbatimTextOutput(ns("run_summary"))
    ),

    # ── Main Panel ──────────────────────────────────────────────────────────

    # Tier 1: Value Boxes (compact)
    layout_columns(
      fill = FALSE,
      col_widths = c(4, 4, 4),
      value_box(
        title = "Subbasins",
        value = textOutput(ns("vb_subbasins")),
        showcase = icon("droplet"),
        theme = "secondary", class = "py-1",
        p(class = "small mb-0", style = "opacity:0.85;", textOutput(ns("vb_area")))
      ),
      value_box(
        title = "Total Zones",
        value = textOutput(ns("vb_zones")),
        showcase = icon("layer-group"),
        theme = "success", class = "py-1",
        p(class = "small mb-0", style = "opacity:0.85;", "Rows in parameter file")
      ),
      value_box(
        title = "Sim. Duration",
        value = textOutput(ns("vb_duration")),
        showcase = icon("calendar-days"),
        theme = "info", class = "py-1",
        p(class = "small mb-0", style = "opacity:0.85;", textOutput(ns("vb_duration_range")))
      )
    ),

    # Tier 2: Configuration editor (defaults.txt)
    navset_card_underline(
      title = tags$strong("Model Configuration (defaults.txt)"),
      id = ns("config_tabs"),

      # ── Tab 1: Simulation ─────────────────────────────────────────────
      nav_panel(
        title = "Simulation",
        icon = icon("calendar"),
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",

          card(
            card_header("Time Period", class = "py-1"),
            card_body(
              class = "p-2",
              div(class = "time-period-grid",
                # -- Start Date + Time --
                tags$label("Start", class = "form-label fw-bold small mb-0"),
                layout_column_wrap(
                  width = NULL, gap = "0.4rem",
                  style = css(grid_template_columns = "1fr 70px 70px"),
                  dateInput(ns("start_date"), NULL, value = "2010-01-01",
                            width = "100%"),
                  selectInput(ns("start_hour"), NULL,
                              choices = sprintf("%02d", 0:23),
                              selected = "00", width = "100%"),
                  selectInput(ns("start_minute"), NULL,
                              choices = sprintf("%02d", 0:59),
                              selected = "00", width = "100%")
                ),
                # -- End Date + Time --
                tags$label("End", class = "form-label fw-bold small mb-0"),
                layout_column_wrap(
                  width = NULL, gap = "0.4rem",
                  style = css(grid_template_columns = "1fr 70px 70px"),
                  dateInput(ns("end_date"), NULL, value = "2015-12-31",
                            width = "100%"),
                  selectInput(ns("end_hour"), NULL,
                              choices = sprintf("%02d", 0:23),
                              selected = "23", width = "100%"),
                  selectInput(ns("end_minute"), NULL,
                              choices = sprintf("%02d", 0:59),
                              selected = "00", width = "100%")
                )
              ),
              numericInput(ns("spinup"), "Spin-up Period (days)", value = 365,
                           min = 1, max = 10000, width = "100%"),
              tags$p(class = "text-muted small mb-0",
                     "Minimum 1 day. Recommended: 365 for cold start.")
            )
          ),

          card(
            card_header("Output Control", class = "py-1"),
            card_body(
              class = "p-2",
              selectInput(ns("output_type"), "Output Type (OUTPUTTYPE)", width = "100%",
                          choices = list(
                            "1 \u2013 Runoff, precipitation, states & statistics" = 1,
                            "2 \u2013 + Meteorology, glacier & monthly summary" = 2,
                            "3 \u2013 + Full water balance, long-term means & monitor" = 3
                          ),
                          selected = 1),
              selectInput(ns("sc_flag"),
                          "Runoff Depth Reference Area (SC_FLAG)", width = "100%",
                          choices = list(
                            "0 \u2013 Local subbasin area (EZFL_B)" = 0,
                            "1 \u2013 Total upstream catchment area (EZFL_T)" = 1
                          ),
                          selected = 1),
              tags$p(class = "text-muted small mb-0",
                     "Area used for converting runoff volume to depth [mm] and flux calculations.")
            )
          )
        )
      ),

      # ── Tab 2: Files ──────────────────────────────────────────────────
      nav_panel(
        title = "Input / Output Files",
        icon = icon("file-lines"),
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",

          card(
            card_header("Input Files", class = "py-1"),
            card_body(
              class = "p-2",

              # PARAFILE — the key addition
              tags$label("Parameter File (PARAFILE)", class = "form-label small fw-bold mb-1"),
              div(
                class = "d-flex gap-1 mb-1",
                div(style = "flex: 1;",
                    textInput(ns("parafile"), NULL, value = "para_ini.txt",
                              width = "100%",
                              placeholder = "e.g. para_ini.txt")),
                shinyFilesButton(ns("parafile_browse_btn"), "Browse",
                                 "Select parameter file from input/ folder",
                                 icon = icon("folder-open"),
                                 class = "btn-outline-secondary btn-sm",
                                 multiple = FALSE)
              ),
              uiOutput(ns("parafile_status_ui")),

              tags$hr(class = "my-2"),

              # DATAFILE
              textInput(ns("datafile"), "Observation File (DATAFILE)",
                        value = "Qobs.txt", width = "100%",
                        placeholder = "e.g. Qobs.txt"),
              tags$p(class = "text-muted small mb-0",
                     "Observed discharge file in input/ directory."),
              tags$hr(class = "my-2"),
              tags$p(class = "text-muted small mb-0",
                     icon("cloud-sun", class = "me-1"),
                     "Meteorological input files (P, T, ET0) are defined in ",
                     tags$code("input/MetDefaults.txt"), ".")
            )
          ),

          card(
            card_header("Output Files", class = "py-1"),
            card_body(
              class = "p-2",
              textInput(ns("runofffile"), "Runoff Output (RUNOFFFILE)",
                        value = "COSERO.runoff", width = "100%"),
              textInput(ns("statsfile"), "Statistics Output (STATSFILE)",
                        value = "statistics.txt", width = "100%"),
              textInput(ns("optfile"), "Optimization Log (OPTFILE)",
                        value = "optprogress.txt", width = "100%"),
              tags$p(class = "text-muted small mb-0",
                     "Written to the output/ directory.")
            )
          )
        )
      ),

      # ── Tab 3: Advanced ───────────────────────────────────────────────
      nav_panel(
        title = "Advanced",
        icon = icon("sliders"),
        layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",

          card(
            card_header("Model Structure", class = "py-1"),
            card_body(
              class = "p-2",
              layout_column_wrap(
                width = 1 / 2, gap = "0.5rem",
                numericInput(ns("ikl"), "Snow Classes (IKL)", value = 5,
                             min = 1, max = 20, width = "100%"),
                numericInput(ns("nclass"), "Landuse Classes (NCLASS)", value = 10,
                             min = 1, max = 20, width = "100%")
              )
            )
          ),

          card(
            card_header("Other", class = "py-1"),
            card_body(
              class = "p-2",
              textInput(ns("project_info"), "Project Description (PROJECTINFO)",
                        value = "COSERO Wildalpen", width = "100%"),
              selectInput(ns("outcontrol"),
                          "Zonal Output (OUTCONTROL)", width = "100%",
                          choices = list(
                            "0 \u2013 Off" = 0,
                            "1 \u2013 On (requires OUTPUTTYPE 3)" = 1
                          ),
                          selected = 0),
              tags$p(class = "text-muted small mb-0",
                     "Writes mean zonal values (annual, monthly, seasonal) to output/cdr/ folder.")
            )
          ),

          card(
            card_header("Boundary Conditions", class = "py-1"),
            card_body(
              class = "p-2",
              div(class = "mb-2",
                  div(class = "d-flex align-items-center gap-2 mb-1",
                      checkboxInput(ns("addfluxcont"),
                                    "Additional Inflow (ADDFLUXCONT)",
                                    value = FALSE, width = "auto")
                  ),
                  textInput(ns("addfluxfile"), "Inflow File (ADDFLUXFILE)",
                            value = "Qadd.txt", width = "100%",
                            placeholder = "m\u00b3/s time series")
              ),
              div(
                div(class = "d-flex align-items-center gap-2 mb-1",
                    checkboxInput(ns("addregcont"),
                                  "Regression Inflow (ADDREGCONT)",
                                  value = FALSE, width = "auto")
                ),
                textInput(ns("addregfile"), "Regression File (ADDREGFILE)",
                          value = "reg_para.txt", width = "100%",
                          placeholder = "Regression parameters")
              )
            )
          )
        )
      ),

      # ── Tab 4: Parameters (View + Modify) ─────────────────────────
      nav_panel(
        title = "Parameters",
        icon = icon("table"),

        navset_card_underline(
          id = ns("param_subtabs"),

          # ── Sub-tab: View All ───────────────────────────────────────
          nav_panel(
            title = "View All",
            icon = icon("eye"),
            card_body(
              class = "p-2",
              uiOutput(ns("param_table_info")),
              DT::DTOutput(ns("param_table"), height = "auto")
            )
          ),

          # ── Sub-tab: Model Structure ──────────────────────────────────
          nav_panel(
            title = "Model Structure",
            icon = icon("diagram-project"),
            card_body(
              class = "p-2 text-center",
              tags$p(class = "text-muted small mb-2",
                     "Conceptual structure of the COSERO hydrological model.",
                     "Parameter names correspond to those in the Modify tab."),
              tags$img(src = "COSERO_Structure_1.png",
                       alt = "COSERO Model Structure",
                       style = "max-width: 100%; height: auto; border: 1px solid var(--bs-border-color); border-radius: 4px;")
            )
          ),

          # ── Sub-tab: Modify ─────────────────────────────────────────
          nav_panel(
            title = "Modify",
            icon = icon("pen-to-square"),
            card_body(
              class = "p-2",

              # Parameter + subbasin selectors
              layout_column_wrap(
                width = 1/2, gap = "0.5rem",
                div(
                  tags$label("Parameters to Modify",
                             class = "form-label small fw-bold mb-1"),
                  selectizeInput(ns("mod_param_select"), NULL,
                                 choices = NULL, multiple = TRUE, width = "100%",
                                 options = list(
                                   placeholder = "Choose parameters\u2026",
                                   plugins = list("remove_button")
                                 ))
                ),
                div(
                  tags$label("Target Subbasins",
                             class = "form-label small fw-bold mb-1"),
                  selectizeInput(ns("mod_subbasin_select"), NULL,
                                 choices = NULL, multiple = TRUE, width = "100%",
                                 options = list(
                                   placeholder = "All subbasins (default)",
                                   plugins = list("remove_button")
                                 )),
                  tags$p(class = "text-muted small mb-0",
                         "Leave empty to modify all zones.")
                )
              ),

              # Current values + target inputs (dynamic table)
              uiOutput(ns("mod_param_table_ui")),

              tags$hr(class = "my-2"),

              # Save controls
              layout_column_wrap(
                width = NULL, gap = "0.5rem",
                style = css(grid_template_columns = "1fr auto"),
                textInput(ns("mod_save_filename"), "Save As",
                          placeholder = "para_modified.txt", width = "100%"),
                div(style = "padding-top: 1.7rem;",
                    checkboxInput(ns("mod_set_active"), "Set as active PARAFILE",
                                  value = TRUE, width = "auto"))
              ),

              div(
                class = "d-flex gap-2 mt-2",
                actionButton(ns("mod_save_btn"), "Save Parameter File",
                             icon = icon("save"), class = "btn-primary btn-sm"),
                actionButton(ns("mod_save_run_btn"), "Save & Run",
                             icon = icon("play"), class = "btn-success btn-sm"),
                actionButton(ns("mod_reset_btn"), "Reset to Defaults",
                             icon = icon("rotate-left"),
                             class = "btn-outline-secondary btn-sm")
              ),

              uiOutput(ns("mod_save_status_ui"))
            )
          )
        )
      ),

      # ── Footer: Save + Load ───────────────────────────────────────────
      card_footer(
        class = "p-2 d-flex justify-content-between",
        actionButton(ns("load_defaults_btn"), "Load from File",
                     class = "btn-outline-secondary btn-sm", icon = icon("sync")),
        actionButton(ns("save_defaults_btn"), "Save Changes",
                     class = "btn-primary btn-sm", icon = icon("save"))
      )
    )
  ) # end layout_sidebar
  ) # end div.run-compact
  ) # end tagList
}

# ── Server ────────────────────────────────────────────────────────────────────
run_server <- function(id, shared) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      params          = NULL,   # Parameter file data (data.frame)
      run_in_progress = FALSE,
      run_result      = NULL,
      trigger_run     = 0       # Increment to programmatically trigger a run
    )

    # ── File browser setup ────────────────────────────────────────────────
    volumes <- c(
      "Working Directory" = normalizePath(getwd(), winslash = "/"),
      "Home" = fs::path_home(),
      getVolumes()()
    )
    shinyDirChoose(input, "project_dir_btn", roots = volumes, session = session)
    shinyFileChoose(input, "parafile_browse_btn", roots = volumes,
                    session = session, filetypes = c("txt"))

    # ── Project directory selection ───────────────────────────────────────
    observeEvent(input$project_dir_btn, {
      if (!is.null(input$project_dir_btn) && !is.integer(input$project_dir_btn)) {
        dir_path <- parseDirPath(volumes, input$project_dir_btn)
        if (length(dir_path) > 0) {
          shared$project_dir <- as.character(dir_path)
          shared$output_dir  <- normalizePath(
            file.path(dir_path, "output"), winslash = "/", mustWork = FALSE
          )
          updateTextInput(session, "project_dir_manual", value = shared$project_dir)
        }
      }
    })

    # Manual path entry — normalize slashes and validate
    observeEvent(input$project_dir_manual, {
      raw <- trimws(input$project_dir_manual)
      if (is.null(raw) || !nzchar(raw)) return()
      # Strip surrounding quotes if pasted from file explorer
      raw <- gsub('^["\']|["\']$', '', raw)
      # Normalize slashes (accept both / and \)
      clean <- normalizePath(raw, winslash = "/", mustWork = FALSE)
      if (dir.exists(clean)) {
        # Only update if different to avoid infinite loop
        if (is.null(shared$project_dir) || shared$project_dir != clean) {
          shared$project_dir <- clean
          shared$output_dir  <- normalizePath(
            file.path(clean, "output"), winslash = "/", mustWork = FALSE
          )
        }
      }
    })

    # ── Load parameter file data for value boxes ──────────────────────────
    load_param_data <- function() {
      req(shared$project_dir)
      tryCatch({
        para_name <- input$parafile
        if (is.null(para_name) || !nzchar(para_name)) {
          defaults <- CoseRo::read_cosero_defaults_safe(shared$project_dir)
          para_name <- defaults$PARAFILE %||% "para.txt"
        }
        para_file <- file.path(shared$project_dir, "input", para_name)
        if (file.exists(para_file)) {
          rv$params <- CoseRo::read_cosero_parameters(para_file)
        }
      }, error = function(e) NULL)
    }

    # Auto-load param data when project changes
    observe({
      req(shared$project_dir)
      if (dir.exists(shared$project_dir)) load_param_data()
    })

    # Reload param data when PARAFILE input changes
    observeEvent(input$parafile, {
      req(shared$project_dir)
      load_param_data()
    }, ignoreInit = TRUE)

    # ── PARAFILE browser ──────────────────────────────────────────────────
    observeEvent(input$parafile_browse_btn, {
      if (!is.null(input$parafile_browse_btn) &&
          !is.integer(input$parafile_browse_btn)) {
        fp <- parseFilePaths(volumes, input$parafile_browse_btn)
        if (nrow(fp) > 0) {
          # Show only filename (relative to input/ folder)
          full_path <- as.character(fp$datapath)
          updateTextInput(session, "parafile", value = basename(full_path))
        }
      }
    })

    # ── PARAFILE status ───────────────────────────────────────────────────
    output$parafile_status_ui <- renderUI({
      req(shared$project_dir)
      pf <- input$parafile
      if (is.null(pf) || !nzchar(pf)) {
        return(tags$p(class = "text-muted small", "Using default from defaults.txt"))
      }
      full_path <- file.path(shared$project_dir, "input", pf)
      if (file.exists(full_path)) {
        fi <- file.info(full_path)
        tags$div(
          class = "alert alert-success py-1 px-2 mb-1 small",
          icon("check-circle"), " ",
          sprintf("Found (%s KB, modified %s)",
                  round(fi$size / 1024, 1),
                  format(fi$mtime, "%Y-%m-%d %H:%M"))
        )
      } else {
        tags$div(
          class = "alert alert-danger py-1 px-2 mb-1 small",
          icon("exclamation-triangle"),
          sprintf(" File not found in input/: %s", pf)
        )
      }
    })

    # ── Project status UI ─────────────────────────────────────────────────
    output$project_status_ui <- renderUI({
      if (is.null(shared$project_dir) || !dir.exists(shared$project_dir)) {
        return(tags$div(class = "text-muted small", "No project selected"))
      }
      exe <- file.path(shared$project_dir, "COSERO.exe")
      if (file.exists(exe)) {
        tags$div(
          class = "alert alert-success py-1 px-2 mb-1 small",
          icon("check-circle"), " ",
          tags$strong("Project Loaded:"),
          tags$br(),
          basename(shared$project_dir)
        )
      } else {
        tags$div(
          class = "alert alert-warning py-1 px-2 mb-1 small",
          icon("exclamation-triangle"), " COSERO.exe not found"
        )
      }
    })

    # ── Warm start status ─────────────────────────────────────────────────
    output$warm_start_ui <- renderUI({
      req(shared$project_dir, input$run_mode == "2")
      sv_input  <- file.path(shared$project_dir, "input", "statevar.dmp")
      sv_output <- file.path(shared$project_dir, "output", "statevar.dmp")
      if (file.exists(sv_input)) {
        tags$div(class = "alert alert-success py-1 px-2 small",
                 icon("check-circle"), " statevar.dmp found in input/")
      } else if (file.exists(sv_output)) {
        tags$div(class = "alert alert-warning py-1 px-2 small",
                 icon("triangle-exclamation"),
                 tags$strong(" statevar.dmp only in output/, not in input/."),
                 " Copy it to input/ for warm start.")
      } else {
        tags$div(class = "alert alert-danger py-1 px-2 small",
                 icon("circle-xmark"),
                 tags$strong(" statevar.dmp not found!"),
                 " Warm start not possible.")
      }
    })

    # Auto-revert to cold start when statevar.dmp is missing from input/
    observe({
      req(shared$project_dir, input$run_mode == "2")
      sv_input <- file.path(shared$project_dir, "input", "statevar.dmp")
      if (!file.exists(sv_input)) {
        updateRadioButtons(session, "run_mode", selected = "1")
        showNotification(
          "Switched to Cold Start \u2014 statevar.dmp not found in input/",
          type = "warning", duration = 5
        )
      }
    })

    # ── TMMon Warning ─────────────────────────────────────────────────────
    output$tmmon_warning_ui <- renderUI({
      req(input$tmmon_option == "2")
      # Check simulation length
      sim_years <- NULL
      if (!is.null(input$start_date) && !is.null(input$end_date)) {
        d <- as.numeric(difftime(input$end_date, input$start_date, units = "days"))
        sim_years <- d / 365.25
      }
      if (!is.null(sim_years) && sim_years <= 10) {
        tags$div(class = "alert alert-danger py-1 px-2 small mb-1",
          icon("triangle-exclamation"),
          tags$strong(" Warning:"),
          " Simulation period is \u2264 10 years. ",
          "COSERO will pause with: ",
          tags$em("'Number of years is lower/equal 10 and TMMON is not representative!'"),
          " Simulation not possible. ",
          tags$strong("Use 'Read from parameter file' or extend the simulation period beyond 10 years.")
        )
      } else {
        tags$p(class = "text-muted small mb-0",
          "Requires simulation period > 10 years. Otherwise COSERO halts.")
      }
    })

    # ── Value Boxes ───────────────────────────────────────────────────────
    output$vb_area <- renderText({
      if (is.null(rv$params)) return("Total area: \u2014")
      # Column may be DFZON_, DFZONE_, or DFZONE
      dfz_col <- grep("^DFZON", colnames(rv$params), value = TRUE)[1]
      if (is.na(dfz_col)) return("Total area: \u2014")
      paste0("Total area: ", round(sum(rv$params[[dfz_col]], na.rm = TRUE), 1),
             " km\u00b2")
    })
    output$vb_subbasins <- renderText({
      if (is.null(rv$params) || !"NB_" %in% colnames(rv$params)) return("\u2014")
      as.character(length(unique(rv$params[["NB_"]])))
    })
    output$vb_zones <- renderText({
      if (is.null(rv$params)) return("\u2014")
      as.character(nrow(rv$params))
    })
    output$vb_duration <- renderText({
      req(input$start_date, input$end_date)
      d <- as.numeric(difftime(input$end_date, input$start_date, units = "days"))
      if (d <= 0) return("\u2014")
      paste0(round(d / 365.25, 1), " Years")
    })
    output$vb_duration_range <- renderText({
      req(input$start_date, input$end_date)
      paste0(format(input$start_date, "%Y"), "\u2013", format(input$end_date, "%Y"))
    })

    # ── Parameter Table ─────────────────────────────────────────────────
    MAX_PARAM_ZONES <- 1000L  # safeguard for very large files

    output$param_table_info <- renderUI({
      if (is.null(rv$params)) {
        return(tags$p(class = "text-muted small", "No parameter file loaded."))
      }
      n <- nrow(rv$params)
      truncated <- n > MAX_PARAM_ZONES
      pf <- input$parafile
      if (is.null(pf) || !nzchar(pf)) pf <- "para.txt"
      info <- sprintf("%s \u2014 %d zones \u00d7 %d parameters", pf, n, ncol(rv$params))
      if (truncated) {
        tags$div(class = "alert alert-warning py-1 px-2 small mb-2",
          icon("triangle-exclamation"),
          sprintf(" Showing first %d of %d zones. ", MAX_PARAM_ZONES, n),
          tags$span(class = "text-muted", info)
        )
      } else {
        tags$p(class = "text-muted small mb-1", icon("circle-info"), " ", info)
      }
    })

    output$param_table <- DT::renderDT({
      req(rv$params)
      df <- rv$params
      if (nrow(df) > MAX_PARAM_ZONES) df <- df[seq_len(MAX_PARAM_ZONES), ]
      # Round numeric columns for display
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- lapply(df[num_cols], function(x) round(x, 4))
      DT::datatable(df,
        options = list(
          pageLength = 50,
          lengthMenu = c(25, 50, 100, 200),
          scrollX = TRUE,
          scrollY = "500px",
          dom = "lfrtip",      # length, filter, table, info, pagination
          autoWidth = FALSE,
          columnDefs = list(list(className = "dt-right", targets = "_all")),
          initComplete = DT::JS(
            "function(settings, json) {",
            "  $(this.api().table().container()).css('font-size', '0.75rem');",
            "}"
          )
        ),
        rownames = FALSE,
        class = "compact stripe hover cell-border",
        style = "bootstrap5"
      )
    }, server = TRUE)

    # ── Modify Parameters ─────────────────────────────────────────────────

    # Parameters with 12 monthly variants in the parameter file
    MONTHLY_PARAMS <- c("TCOR", "PCOR", "TMMON", "INTMAX", "ETVEGCOR",
                        "DAYSDRY", "DAYSWET", "ETSYSCOR")

    # Load parameter_bounds.csv choices (grouped by category)
    observe({
      bounds <- tryCatch(CoseRo::load_parameter_bounds(), error = function(e) NULL)
      if (is.null(bounds)) return()
      # Build grouped choices: list(category = c(param1, param2, ...))
      cats <- split(bounds$parameter, bounds$category)
      grouped <- lapply(cats, function(params) setNames(params, params))
      updateSelectizeInput(session, "mod_param_select", choices = grouped)
    })

    # Populate subbasin selector from parameter file (preserve current selection)
    observe({
      req(rv$params)
      if ("NB_" %in% colnames(rv$params)) {
        sbs <- sort(unique(as.character(rv$params[["NB_"]])))
        choices <- setNames(sbs, paste0("Subbasin ", sbs))
        # Keep current selection if still valid
        current <- isolate(input$mod_subbasin_select)
        keep <- if (!is.null(current)) intersect(current, sbs) else character(0)
        updateSelectizeInput(session, "mod_subbasin_select",
                             choices = choices,
                             selected = if (length(keep) > 0) keep else character(0))
      }
    })

    # Reactive: current spatial means for selected parameters (respecting subbasin filter)
    # For monthly parameters (e.g., PCOR → PCor1_..PCor12_), computes mean across
    # all 12 months × zones to give a representative single value.
    mod_current_means <- reactive({
      req(shared$project_dir, rv$params)
      selected <- input$mod_param_select
      if (is.null(selected) || length(selected) == 0) return(NULL)

      par_file <- file.path(shared$project_dir, "input",
                            input$parafile %||% "para.txt")
      if (!file.exists(par_file)) return(NULL)

      tryCatch({
        # Read full parameter data for column lookups
        full_data <- CoseRo::read_cosero_parameters(par_file, skip_lines = 1, quiet = TRUE)

        # Determine zone mask from subbasin filter
        target_sbs <- input$mod_subbasin_select
        if (!is.null(target_sbs) && length(target_sbs) > 0 &&
            "NB_" %in% colnames(full_data)) {
          zone_mask <- full_data$NB_ %in% target_sbs
        } else {
          zone_mask <- rep(TRUE, nrow(full_data))
        }

        # Calculate spatial means, handling monthly params specially
        means <- sapply(selected, function(p) {
          is_monthly <- toupper(p) %in% MONTHLY_PARAMS
          if (is_monthly) {
            # Get all 12 monthly columns
            month_cols <- CoseRo::find_parameter_column(p, colnames(full_data), return_all = TRUE)
            if (length(month_cols) == 0) return(NA_real_)
            # Mean across all months × zones
            all_vals <- unlist(full_data[zone_mask, month_cols, drop = FALSE])
            round(mean(all_vals, na.rm = TRUE), 4)
          } else {
            col <- CoseRo::find_parameter_column(p, colnames(full_data), return_all = FALSE)
            if (length(col) == 0) return(NA_real_)
            round(mean(full_data[zone_mask, col], na.rm = TRUE), 4)
          }
        })
        setNames(means, selected)
      }, error = function(e) NULL)
    })

    # Reactive: per-month spatial means for monthly params (for tooltip/detail)
    mod_monthly_means <- reactive({
      req(shared$project_dir, rv$params)
      selected <- input$mod_param_select
      if (is.null(selected) || length(selected) == 0) return(NULL)

      par_file <- file.path(shared$project_dir, "input",
                            input$parafile %||% "para.txt")
      if (!file.exists(par_file)) return(NULL)

      tryCatch({
        full_data <- CoseRo::read_cosero_parameters(par_file, skip_lines = 1, quiet = TRUE)

        target_sbs <- input$mod_subbasin_select
        if (!is.null(target_sbs) && length(target_sbs) > 0 &&
            "NB_" %in% colnames(full_data)) {
          zone_mask <- full_data$NB_ %in% target_sbs
        } else {
          zone_mask <- rep(TRUE, nrow(full_data))
        }

        monthly_selected <- selected[toupper(selected) %in% MONTHLY_PARAMS]
        if (length(monthly_selected) == 0) return(NULL)

        result <- lapply(monthly_selected, function(p) {
          month_cols <- CoseRo::find_parameter_column(p, colnames(full_data), return_all = TRUE)
          if (length(month_cols) == 0) return(NULL)
          sapply(month_cols, function(mc) round(mean(full_data[zone_mask, mc], na.rm = TRUE), 3))
        })
        setNames(result, monthly_selected)
      }, error = function(e) NULL)
    })

    # Reactive: bounds for selected parameters
    mod_bounds <- reactive({
      selected <- input$mod_param_select
      if (is.null(selected) || length(selected) == 0) return(NULL)
      tryCatch(
        CoseRo::load_parameter_bounds(parameters = selected),
        error = function(e) NULL
      )
    })

    # Dynamic UI: table of parameters with current mean + editable target
    output$mod_param_table_ui <- renderUI({
      selected <- input$mod_param_select
      if (is.null(selected) || length(selected) == 0) {
        return(tags$p(class = "text-muted small mt-2",
                      icon("info-circle"),
                      " Select parameters above to modify their spatial-mean targets."))
      }

      means <- mod_current_means()
      bounds <- mod_bounds()
      if (is.null(means) || is.null(bounds)) {
        return(tags$p(class = "text-muted small", "Loading parameter data\u2026"))
      }

      # Subbasin scope label
      target_sbs <- input$mod_subbasin_select
      scope_label <- if (is.null(target_sbs) || length(target_sbs) == 0) {
        "all zones"
      } else {
        paste0("subbasin ", paste(target_sbs, collapse = ", "))
      }

      # Per-month detail for monthly params
      month_means <- mod_monthly_means()

      MONTH_ABBR <- c("Jan","Feb","Mar","Apr","May","Jun",
                      "Jul","Aug","Sep","Oct","Nov","Dec")

      # Build table rows
      rows <- lapply(selected, function(p) {
        b <- bounds[bounds$parameter == p, ]
        cur <- if (p %in% names(means)) means[[p]] else NA
        mod_type <- if (nrow(b) > 0) b$modification_type[1] else "relchg"
        p_min <- if (nrow(b) > 0) b$min[1] else NA
        p_max <- if (nrow(b) > 0) b$max[1] else NA
        desc <- if (nrow(b) > 0 && "description" %in% colnames(b)) b$description[1] else ""
        badge_class <- if (mod_type == "relchg") "bg-info" else "bg-warning"
        is_monthly <- toupper(p) %in% MONTHLY_PARAMS

        # Per-month toggle state (FALSE on first render, preserved across re-renders)
        per_month_on <- isTRUE(input[[paste0("mod_permonth_", p)]])

        # Per-month spatial means
        mv <- NULL
        if (is_monthly && !is.null(month_means) && p %in% names(month_means)) {
          mv <- month_means[[p]]
        }

        # Monthly detail string for compact display
        monthly_detail <- NULL
        if (is_monthly && !is.null(mv) && length(mv) > 0 && !per_month_on) {
          monthly_detail <- paste(
            paste0(MONTH_ABBR[seq_along(mv)], ":", format(mv, nsmall = 2)),
            collapse = "  "
          )
        }

        # Main row
        main_row <- tags$tr(
          tags$td(
            tags$strong(p),
            tags$span(class = paste("badge", badge_class, "ms-1"),
                      style = "font-size: 0.65rem;", mod_type),
            if (is_monthly) tags$span(class = "badge bg-secondary text-white ms-1",
                                       style = "font-size: 0.65rem;", "\u00d712 months"),
            if (is_monthly) tags$a(
              href = "#",
              class = paste0("badge ms-1 ",
                             if (per_month_on) "bg-primary text-white"
                             else "border text-secondary"),
              style = "font-size: 0.65rem; text-decoration: none; cursor: pointer;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', !%s, {priority: 'event'}); return false;",
                ns(paste0("mod_permonth_", p)),
                tolower(as.character(per_month_on))
              ),
              icon(if (per_month_on) "compress" else "expand"),
              if (per_month_on) " Uniform" else " Per month"
            ),
            if (nzchar(desc)) tags$br(tags$span(class = "text-muted",
                                                 style = "font-size: 0.72rem;", desc)),
            if (!is.null(monthly_detail))
              tags$br(tags$span(class = "text-muted",
                                style = "font-size: 0.68rem; font-family: monospace;",
                                monthly_detail))
          ),
          tags$td(style = "text-align: right; vertical-align: middle;",
                  tags$code(format(cur, nsmall = 2))),
          tags$td(style = "vertical-align: middle;",
                  if (!per_month_on)
                    numericInput(ns(paste0("mod_target_", p)), NULL,
                                 value = cur, min = p_min, max = p_max,
                                 step = if (!is.na(p_max) && !is.na(p_min))
                                   round((p_max - p_min) / 50, 4) else 0.1,
                                 width = "100%")
                  else
                    tags$span(class = "text-muted small", "\u2190 see below")
          ),
          tags$td(style = "text-align: center; vertical-align: middle; font-size: 0.75rem;",
                  if (!is.na(p_min)) paste0("[", p_min, ", ", p_max, "]") else "\u2014")
        )

        # Expanded per-month row (4 columns × 3 rows grid)
        month_row <- NULL
        if (is_monthly && per_month_on && !is.null(mv) && length(mv) > 0) {
          step_val <- if (!is.na(p_max) && !is.na(p_min))
            round((p_max - p_min) / 50, 4) else 0.1

          month_inputs <- lapply(seq_along(mv), function(m) {
            tags$div(
              style = "min-width: 0;",
              tags$label(class = "form-label mb-0",
                         style = "font-size: 0.7rem; color: var(--bs-secondary-color);",
                         MONTH_ABBR[m],
                         tags$span(style = "font-family: monospace; font-size: 0.65rem;",
                                   paste0(" (", format(mv[m], nsmall = 2), ")"))),
              numericInput(ns(paste0("mod_month_", p, "_", m)), NULL,
                           value = round(mv[m], 4), min = p_min, max = p_max,
                           step = step_val, width = "100%")
            )
          })

          month_row <- tags$tr(
            tags$td(
              colspan = 4,
              style = "padding: 0.3rem 0.5rem; border-top: none;",
              tags$div(
                style = "display: grid; grid-template-columns: repeat(4, 1fr); gap: 0.3rem 0.5rem;",
                month_inputs
              )
            )
          )
        }

        tagList(main_row, month_row)
      })

      tagList(
        tags$p(class = "small text-muted mb-1 mt-2",
               icon("crosshairs"), " Spatial-mean targets for ",
               tags$strong(scope_label),
               ". Spatial patterns preserved via relchg/abschg.",
               " Click ", tags$em("Per month"), " to set individual monthly targets."),
        tags$div(
          style = "overflow-x: auto;",
          tags$table(
            class = "table table-sm table-hover mb-0",
            style = "font-size: 0.82rem;",
            tags$thead(
              tags$tr(
                tags$th("Parameter"),
                tags$th(style = "text-align: right;", "Current Mean"),
                tags$th(style = "min-width: 120px;", "New Target"),
                tags$th(style = "text-align: center;", "Bounds")
              )
            ),
            tags$tbody(rows)
          )
        )
      )
    })

    # Reset targets to current means (both uniform and per-month inputs)
    observeEvent(input$mod_reset_btn, {
      means <- mod_current_means()
      month_means_data <- mod_monthly_means()
      if (is.null(means)) return()
      for (p in names(means)) {
        updateNumericInput(session, paste0("mod_target_", p), value = means[[p]])
        # Also reset per-month inputs if they exist
        if (toupper(p) %in% MONTHLY_PARAMS && !is.null(month_means_data) &&
            p %in% names(month_means_data)) {
          mv <- month_means_data[[p]]
          for (m in seq_along(mv)) {
            updateNumericInput(session, paste0("mod_month_", p, "_", m),
                               value = round(mv[m], 4))
          }
        }
      }
      showNotification("Targets reset to current spatial means.", type = "message",
                       duration = 3)
    })

    # Reusable: save modified parameter file. Returns save filename on success, FALSE on failure.
    save_modified_params <- function() {
      selected <- input$mod_param_select
      if (is.null(selected) || length(selected) == 0) {
        showNotification("No parameters selected.", type = "warning")
        return(FALSE)
      }

      parafile_name <- input$parafile
      if (is.null(parafile_name) || !nzchar(parafile_name)) parafile_name <- "para.txt"
      par_file_src <- file.path(shared$project_dir, "input", parafile_name)
      if (!file.exists(par_file_src)) {
        showNotification(paste("Source parameter file not found:", parafile_name),
                         type = "error")
        return(FALSE)
      }

      save_name <- input$mod_save_filename
      if (is.null(save_name) || !nzchar(trimws(save_name))) {
        save_name <- paste0("para_modified_",
                            format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
        updateTextInput(session, "mod_save_filename", value = save_name)
      }
      par_file_dst <- file.path(shared$project_dir, "input", save_name)

      tryCatch({
        # Read parameter file columns for month-name resolution
        full_data <- CoseRo::read_cosero_parameters(par_file_src, skip_lines = 1, quiet = TRUE)

        # If saving to the same file, work on a temp copy first
        same_file <- normalizePath(par_file_src, winslash = "/", mustWork = FALSE) ==
                     normalizePath(par_file_dst, winslash = "/", mustWork = FALSE)
        if (same_file) {
          tmp <- tempfile(fileext = ".txt")
          file.copy(par_file_src, tmp, overwrite = TRUE)
          par_file_work <- tmp
        } else {
          file.copy(par_file_src, par_file_dst, overwrite = TRUE)
          par_file_work <- par_file_dst
        }
        all_cols <- colnames(full_data)

        new_params <- list()
        par_bounds <- CoseRo::load_parameter_bounds(parameters = selected)
        extra_bounds <- list()  # per-month bound rows to append

        for (p in selected) {
          is_monthly <- toupper(p) %in% MONTHLY_PARAMS
          per_month_on <- is_monthly && isTRUE(input[[paste0("mod_permonth_", p)]])

          if (per_month_on) {
            # Per-month mode: resolve actual column names and build individual entries
            month_cols <- CoseRo::find_parameter_column(p, all_cols, return_all = TRUE)
            b_row <- par_bounds[par_bounds$parameter == p, ]
            for (m in seq_along(month_cols)) {
              val <- input[[paste0("mod_month_", p, "_", m)]]
              if (!is.null(val) && !is.na(val)) {
                col_name <- month_cols[m]
                new_params[[col_name]] <- val
                # Create a bounds row for this individual month column
                extra_bounds[[col_name]] <- data.frame(
                  parameter = col_name,
                  min = b_row$min, max = b_row$max,
                  default = b_row$default,
                  modification_type = b_row$modification_type,
                  stringsAsFactors = FALSE
                )
              }
            }
          } else {
            # Uniform mode (original behaviour)
            val <- input[[paste0("mod_target_", p)]]
            if (!is.null(val) && !is.na(val)) new_params[[p]] <- val
          }
        }

        if (length(new_params) == 0) {
          showNotification("No valid target values entered.", type = "warning")
          return(FALSE)
        }

        # Append per-month bound rows so CoseRo::modify_parameter_table() can look them up
        if (length(extra_bounds) > 0) {
          extra_df <- do.call(rbind, extra_bounds)
          # Ensure matching columns (fill missing with NA)
          for (col in setdiff(colnames(par_bounds), colnames(extra_df)))
            extra_df[[col]] <- NA
          par_bounds <- rbind(par_bounds, extra_df[, colnames(par_bounds)])
        }

        original_values <- CoseRo::read_parameter_table(par_file_src, selected,
                                                zone_id = "all", quiet = TRUE)

        zones_to_mod <- NULL
        target_sbs <- input$mod_subbasin_select
        if (!is.null(target_sbs) && length(target_sbs) > 0) {
          zone_map <- CoseRo::get_zones_for_subbasins(
            shared$project_dir, subbasins = target_sbs,
            defaults_settings = list(PARAFILE = parafile_name), quiet = TRUE
          )
          zones_to_mod <- zone_map$zones
        }

        CoseRo::modify_parameter_table(par_file_work, new_params, par_bounds,
                               original_values, zones = zones_to_mod,
                               add_timestamp = TRUE)

        # If same file, copy the modified temp file back to destination
        if (same_file) {
          file.copy(par_file_work, par_file_dst, overwrite = TRUE)
          unlink(par_file_work)
        }

        if (isTRUE(input$mod_set_active)) {
          updateTextInput(session, "parafile", value = save_name)
        }

        rv$params <- CoseRo::read_cosero_parameters(par_file_dst)

        showNotification(
          paste0("Saved: input/", save_name,
                 if (!is.null(zones_to_mod))
                   paste0(" (", length(zones_to_mod), " zones modified)")
                 else " (all zones modified)"),
          type = "message", duration = 5
        )
        return(save_name)
      }, error = function(e) {
        showNotification(paste("Error saving parameters:", e$message),
                         type = "error", duration = 8)
        return(FALSE)
      })
    }

    # Save only
    observeEvent(input$mod_save_btn, {
      req(shared$project_dir)
      save_modified_params()
    })

    # Save & Run — save params, write defaults.txt, then trigger model run
    observeEvent(input$mod_save_run_btn, {
      req(shared$project_dir)
      saved <- save_modified_params()
      if (!isFALSE(saved)) {
        # saved is the filename; write it to defaults.txt now
        # (can't rely on updateTextInput round-trip before run)
        if (isTRUE(input$mod_set_active)) {
          defaults_file <- file.path(shared$project_dir, "input", "defaults.txt")
          if (file.exists(defaults_file)) {
            tryCatch(
              CoseRo::modify_defaults(defaults_file, list(PARAFILE = saved), quiet = TRUE),
              error = function(e) NULL
            )
          }
        }
        # Store the filename so execute_run can use it
        rv$pending_parafile <- saved
        rv$trigger_run <- rv$trigger_run + 1
      }
    })

    # Save status feedback
    output$mod_save_status_ui <- renderUI({
      req(shared$project_dir)
      save_name <- input$mod_save_filename
      if (is.null(save_name) || !nzchar(trimws(save_name))) return(NULL)
      full_path <- file.path(shared$project_dir, "input", save_name)
      if (file.exists(full_path)) {
        fi <- file.info(full_path)
        tags$div(class = "alert alert-info py-1 px-2 small mt-2 mb-0",
          icon("circle-info"),
          sprintf(" File exists (%s KB, %s) \u2014 will be overwritten.",
                  round(fi$size / 1024, 1),
                  format(fi$mtime, "%Y-%m-%d %H:%M"))
        )
      }
    })

    # ── Load Defaults ─────────────────────────────────────────────────────
    # Reusable function to load defaults into UI
    load_defaults_into_ui <- function(notify = TRUE) {
      d <- CoseRo::read_cosero_defaults_safe(shared$project_dir)
      sp <- CoseRo::parse_cosero_date(d$STARTDATE)
      ep <- CoseRo::parse_cosero_date(d$ENDDATE)

      # Simulation tab — date + time
      updateDateInput(session,    "start_date",    value    = sp$date)
      updateSelectInput(session,  "start_hour",    selected = sprintf("%02d", sp$hour))
      updateSelectInput(session,  "start_minute",  selected = sprintf("%02d", sp$minute))
      updateDateInput(session,    "end_date",      value    = ep$date)
      updateSelectInput(session,  "end_hour",      selected = sprintf("%02d", ep$hour))
      updateSelectInput(session,  "end_minute",    selected = sprintf("%02d", ep$minute))
      updateNumericInput(session, "spinup",        value    = as.integer(d$SPINUP))
      updateSelectInput(session,  "output_type", selected = as.integer(d$OUTPUTTYPE))
      updateSelectInput(session,  "sc_flag",     selected = as.integer(d$SC_FLAG))
      updateSelectInput(session,  "outcontrol",  selected = as.integer(d$OUTCONTROL))

      # Files tab
      updateTextInput(session, "parafile",   value = as.character(d$PARAFILE))
      updateTextInput(session, "datafile",   value = as.character(d$DATAFILE))
      updateTextInput(session, "runofffile", value = as.character(d$RUNOFFFILE))
      updateTextInput(session, "statsfile",  value = as.character(d$STATSFILE))
      updateTextInput(session, "optfile",    value = as.character(d$OPTFILE))

      # Advanced tab
      updateNumericInput(session, "ikl",          value = as.integer(d$IKL))
      updateNumericInput(session, "nclass",       value = as.integer(d$NCLASS))
      updateTextInput(session,    "project_info", value = as.character(d$PROJECTINFO))
      updateCheckboxInput(session, "addfluxcont",
                          value = as.integer(d$ADDFLUXCONT) == 1)
      updateTextInput(session,     "addfluxfile", value = as.character(d$ADDFLUXFILE))
      updateCheckboxInput(session, "addregcont",
                          value = as.integer(d$ADDREGCONT) == 1)
      updateTextInput(session,     "addregfile",  value = as.character(d$ADDREGFILE))

      if (notify) {
        showNotification("Defaults loaded from file!", type = "message", duration = 3)
      }
    }

    # Button handler
    observeEvent(input$load_defaults_btn, {
      req(shared$project_dir)
      tryCatch(load_defaults_into_ui(notify = TRUE), error = function(e) {
        showNotification(paste("Error loading defaults:", e$message),
                         type = "error", duration = 5)
      })
    })

    # Auto-load defaults when project dir is set
    observeEvent(shared$project_dir, {
      req(shared$project_dir)
      # Sync the manual path text input
      updateTextInput(session, "project_dir_manual", value = shared$project_dir)
      tryCatch(load_defaults_into_ui(notify = FALSE), error = function(e) NULL)
    })

    # ── Save Defaults ─────────────────────────────────────────────────────
    observeEvent(input$save_defaults_btn, {
      req(shared$project_dir)
      defaults_file <- file.path(shared$project_dir, "input", "defaults.txt")
      if (!file.exists(defaults_file)) {
        showNotification("defaults.txt not found in input/", type = "error")
        return()
      }
      tryCatch({
        settings <- list(
          # Simulation
          STARTDATE   = CoseRo::format_cosero_date(input$start_date,
                                           as.integer(input$start_hour),
                                           as.integer(input$start_minute)),
          ENDDATE     = CoseRo::format_cosero_date(input$end_date,
                                           as.integer(input$end_hour),
                                           as.integer(input$end_minute)),
          SPINUP      = as.integer(input$spinup),
          OUTPUTTYPE  = as.integer(input$output_type),
          SC_FLAG     = as.integer(input$sc_flag),
          OUTCONTROL  = as.integer(input$outcontrol),
          # Files
          PARAFILE    = input$parafile,
          DATAFILE    = input$datafile,
          RUNOFFFILE  = input$runofffile,
          STATSFILE   = input$statsfile,
          OPTFILE     = input$optfile,
          # Advanced
          IKL         = as.integer(input$ikl),
          NCLASS      = as.integer(input$nclass),
          PROJECTINFO = input$project_info,
          ADDFLUXCONT = as.integer(input$addfluxcont),
          ADDFLUXFILE = input$addfluxfile,
          ADDREGCONT  = as.integer(input$addregcont),
          ADDREGFILE  = input$addregfile
        )
        CoseRo::modify_defaults(defaults_file, settings, quiet = TRUE)
        showNotification("defaults.txt saved!", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Save error:", e$message), type = "error", duration = 5)
      })
    })

    # ── Run progress UI ───────────────────────────────────────────────────
    output$run_progress_ui <- renderUI({
      if (rv$run_in_progress) {
        tags$p(class = "text-primary fw-bold small",
               icon("spinner", class = "fa-spin"), " COSERO is running\u2026")
      }
    })

    # ── Run COSERO ────────────────────────────────────────────────────────
    # Reusable run function (called by Run button and Save & Run trigger)
    execute_run <- function() {
      project_path <- shared$project_dir

      # Validations
      if (!file.exists(file.path(project_path, "COSERO.exe"))) {
        showNotification("COSERO.exe not found in project folder",
                         type = "error"); return()
      }
      if (input$end_date <= input$start_date) {
        showNotification("End date must be after start date",
                         type = "error"); return()
      }
      if (as.integer(input$run_mode) == 1 &&
          (is.null(input$spinup) || input$spinup < 1)) {
        showNotification("Spinup must be \u2265 1 day for cold start",
                         type = "error"); return()
      }
      if (as.integer(input$run_mode) == 2) {
        sv1 <- file.path(project_path, "input", "statevar.dmp")
        sv2 <- file.path(project_path, "output", "statevar.dmp")
        if (!file.exists(sv1) && !file.exists(sv2)) {
          showNotification("statevar.dmp not found \u2014 warm start will fail",
                           type = "error"); return()
        }
      }

      rv$run_in_progress <- TRUE
      rv$run_result      <- NULL

      settings <- list(
        STARTDATE   = CoseRo::format_cosero_date(input$start_date,
                                         as.integer(input$start_hour),
                                         as.integer(input$start_minute)),
        ENDDATE     = CoseRo::format_cosero_date(input$end_date,
                                         as.integer(input$end_hour),
                                         as.integer(input$end_minute)),
        SPINUP      = as.integer(input$spinup),
        OUTPUTTYPE  = as.integer(input$output_type),
        SC_FLAG     = as.integer(input$sc_flag),
        OUTCONTROL  = as.integer(input$outcontrol),
        PARAFILE    = if (!is.null(rv$pending_parafile)) rv$pending_parafile else input$parafile,
        DATAFILE    = input$datafile,
        RUNOFFFILE  = input$runofffile,
        STATSFILE   = input$statsfile,
        OPTFILE     = input$optfile,
        IKL         = as.integer(input$ikl),
        NCLASS      = as.integer(input$nclass),
        PROJECTINFO = input$project_info,
        ADDFLUXCONT = as.integer(input$addfluxcont),
        ADDFLUXFILE = input$addfluxfile,
        ADDREGCONT  = as.integer(input$addregcont),
        ADDREGFILE  = input$addregfile
      )

      withProgress(message = "Running COSERO\u2026", value = 0.1, {
        tryCatch({
          result <- CoseRo::run_cosero(
            project_path      = project_path,
            defaults_settings = settings,
            statevar_source   = as.integer(input$run_mode),
            tmmon_option      = as.integer(input$tmmon_option),
            read_outputs      = FALSE,
            quiet             = FALSE
          )
          incProgress(0.8)
          rv$run_result      <- result
          rv$run_in_progress <- FALSE

          if (result$success) {
            shared$output_dir <- file.path(project_path, "output")
            showNotification(
              paste0("Completed in ", round(result$runtime, 1), "s! ",
                     "Switch to Time Series \u2192 Load Data"),
              type = "message", duration = 8
            )
          } else {
            showNotification(paste("Run failed:", result$error_message),
                             type = "error", duration = 10)
          }
        }, error = function(e) {
          rv$run_in_progress <- FALSE
          showNotification(paste("Error:", e$message), type = "error", duration = 10)
        })
      })
      # Clear pending parafile override after run
      rv$pending_parafile <- NULL
    }

    # Run button handler
    observeEvent(input$run_btn, {
      req(shared$project_dir)
      execute_run()
    })

    # Triggered by Save & Run
    observeEvent(rv$trigger_run, {
      req(rv$trigger_run > 0, shared$project_dir)
      execute_run()
    }, ignoreInit = TRUE)

    # ── Run summary text ──────────────────────────────────────────────────
    output$run_summary <- renderText({
      if (is.null(rv$run_result)) return("No run completed yet.")
      r <- rv$run_result
      if (r$success) {
        lines <- c(
          "Status: SUCCESS",
          paste("Runtime:", round(r$runtime, 2), "seconds")
        )
        if (!is.null(r$output_data$statistics)) {
          stats <- r$output_data$statistics
          for (i in seq_len(nrow(stats))) {
            nse <- if ("NSE" %in% names(stats)) sprintf("%.3f", stats$NSE[i]) else "N/A"
            kge <- if ("KGE" %in% names(stats)) sprintf("%.3f", stats$KGE[i]) else "N/A"
            lines <- c(lines,
                        sprintf("  SB %s: NSE=%s  KGE=%s", stats$SUBBASIN[i], nse, kge))
          }
        }
        paste(lines, collapse = "\n")
      } else {
        paste("FAILED:", r$error_message)
      }
    })
  })
}
