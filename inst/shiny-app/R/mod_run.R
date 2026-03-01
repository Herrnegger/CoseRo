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
                     class = "btn-primary btn-sm w-100 mb-2"),

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
            card_header("Time Period", class = "py-1 bg-light"),
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
            card_header("Output Control", class = "py-1 bg-light"),
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
            card_header("Input Files", class = "py-1 bg-light"),
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
            card_header("Output Files", class = "py-1 bg-light"),
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
            card_header("Model Structure", class = "py-1 bg-light"),
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
            card_header("Other", class = "py-1 bg-light"),
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
            card_header("Boundary Conditions", class = "py-1 bg-light"),
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

      # ── Tab 4: Parameter Table ──────────────────────────────────────
      nav_panel(
        title = "Parameters",
        icon = icon("table"),
        card(
          card_body(
            class = "p-2",
            uiOutput(ns("param_table_info")),
            DT::DTOutput(ns("param_table"), height = "auto")
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
      run_result      = NULL
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
        }
      }
    })

    # ── Load parameter file data for value boxes ──────────────────────────
    load_param_data <- function() {
      req(shared$project_dir)
      tryCatch({
        para_name <- input$parafile
        if (is.null(para_name) || !nzchar(para_name)) {
          defaults <- read_cosero_defaults_safe(shared$project_dir)
          para_name <- defaults$PARAFILE %||% "para.txt"
        }
        para_file <- file.path(shared$project_dir, "input", para_name)
        if (file.exists(para_file)) {
          rv$params <- read_cosero_parameters(para_file)
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

    # ── Load Defaults ─────────────────────────────────────────────────────
    # Reusable function to load defaults into UI
    load_defaults_into_ui <- function(notify = TRUE) {
      d <- read_cosero_defaults_safe(shared$project_dir)
      sp <- parse_cosero_date(d$STARTDATE)
      ep <- parse_cosero_date(d$ENDDATE)

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
          STARTDATE   = format_cosero_date(input$start_date,
                                           as.integer(input$start_hour),
                                           as.integer(input$start_minute)),
          ENDDATE     = format_cosero_date(input$end_date,
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
        modify_defaults(defaults_file, settings, quiet = TRUE)
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
    observeEvent(input$run_btn, {
      req(shared$project_dir)
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

      # Build settings from all three tabs
      settings <- list(
        STARTDATE   = format_cosero_date(input$start_date,
                                         as.integer(input$start_hour),
                                         as.integer(input$start_minute)),
        ENDDATE     = format_cosero_date(input$end_date,
                                         as.integer(input$end_hour),
                                         as.integer(input$end_minute)),
        SPINUP      = as.integer(input$spinup),
        OUTPUTTYPE  = as.integer(input$output_type),
        SC_FLAG     = as.integer(input$sc_flag),
        OUTCONTROL  = as.integer(input$outcontrol),
        PARAFILE    = input$parafile,
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
          result <- run_cosero(
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
    })

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
