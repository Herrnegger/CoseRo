#' @importFrom bslib bs_theme
NULL

#' Launch COSERO Workbench Shiny Application
#'
#' @description
#' Launches the interactive COSERO Workbench Shiny application for running
#' COSERO model simulations and visualizing outputs.
#'
#' @param project_dir Path to COSERO project directory. If provided, the app
#'   will automatically load output data from this directory.
#' @param launch.browser Logical. If TRUE (default), opens the app in your
#'   default web browser. If FALSE, prints the URL to access the app.
#' @param port Integer. The TCP port that the application should listen on.
#'   If NULL (default), a random available port is used.
#' @param ... Additional arguments passed to \code{\link[shiny]{runApp}}
#'
#' @return No return value, called for side effects (launches Shiny app)
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Launch with example project
#' launch_cosero_app("D:/COSERO_example")
#'
#' # Launch without pre-loading data
#' launch_cosero_app()
#'
#' # Launch on specific port
#' launch_cosero_app(port = 3838)
#' }
launch_cosero_app <- function(project_dir = NULL, launch.browser = TRUE, port = NULL, ...) {
  # Get the app directory from the installed package
  app_dir <- system.file("shiny-app", package = "CoseRo")

  # Check if app directory exists
  if (app_dir == "") {
    stop("Could not find Shiny app. Try re-installing the CoseRo package.",
         call. = FALSE)
  }

  # Check if logo exists
  logo_path <- file.path(app_dir, "www", "logo.svg")
  if (!file.exists(logo_path)) {
    message("Note: Logo file not found at ", logo_path)
    message("App will launch but logo may not display.")
  }

  # Validate and normalize project directory if provided
  if (!is.null(project_dir)) {
    if (!dir.exists(project_dir)) {
      stop("Project directory does not exist: ", project_dir, call. = FALSE)
    }
    project_dir <- normalizePath(project_dir, winslash = "/")
    message("Pre-loading project: ", project_dir)

    # Pass project directory to app via global option (more reliable than shinyOptions)
    options(cosero_project_dir = project_dir)
  } else {
    # Clear any existing option
    options(cosero_project_dir = NULL)
  }

  # Launch the app
  message("Launching COSERO Workbench...")
  message("App directory: ", app_dir)

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    port = port,
    ...
  )
}
