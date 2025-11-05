#' Launch COSERO Workbench Shiny Application
#'
#' @description
#' Launches the interactive COSERO Workbench Shiny application for running
#' COSERO model simulations and visualizing outputs.
#'
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
#' # Launch the app in your browser
#' launch_cosero_app()
#'
#' # Launch on specific port
#' launch_cosero_app(port = 3838)
#'
#' # Launch without opening browser (prints URL)
#' launch_cosero_app(launch.browser = FALSE)
#' }
launch_cosero_app <- function(launch.browser = TRUE, port = NULL, ...) {
  # Get the app directory from the installed package
  app_dir <- system.file("shiny-app", package = "COSERO")

  # Check if app directory exists
  if (app_dir == "") {
    stop("Could not find Shiny app. Try re-installing the COSERO package.",
         call. = FALSE)
  }

  # Check if logo exists
  logo_path <- file.path(app_dir, "www", "logo.svg")
  if (!file.exists(logo_path)) {
    message("Note: Logo file not found at ", logo_path)
    message("App will launch but logo may not display.")
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
