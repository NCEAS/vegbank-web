#' Run VegBank Web Application
#'
#' Launches the VegBank Shiny application.
#'
#' Running the application requires:
#'   - R (version 4.0 or higher) installed on the local machine
#'   - The `vegbankweb` package and its dependencies installed in the R environment
#'   - Optionally, an R_CONFIG_FILE environment variable set to the absolute path of a custom
#'     configuration file; defaults to the `config.yml` bundled with the package
#'     (source path: `inst/config.yml`; resolved at runtime via `system.file("config.yml", package = "vegbankweb")`)
#'   - An active internet connection to access the VegBank API
#'   - A modern web browser to view the user interface
#'
#' @param options A list of options to pass to the Shiny application.
#'   Default is an empty list.
#' @family Standalone Application
#' @return A **Shiny application** object.
#' @export
run_app <- function(options = list()) {
  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url",
    uiPattern = ".*",
    options = options
  )
}
