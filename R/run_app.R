#' Run Vegbank Web Application
#'
#' Launches the Vegbank Shiny application.
#'
#' @family **Standalone Application**
#' @section Details:
#' The application uses:
#' - **UI**: Defined in [`ui.R`].
#' - **Server Logic**: Defined in [`server.R`].
#' @return A **Shiny application** object.
#' @export
run_app <- function(options = list()) {
  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url",
    options = options
  )
}