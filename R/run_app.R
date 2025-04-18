#' Run Vegbank Web Application
#'
#' Launches the Vegbank Shiny application.
#'
#' @param options A list of options to pass to the Shiny application.
#'   Default is an empty list.
#' @family **Standalone Application**
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
