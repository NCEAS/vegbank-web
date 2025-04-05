#' Run Vegbank Web Application
#'
#' Launches the Vegbank Shiny application.
#'
#' @export
run_app <- function() {
  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url"
  )
}
