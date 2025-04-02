#' Run Vegbank Web Application
#'
#' Launches the Vegbank Shiny application.
#'
#' @export
runVegBankApp <- function() {
  shiny::shinyApp(
    ui = ui,
    server = server,
    enableBookmarking = "url"
  )
}
