#' Run Vegbank Web Application
#'
#' Launches the Vegbank Shiny application.
#'
#' @export
runVegBankApp <- function() {
  shiny::shinyApp(
    ui = VegBankWeb::ui,
    server = VegBankWeb::server,
    enableBookmarking = "url"
  )
}
