library(shiny)
library(htmltools)

# Source helpers and build the UI
source("helpers.R", local = TRUE)

ui <- function(req) {
  navbar_with_search <- build_navbar()
  overlay <- build_overlay()

  script <- tags$script(HTML(
    "Shiny.addCustomMessageHandler('openOverlay', function(message) {
         document.getElementById('detail-overlay').style.right = '0px';
     });"
  ))

  btn_script <- tags$script(HTML(
    '$(document).on("click", ".details-btn", function() {
         var idx = $(this).data("row");
         Shiny.setInputValue("see_details", idx, {priority:"event"});
    });'
  ))
  tagList(navbar_with_search, overlay, script, btn_script)
}
