#' UI Helper Functions
#'
#' Functions for generating UI components and HTML elements.
#' @keywords internal

#' Build Taxa List
#'
#' Generates an HTML ordered list of taxa from a data row.
#'
#' @param data_row A row of data that contains taxa information.
#' @return A character string containing HTML markup.
#' @keywords internal
build_taxa_list <- function(data_row) {
  tryCatch(
    {
      taxa <- data_row[["taxa"]]
      if (!is.null(taxa) && nrow(taxa) > 0) {
        sorted_taxa <- taxa[order(-taxa$maxcover), ]
        top5 <- utils::head(sorted_taxa, 5)
        taxa_items <- sprintf("<li>%s <b>(%g%%)</b></li>", top5$authorplantname, top5$maxcover)
        paste("<ol>", paste(taxa_items, collapse = "\n"), "</ol>", sep = "\n")
      } else {
        "No taxa recorded"
      }
    },
    error = function(e) {
      print(paste("Error in build_taxa_list:", e$message))
      "Error processing taxa"
    }
  )
}

#' Build Action Buttons
#'
#' Creates action buttons for viewing details and mapping a plot.
#'
#' @param i Index used to uniquely identify buttons.
#' @return A character string containing the button HTML.
#' @importFrom htmltools tagList
#' @importFrom shiny actionButton
#' @keywords internal
build_action_buttons <- function(i) {
  as.character(
    htmltools::tagList(
      shiny::actionButton(
        inputId = paste0("see_details_", i),
        label = "See Details",
        class = "btn btn-info btn-sm details-btn mb-1",
        onclick = sprintf("Shiny.setInputValue('see_details', %d, {priority: 'event'})", i)
      ),
      shiny::actionButton(
        inputId = paste0("map_btn_", i),
        label = "Show on Map",
        class = "btn btn-primary btn-sm map-btn",
        onclick = sprintf("Shiny.setInputValue('show_on_map', '%s', {priority: 'event'})", i)
      )
    )
  )
}

#' Build Community Link
#'
#' Creates a clickable link for community names in the table.
#'
#' @param name The community name to display as link text.
#' @param code The accession code to pass when the link is clicked.
#' @return A character string containing the HTML link, or the original name if no code.
#' @keywords internal
build_community_link <- function(name, code) {
  if (is.na(name) || name == "Unknown" || is.na(code)) {
    return(as.character(name))
  }
  # Return a raw HTML string for the clickable link
  sprintf(
    '<a href="#" style="color:#2c5443; text-decoration:underline; font-weight:bold;"
        onclick="Shiny.setInputValue(\'comm_link_click\', \'%s\', {priority: \'event\'}); return false;">%s</a>',
    code, name
  )
}
