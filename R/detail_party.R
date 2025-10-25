#' Build Party Details View
#' @noRd
build_party_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(list(
      party_name = shiny::renderUI({
        htmltools::tags$p("Party details not available")
      }),
      party_organization = shiny::renderUI({
        htmltools::tags$p("No organization available")
      }),
      party_contact = shiny::renderUI({
        htmltools::tags$p("No contact information available")
      }),
      party_projects = shiny::renderUI({
        htmltools::tags$p("No projects available")
      })
    ))
  }

  full_name <- paste(
    ifelse(is.na(result$salutation), "", paste0(result$salutation, " ")),
    ifelse(is.na(result$given_name), "", result$given_name),
    ifelse(is.na(result$middle_name), "", paste0(" ", result$middle_name)),
    ifelse(is.na(result$surname), "", paste0(" ", result$surname))
  )
  full_name <- trimws(full_name)
  if (identical(full_name, "")) {
    full_name <- if (!is.na(result$organization_name)) {
      result$organization_name
    } else {
      "Unknown Name"
    }
  }

  list(
    party_name = shiny::renderUI({
      htmltools::tags$p(full_name)
    }),
    party_organization = shiny::renderUI({
      if (is.na(result$organization_name)) {
        htmltools::tags$p("No organization specified")
      } else {
        htmltools::tags$p(result$organization_name)
      }
    }),
    party_contact = render_detail_table(c("contact_instructions"), result),
    party_projects = shiny::renderUI({
      htmltools::tags$p("Associated projects would be displayed here")
    })
  )
}
