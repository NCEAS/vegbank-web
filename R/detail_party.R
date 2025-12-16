#' Build Party Details View
#' @noRd
build_party_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("party_name", "party_organization", "party_contact", "party_contributions"),
      "Party details"
    ))
  }

  label <- result$party_label %|||% "Unknown Name"
  organization <- result$organization_name %|||% "No organization specified"
  # TODO: if this is filled it directs to something like the email field which we don't display here
  contact <- result$contact_instructions %|||% "No contact information provided"
  contributions <- result$obs_count %|||% 0

  list(
    party_name = shiny::renderUI({
      htmltools::tags$p(label)
    }),
    party_organization = shiny::renderUI({
      htmltools::tags$p(organization)
    }),
    party_contact = shiny::renderUI({
      htmltools::tags$p(contact)
    }),
    party_contributions = shiny::renderUI({
      htmltools::tags$p("Number of observations: ", htmltools::tags$strong(contributions))
    })
  )
}
