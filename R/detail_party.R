#' Build Party Details View
#' @noRd
build_party_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c(
        "party_header", "party_organization",
        "party_contact", "party_contributions"
      ),
      "Party details"
    ))
  }

  vb_code <- result$py_code
  label <- result$party_label %|||% "Unknown Name"
  organization <- result$organization_name %|||% "No organization specified"
  # TODO: if this is filled it directs to something like the email field which we don't display here
  contact <- result$contact_instructions %|||% "No contact information provided"
  contributions <- result$obs_count %|||% 0

  # Construct full name from name components, filtering out NULL/NA/empty values
  name_parts <- c(
    result$salutation,
    result$given_name,
    result$middle_name,
    result$surname
  )
  # Keep only non-null, non-NA, non-empty parts
  name_parts <- name_parts[!sapply(name_parts, function(x) {
    is.null(x) || is.na(x) || trimws(as.character(x)) == ""
  })]
  full_name <- if (length(name_parts) > 0) {
    paste(name_parts, collapse = " ")
  } else {
    NA
  }

  list(
    party_header = shiny::renderUI({
      htmltools::div(
        htmltools::tags$h5(label, style = "font-weight: 600; margin-bottom: 0px;"),
        if (!is.na(full_name)) htmltools::tags$p(paste0("(", full_name, ")"), style = "margin-bottom: 0px;"),
        htmltools::tags$h5(vb_code, style = "color: var(--vb-green); font-weight: 600; margin-bottom: 0px;")
      )
    }),
    party_organization = shiny::renderUI({
      htmltools::tags$p(organization)
    }),
    party_contact = shiny::renderUI({
      htmltools::tags$p(contact)
    }),
    party_contributions = shiny::renderUI({
      htmltools::tags$p(
        "Number of observations: ",
        create_obs_count_link(
          contributions,
          vb_code,
          label
        )
      )
    })
  )
}
