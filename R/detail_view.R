#' Detail View Functions
#'
#' Functions for building the detail view panel.
#' @keywords internal

#' Build Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed plot information.
#'
#' @param selected_data A data row representing the selected plot.
#' @return A list of Shiny UI outputs.
#' @importFrom htmltools tags
#' @importFrom shiny renderUI
#' @keywords internal
build_details_view <- function(selected_data) {
  # Mapping internal field names to their display names.
  display_names <- list(
    authorplotcode = "Author Plot Code",
    authorobscode = "Author Observation Code",
    area = "Area",
    permanence = "Permanent",
    elevation = "Elevation",
    slopeaspect = "Slope Aspect",
    slopegradient = "Slope Gradient",
    confidentialitystatus = "Confidentiality Status",
    confidentialitytext = "Confidentiality Status",
    latitude = "Latitude",
    longitude = "Longitude",
    locationnarrative = "Location Description",
    stateprovince = "State/Province",
    country = "Country",
    obsstartdate = "Observation Start Date",
    project_id = "Project ID",
    projectname = "Project Name",
    covermethod_id = "Cover Method ID",
    covertype = "Cover Type",
    stratummethod_id = "Stratum Method ID",
    stratummethodname = "Stratum Method",
    stratummethoddescription = "Stratum Method Description",
    taxonobservationarea = "Taxon Observation Area",
    autotaxoncover = "Taxon Cover Automatically Calculated",
    plotvalidationlevel = "Plot Validation Level",
    plotvalidationleveldescr = "Validation Level"
  )

  create_table <- function(details, col_names) {
    htmltools::tags$table(
      class = "table table-sm table-striped table-hover",
      htmltools::tags$tbody(
        lapply(names(details), function(name) {
          display_name <- if (!is.null(col_names[[name]])) col_names[[name]] else name
          htmltools::tags$tr(
            htmltools::tags$td(htmltools::tags$strong(display_name)),
            htmltools::tags$td(class = "text-end", details[[name]])
          )
        })
      )
    )
  }

  safe_render_details <- function(fields) {
    shiny::renderUI({
      values <- lapply(selected_data[fields], function(x) {
        if (is.null(x) || all(is.na(x))) "Not recorded" else x
      })
      create_table(values, col_names = display_names)
    })
  }

  taxa_details_ui <- shiny::renderUI({
    tryCatch(
      {
        taxa <- selected_data[["taxa"]]
        if (is.null(taxa)) {
          "No taxa recorded"
        }
        if (!is.data.frame(taxa)) taxa <- as.data.frame(taxa)
        if (nrow(taxa) == 0) {
          "No taxa recorded"
        }

        taxa$cover <- as.numeric(taxa$cover)
        sorted_taxa <- taxa[order(-taxa$cover), ]
        rows <- lapply(seq_len(nrow(sorted_taxa)), function(i) {
          row <- sorted_taxa[i, ]
          htmltools::tags$tr(
            htmltools::tags$td(row$authorplantname),
            htmltools::tags$td(style = "text-align: right;", sprintf("%.2f%%", row$cover))
          )
        })
        htmltools::tags$table(
          class = "table table-sm table-striped table-hover",
          htmltools::tags$thead(
            htmltools::tags$tr(
              htmltools::tags$th("Author Plant Name"),
              htmltools::tags$th("Cover")
            )
          ),
          htmltools::tags$tbody(rows)
        )
      },
      error = function(e) {
        paste("Error processing taxa:", e$message)
      }
    )
  })

  list(
    plot_id_details = safe_render_details(c("authorobscode", "authorplotcode")),
    location_details = safe_render_details(c(
      "confidentialitytext", "latitude", "longitude",
      "locationnarrative", "stateprovince", "country"
    )),
    layout_details = safe_render_details(c("area", "permanence")),
    environmental_details = safe_render_details(c("elevation", "slopeaspect", "slopegradient")),
    methods_details = safe_render_details(c(
      "obsstartdate", "projectname", "covertype",
      "stratummethodname", "stratummethoddescription",
      "taxonobservationarea", "autotaxoncover"
    )),
    plot_quality_details = safe_render_details("plotvalidationleveldescr"),
    taxa_details = taxa_details_ui
  )
}

#' Build Community Details View
#'
#' Constructs a list of Shiny UI outputs for displaying detailed community information.
#'
#' @param community_data A data row representing the selected community.
#' @return A list of Shiny UI outputs.
#' @importFrom htmltools tags HTML
#' @importFrom shiny renderUI
#' @keywords internal
build_community_details_view <- function(community_data) {
  if (is.null(community_data) || nrow(community_data) == 0) {
    return(list(
      community_name = shiny::renderUI({
        htmltools::tags$p("Community details not available")
      }),
      community_description = shiny::renderUI({
        htmltools::tags$p("No description available")
      })
    ))
  }

  # Take first row if multiple are returned
  community_data <- community_data[1, ]

  list(
    community_name = shiny::renderUI({
      htmltools::tags$b(community_data$commname)
    }),
    community_description = shiny::renderUI({
      # The description contains HTML entities that need to be properly rendered
      htmltools::tags$div(
        id = "community-description",
        htmltools::HTML(community_data$commdescription)
      )
    })
  )
}
