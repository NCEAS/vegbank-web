#' Table Data Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.
#' 
#' @importFrom DT datatable
#' @importFrom shiny withProgress incProgress showNotification
#' @importFrom dplyr left_join group_by summarize
#' @importFrom htmltools HTML

# ---- App-facing functions ----

#' Main function to process and build the plot table
#'
#' @param plot_data Data frame of plot observation data
#' @param taxa_data Data frame of taxon obseration data
#' @param comm_data Data frame of community classification data
#' @returns A DT datatable object ready for display in a Shiny app
#' @export
process_table_data <- function(plot_data, taxa_data, comm_data) {
  data_sources <- list(
    plot_data = plot_data,
    taxa_data = taxa_data,
    comm_data = comm_data
  )
  
  required_sources <- c("plot_data", "taxa_data", "comm_data")
  
  table_config <- list(
    column_defs = list(
      list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
      list(targets = 1, width = "15%"),
      list(targets = 2, width = "10%"),
      list(targets = 3, orderable = FALSE, searchable = TRUE, width = "45%"),
      list(targets = 4, width = "20%")
    ),
    progress_message = "Processing plot table data"
  )
  
  create_table(
    data_sources = data_sources,
    required_sources = required_sources,
    process_function = process_plot_data,
    table_config = table_config
  )
}

#' Process plot data into display format
#'
#' @param data_sources List containing plot_data, taxa_data, and comm_data
#' @returns A data frame ready for display
#' @noRd
process_plot_data <- function(data_sources) {
  plot_data <- data_sources$plot_data
  taxa_data <- data_sources$taxa_data
  comm_data <- data_sources$comm_data
  
  shiny::incProgress(0.2, detail = "Cleaning author observation codes")
  author_codes <- clean_column_data(plot_data, "author_plot_code")
  
  shiny::incProgress(0.1, detail = "Cleaning location data")
  locations <- clean_column_data(plot_data, "state_province")
  
  shiny::incProgress(0.1, detail = "Creating taxa lists...")
  taxa_html <- create_taxa_vectors(plot_data, taxa_data)
  
  shiny::incProgress(0.1, detail = "Creating community links...")
  community_html <- create_community_vectors(plot_data, comm_data)
  
  shiny::incProgress(0.1, detail = "Creating action buttons...")
  action_buttons <- create_action_buttons(plot_data, list(
    list(id = "see_details", label = "Details", class = "btn-outline-primary"),
    list(id = "show_on_map", label = "Map", class = "btn-outline-secondary")
  ))
  
  shiny::incProgress(0.2, detail = "Building table...")
  data.frame(
    "Actions" = action_buttons,
    "Author Plot Code" = author_codes,
    "Location" = locations,
    "Top Taxa" = taxa_html,
    "Community" = community_html,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# ---- Helpers below (internal module use only) ----

#' Create HTML vectors for taxa lists
#'
#' @param plot_data Data frame of plot observation data
#' @param taxa_data Data frame of taxon observation data
#' @returns A character vector of HTML strings for top taxa by max cover for each plot observation
#'
#' @importFrom dplyr left_join group_by summarize
#' @noRd
create_taxa_vectors <- function(plot_data, taxa_data) {
  merged <- dplyr::left_join(plot_data, taxa_data, by = "observation_id")
  taxa_lists <- merged |>
    dplyr::group_by(.data$observation_id) |>
    dplyr::summarize(
      top_taxa = {
        if (all(is.na(.data$int_curr_plant_sci_name_no_auth)) & all(is.na(.data$max_cover))) {
          "No Taxa Data"
        } else {
          items <- ifelse(
            is.na(.data$int_curr_plant_sci_name_no_auth) & is.na(.data$max_cover),
            "<li>No Taxa Data</li>",
            paste0(
              "<li><a href=\"#\" onclick=\"Shiny.setInputValue('taxa_link_click', '",
              .data$taxon_observation_accession_code,
              "', {priority: 'event'}); return false;\">",
              ifelse(is.na(.data$int_curr_plant_sci_name_no_auth), "Unnamed", .data$int_curr_plant_sci_name_no_auth),
              "</a> (",
              ifelse(is.na(.data$max_cover), "No cover", .data$max_cover),
              "%)</li>"
            )
          )
          paste0('<ul class="taxa-list">', paste0(items, collapse = ""), "</ul>")
        }
      },
      .groups = "drop"
    )
  result <- rep("No Taxa Data", nrow(plot_data))
  match_idx <- match(plot_data$observation_id, taxa_lists$observation_id)
  result[!is.na(match_idx)] <- taxa_lists$top_taxa[match_idx[!is.na(match_idx)]]
  result
}

#' Create HTML vectors for community lists
#'
#' @param plot_data Data frame of plot observation data
#' @param comm_data Data frame of community classification data
#' @returns A character vector of HTML strings for community lists
#'
#' @importFrom dplyr left_join group_by summarize
#' @noRd
create_community_vectors <- function(plot_data, comm_data) {
  merged <- dplyr::left_join(plot_data, comm_data, by = "obs_accession_code")
  community_lists <- merged |>
    dplyr::group_by(.data$obs_accession_code) |>
    dplyr::summarize(
      comm_list = {
        if (all(is.na(.data$comm_name)) & all(is.na(.data$comm_concept_accession_code))) {
          "No Community Data"
        } else {
          items <- ifelse(
            is.na(.data$comm_concept_accession_code) & is.na(.data$comm_name),
            "<li>No Community Data</li>",
            paste0(
              "<li><a href=\"#\" onclick=\"Shiny.setInputValue('comm_link_click', '",
              .data$comm_concept_accession_code,
              "', {priority: 'event'}); return false;\">",
              ifelse(is.na(.data$comm_name), "Unnamed", .data$comm_name),
              "</a></li>"
            )
          )
          paste0('<ul class="comm-list">', paste0(items, collapse = ""), "</ul>")
        }
      },
      .groups = "drop"
    )
  result <- rep("No Community Data", nrow(plot_data))
  match_idx <- match(plot_data$obs_accession_code, community_lists$obs_accession_code)
  result[!is.na(match_idx)] <- community_lists$comm_list[match_idx[!is.na(match_idx)]]
  result
}
