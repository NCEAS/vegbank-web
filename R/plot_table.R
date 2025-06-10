#' Table Data Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.

# ---- App-facing functions ----

#' Main function to process and build the plot table
#'
#' @param plot_data Data frame of plot observation data
#' @param taxa_data Data frame of taxon obseration data
#' @param comm_data Data frame of community classification data
#' @returns A DT datatable object ready for display in a Shiny app
#'
#' @importFrom DT datatable
#' @importFrom shiny withProgress incProgress showNotification
#' @importFrom dplyr left_join group_by summarize
#' @importFrom htmltools HTML
#'
#' @noRd
process_table_data <- function(plot_data, taxa_data, comm_data) {
  shiny::withProgress(
    expr = {
      if (is_missing_data(plot_data, taxa_data, comm_data)) {
        return(create_empty_table())
      }
      shiny::incProgress(0.2, detail = "Cleaning author observation codes")
      author_codes <- clean_column_data(plot_data, "authorplotcode")
      shiny::incProgress(0.1, detail = "Cleaning location data")
      locations <- clean_column_data(plot_data, "stateprovince")
      shiny::incProgress(0.1, detail = "Creating taxa lists...")
      taxa_html <- create_taxa_vectors(plot_data, taxa_data)
      shiny::incProgress(0.1, detail = "Creating community links...")
      community_html <- create_community_vectors(plot_data, comm_data)
      shiny::incProgress(0.1, detail = "Creating action buttons...")
      action_buttons <- create_action_buttons(plot_data)
      shiny::incProgress(0.2, detail = "Building table...")
      display_data <- build_display_data(
        author_codes, locations, taxa_html, community_html, action_buttons
      )
      DT::datatable(
        display_data,
        rownames = FALSE,
        escape = FALSE,
        selection = list(mode = "single", target = "row", selectable = FALSE),
        options = list(
          dom = "frtip",
          pageLength = 100,
          scrollY = "calc(100vh - 300px)",
          scrollX = TRUE,
          scrollCollapse = TRUE,
          deferRender = TRUE,
          processing = TRUE,
          columnDefs = list(
            list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
            list(targets = 1, width = "15%"),
            list(targets = 2, width = "10%"),
            list(targets = 3, orderable = FALSE, searchable = TRUE, width = "45%"),
            list(targets = 4, width = "20%")
          )
        )
      )
    },
    message = "Processing table data",
    value = 0
  )
}

# ---- Helpers below (internal module use only) ----

#' Create an empty DT table for missing data
#'
#' @returns A DT datatable with a message indicating no data is available
#'
#' @importFrom DT datatable
#' @noRd
create_empty_table <- function() {
  DT::datatable(
    data.frame(
      "No.Data.Available" = "Please try again or check your connection",
      check.names = FALSE, stringsAsFactors = FALSE
    ),
    options = list(dom = "t"),
    rownames = FALSE
  )
}

#' Check if any required data is missing
#'
#' @param plot_data Data frame of plot observation data
#' @param taxa_data Data frame of taxon obseration data
#' @param comm_data Data frame of community classification data
#' @returns TRUE if any data is missing, FALSE otherwise
#' @noRd
is_missing_data <- function(plot_data, taxa_data, comm_data) {
  if (is.null(plot_data) || nrow(plot_data) == 0 ||
    is.null(taxa_data) || nrow(taxa_data) == 0 ||
    is.null(comm_data) || nrow(comm_data) == 0) {
    shiny::showNotification(
      "Missing required data. Please try again or check your connection.",
      type = "error"
    )
    return(TRUE)
  }
  FALSE
}

#' Clean a column in plot data, replacing NA/empty with 'Not Provided'
#'
#' @param plot_data Data frame of plot observation data
#' @param column_name Name of the column to clean
#' @returns A character vector with cleaned data
#' @noRd
clean_column_data <- function(plot_data, column_name) {
  if (column_name %in% colnames(plot_data)) {
    ifelse(is.na(plot_data[[column_name]]) | plot_data[[column_name]] == "",
      "Not Provided",
      plot_data[[column_name]]
    )
  } else {
    rep("Not Provided", nrow(plot_data))
  }
}

#' Create action buttons for each row
#'
#' @param data Data frame of plot observation data
#' @returns A character vector of HTML strings for action buttons
#' @noRd
create_action_buttons <- function(data) {
  vapply(seq_len(nrow(data)), function(i) {
    sprintf(
      '<div class="btn-group btn-group-sm">
      <button class="btn btn-sm btn-outline-primary"
        onclick="Shiny.setInputValue(\'see_details\', %d, {priority: \'event\'})">
        Details
      </button>
      <button class="btn btn-sm btn-outline-secondary"
        onclick="Shiny.setInputValue(\'show_on_map\', %d, {priority: \'event\'})">
        Map
      </button>
     </div>',
      i, i
    )
  }, character(1))
}

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

#' Build the final display data.frame for the table
#'
#' @param author_codes Vector of cleaned author plot codes
#' @param locations Vector of cleaned location data
#' @param taxa_html Vector of HTML strings for taxa lists
#' @param community_html Vector of HTML strings for community lists
#' @param action_buttons Vector of HTML strings for action buttons
#' @returns A data frame ready for display in a DT table
#' @noRd
build_display_data <- function(author_codes, locations, taxa_html, community_html, action_buttons) {
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
