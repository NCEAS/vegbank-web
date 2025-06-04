#' Table Data Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.

# ---- App-facing functions ----

#' Main function to process and build the plot table
#'
#' @param plot_data Data frame of plot data
#' @param taxa_data Data frame of taxa data
#' @param comm_data Data frame of community data
#' @noRd
process_table_data <- function(plot_data, taxa_data, comm_data) {
  shiny::withProgress(
    expr = {
      if (is_missing_data(plot_data, taxa_data, comm_data, show_notifications = TRUE)) {
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
#' @param plot_data Data frame of plot data
#' @param taxa_data Data frame of taxa data
#' @param comm_data Data frame of community data
#' @param show_notifications Logical, show notification if TRUE
#' @noRd
is_missing_data <- function(plot_data, taxa_data, comm_data, show_notifications = TRUE) {
  if (is.null(plot_data) || nrow(plot_data) == 0 ||
    is.null(taxa_data) || nrow(taxa_data) == 0 ||
    is.null(comm_data) || nrow(comm_data) == 0) {
    if (show_notifications) {
      shiny::showNotification(
        "Missing required data. Please try again or check your connection.",
        type = "error"
      )
    }
    return(TRUE)
  }
  FALSE
}

#' Clean a column in plot data, replacing NA/empty with 'Not Provided'
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
#' @noRd
create_taxa_vectors <- function(plot_data, taxa_data) {
  merged <- dplyr::left_join(plot_data, taxa_data, by = "observation_id")
  taxa_lists <- merged %>%
    dplyr::group_by(observation_id) %>%
    dplyr::summarize(
      top_taxa = {
        if (all(is.na(int_currplantscinamenoauth)) & all(is.na(maxcover))) {
          "No Taxa Data"
        } else {
          items <- ifelse(
            is.na(int_currplantscinamenoauth) & is.na(maxcover),
            "<li>No Taxa Data</li>",
            paste0(
              "<li><a href=\"#\" onclick=\"Shiny.setInputValue('taxa_link_click', '",
              accessioncode,
              "', {priority: 'event'}); return false;\">",
              ifelse(is.na(int_currplantscinamenoauth), "Unnamed", int_currplantscinamenoauth),
              "</a> (",
              ifelse(is.na(maxcover), "No cover", maxcover),
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
#' @noRd
create_community_vectors <- function(plot_data, comm_data) {
  merged <- dplyr::left_join(plot_data, comm_data, by = "obsaccessioncode")
  community_lists <- merged %>%
    dplyr::group_by(obsaccessioncode) %>%
    dplyr::summarize(
      comm_list = {
        if (all(is.na(commname)) & all(is.na(commconceptaccessioncode))) {
          "No Community Data"
        } else {
          items <- ifelse(
            is.na(commconceptaccessioncode) & is.na(commname),
            "<li>No Community Data</li>",
            paste0(
              "<li><a href=\"#\" onclick=\"Shiny.setInputValue('comm_link_click', '",
              commconceptaccessioncode,
              "', {priority: 'event'}); return false;\">",
              ifelse(is.na(commname), "Unnamed", commname),
              "</a></li>"
            )
          )
          paste0('<ul class="comm-list">', paste0(items, collapse = ""), "</ul>")
        }
      },
      .groups = "drop"
    )
  result <- rep("No Community Data", nrow(plot_data))
  match_idx <- match(plot_data$obsaccessioncode, community_lists$obsaccessioncode)
  result[!is.na(match_idx)] <- community_lists$comm_list[match_idx[!is.na(match_idx)]]
  result
}

#' Build the final display data.frame for the table
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
