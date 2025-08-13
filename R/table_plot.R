#' Plot Table Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.

#' Main function to process and build the plot table
#'
#' @param plot_data Data frame of plot observation data
#' @param taxa_data Data frame of taxon obseration data
#' @param comm_data Data frame of community classification data
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_plot_table <- function(plot_data, taxa_data, comm_data) {
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
      list(
        targets = 3, orderable = FALSE, searchable = TRUE, width = "25%",
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display') {",
          "    if(!data || data.length === 0) return 'No Taxa Data';",
          "    var items = data.map(function(t) {",
          "      if(!t.code && !t.name) return '<div style=\"margin-bottom: 4px;\">No Taxa Data</div>';",
          "      return '<div style=\"display: flex; justify-content: space-between; margin-bottom: 4px;\"><a href=\"#\" class=\"taxa-link\" onclick=\"Shiny.setInputValue(\\'taxa_link_click\\', \\'' + t.code + '\\', {priority: \\'event\\'});\" style=\"text-align: left;\">' + (t.name || 'Unnamed') + '</a><span style=\"text-align: right; margin-left: 10px; font-family: Inter, -apple-system, BlinkMacSystemFont, system-ui, sans-serif; font-variant-numeric: tabular-nums;\">' + (t.cover || 'No cover ') + '%</span></div>';",
          "    });",
          "    return '<div class=\"taxa-list\">' + items.join('') + '</div>';",
          "  }",
          "  return data;",
          "}"
        )
      ),
      list(
        targets = 4, width = "40%",
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display') {",
          "    if(!data || data.length === 0) return 'No Community Data';",
          "    var items = data.map(function(c) {",
          "      if(!c.code && !c.name) return '<li>No Community Data</li>';",
          "      return '<div style=\"margin-bottom: 4px;\"><a href=\"#\" class=\"comm-link\" onclick=\"Shiny.setInputValue(\\'comm_class_link_click\\', \\'' + c.code + '\\', {priority: \\'event\\'});\">' + (c.name || 'Unnamed') + '</a></div>';",
          "    });",
          "    return '<ul class=\"comm-list\">' + items.join('') + '</ul>';",
          "  }",
          "  return data;",
          "}"
        )
      )
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

  shiny::incProgress(0.1, detail = "Aggregating taxa data...")
  taxa_data_list <- create_taxa_vectors(plot_data, taxa_data)

  shiny::incProgress(0.1, detail = "Aggregating community data...")
  community_data_list <- create_community_vectors(plot_data, comm_data)

  shiny::incProgress(0.1, detail = "Creating action buttons...")
  action_buttons <- create_action_buttons(plot_data, list(
    list(input_id = "see_details", label = "Details", class = "btn-outline-primary"),
    list(input_id = "show_on_map", label = "Map", class = "btn-outline-secondary")
  ))

  shiny::incProgress(0.2, detail = "Building table...")
  data.frame(
    "Actions" = action_buttons,
    "Author Plot Code" = author_codes,
    "Location" = locations,
    "Top Taxa" = I(taxa_data_list),
    "Community" = I(community_data_list),
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
#' TODO: Can this be done on each page render on a callback?
create_taxa_vectors <- function(plot_data, taxa_data) {
  merged <- dplyr::left_join(plot_data, taxa_data, by = "observation_id")
  taxa_lists <- merged |>
    dplyr::group_by(.data$observation_id) |>
    dplyr::summarize(
      taxa = list(
        if (all(is.na(.data$int_curr_plant_sci_name_no_auth)) & all(is.na(.data$max_cover))) {
          list()
        } else {
          lapply(seq_along(.data$taxon_observation_accession_code), function(i) {
            list(
              code = .data$taxon_observation_accession_code[i],
              name = .data$int_curr_plant_sci_name_no_auth[i],
              cover = round(.data$max_cover[i], 2)
            )
          })
        }
      ),
      .groups = "drop"
    )
  result <- vector("list", nrow(plot_data))
  match_idx <- match(plot_data$observation_id, taxa_lists$observation_id)
  result[!is.na(match_idx)] <- taxa_lists$taxa[match_idx[!is.na(match_idx)]]
  result[sapply(result, is.null)] <- list(list())
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
      communities = list(
        if (all(is.na(.data$comm_name)) & all(is.na(.data$comm_class_accession_code))) {
          list()
        } else {
          lapply(seq_along(.data$comm_class_accession_code), function(i) {
            list(
              code = .data$comm_class_accession_code[i],
              name = .data$comm_name[i]
            )
          })
        }
      ),
      .groups = "drop"
    )
  result <- vector("list", nrow(plot_data))
  match_idx <- match(plot_data$obs_accession_code, community_lists$obs_accession_code)
  result[!is.na(match_idx)] <- community_lists$communities[match_idx[!is.na(match_idx)]]
  result[sapply(result, is.null)] <- list(list())
  result
}
