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
      list(
        targets = 0, orderable = FALSE, searchable = FALSE, width = "10%",
        render = DT::JS(
          "
          function(data, type, row, meta) {
            if(type === 'display') {
              if(!data || typeof data !== 'object') return '<span>No Data</span>';
              var code = data.code || '';
              var count = data.count || '';
              if(!code) return '<span>No Accession Code</span>';
              return (
                '<div class=\"btn-group btn-group-sm\">' +
                  '<button class=\"btn btn-sm btn-outline-primary\" onclick=\"Shiny.setInputValue(\\'see_obs_details\\', \\'' + code + '\\', {priority: \\'event\\'});\">Details</button>' +
                  '<button class=\"btn btn-sm btn-outline-secondary\" onclick=\"Shiny.setInputValue(\\'show_on_map\\', \\'' + count + '\\', {priority: \\'event\\'});\">Map</button>' +
                '</div>'
              );
            }
            return data;
          }
          "
        )
      ),
      list(targets = 1, width = "15%"),
      list(targets = 2, width = "10%"),
      list(
        targets = 3, orderable = FALSE, searchable = TRUE, width = "25%",
        render = DT::JS(
          "
          function(data, type, row, meta) {
            if(type === 'display') {
              if(!data || data.length === 0) return 'No Taxa Data';
              var items = data.map(function(t) {
                if(!t.code && !t.name) return '<div style=\"margin-bottom: 4px;\">No Taxa Data</div>';
                return (
                  '<div style=\"display: flex; justify-content: space-between; margin-bottom: 4px;\">' +
                    '<a href=\"#\" class=\"taxa-link\" onclick=\"Shiny.setInputValue(\\'taxa_link_click\\', \\'' + t.code + '\\', {priority: \\'event\\'});\" style=\"text-align: left;\">' +
                      (t.name || 'Unnamed') +
                    '</a>' +
                    '<span style=\"text-align: right; margin-left: 10px; font-family: Inter, -apple-system, BlinkMacSystemFont, system-ui, sans-serif; font-variant-numeric: tabular-nums;\">' +
                      (t.cover || 'No cover ') + '%' +
                    '</span>' +
                  '</div>'
                );
              });
              return '<div class=\"taxa-list\">' + items.join('') + '</div>';
            }
            return data;
          }
          "
        )
      ),
      list(
        targets = 4, width = "40%",
        render = DT::JS(
          "
          function(data, type, row, meta) {
            if(type === 'display') {
              if(!data || data.length === 0) return 'No Community Data';
              var items = data.map(function(c) {
                if(!c.code && !c.name) return '<div style=\"margin-bottom: 4px;\">No Community Data</div>';
                return (
                  '<div style=\"display: flex; margin-bottom: 4px;\">' +
                    '<a href=\"#\" class=\"comm-link\" onclick=\"Shiny.setInputValue(\\'comm_class_link_click\\', \\'' + c.code + '\\', {priority: \\'event\\'});\">' +
                      (c.name || 'Unnamed') +
                    '</a>' +
                  '</div>'
                );
              });
              return '<div class=\"comm-list\">' + items.join('') + '</div>';
            }
            return data;
          }
          "
        )
      )
    ),
    progress_message = "Processing table data:"
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

  shiny::incProgress(0.1, detail = "Cleaning locations")
  locations <- clean_column_data(plot_data, "state_province")

  shiny::incProgress(0.1, detail = "Compiling taxa...")
  taxa_data_list <- create_taxa_vectors(plot_data, taxa_data)

  shiny::incProgress(0.1, detail = "Compiling communities...")
  community_data_list <- create_community_vectors(plot_data, comm_data)

  shiny::incProgress(0.1, detail = "Compiling button data...")
  action_data_list <- create_plot_action_buttons(plot_data)

  shiny::incProgress(0.2, detail = "Building table...")
  data.frame(
    "Actions" = I(action_data_list),
    "Author Plot Code" = author_codes,
    "Location" = locations,
    "Top Taxa by Cover %" = I(taxa_data_list),
    "Community" = I(community_data_list),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

# ---- Helpers below (internal module use only) ----

#' Create action buttons for each plot row
#' 
#' @param plot_data Data frame of plot observation data
#' @param actions List of action button specifications
#' @returns A list of action button HTML strings for each row
#' @noRd
create_plot_action_buttons <- function(plot_data) {
  lapply(seq_len(nrow(plot_data)), function(i) {
    list(
      code = plot_data$obs_accession_code[i],
      count = i
      # TODO: If lat/long are needed for downstream use, add them here from plot_data$lat[i] and plot_data$long[i]
    )
  })
}

#' TODO: Should the create_taxa_vectors() function be called on each page render via a callback, or should its results be passed up to the API for more efficient data handling?
#' Create data vectors for taxa lists
#'
#' @param plot_data Data frame of plot observation data
#' @param taxa_data Data frame of taxon observation data
#' @returns A vector of lists containing top taxa ordered by max cover for each plot observation
#'
#' @importFrom dplyr left_join group_by summarize
#' @noRd
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
              cover = format(round(.data$max_cover[i], 2), nsmall = 2)
            )
          })
        }
      ),
      .groups = "drop"
    )
  # Match index to plot_data to avoid duplicates / NAs where no taxa exist
  result <- vector("list", nrow(plot_data))
  match_idx <- match(plot_data$observation_id, taxa_lists$observation_id)
  result[!is.na(match_idx)] <- taxa_lists$taxa[match_idx[!is.na(match_idx)]]
  result[sapply(result, is.null)] <- list(list())
  result
}

#' Create data vectors for community lists
#'
#' @param plot_data Data frame of plot observation data
#' @param comm_data Data frame of community classification data
#' @returns A vector lists of community names and codes for each plot observation
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
  # Match index to plot_data to avoid duplicates / NAs where no communities exist
  result <- vector("list", nrow(plot_data))
  match_idx <- match(plot_data$obs_accession_code, community_lists$obs_accession_code)
  result[!is.na(match_idx)] <- community_lists$communities[match_idx[!is.na(match_idx)]]
  result[sapply(result, is.null)] <- list(list())
  result
}
