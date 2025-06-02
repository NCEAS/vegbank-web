#' Table Data Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.
#'
#' @importFrom DT datatable
#'
#' @noRd
plot_table <- (function() {
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
  
  is_missing_data <- function(plot_data, taxa_data, comm_data, show_notifications = TRUE) {
    # Return early if no data
    if (is.null(plot_data) || nrow(plot_data) == 0 ||
      is.null(taxa_data) || nrow(taxa_data) == 0 ||
      is.null(comm_data) || nrow(comm_data) == 0) {
      # Show notification if enabled using the shared handler
      if (show_notifications) {
        progress_handler$show_notification(
          "Missing required data. Please try again or check your connection.",
          type = "error"
        )
      }
      return(TRUE)
    }
    FALSE
  }

  clean_column_data <- function(plot_data, column_name) {
    if (column_name %in% colnames(plot_data)) {
      # Replace NA or empty values with "Not Provided"
      ifelse(is.na(plot_data[[column_name]]) | plot_data[[column_name]] == "",
        "Not Provided",
        plot_data[[column_name]]
      )
    } else {
      rep("Not Provided", nrow(plot_data))
    }
  }

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

  create_taxa_vectors <- function(plot_data, taxa_data) {
    # Join plot and taxa data
    merged <- dplyr::left_join(plot_data, taxa_data, by = "observation_id")

    # Generate HTML for each observation_id
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

    # Return vector in the same order as plot_data
    result <- rep("No Taxa Data", nrow(plot_data))
    match_idx <- match(plot_data$observation_id, taxa_lists$observation_id)
    result[!is.na(match_idx)] <- taxa_lists$top_taxa[match_idx[!is.na(match_idx)]]
    result
  }

  create_community_vectors <- function(plot_data, comm_data) {
    # Join plot and community data
    merged <- dplyr::left_join(plot_data, comm_data, by = "obsaccessioncode")

    # Generate HTML for each obsaccessioncode
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

    # Return vector in the same order as plot_data
    result <- rep("No Community Data", nrow(plot_data))
    match_idx <- match(plot_data$obsaccessioncode, community_lists$obsaccessioncode)
    result[!is.na(match_idx)] <- community_lists$comm_list[match_idx[!is.na(match_idx)]]
    result
  }

  build_display_data <- function(author_codes, locations, taxa_html, community_html, action_buttons) {
    # Compose the final table data.frame
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

  # Fix the progress handling system
  process_table_data <- function(plot_data, taxa_data, comm_data) {
    # Use the shared progress handler
    progress_handler$with_safe_progress(
      expr = {
        if (is_missing_data(plot_data, taxa_data, comm_data, show_notifications = TRUE)) {
          return(create_empty_table())
        }

        progress_handler$inc_progress(0.2, detail = "Cleaning author observation codes")
        author_codes <- clean_column_data(plot_data, "authorplotcode")

        progress_handler$inc_progress(0.1, detail = "Cleaning location data")
        locations <- clean_column_data(plot_data, "stateprovince")

        progress_handler$inc_progress(0.1, detail = "Creating taxa lists...")
        taxa_html <- create_taxa_vectors(plot_data, taxa_data)

        progress_handler$inc_progress(0.1, detail = "Creating community links...")
        community_html <- create_community_vectors(plot_data, comm_data)

        progress_handler$inc_progress(0.1, detail = "Creating action buttons...")
        action_buttons <- create_action_buttons(plot_data)

        progress_handler$inc_progress(0.2, detail = "Building table...")
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

  # Make the functions accessible for testing with environment() approach
  result <- list(
    create_empty_table = create_empty_table,
    is_missing_data = is_missing_data,
    clean_column_data = clean_column_data,
    create_action_buttons = create_action_buttons,
    create_taxa_vectors = create_taxa_vectors,
    create_community_vectors = create_community_vectors,
    build_display_data = build_display_data,
    process_table_data = process_table_data
  )

  return(result)
})()
