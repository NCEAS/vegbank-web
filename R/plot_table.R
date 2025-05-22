#' Table Data Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.
#' @keywords internal

#' @importFrom DT datatable
plot_table <- (function() {
  create_empty_table <- function() {
    DT::datatable(
      data.frame("No Data Available" = "Please try again or check your connection"),
      options = list(dom = "t")
    )
  }

  is_missing_data <- function(plot_data, taxa_data) {
    # Return early if no data
    if (is.null(plot_data) || nrow(plot_data) == 0 ||
          is.null(taxa_data) || nrow(taxa_data) == 0) {
      shiny::showNotification(
        "Missing required data. Please try again or check your connection.",
        type = "error"
      )
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

  create_taxa_lists <- function(plot_data, taxa_data) {
    data <- dplyr::left_join(plot_data, taxa_data, by = "observation_id")

    taxa_lists <- data %>%
      dplyr::group_by(observation_id) %>%
      dplyr::summarize(
        top_taxa = paste0(
          '<ul class="taxa-list">',
          paste0(
            "<li>",
            ifelse(is.na(int_currplantscinamenoauth) & is.na(maxcover),
              "No Taxa Data",
              paste0(
                '<a href="#" onclick="Shiny.setInputValue(\'taxa_link_click\', \'',
                accessioncode, '\', {priority: \'event\'}); return false;">',
                ifelse(is.na(int_currplantscinamenoauth), "Unnamed", int_currplantscinamenoauth),
                "</a> (", maxcover, "%)"
              )
            ),
            "</li>",
            collapse = ""
          ),
          "</ul>"
        ),
        .groups = "drop"
      )

    # Join the taxa lists back to the plot data
    dplyr::left_join(plot_data, taxa_lists, by = "observation_id")
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

  create_community_links <- function(data) {
    vapply(seq_len(nrow(data)), function(i) {
      comm_name <- if (!is.null(data$commname) && !is.na(data$commname[i])) {
        data$commname[i]
      } else {
        "Not Provided"
      }
      comm_code <- if (!is.null(data$commconceptaccessioncode) &&
                         !is.na(data$commconceptaccessioncode[i])) {
        data$commconceptaccessioncode[i]
      } else {
        ""
      }
      if (comm_code != "") {
        sprintf(
          '<a href="#" onclick="Shiny.setInputValue(\'comm_link_click\', \'%s\',
            {priority: \'event\'}); return false;">%s</a>',
          comm_code, comm_name
        )
      } else {
        comm_name
      }
    }, character(1))
  }

  build_display_data <- function(data, author_codes, locations, community_links, action_buttons) {
    data.frame(
      "Actions" = action_buttons,
      "Author Plot Code" = author_codes,
      "Location" = locations,
      "Top Taxa" = data$top_taxa,
      "Community" = community_links,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }

  process_table_data <- function(plot_data, taxa_data) {
    withProgress(
      message = "Compiling table data...",
      value = 0,
      {
        if (is_missing_data(plot_data, taxa_data)) {
          return(create_empty_table())
        }

        setProgress(0.2, "Cleaning author observation codes")
        author_codes <- clean_column_data(plot_data, "authorplotcode")

        setProgress(0.3, "Cleaning location data")
        locations <- clean_column_data(plot_data, "stateprovince")

        setProgress(0.4, "Creating taxa lists...")
        data <- create_taxa_lists(plot_data, taxa_data)

        setProgress(0.6, "Creating action buttons...")
        action_buttons <- create_action_buttons(data)

        setProgress(0.7, "Creating community links...")
        community_links <- create_community_links(data)

        setProgress(0.8, "Building table...")
        display_data <- build_display_data(
          data, author_codes, locations,
          community_links, action_buttons
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
      }
    )
  }

  list(
    create_empty_table = create_empty_table,
    process_table_data = process_table_data
  )
})()
