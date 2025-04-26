#' Table Data Functions Module
#'
#' Provides functions for creating and manipulating the plot data table.
#' @keywords internal

#' @importFrom DT datatable
#' @importFrom utils head
plot_table <- (function() {
  create_empty_table <- function() {
    DT::datatable(
      data.frame("No Data Available" = "Please try again or check your connection"),
      options = list(dom = "t")
    )
  }

  process_table_data <- function(table_data) {
    if (is.null(table_data) || nrow(table_data) == 0) {
      return(create_empty_table())
    }

    # Debug: Print column names to check data structure
    message("Table data columns: ", paste(names(table_data), collapse = ", "))

    n_rows <- nrow(table_data)

    taxa_lists <- tryCatch(
      {
        apply(table_data, 1, build_taxa_list)
      },
      error = function(e) {
        message("Error generating taxa lists: ", e$message)
        rep("Error loading taxa", n_rows)
      }
    )

    # Provide fallback vectors if columns are missing
    author_codes <- if (!is.null(table_data$authorobscode)) {
      table_data$authorobscode
    } else {
      rep("Unknown", n_rows)
    }
    locations <- if (!is.null(table_data$stateprovince)) {
      table_data$stateprovince
    } else {
      rep("Unknown", n_rows)
    }

    # Prepare community names and accession codes with more robust column detection
    communities <- rep("Unknown", n_rows)
    comm_codes <- rep(NA, n_rows)

    # Find the correct column for community names
    comm_name_col <- NULL
    possible_comm_cols <- c("commname", "communityname", "community_name")
    for (col in possible_comm_cols) {
      if (col %in% names(table_data)) {
        comm_name_col <- col
        break
      }
    }

    if (!is.null(comm_name_col)) {
      communities <- table_data[[comm_name_col]]
      message("Using community name column: ", comm_name_col)
    }

    # Find the correct column for community accession codes
    comm_code_col <- NULL
    possible_code_cols <- c("commaccessioncode", "community_accession_code", "commconceptaccessioncode")
    for (col in possible_code_cols) {
      if (col %in% names(table_data)) {
        comm_code_col <- col
        break
      }
    }

    if (!is.null(comm_code_col)) {
      comm_codes <- table_data[[comm_code_col]]
      message("Using community accession code column: ", comm_code_col)
      message("Sample accession code: ", paste(head(comm_codes, 1), collapse = ", "))
    } else {
      message("WARNING: No community accession code column found!")
    }

    # Store accession codes in a special attribute to use with JS callback
    community_data <- data.frame(
      name = communities,
      code = comm_codes,
      stringsAsFactors = FALSE
    )

    # Ensure 'actions' matches the row count
    actions <- mapply(build_action_buttons, seq_len(n_rows))

    display_data <- data.frame(
      "Actions" = actions,
      "Author Plot Code" = author_codes,
      "Location" = locations,
      "Top Taxa" = taxa_lists,
      "Community" = communities, # Just use the plain text name here
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    # Add accession codes as a row attribute for later use in JS
    attr(display_data, "comm_accession_codes") <- comm_codes

    # Create datatable with JavaScript callback for the community column
    dt <- DT::datatable(display_data,
      rownames = FALSE,
      escape = FALSE,
      selection = list(mode = "single", target = "row", selectable = FALSE),
      options = list(
        dom = "ft",
        pageLength = 100,
        scrollY = "calc(100vh - 300px)",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        columnDefs = list(
          list(targets = c(0), width = "10%"),
          list(targets = c(1), width = "10%"),
          list(targets = c(2), width = "10%"),
          list(targets = c(3), width = "45%"),
          list(
            targets = c(4), width = "20%",
            # Use a rendering function to create links
            render = DT::JS(
              "function(data, type, row, meta) {
                    if (type === 'display' && data !== 'Unknown') {
                      var accCodes = ",
              jsonlite::toJSON(comm_codes),
              ";
                      var accCode = accCodes[meta.row];
                      if (accCode && accCode !== 'NA') {
                        return '<a href=\"#\" onclick=\"Shiny.setInputValue(\\'comm_link_click\\', \\'' +
                               accCode + '\\', {priority: \\'event\\'}); return false;\">' + data + '</a>';
                      }
                    }
                    return data;
                  }"
            )
          )
        )
      )
    )

    return(dt)
  }

  list(
    create_empty_table = create_empty_table,
    process_table_data = process_table_data
  )
})()
