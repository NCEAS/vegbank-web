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
    communities <- if (!is.null(table_data$commname)) {
      table_data$commname
    } else {
      rep("Unknown", n_rows)
    }

    # Ensure 'actions' matches the row count
    actions <- mapply(build_action_buttons, seq_len(n_rows))

    display_data <- data.frame(
      "Actions" = actions,
      "Author Plot Code" = author_codes,
      "Location" = locations,
      "Top Taxa" = taxa_lists,
      "Community" = communities,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    DT::datatable(display_data,
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
          list(targets = c(4), width = "20%")
        )
      )
    )
  }

  list(
    create_empty_table = create_empty_table,
    process_table_data = process_table_data
  )
})()
