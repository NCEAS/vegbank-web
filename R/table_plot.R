#' Plot Table Module
#'
#' Provides a remote (server-side) DataTable for VegBank plot observations.
#' @noRd

PLOT_TABLE_FIELDS <- c(
  "ob_code",
  "author_obs_code",
  "state_province",
  "country",
  "latitude",
  "longitude",
  "elevation",
  "area",
  "year",
  "taxon_count",
  "taxon_count_returned",
  "top_taxon_observations",
  "top_classifications"
)

PLOT_TABLE_SCHEMA_TEMPLATE <- build_schema_template(
  column_names = PLOT_TABLE_FIELDS,
  numeric_columns = c("latitude", "longitude", "elevation", "area"),
  integer_columns = c("taxon_count", "taxon_count_returned"),
  list_columns = c("top_taxon_observations", "top_classifications")
)

PLOT_TABLE_DISPLAY_TEMPLATE <- build_display_template(c(
  "Actions",
  "Vegbank Code",
  "Author Code",
  "Location",
  "Top Taxa",
  "Communities",
  "Survey Year"
))

#' Create column definitions for plot table
#'
#' Defines column widths, ordering, and custom renderers for the plot DataTable.
#'
#' @return List of DataTables columnDefs configuration objects
#' @noRd
create_plot_column_defs <- function() {
  list(
    list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"), # Actions
    list(targets = 1, width = "10%", orderable = TRUE), # Vegbank Code
    list(targets = 2, width = "10%", orderable = TRUE), # Author Code
    list(targets = 3, width = "15%", orderable = FALSE), # Location
    list(targets = 4, width = "25%", orderable = FALSE), # Top Taxa
    list(targets = 5, width = "20%", orderable = FALSE), # Communities
    list(targets = 6, width = "10%", orderable = FALSE) # Year
  )
}

#' Build Plot Table
#'
#' Configures the plot table to fetch rows via AJAX with nested taxon and classification data.
#'
#' @return A DT::datatable object
#' @noRd
build_plot_table <- function() {
  build_table_from_spec(PLOT_TABLE_SPEC)
}

#' Build Plot Table with optional cross-resource filter
#'
#' Configures the plot table with an optional vb_code parameter for
#' cross-resource queries (e.g., showing plots for a specific project).
#' For citation filters of single entities, directly fetches and displays the single cited entity.
#' For collection citations (datasets), uses AJAX table with vb_code query parameter.
#'
#' @param vb_code Optional VegBank code for cross-resource filtering (e.g., "pj.340", "ob.2948")
#' @param filter_type Optional filter type ("plant concept", "community concept", "project", 
#'                    "party", "single-entity-citation", "collection-citation")
#' @return A DT::datatable object
#' @noRd
build_plot_table_with_filter <- function(vb_code = NULL, filter_type = NULL) {
  # Deep copy to avoid mutating the shared PLOT_TABLE_SPEC
  spec <- PLOT_TABLE_SPEC
  spec$data_source <- utils::modifyList(spec$data_source, list())
  spec$data_source$query <- as.list(spec$data_source$query)

  # Determine if filter is active
  has_filter <- !is.null(vb_code) && !is.na(vb_code) && nzchar(vb_code)

  # For single-entity-citation filters, fetch the specific plot observation directly
  # Collection citations (datasets) use AJAX table with vb_code for proper pagination
  if (has_filter && !is.null(filter_type) && filter_type == "single-entity-citation") {
    tryCatch(
      {
        # Fetch the single plot observation using the same parameters as the AJAX table
        # Use detail="minimal" with nested data to match the table's expected structure
        # Limit to top 5 taxa and communities like the regular table
        plot_data <- vegbankr::vb_get_plot_observations(
          vb_code,
          detail = "minimal",
          with_nested = TRUE,
          num_taxa = 5L,
          num_comms = 5L
        )

        if (!is.null(plot_data) && nrow(plot_data) > 0) {
          # Coerce and normalize the data through the same pipeline as AJAX
          coerced <- coerce_plot_page(plot_data)
          normalized <- normalize_plot_data(coerced)
          display_data <- process_plot_data(normalized)

          # Build static table (no AJAX) with the single row
          # Create a minimal table config without ajax to display static data
          static_config <- list(
            initial_data = display_data,
            column_defs = spec$column_defs,
            page_length = 1L,
            options = utils::modifyList(
              spec$options %||% list(),
              list(
                scrollY = "calc(100vh - 300px)",  # Accommodate citation alert notification
                serverSide = FALSE,  # Disable server-side processing
                searching = FALSE,    # No need to search a single row
                paging = FALSE        # No pagination for single row
              )
            ),
            datatable_args = spec$datatable_args,
            escape = FALSE  # Allow HTML in cells (already escaped by process functions)
          )

          return(create_table(static_config))
        } else {
          shiny::showNotification(
            paste("Could not load cited plot observation:", vb_code),
            type = "warning"
          )
        }
      },
      error = function(e) {
        shiny::showNotification(
          paste("Error loading cited plot:", conditionMessage(e)),
          type = "error"
        )
      }
    )
    # Fall through to normal AJAX table if fetch fails
  }

  # Add vb_code to query if filtering is active
  # This includes: cross-resource filters, collection citations, and fallback from failed single-entity citation fetch
  if (has_filter && (is.null(filter_type) || filter_type != "single-entity-citation")) {
    spec$data_source$query$vb_code <- vb_code
  }

  spec$options <- utils::modifyList(spec$options, list())

  # Accomodate filter alert, download button, and search bar so we don't overflow viewport
  spec$options$scrollY <- if (has_filter) "calc(100vh - 300px)" else "calc(100vh - 235px)"

  build_table_from_spec(spec)
}

process_plot_data <- function(plot_data) {
  if (is.null(plot_data)) {
    plot_data <- PLOT_TABLE_SCHEMA_TEMPLATE
  }

  row_count <- nrow(plot_data)
  if (!row_count) {
    return(PLOT_TABLE_DISPLAY_TEMPLATE)
  }

  ob_codes <- plot_data$ob_code
  author_codes <- clean_column_data(plot_data, "author_obs_code")
  years <- clean_column_data(plot_data, "year")

  # Format numeric columns
  latitudes <- suppressWarnings(as.numeric(plot_data$latitude))
  longitudes <- suppressWarnings(as.numeric(plot_data$longitude))
  elevations <- suppressWarnings(as.numeric(plot_data$elevation))

  locations <- format_location_column(plot_data, latitudes, longitudes, elevations)
  actions_html <- format_plot_action_buttons(ob_codes, author_codes, latitudes, longitudes)
  top_taxa_html <- format_plot_taxa_list(plot_data$top_taxon_observations, plot_data$taxon_count)
  communities_html <- format_plot_community_list(plot_data$top_classifications)

  data.frame(
    "Actions" = actions_html,
    "Vegbank Code" = vapply(ob_codes, htmltools::htmlEscape, character(1)),
    "Author Code" = vapply(author_codes, htmltools::htmlEscape, character(1)),
    "Location" = locations,
    "Top Taxa" = top_taxa_html,
    "Communities" = communities_html,
    "Survey Year" = vapply(years, htmltools::htmlEscape, character(1)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
#' Format HTML for plot table action buttons (Details + Map)
#'
#' @param ob_codes Character vector of plot observation codes
#' @param author_codes Character vector of author observation codes
#' @param latitudes Numeric vector of latitudes
#' @param longitudes Numeric vector of longitudes
#' @return Character vector of HTML for action buttons
#' @noRd
format_plot_action_buttons <- function(ob_codes, author_codes, latitudes, longitudes) {
  row_count <- length(ob_codes)
  vapply(seq_len(row_count), function(idx) {
    detail_code <- ob_codes[[idx]]
    lat <- latitudes[[idx]]
    lng <- longitudes[[idx]]
    code <- author_codes[[idx]]
    has_coords <- !is.na(lat) && !is.na(lng)
    # Details button
    detail_btn <- if (!is.null(detail_code) && nzchar(detail_code)) {
      htmltools::tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-primary dt-icon-btn dt-shiny-action",
        `data-input-id` = "plot_link_click",
        `data-value` = htmltools::htmlEscape(detail_code, attribute = TRUE),
        htmltools::HTML(paste0(.BTN_ICON_EYE, "Details"))
      )
    } else {
      htmltools::tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-primary dt-icon-btn",
        disabled = NA,
        htmltools::HTML(paste0(.BTN_ICON_EYE, "Details"))
      )
    }
    # Map/pin button
    map_btn <- if (has_coords) {
      map_attrs <- list(
        type = "button",
        class = "btn btn-sm btn-outline-primary dt-icon-btn dt-map-action",
        `data-lat` = htmltools::htmlEscape(lat, attribute = TRUE),
        `data-lng` = htmltools::htmlEscape(lng, attribute = TRUE),
        htmltools::HTML(paste0(.BTN_ICON_PIN, "Map"))
      )
      if (!is.null(code) && nzchar(code)) {
        map_attrs[["data-code"]] <- htmltools::htmlEscape(code, attribute = TRUE)
      }
      do.call(htmltools::tags$button, map_attrs)
    } else {
      htmltools::tags$button(
        type = "button",
        class = "btn btn-sm btn-outline-primary dt-icon-btn",
        disabled = NA,
        htmltools::HTML(paste0(.BTN_ICON_PIN, "Map"))
      )
    }
    as.character(htmltools::tags$div(
      class = "btn-group btn-group-sm",
      role = "group",
      detail_btn,
      map_btn
    ))
  }, character(1))
}

#' Format HTML for plot table top taxa list
#'
#' @param taxa_list List of data.frames for top_taxon_observations
#' @param taxon_counts Integer vector of total taxon counts
#' @return Character vector of HTML for taxa list
#' @noRd
format_plot_taxa_list <- function(taxa_list, taxon_counts) {
  row_count <- length(taxa_list)
  vapply(seq_len(row_count), function(idx) {
    taxa <- taxa_list[[idx]]
    total <- suppressWarnings(as.integer(taxon_counts[[idx]]))
    if (is.null(taxa) || !is.data.frame(taxa) || nrow(taxa) == 0) {
      return('<span class="text-muted">Not provided</span>')
    }
    links <- vapply(seq_len(nrow(taxa)), function(j) {
      name <- taxa[j, "name", drop = TRUE]
      pc_code <- taxa[j, "pc_code", drop = TRUE]
      max_cover <- taxa[j, "max_cover", drop = TRUE]
      safe_name <- htmltools::htmlEscape(name)
      name_link <- if (!is.null(pc_code) && nzchar(pc_code)) {
        sprintf('<a href="#" class="dt-shiny-action" data-input-id="plant_link_click" data-value="%s">%s</a>', htmltools::htmlEscape(pc_code, attribute = TRUE), safe_name)
      } else {
        sprintf("<span>%s</span>", safe_name)
      }
      if (!is.null(max_cover) && !is.na(max_cover)) {
        cover_text <- sprintf("(%0.1f%%)", as.numeric(max_cover))
        sprintf('<div style="display: flex; justify-content: space-between;"><span>%s</span><span style="margin-left: 8px;">%s</span></div>', name_link, htmltools::htmlEscape(cover_text, attribute = TRUE))
      } else {
        sprintf("<div>%s</div>", name_link)
      }
    }, character(1))
    html <- paste(links, collapse = "")
    if (!is.null(total) && !is.na(total) && total > length(links)) {
      remainder <- total - length(links)
      html <- paste0(html, sprintf('<div class="text-muted small">+%d other taxa</div>', remainder))
    }
    html
  }, character(1))
}

#' Format HTML for plot table community list
#'
#' @param comm_list List of data.frames for top_classifications
#' @return Character vector of HTML for community list
#' @noRd
format_plot_community_list <- function(comm_list) {
  row_count <- length(comm_list)
  vapply(seq_len(row_count), function(idx) {
    comms <- comm_list[[idx]]
    if (is.null(comms) || !is.data.frame(comms) || nrow(comms) == 0) {
      return('<span class="text-muted">Not provided</span>')
    }
    links <- vapply(seq_len(nrow(comms)), function(j) {
      comm_name <- comms[j, "comm_name", drop = TRUE]
      cl_code <- comms[j, "cl_code", drop = TRUE]
      comm_code <- comms[j, "comm_code", drop = TRUE]
      safe_name <- htmltools::htmlEscape(comm_name)
      entry <- if (!is.null(cl_code) && nzchar(cl_code)) {
        sprintf('<a href="#" class="dt-shiny-action" data-input-id="comm_class_link_click" data-value="%s">%s</a>', htmltools::htmlEscape(cl_code, attribute = TRUE), safe_name)
      } else {
        sprintf("<span>%s</span>", safe_name)
      }
      if (!is.null(comm_code) && nzchar(comm_code) && grepl("^CEGL", comm_code, ignore.case = TRUE)) {
        entry <- paste0(entry, sprintf('<br><span class="text-muted small">%s</span>', htmltools::htmlEscape(comm_code, attribute = TRUE)))
      }
      entry
    }, character(1))
    paste(links, collapse = "<br>")
  }, character(1))
}


#' Build HTML-friendly location string from components
#'
#' Creates multi-line HTML location strings with state, country, coordinates, and elevation.
#' Country and coordinate/elevation details are styled with muted text for visual hierarchy.
#'
#' @param data Data frame containing location columns (state_province, country)
#' @param latitudes Numeric latitude vector
#' @param longitudes Numeric longitude vector
#' @param elevations Numeric elevation vector (meters)
#' @return Character vector of HTML-formatted location strings
#' @noRd
format_location_column <- function(data, latitudes = NULL, longitudes = NULL, elevations = NULL) {
  row_total <- nrow(data)
  if (!row_total) {
    return(character(0))
  }

  state <- clean_column_data(data, "state_province", default_value = "")
  country <- clean_column_data(data, "country", default_value = "")

  vapply(seq_len(row_total), function(idx) {
    lines <- character(0)

    # Add state if provided
    if (nzchar(state[[idx]])) {
      lines <- c(lines, htmltools::htmlEscape(state[[idx]]))
    }

    # Add country in muted text if provided
    if (nzchar(country[[idx]])) {
      lines <- c(lines, sprintf(
        '<span class="text-muted small">%s</span>',
        htmltools::htmlEscape(country[[idx]])
      ))
    }

    # Default if no location provided
    if (!length(lines)) {
      lines <- "Not provided"
    }

    # Add coordinates in muted text if available
    lat <- latitudes[[idx]]
    lng <- longitudes[[idx]]
    if (!is.na(lat) && !is.na(lng)) {
      lines <- c(lines, sprintf(
        '<span class="text-muted small">%.4f, %.4f</span>',
        lat, lng
      ))
    }

    # Add elevation in muted text if available
    elev <- elevations[[idx]]
    if (!is.na(elev)) {
      lines <- c(lines, sprintf(
        '<span class="text-muted small">Elevation: %dm</span>',
        round(elev)
      ))
    }

    paste(lines, collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
}

#' Serialize nested list/data-frame columns into JSON strings
#'
#' @param list_col Column containing nested data frames/lists
#' @returns Character vector of JSON strings expected by JS renderers
#'
#' @importFrom jsonlite toJSON
#' @noRd
serialize_nested_column <- function(list_col) {
  if (is.null(list_col)) {
    return(character(0))
  }

  if (!is.list(list_col)) {
    warning("serialize_nested_column: expected a list, got ", class(list_col))
    return(rep("[]", length(list_col)))
  }

  vapply(list_col, function(item) {
    if (is.null(item)) {
      return("[]")
    }
    if (!is.data.frame(item)) {
      if (is.character(item) && length(item) == 1) {
        return(item)
      }
      return("[]")
    }
    if (nrow(item) == 0) {
      return("[]")
    }
    jsonlite::toJSON(item, auto_unbox = FALSE, dataframe = "rows", null = "null")
  }, character(1), USE.NAMES = FALSE)
}

#' Serialize top taxa rows with total counts for JS renderers
#'
#' @param items Data frame (or coercible object) of top taxa rows
#' @param total_count Total taxon count reported by the API
#' @returns JSON string containing `items` array and `total` metadata
#'
#' @importFrom jsonlite toJSON
#' @noRd
serialize_taxa_payload <- function(items, total_count) {
  normalized_items <- normalize_taxa_items(items)
  total_value <- suppressWarnings(as.integer(total_count))
  if (length(total_value) == 0 || is.na(total_value)) {
    total_value <- NULL
  }

  jsonlite::toJSON(
    list(
      total = total_value,
      items = normalized_items
    ),
    auto_unbox = TRUE,
    dataframe = "rows",
    null = "null"
  )
}

#' Normalize taxa items to data frame format
#'
#' Converts various input formats (data frame, JSON string, list) into a consistent
#' data frame structure for taxa serialization.
#'
#' @param items Taxa data as data frame, JSON string, or coercible object
#' @return Data frame of taxa items, or empty data frame if input is invalid
#'
#' @importFrom jsonlite toJSON
#' @noRd
normalize_taxa_items <- function(items) {
  if (is.null(items)) {
    return(data.frame())
  }
  if (is.data.frame(items)) {
    return(items)
  }

  if (is.character(items) && length(items) == 1) {
    parsed <- tryCatch(jsonlite::fromJSON(items), error = function(e) NULL)
    if (is.data.frame(parsed)) {
      return(parsed)
    }
  }

  tryCatch(
    as.data.frame(items, stringsAsFactors = FALSE),
    error = function(e) data.frame()
  )
}

#' Normalize plot API responses into a consistent schema
#'
#' @param df Raw data frame or list from vegbankr
#' @return Normalized data frame containing PLOT_TABLE_FIELDS
#' @noRd
normalize_plot_data <- create_normalizer(
  PLOT_TABLE_SCHEMA_TEMPLATE,
  na_to_zero_fields = c("taxon_count", "taxon_count_returned")
)

#' Coerce VegBank plot response to a data frame
#' @noRd
coerce_plot_page <- create_coercer(PLOT_TABLE_SCHEMA_TEMPLATE)
PLOT_TABLE_SPEC <- list(
  table_id = "plot_table",
  resource = "plot-observations",
  loading_label = "plot observations",
  column_defs = create_plot_column_defs(),
  schema_fields = PLOT_TABLE_FIELDS,
  schema_template = PLOT_TABLE_SCHEMA_TEMPLATE,
  coerce_fn = coerce_plot_page,
  normalize_fn = normalize_plot_data,
  display_fn = process_plot_data,
  data_source = list(
    detail = "minimal",
    clean_names = FALSE,
    clean_rows_fn = sanitize_dt_rows,
    query = list(with_nested = "TRUE"),
    sort_field_map = list(
      "1" = "default", # Vegbank Code
      "2" = "author_obs_code"
    )
  ),
  page_length = NULL,
  options = list(
    scrollY = "calc(100vh - 235px)",
    dom = "Bfrtip",
    buttons = I(list(
      list(
        extend = "csv",
        text = paste0(.BTN_ICON_DOWNLOAD, "Download CSV (up to ", format(DOWNLOAD_MAX_RECORDS, big.mark = ","), " entries)"),
        className = "btn btn-sm btn-outline-info",
        action = DT::JS("function(e, dt, node, config) { Shiny.setInputValue('plot_download_trigger', Math.random()); }")
      )
    )),
    initComplete = DT::JS(
      "function(settings, json) {
        // Signal that table is ready so server can set button state
        Shiny.setInputValue('plot_table_ready', Math.random());
      }"
    )
  ),
  datatable_args = list(
    extensions = "Buttons"
  ),
  initial_display = PLOT_TABLE_DISPLAY_TEMPLATE
)
