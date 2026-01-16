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
#'
#' @param vb_code Optional VegBank code for cross-resource filtering (e.g., "pj.340")
#' @return A DT::datatable object
#' @noRd
build_plot_table_with_filter <- function(vb_code = NULL) {
  # Deep copy to avoid mutating the shared PLOT_TABLE_SPEC
  spec <- PLOT_TABLE_SPEC
  spec$data_source <- utils::modifyList(spec$data_source, list())
  spec$data_source$query <- as.list(spec$data_source$query)

  # Add vb_code to query if filtering is active
  if (!is.null(vb_code) && !is.na(vb_code) && nzchar(vb_code)) {
    spec$data_source$query$vb_code <- vb_code
  }

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
      sprintf(
        '<button type="button" class="btn btn-sm btn-outline-primary dt-shiny-action" data-input-id="plot_link_click" data-value="%s">Details</button>',
        htmltools::htmlEscape(detail_code, attribute = TRUE))
    } else {
      '<button type="button" class="btn btn-sm btn-outline-primary" disabled>Details</button>'
    }
    # Map button
    map_btn <- if (has_coords) {
      code_attr <- if (!is.null(code) && nzchar(code)) sprintf(' data-code="%s"', htmltools::htmlEscape(code, attribute = TRUE)) else ''
      sprintf(
        '<button type="button" class="btn btn-sm btn-outline-primary dt-map-action" data-lat="%s" data-lng="%s"%s>Map</button>',
        htmltools::htmlEscape(lat, attribute = TRUE),
        htmltools::htmlEscape(lng, attribute = TRUE),
        code_attr
      )
    } else {
      '<button type="button" class="btn btn-sm btn-outline-primary" disabled>Map</button>'
    }
    sprintf('<div class="btn-group btn-group-sm" role="group">%s%s</div>', detail_btn, map_btn)
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
        sprintf('<span>%s</span>', safe_name)
      }
      if (!is.null(max_cover) && !is.na(max_cover)) {
        cover_text <- sprintf('(%0.1f%%)', as.numeric(max_cover))
        sprintf('<div style="display: flex; justify-content: space-between;"><span>%s</span><span style="margin-left: 8px;">%s</span></div>', name_link, htmltools::htmlEscape(cover_text, attribute = TRUE))
      } else {
        sprintf('<div>%s</div>', name_link)
      }
    }, character(1))
    html <- paste(links, collapse = '')
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
        sprintf('<span>%s</span>', safe_name)
      }
      if (!is.null(comm_code) && nzchar(comm_code) && grepl('^CEGL', comm_code, ignore.case = TRUE)) {
        entry <- paste0(entry, sprintf('<br><span class="text-muted small">%s</span>', htmltools::htmlEscape(comm_code, attribute = TRUE)))
      }
      entry
    }, character(1))
    paste(links, collapse = '<br>')
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
  remote_label = "plot observations",
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
  options = list(),
  datatable_args = list(),
  initial_display = PLOT_TABLE_DISPLAY_TEMPLATE
)
