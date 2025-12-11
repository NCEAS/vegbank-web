#' Plot Table Module
#'
#' Provides a remote (server-side) DataTable for VegBank plot observations.
#' @noRd

PLOT_TABLE_FIELDS <- c(
  "ob_code",
  "pl_code",
  "author_plot_code",
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
  "Author Plot Code",
  "Location",
  "Top Taxa",
  "Communities",
  "Year"
))

#' Create column definitions for plot table
#'
#' Defines column widths, ordering, and custom renderers for the plot DataTable.
#'
#' @return List of DataTables columnDefs configuration objects
#' @noRd
create_plot_column_defs <- function() {
  list(
    list(
      targets = 0,
      orderable = FALSE,
      searchable = FALSE,
      width = "10%",
      render = create_plot_action_renderer()
    ),
    list(targets = 1, width = "10%"), # Author Plot Code
    list(targets = 2, width = "20%"), # Location
    list(
      targets = 3,
      width = "25%",
      orderable = FALSE,
      render = create_taxon_list_renderer()
    ),
    list(
      targets = 4,
      width = "25%",
      orderable = FALSE,
      render = create_community_list_renderer()
    ),
    list(targets = 5, width = "10%") # Year
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

#' Transform normalized plot data into display rows
#'
#' @param plot_data Data frame with normalized plot columns including nested lists
#' @return Data frame formatted for DataTables consumption
#' @noRd
process_plot_data <- function(plot_data) {
  if (is.null(plot_data)) {
    plot_data <- PLOT_TABLE_SCHEMA_TEMPLATE
  }

  row_count <- nrow(plot_data)
  if (!row_count) {
    return(PLOT_TABLE_DISPLAY_TEMPLATE)
  }

  ob_codes <- plot_data$ob_code

  # Clean text columns
  author_codes <- clean_column_data(plot_data, "author_obs_code")
  years <- clean_column_data(plot_data, "year")

  # Format numeric columns
  latitudes <-   suppressWarnings(as.numeric(plot_data$latitude))
  longitudes <- suppressWarnings(as.numeric(plot_data$longitude))
  elevations <- suppressWarnings(as.numeric(plot_data$elevation))

  locations <- format_location_column(plot_data, latitudes, longitudes, elevations)


  # Serialize nested list columns as JSON strings for the renderer
  top_taxa_json <- vapply(seq_len(row_count), function(idx) {
    serialize_taxa_payload(
      plot_data$top_taxon_observations[[idx]],
      plot_data$taxon_count[[idx]]
    )
  }, character(1))
  communities_json <- serialize_nested_column(plot_data$top_classifications)

  # Encode both button actions (details + map) as a JSON payload for the renderer
  action_payloads <- vapply(seq_len(row_count), function(idx) {
    payload <- list(
      detail_code = ob_codes[[idx]],
      map = list(
        lat = if (is.na(latitudes[[idx]])) NULL else latitudes[[idx]],
        lng = if (is.na(longitudes[[idx]])) NULL else longitudes[[idx]],
        code = author_codes[[idx]]
      )
    )

    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  }, character(1))

  data.frame(
    "Actions" = action_payloads,
    "Author Plot Code" = author_codes,
    "Location" = locations,
    "Top Taxa" = top_taxa_json,
    "Communities" = communities_json,
    "Year" = years,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
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

  build_coord_line <- function(lat, lng) {
    if (is.na(lat) || is.na(lng)) {
      return(NULL)
    }
    sprintf("%.4f, %.4f", lat, lng)
  }

  build_elev_line <- function(elev) {
    if (is.na(elev)) {
      return(NULL)
    }
    paste0(round(elev), "m")
  }

  vapply(seq_len(row_total), function(idx) {
    lines <- character(0)

    if (nzchar(state[[idx]])) {
      lines <- c(lines, as.character(htmltools::htmlEscape(state[[idx]])))
    }
    if (nzchar(country[[idx]])) {
      country_line <- sprintf(
        '<span class="text-muted small">%s</span>',
        as.character(htmltools::htmlEscape(country[[idx]]))
      )
      lines <- c(lines, country_line)
    }
    if (!length(lines)) {
      lines <- "Not provided"
    }

    coord_line <- build_coord_line(latitudes[[idx]], longitudes[[idx]])
    elev_line <- build_elev_line(elevations[[idx]])

    detail_parts <- c()
    if (!is.null(coord_line)) {
      detail_parts <- c(detail_parts, coord_line)
    }
    if (!is.null(elev_line)) {
      detail_parts <- c(detail_parts, elev_line)
    }

    if (length(detail_parts)) {
      detail_line <- paste(detail_parts, collapse = " &bull; ")
      detail_line <- sprintf('<span class="text-muted small">%s</span>', detail_line)
      lines <- c(lines, detail_line)
    }

    paste(lines, collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
}

#' Serialize nested list/data-frame columns into JSON strings
#'
#' @param list_col Column containing nested data frames/lists
#' @returns Character vector of JSON strings expected by JS renderers
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

#' Create JS renderer for plot action buttons (Details + Map)
#' @noRd
create_plot_action_renderer <- function() {
  js_code <- "function(data, type, row, meta) {
    if (type === 'display') {
      if (!data || data === '') {
        return '<span class=\"text-muted\">Unavailable</span>';
      }

      var payload;
      try {
        payload = JSON.parse(data);
      } catch (err) {
        console.error('Failed to parse plot action payload:', err);
        return '<span class=\"text-muted\">Unavailable</span>';
      }

        var detailCode = payload && payload.detail_code ? String(payload.detail_code) : '';
        var mapPayload = payload && payload.map ? payload.map : null;
        var hasCoords = Boolean(mapPayload && mapPayload.lat !== null && mapPayload.lat !== undefined &&
          mapPayload.lng !== null && mapPayload.lng !== undefined &&
          !Number.isNaN(mapPayload.lat) && !Number.isNaN(mapPayload.lng));
        var mapCode = mapPayload && mapPayload.code ? String(mapPayload.code) : detailCode;

        var escapeAttr = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/\"/g, '&quot;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/'/g, '&#39;');
        };

        var detailButton;
        if (detailCode) {
          detailButton = '<button type=\"button\" class=\"btn btn-sm btn-outline-primary dt-shiny-action\" ' +
            'data-input-id=\"plot_link_click\" data-value=\"' + escapeAttr(detailCode) + '\">Details</button>';
        } else {
          detailButton = '<button type=\"button\" class=\"btn btn-sm btn-outline-primary\" disabled>Details</button>';
        }

        var mapButton;
        if (hasCoords) {
          var latAttr = escapeAttr(mapPayload.lat);
          var lngAttr = escapeAttr(mapPayload.lng);
          var codeAttr = mapCode ? ' data-code=\"' + escapeAttr(mapCode) + '\"' : '';
          mapButton = '<button type=\"button\" class=\"btn btn-sm btn-outline-secondary dt-map-action\" ' +
            'data-lat=\"' + latAttr + '\" data-lng=\"' + lngAttr + '\"' + codeAttr + '>Map</button>';
        } else {
          mapButton = '<button type=\"button\" class=\"btn btn-sm btn-outline-secondary\" disabled>Map</button>';
        }

        return '<div class=\"btn-group btn-group-sm\" role=\"group\">' + detailButton + mapButton + '</div>';
    }
    return data;
  }"

  DT::JS(js_code)
}

#' Create JS renderer for taxon list with links
#' @noRd
create_taxon_list_renderer <- function() {
  js_code <- "function(data, type, row, meta) {
    if (type === 'display') {
      if (!data || data === '') {
        return '<span class=\"text-muted\">Not provided</span>';
      }

      try {
        var payload = JSON.parse(data);
        var taxa = [];
        var total = null;

        var escapeHtml = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\"/g, '&quot;')
            .replace(/'/g, '&#39;');
        };

        var escapeText = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
        };

        if (Array.isArray(payload)) {
          taxa = payload;
        } else if (payload && typeof payload === 'object') {
          if (Array.isArray(payload.items)) {
            taxa = payload.items;
          }
          if (payload.total !== null && payload.total !== undefined) {
            var parsedTotal = Number(payload.total);
            if (!Number.isNaN(parsedTotal)) {
              total = parsedTotal;
            }
          }
        }

        if (!Array.isArray(taxa) || taxa.length === 0) {
          return '<span class=\"text-muted\">Not provided</span>';
        }

        var links = taxa.map(function(taxon) {
          var name = taxon.name || 'Unknown';
          var pcCode = taxon.pc_code || '';
          var maxCover = taxon.max_cover;
          var safeName = escapeText(name);

          var nameLink;
          if (pcCode) {
            nameLink = '<a href=\"#\" class=\"dt-shiny-action\" data-input-id=\"plant_link_click\" data-value=\"' +
                   escapeHtml(pcCode) + '\">' + safeName + '</a>';
          } else {
            nameLink = '<span>' + safeName + '</span>';
          }

          if (maxCover !== null && maxCover !== undefined) {
            var coverText = '(' + Number(maxCover).toFixed(1) + '%)';
            return '<div style=\"display: flex; justify-content: space-between;\"><span>' +
                   nameLink + '</span><span style=\"margin-left: 8px;\">' + escapeText(coverText) + '</span></div>';
          }

          return '<div>' + nameLink + '</div>';
        });

        var html = links.join('');
        if (links.length && total !== null && !Number.isNaN(total)) {
          var remainder = Math.floor(total) - links.length;
          if (remainder > 0) {
            html += '<div class=\"text-muted small\">+' + remainder + ' other taxa</div>';
          }
        }

        return html;
      } catch(e) {
        console.error('Error parsing taxon data:', e);
        return '<span class=\"text-muted\">Error</span>';
      }
    }
    return data;
  }"

  DT::JS(js_code)
}

#' Create JS renderer for community classification list with links
#' @noRd
create_community_list_renderer <- function() {
  js_code <- "function(data, type, row, meta) {
    if (type === 'display') {
      if (!data || data === '[]' || data === '') {
        return '<span class=\"text-muted\">Not provided</span>';
      }

      try {
        var communities = JSON.parse(data);
        if (!Array.isArray(communities) || communities.length === 0) {
          return '<span class=\"text-muted\">Not provided</span>';
        }

        var escapeHtml = function(value) {
          return String(value)
            .replace(/&/g, '&amp;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;')
            .replace(/\"/g, '&quot;')
            .replace(/'/g, '&#39;');
        };

        var links = communities.map(function(comm) {
          var commName = comm.comm_name || 'Unknown';
          var clCode = comm.cl_code || '';
          var commCode = comm.comm_code || '';
          var entry;
          var safeName = escapeHtml(commName);

          if (clCode) {
            entry = '<a href=\"#\" class=\"dt-shiny-action\" data-input-id=\"comm_class_link_click\" data-value=\"' +
                   escapeHtml(clCode) + '\">' + safeName + '</a>';
          } else {
            entry = '<span>' + safeName + '</span>';
          }

          if (commCode && commCode.toUpperCase().indexOf('CEGL') === 0) {
            entry += '<br><span class=\"text-muted small\">' + escapeHtml(commCode) + '</span>';
          }

          return entry;
        });

        return links.join('<br>');
      } catch(e) {
        console.error('Error parsing community data:', e);
        return '<span class=\"text-muted\">Error</span>';
      }
    }
    return data;
  }"

  DT::JS(js_code)
}

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
    query = list(with_nested = "TRUE")
  ),
  page_length = NULL,
  options = list(),
  datatable_args = list(),
  initial_display = PLOT_TABLE_DISPLAY_TEMPLATE
)