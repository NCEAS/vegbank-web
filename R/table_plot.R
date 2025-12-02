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

#' Build Plot Table
#'
#' Configures the plot table to fetch rows via AJAX with nested taxon and classification data.
#'
#' @return A DT::datatable object
#' @noRd
build_plot_table <- function() {
  plot_table_config <- create_plot_table_config()

  create_table(
    data_sources = list(),
    required_sources = character(0),
    process_function = NULL,
    table_config = plot_table_config
  )
}

#' Plot table configuration (columns, AJAX, and options)
#' @noRd
create_plot_table_config <- function() {
  column_defs <- list(
    list(
      targets = 0,
      orderable = FALSE,
      searchable = FALSE,
      width = "10%",
      render = create_plot_action_renderer()
    ),
    list(targets = 1, width = "15%"),  # Author Plot Code
    list(targets = 2, width = "12%"),  # Location
    list(targets = 3, width = "8%"),   # Latitude
    list(targets = 4, width = "8%"),   # Longitude
    list(targets = 5, width = "8%"),   # Elevation
    list(targets = 6, width = "8%"),   # Year
    list(
      targets = 7,
      width = "15%",
      orderable = FALSE,
      render = create_taxon_list_renderer()
    ),
    list(
      targets = 8,
      width = "16%",
      orderable = FALSE,
      render = create_community_list_renderer()
    )
  )

  empty_source <- create_empty_plot_df()
  initial_display <- process_plot_data(empty_source)

  data_source_spec <- build_data_source_spec(
    table_id = "plot_table",
    endpoint = "plot-observations",
    coerce_fn = coerce_plot_page,
    normalize_fn = normalize_plot_data,
    display_fn = process_plot_data,
    label = "plot observations",
    schema_fields = PLOT_TABLE_FIELDS,
    detail = "minimal",
    clean_names = FALSE,
    clean_rows_fn = sanitize_dt_rows,
    query = list(with_nested = "TRUE")
  )

  build_remote_table_config(
    column_defs = column_defs,
    initial_data = initial_display,
    data_source_spec = data_source_spec,
    remote_label = "plot observations"
  )
}

#' Transform normalized plot data into display rows
#'
#' @param plot_data Data frame with normalized plot columns including nested lists
#' @return Data frame formatted for DataTables consumption
#' @noRd
process_plot_data <- function(plot_data) {
  if (is.null(plot_data)) {
    plot_data <- create_empty_plot_df()
  }

  row_count <- nrow(plot_data)
  if (!row_count) {
    return(data.frame(
      "Actions" = character(0),
      "Author Plot Code" = character(0),
      "Location" = character(0),
      "Latitude" = numeric(0),
      "Longitude" = numeric(0),
      "Elevation (m)" = character(0),
      "Year" = character(0),
      "Top Taxa" = character(0),
      "Communities" = character(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }

  # Extract action codes
  action_codes <- plot_data$ob_code
  action_codes <- if (is.null(action_codes)) rep("", row_count) else as.character(action_codes)
  action_codes[is.na(action_codes)] <- ""

  # Preserve author observation codes for map payloads
  author_obs_codes <- plot_data$author_obs_code
  if (is.null(author_obs_codes)) {
    author_obs_codes <- rep(NA_character_, row_count)
  } else {
    author_obs_codes <- as.character(author_obs_codes)
  }
  map_codes <- ifelse(!is.na(author_obs_codes) & nzchar(author_obs_codes), author_obs_codes, action_codes)

  # Clean text columns
  author_codes <- clean_column_data(plot_data, "author_plot_code")
  locations <- format_location_column(plot_data)
  
  # Format numeric columns
  latitudes <- format_coordinate(plot_data$latitude)
  longitudes <- format_coordinate(plot_data$longitude)
  elevations <- format_elevation(plot_data$elevation)
  years <- clean_column_data(plot_data, "year")

  # Serialize nested list columns as JSON strings for the renderer
  top_taxa_json <- serialize_nested_column(plot_data$top_taxon_observations)
  communities_json <- serialize_nested_column(plot_data$top_classifications)

  # Encode both button actions (details + map) as a JSON payload for the renderer
  action_payloads <- vapply(seq_len(row_count), function(idx) {
    payload <- list(
      detail_code = action_codes[[idx]],
      map = list(
        lat = if (is.na(latitudes[[idx]])) NULL else latitudes[[idx]],
        lng = if (is.na(longitudes[[idx]])) NULL else longitudes[[idx]],
        code = if (!is.na(map_codes[[idx]]) && nzchar(map_codes[[idx]])) map_codes[[idx]] else NULL
      )
    )

    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
  }, character(1))

  data.frame(
    "Actions" = action_payloads,
    "Author Plot Code" = author_codes,
    "Location" = locations,
    "Latitude" = latitudes,
    "Longitude" = longitudes,
    "Elevation (m)" = elevations,
    "Year" = years,
    "Top Taxa" = top_taxa_json,
    "Communities" = communities_json,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Format location as "State/Province, Country"
#' @noRd
format_location_column <- function(data) {
  state <- clean_column_data(data, "state_province", default_value = "")
  country <- clean_column_data(data, "country", default_value = "")
  
  location <- ifelse(
    nzchar(state) & nzchar(country),
    paste(state, country, sep = ", "),
    ifelse(nzchar(state), state, 
           ifelse(nzchar(country), country, "Not provided"))
  )
  
  location
}

#' Format coordinate values
#' @noRd
format_coordinate <- function(values) {
  if (is.null(values)) {
    return(rep(NA_real_, length(values)))
  }
  
  coords <- suppressWarnings(as.numeric(values))
  coords
}

#' Format elevation values
#' @noRd
format_elevation <- function(values) {
  if (is.null(values)) {
    return(rep("Not provided", length(values)))
  }
  
  elev <- suppressWarnings(as.numeric(values))
  ifelse(is.na(elev), "Not provided", as.character(round(elev)))
}

#' Serialize nested data frames to JSON strings for JS renderers
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
      # Item might already be a string or other type
      if (is.character(item) && length(item) == 1) {
        return(item)
      }
      return("[]")
    }
    if (nrow(item) == 0) {
      return("[]")
    }
    # Serialize the data frame to JSON
    jsonlite::toJSON(item, auto_unbox = FALSE, dataframe = "rows", null = "null")
  }, character(1), USE.NAMES = FALSE)
}

#' Normalize plot API responses into a consistent schema
#'
#' @param df Raw data frame or list from vegbankr
#' @return Normalized data frame containing PLOT_TABLE_FIELDS
#' @noRd
normalize_plot_data <- function(df) {
  if (is.null(df)) {
    return(create_empty_plot_df())
  }

  if (!is.data.frame(df)) {
    df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) create_empty_plot_df())
  }

  # Ensure all expected fields exist
  missing_fields <- setdiff(PLOT_TABLE_FIELDS, names(df))
  for (field in missing_fields) {
    if (field %in% c("latitude", "longitude", "elevation")) {
      df[[field]] <- rep(NA_real_, nrow(df))
    } else if (field %in% c("taxon_count", "taxon_count_returned")) {
      df[[field]] <- rep(NA_integer_, nrow(df))
    } else if (field %in% c("top_taxon_observations", "top_classifications")) {
      df[[field]] <- vector("list", nrow(df))
    } else {
      df[[field]] <- rep(NA_character_, nrow(df))
    }
  }

  df <- df[, PLOT_TABLE_FIELDS, drop = FALSE]

  # Type coercion
  char_fields <- c("ob_code", "pl_code", "author_plot_code", "author_obs_code", 
                   "state_province", "country", "year")
  for (field in char_fields) {
    if (field %in% names(df)) {
      df[[field]] <- as.character(df[[field]])
    }
  }

  numeric_fields <- c("latitude", "longitude", "elevation")
  for (field in numeric_fields) {
    if (field %in% names(df)) {
      df[[field]] <- suppressWarnings(as.numeric(df[[field]]))
    }
  }

  int_fields <- c("taxon_count", "taxon_count_returned")
  for (field in int_fields) {
    if (field %in% names(df)) {
      df[[field]] <- suppressWarnings(as.integer(df[[field]]))
      df[[field]][is.na(df[[field]])] <- 0L
    }
  }

  rownames(df) <- NULL
  df
}

#' Coerce VegBank plot response to a data frame
#' @noRd
coerce_plot_page <- function(parsed) {
  if (is.null(parsed)) {
    return(create_empty_plot_df())
  }
  if (is.data.frame(parsed)) {
    return(parsed)
  }
  
  # Handle list of records (common vegbankr return format)
  if (is.list(parsed)) {
    if (!is.null(parsed$data)) {
      return(coerce_plot_page(parsed$data))
    }
    if (length(parsed) == 1 && !is.null(names(parsed))) {
      return(coerce_plot_page(parsed[[1]]))
    }
    
    # Try to convert list of records to data frame
    # This handles the case where vegbankr returns a list where each element is a record
    tryCatch({
      # Use bind_rows to handle list columns properly
      df <- dplyr::bind_rows(parsed)
      return(df)
    }, error = function(e) {
      # Fall back to as.data.frame
      tryCatch(
        as.data.frame(parsed, stringsAsFactors = FALSE),
        error = function(e2) create_empty_plot_df()
      )
    })
  }

  tryCatch(
    as.data.frame(parsed, stringsAsFactors = FALSE),
    error = function(e) create_empty_plot_df()
  )
}

#' Create an empty plot data frame following the canonical schema
#' @noRd
create_empty_plot_df <- function() {
  df <- build_zero_row_df(PLOT_TABLE_FIELDS)
  
  # Ensure list columns for nested data
  df$top_taxon_observations <- list()
  df$top_classifications <- list()
  
  df
}

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
            .replace(/>/g, '&gt;');
        };

        var detailButton;
        if (detailCode) {
          detailButton = '<button type=\"button\" class=\"btn btn-sm btn-outline-primary dt-shiny-action\" ' +
            'data-input-id=\"see_obs_details\" data-value=\"' + escapeAttr(detailCode) + '\">Details</button>';
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
      if (!data || data === '[]' || data === '') {
        return '<span class=\"text-muted\">None</span>';
      }
      
      try {
        var taxa = JSON.parse(data);
        if (!Array.isArray(taxa) || taxa.length === 0) {
          return '<span class=\"text-muted\">None</span>';
        }
        
        var links = taxa.map(function(taxon) {
          var name = taxon.name || 'Unknown';
          var pcCode = taxon.pc_code || '';
          var maxCover = taxon.max_cover;
          
          var coverText = '';
          if (maxCover !== null && maxCover !== undefined) {
            coverText = ' (' + Number(maxCover).toFixed(1) + '%)';
          }
          
          if (pcCode) {
            return '<a href=\"#\" class=\"dt-shiny-action\" data-input-id=\"plant_link_click\" data-value=\"' + 
                   pcCode.replace(/\"/g, '&quot;') + '\">' + name + '</a>' + coverText;
          }
          return name + coverText;
        });
        
        return links.join('<br>');
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
        return '<span class=\"text-muted\">None</span>';
      }
      
      try {
        var communities = JSON.parse(data);
        if (!Array.isArray(communities) || communities.length === 0) {
          return '<span class=\"text-muted\">None</span>';
        }
        
        var links = communities.map(function(comm) {
          var commName = comm.comm_name || 'Unknown';
          var clCode = comm.cl_code || '';
          
          if (clCode) {
            return '<a href=\"#\" class=\"dt-shiny-action\" data-input-id=\"comm_class_link_click\" data-value=\"' + 
                   clCode.replace(/\"/g, '&quot;') + '\">' + commName + '</a>';
          }
          return commName;
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
