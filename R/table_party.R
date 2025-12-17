#' Party Table Module
#'
#' Provides a remote (server-side) DataTable for VegBank parties.
#'
#' @noRd
PARTY_TABLE_FIELDS <- c(
  "py_code",
  "party_label",
  "organization_name",
  "obs_count",
  "contact_instructions"
)

PARTY_TABLE_SCHEMA_TEMPLATE <- build_schema_template(
  column_names = PARTY_TABLE_FIELDS,
  integer_columns = "obs_count"
)

PARTY_TABLE_DISPLAY_TEMPLATE <- build_display_template(
  column_names = c("Actions", "Party", "Organization", "Observations", "Contact"),
  column_types = list("Observations" = integer())
)

create_party_column_defs <- function() {
  list(
    list(
      targets = 0,
      orderable = FALSE,
      searchable = FALSE,
      width = "10%",
      render = create_action_button_renderer("party_link_click", "Details")
    ),
    list(targets = 1, width = "25%"),
    list(targets = 2, width = "30%"),
    list(targets = 3, width = "10%", className = "dt-right", type = "num"),
    list(targets = 4, width = "25%")
  )
}

#' Build Party Table
#'
#' Configures the party table to fetch rows via AJAX, mirroring the concept tables.
#'
#' @return A DT::datatable object
#' @noRd
build_party_table <- function() {
  build_table_from_spec(PARTY_TABLE_SPEC)
}

#' Transform normalized party data into display rows
#'
#' @param party_data Data frame with normalized party columns
#' @return Data frame formatted for DataTables consumption
#' @noRd
process_party_data <- function(party_data) {
  if (is.null(party_data)) {
    party_data <- PARTY_TABLE_SCHEMA_TEMPLATE
  }

  row_count <- nrow(party_data)
  if (!row_count) {
    return(PARTY_TABLE_DISPLAY_TEMPLATE)
  }

  party_labels <- clean_column_data(party_data, "party_label")
  py_codes <- party_data$py_code
  organizations <- clean_column_data(party_data, "organization_name")
  contact_info <- clean_column_data(party_data, "contact_instructions")

  # Format party column with party_label and py_code in green below
  formatted_parties <- format_party_name_column(party_labels, py_codes)

  action_codes <- party_data$py_code
  action_codes <- if (is.null(action_codes)) rep("", row_count) else as.character(action_codes)
  action_codes[is.na(action_codes)] <- ""

  obs_counts <- suppressWarnings(as.integer(party_data$obs_count))
  obs_counts[is.na(obs_counts)] <- 0L

  data.frame(
    "Actions" = action_codes,
    "Party" = formatted_parties,
    "Organization" = organizations,
    "Observations" = obs_counts,
    "Contact" = contact_info,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Build HTML-friendly party string with py_code
#'
#' Creates multi-line HTML party strings with the party_label on top and
#' the party code (py_code) in green below.
#'
#' @param labels Character vector of party labels
#' @param codes Character vector of party codes (py_code)
#' @return Character vector of HTML-formatted party strings
#' @noRd
format_party_name_column <- function(labels, codes) {
  vapply(seq_along(labels), function(idx) {
    label <- labels[[idx]]
    code <- codes[[idx]]

    lines <- character(0)

    if (!is.null(label) && !is.na(label) && nzchar(label)) {
      lines <- c(lines, as.character(htmltools::htmlEscape(label)))
    } else {
      lines <- c(lines, "Not provided")
    }

    if (!is.null(code) && !is.na(code) && nzchar(code)) {
      code_line <- sprintf(
        '<span style="color: #2c5443; font-size: small;">%s</span>',
        as.character(htmltools::htmlEscape(code))
      )
      lines <- c(lines, code_line)
    }

    paste(lines, collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
}

#' Normalize party API responses into a consistent schema
#'
#' @param df Raw data frame or list from vegbankr
#' @return Normalized data frame containing PARTY_TABLE_FIELDS
#' @noRd
normalize_party_data <- create_normalizer(PARTY_TABLE_SCHEMA_TEMPLATE, na_to_zero_fields = "obs_count")

#' Coerce VegBank party response to a data frame
#' @noRd
coerce_party_page <- create_coercer(PARTY_TABLE_SCHEMA_TEMPLATE)

PARTY_TABLE_SPEC <- list(
  table_id = "party_table",
  resource = "parties",
  remote_label = "party records",
  column_defs = create_party_column_defs(),
  schema_fields = PARTY_TABLE_FIELDS,
  schema_template = PARTY_TABLE_SCHEMA_TEMPLATE,
  coerce_fn = coerce_party_page,
  normalize_fn = normalize_party_data,
  display_fn = process_party_data,
  data_source = list(
    detail = "full",
    clean_names = FALSE,
    clean_rows_fn = sanitize_dt_rows
  ),
  page_length = NULL,
  options = list(),
  datatable_args = list(),
  initial_display = PARTY_TABLE_DISPLAY_TEMPLATE
)
