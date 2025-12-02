#' Party Table Module
#'
#' Provides a remote (server-side) DataTable for VegBank parties.
#'
#' @noRd
PARTY_TABLE_FIELDS <- c(
  "py_code",
  "given_name",
  "surname",
  "organization_name",
  "obs_count",
  "contact_instructions"
)

PARTY_TABLE_SCHEMA_TEMPLATE <- build_schema_template(
  column_names = PARTY_TABLE_FIELDS,
  integer_columns = "obs_count"
)

PARTY_TABLE_DISPLAY_TEMPLATE <- build_display_template(
  column_names = c("Actions", "Given Name", "Surname", "Organization", "Observations", "Contact"),
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
    list(targets = 1, width = "15%"),
    list(targets = 2, width = "15%"),
    list(targets = 3, width = "30%"),
    list(targets = 4, width = "10%", className = "dt-right", type = "num"),
    list(targets = 5, width = "30%")
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

  given_names <- clean_column_data(party_data, "given_name")
  surnames <- clean_column_data(party_data, "surname")
  organizations <- clean_column_data(party_data, "organization_name")
  contact_info <- clean_column_data(party_data, "contact_instructions")

  action_codes <- party_data$py_code
  action_codes <- if (is.null(action_codes)) rep("", row_count) else as.character(action_codes)
  action_codes[is.na(action_codes)] <- ""

  obs_counts <- suppressWarnings(as.integer(party_data$obs_count))
  obs_counts[is.na(obs_counts)] <- 0L

  data.frame(
    "Actions" = action_codes,
    "Given Name" = given_names,
    "Surname" = surnames,
    "Organization" = organizations,
    "Observations" = obs_counts,
    "Contact" = contact_info,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' Normalize party API responses into a consistent schema
#'
#' @param df Raw data frame or list from vegbankr
#' @return Normalized data frame containing PARTY_TABLE_FIELDS
#' @noRd
normalize_party_data <- function(df) {
  if (is.null(df)) {
    return(PARTY_TABLE_SCHEMA_TEMPLATE)
  }

  if (!is.data.frame(df)) {
    df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) PARTY_TABLE_SCHEMA_TEMPLATE)
  }

  missing_fields <- setdiff(PARTY_TABLE_FIELDS, names(df))
  for (field in missing_fields) {
    if (field == "obs_count") {
      df[[field]] <- rep(NA_integer_, nrow(df))
    } else {
      df[[field]] <- rep(NA_character_, nrow(df))
    }
  }

  df <- df[, PARTY_TABLE_FIELDS, drop = FALSE]

  char_fields <- setdiff(PARTY_TABLE_FIELDS, "obs_count")
  for (field in char_fields) {
    df[[field]] <- as.character(df[[field]])
  }

  suppressWarnings(df$obs_count <- as.integer(df$obs_count))
  df$obs_count[is.na(df$obs_count)] <- 0L

  rownames(df) <- NULL
  df
}

#' Coerce VegBank party response to a data frame
#' @noRd
coerce_party_page <- function(parsed) {
  if (is.null(parsed)) {
    return(PARTY_TABLE_SCHEMA_TEMPLATE)
  }
  if (is.data.frame(parsed)) {
    return(parsed)
  }
  if (is.list(parsed)) {
    if (!is.null(parsed$data)) {
      return(coerce_party_page(parsed$data))
    }
    if (length(parsed) == 1) {
      return(coerce_party_page(parsed[[1]]))
    }
  }

  tryCatch(
    as.data.frame(parsed, stringsAsFactors = FALSE),
    error = function(e) PARTY_TABLE_SCHEMA_TEMPLATE
  )
}

PARTY_TABLE_SPEC <- list(
  table_id = "party_table",
  endpoint = "parties",
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
