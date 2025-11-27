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
  "contact_instructions",
  "obs_count"
)

#' Build Party Table
#'
#' Configures the party table to fetch rows via AJAX, mirroring the concept tables.
#'
#' @return A DT::datatable object
#' @noRd
build_party_table <- function() {
  party_table_config <- create_party_table_config()

  create_table(
    data_sources = list(),
    required_sources = character(0),
    process_function = NULL,
    table_config = party_table_config
  )
}

#' Party table configuration (columns, AJAX, and options)
#' @noRd
create_party_table_config <- function() {
  column_defs <- list(
    list(
      targets = 0,
      orderable = FALSE,
      searchable = FALSE,
      width = "10%",
      render = create_action_button_renderer("party_link_click", "Details")
    ), # Actions
    list(targets = 1, width = "20%"), # Given Name
    list(targets = 2, width = "20%"), # Surname
    list(targets = 3, width = "30%"), # Organization
    list(targets = 4, width = "20%"), # Contact
    list(targets = 5, width = "10%", className = "dt-right", type = "num") # Observations
  )

  empty_source <- create_empty_party_df()
  initial_display <- process_party_data(empty_source)

  data_source_spec <- build_data_source_spec(
    table_id = "party_table",
    endpoint = "parties",
    coerce_fn = coerce_party_page,
    normalize_fn = normalize_party_data,
    display_fn = process_party_data,
    label = "party records",
    schema_fields = PARTY_TABLE_FIELDS,
    detail = "full",
    clean_names = FALSE,
    clean_rows_fn = sanitize_dt_rows
  )

  build_remote_table_config(
    column_defs = column_defs,
    initial_data = initial_display,
    data_source_spec = data_source_spec,
    remote_label = "party records"
  )
}

#' Transform normalized party data into display rows
#'
#' @param party_data Data frame with normalized party columns
#' @return Data frame formatted for DataTables consumption
#' @noRd
process_party_data <- function(party_data) {
  if (is.null(party_data)) {
    party_data <- create_empty_party_df()
  }

  row_count <- nrow(party_data)
  if (!row_count) {
    return(data.frame(
      "Actions" = character(0),
      "Given Name" = character(0),
      "Surname" = character(0),
      "Organization" = character(0),
      "Contact" = character(0),
      "Observations" = integer(0),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
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
    "Contact" = contact_info,
    "Observations" = obs_counts,
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
    return(create_empty_party_df())
  }

  if (!is.data.frame(df)) {
    df <- tryCatch(as.data.frame(df, stringsAsFactors = FALSE), error = function(e) create_empty_party_df())
  }

  missing_fields <- setdiff(PARTY_TABLE_FIELDS, names(df))
  for (field in missing_fields) {
    df[[field]] <- NA
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
    return(create_empty_party_df())
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
    error = function(e) create_empty_party_df()
  )
}

#' Create an empty party data frame following the canonical schema
#' @noRd
create_empty_party_df <- function() {
  build_zero_row_df(PARTY_TABLE_FIELDS)
}
