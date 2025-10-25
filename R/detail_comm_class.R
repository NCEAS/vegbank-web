#' Build Community Classification Details View
#'
#' @param result A data frame returned by vegbankr::get_community_classification()
#' @noRd
build_comm_class_details_view <- function(result) {
  if (is.null(result) || nrow(result) == 0) {
    return(create_empty_detail_view(
      c("observation_details", "community_interpretation"),
      "Community details"
    ))
  }

  list(
    observation_details = render_detail_table(
      c("cl_code", "inspection", "table_analysis", "multivariate_analysis"),
      result
    ),
    community_interpretation = render_detail_table(
      c("cc_code", "class_fit", "class_confidence", "comm_authority_id", "type"),
      result
    )
  )
}
