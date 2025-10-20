#' Community Table Functions Module
#'
#' Uses the generalized concept table module functions.

#' Main function to process and build the community table
#'
#' @param community_data Data frame of community data
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_community_table <- function(community_data) {
  build_concept_table(community_data, concept_type = "community")
}