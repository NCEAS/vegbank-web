#' Community Table Functions Module
#'
#' Uses the generalized concept table module functions.

#' Main function to process and build the community table
#'
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_community_table <- function() {
  build_concept_table(concept_type = "community")
}