#' Plant Table Functions Module
#'
#' Uses the generalized concept table module functions.

#' Main function to process and build the plant table
#'
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_plant_table <- function() {
  build_concept_table(concept_type = "plant")
}