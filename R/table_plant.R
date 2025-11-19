#' Plant Table Functions Module
#'
#' Uses the generalized concept table module functions.

#' Main function to process and build the plant table
#'
#' @param plant_data Optional data frame of plant concept data
#' @returns A DT datatable object ready for display in a Shiny app
#' @noRd
build_plant_table <- function(plant_data = NULL) {
  build_concept_table(plant_data, concept_type = "plant")
}