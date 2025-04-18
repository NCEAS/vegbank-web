#' VegBank API Client Module
#'
#' Provides functions for interacting with the VegBank API.
#' @keywords internal

#' @importFrom httr GET status_code content
#' @importFrom jsonlite validate fromJSON
veg_bank_api <- (function() {
  base_url <- "http://127.0.0.1:28015"

  fetch <- function(path, ...) {
    api_url <- paste0(base_url, path)
    message("Fetching from API: ", api_url)
    start_time <- Sys.time()

    response <- tryCatch(
      {
        httr::GET(api_url, ...)
      },
      error = function(e) {
        message("Error connecting to API: ", e$message)
        NULL
      }
    )

    end_time <- Sys.time()
    message(
      paste("API call took", round(difftime(end_time, start_time, units = "secs"), 2), "seconds")
    )

    response
  }

  process_response <- function(response) {
    if (is.null(response)) {
      list(success = FALSE, data = NULL, status = "connection failed")
    } else if (httr::status_code(response) != 200) {
      list(success = FALSE, data = NULL, status = httr::status_code(response))
    } else {
      raw_content <- httr::content(response, "text")
      if (nchar(raw_content) == 0 || !jsonlite::validate(raw_content)) {
        list(success = FALSE, data = NULL, status = "invalid json")
      } else {
        data <- jsonlite::fromJSON(raw_content)
        list(success = TRUE, data = data, status = httr::status_code(response))
      }
    }
  }

  list(
    get_table_data = function(page_size, prev_plot_id = NULL) {
      endpoint <- if (is.null(prev_plot_id)) {
        paste0("/get_observation_table/", page_size)
      } else {
        paste0("/get_observation_table/", page_size, "/", prev_plot_id)
      }

      response <- fetch(endpoint)
      process_response(response)
    },
    get_map_points = function() {
      response <- fetch("/get_map_points")
      process_response(response)
    },
    get_observation_details = function(accession_code) {
      response <- fetch(paste0("/get_observation_details/", accession_code))
      process_response(response)
    }
  )
})()
