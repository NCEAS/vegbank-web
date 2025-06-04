#' Mock Shiny Functions for Testing
#'
#' This file provides consistent mocks for Shiny functions used across tests.

#' Mock for shiny::showNotification
mock_show_notification <- function(message, type = "default", ...) {
  message(paste0("[TEST-", type, "] ", message))
  invisible(NULL)
}

#' Mock for shiny::withProgress
mock_with_progress <- function(expr, message = "Processing...", value = 0, ...) {
  # Just evaluate the expression without progress tracking
  force(expr)
}

#' Mock for shiny::incProgress
mock_inc_progress <- function(amount, detail = NULL, ...) {
  # Do nothing in tests
  invisible(NULL)
}

#' Set up mocked Shiny functions
#'
#' Call this at the beginning of test blocks that need mocked Shiny functions
mock_shiny_functions <- function() {
  testthat::with_mocked_bindings(
    showNotification = mock_show_notification,
    withProgress = mock_with_progress,
    incProgress = mock_inc_progress,
    .package = "shiny"
  )
}
