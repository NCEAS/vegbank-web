#' Mock Shiny Functions for Testing
#'
#' This file provides consistent mocks for Shiny functions used across tests.

#' Environment to store notifications for testing
mock_notification_env <- new.env(parent = emptyenv())
mock_notification_env$notifications <- list()

#' Mock for shiny::showNotification
mock_show_notification <- function(message, type = "default", ...) {
  # Store the notification
  notification <- list(message = message, type = type)
  mock_notification_env$notifications <- c(mock_notification_env$notifications, list(notification))

  # Still output for debugging
  message(paste0("[TEST-", type, "] ", message))
  invisible(NULL)
}

#' Get the stored notifications
get_mock_notifications <- function() {
  mock_notification_env$notifications
}

#' Get the last notification
get_last_notification <- function() {
  notifications <- get_mock_notifications()
  if (length(notifications) > 0) {
    return(notifications[[length(notifications)]])
  }
  NULL
}

#' Reset stored notifications
reset_mock_notifications <- function() {
  mock_notification_env$notifications <- list()
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
with_mock_shiny_notifications <- function(tests) {
  # Reset notifications before each test setup
  reset_mock_notifications()

  testthat::with_mocked_bindings(
    tests,
    showNotification = mock_show_notification,
    withProgress = mock_with_progress,
    incProgress = mock_inc_progress,
    .package = "shiny"
  )
}
