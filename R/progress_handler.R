#' Progress and Notification Handler Module
#'
#' Provides functions for handling progress indicators and notifications in a test-safe way.
#' @keywords internal

#' Check if current environment is a test environment 
#' 
#' @return Boolean indicating if the current environment is a test
#' @keywords internal
#' @export
ph_is_in_test_env <- function() {
  identical(Sys.getenv("TESTTHAT"), "true") ||
    identical(getOption("shiny.testmode"), TRUE)
}

#' Check if current environment is an interactive Shiny session
#' 
#' @return Boolean indicating if the current environment is an interactive Shiny session
#' @keywords internal
#' @export
ph_is_in_shiny_session <- function() {
  !ph_is_in_test_env() && !is.null(shiny::getDefaultReactiveDomain())
}

#' Show a notification in a test-safe way
#' 
#' @param message The message to display
#' @param type The notification type (default, error, warning, message)
#' @return Invisible NULL
#' @keywords internal
#' @export
ph_show_notification <- function(message, type = "default") {
  if (!ph_is_in_test_env()) {
    # Not in testing - show notification in Shiny
    tryCatch(
      {
        shiny::showNotification(message, type = type)
      },
      error = function(e) {
        # Fall back to console message if Shiny notification fails
        message(paste0("[", type, "] ", message))
      }
    )
  } else {
    # In testing - just log to console
    message(paste0("[TEST-", type, "] ", message))
  }
  invisible(NULL)
}

#' Safely execute a function with progress indicator
#' 
#' @param expr The expression to execute
#' @param message The progress message to display
#' @param value The initial progress value
#' @return The result of evaluating expr
#' @keywords internal
#' @export
ph_with_safe_progress <- function(expr, message = "Processing...", value = 0) {
  if (ph_is_in_shiny_session()) {
    # In Shiny session - use withProgress
    shiny::withProgress(message = message, value = value, expr)
  } else {
    # In testing or non-Shiny environment - just evaluate without progress
    expr
  }
}

#' Increment progress in a test-safe way
#' 
#' @param amount The amount to increment
#' @param detail The detail message
#' @return Invisible NULL
#' @keywords internal
#' @export
ph_inc_progress <- function(amount, detail = NULL) {
  if (ph_is_in_shiny_session()) {
    shiny::incProgress(amount, detail = detail)
  }
  invisible(NULL)
}

# Create a convenience list for imported code to use the same interface
progress_handler <- list(
  is_in_test_env = ph_is_in_test_env,
  is_in_shiny_session = ph_is_in_shiny_session,
  show_notification = ph_show_notification,
  with_safe_progress = ph_with_safe_progress,
  inc_progress = ph_inc_progress
)
