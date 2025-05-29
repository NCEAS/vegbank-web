#' Progress and Notification Handler Module
#'
#' Provides functions for handling progress indicators and notifications in a test-safe way.
#' @keywords internal

progress_handler <- (function() {
  #' Check if we're in a testing environment
  #'
  #' @return Boolean indicating if the current environment is a test
  is_in_test_env <- function() {
    identical(Sys.getenv("TESTTHAT"), "true") || 
    identical(getOption("shiny.testmode"), TRUE)
  }
  
  #' Check if we're in an interactive Shiny session
  #'
  #' @return Boolean indicating if the current environment is an interactive Shiny session
  is_in_shiny_session <- function() {
    !is_in_test_env() && !is.null(getDefaultReactiveDomain())
  }
  
  #' Show a notification in a test-safe way
  #'
  #' @param message The message to display
  #' @param type The notification type (default, error, warning, message)
  #' @return Invisible NULL
  show_notification <- function(message, type = "default") {
    if (!is_in_test_env()) {
      # Not in testing - show notification in Shiny
      tryCatch({
        shiny::showNotification(message, type = type)
      }, error = function(e) {
        # Fall back to console message if Shiny notification fails
        message(paste0("[", type, "] ", message))
      })
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
  with_safe_progress <- function(expr, message = "Processing...", value = 0) {
    if (is_in_shiny_session()) {
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
  inc_progress <- function(amount, detail = NULL) {
    if (is_in_shiny_session()) {
      shiny::incProgress(amount, detail = detail)
    }
    invisible(NULL)
  }
  
  # Return public functions
  list(
    is_in_test_env = is_in_test_env,
    is_in_shiny_session = is_in_shiny_session,
    show_notification = show_notification,
    with_safe_progress = with_safe_progress,
    inc_progress = inc_progress
  )
})()
