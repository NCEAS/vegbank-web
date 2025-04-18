#' Progress Handler
#'
#' Simplified progress handler that works properly with Shiny.
#'
#' @param message The message to display in the progress bar.
#' @return A function that takes a code block and executes it with progress reporting.
#' @keywords internal

#' @importFrom shiny withProgress incProgress
show_progress <- function(message) {
  function(code) {
    shiny::withProgress(
      message = message,
      value = 0,
      {
        # Create a local environment to track progress
        env <- new.env()
        env$current <- 0

        # Function to increment progress
        step <- function(amount, detail = NULL) {
          env$current <- min(1, env$current + amount)
          shiny::incProgress(amount = amount, detail = detail)
        }

        # Function to complete progress
        complete <- function(detail = "Done") {
          remaining <- 1 - env$current
          if (remaining > 0) {
            shiny::incProgress(amount = remaining, detail = detail)
          }
        }

        # Execute the provided code with access to progress functions
        result <- code(step, complete)

        # Ensure progress completes
        if (env$current < 1) {
          complete()
        }

        return(result)
      }
    )
  }
}
