# Shared Shiny session/output stubs used across detail-view tests

# Mock Shiny output environment
mock_output <- new.env()

# Mock Shiny session with message capture
mock_session <- list(
  sendCustomMessage = function(type, message) {
    if (!exists("messages", envir = mock_session, inherits = FALSE)) {
      mock_session$messages <- list()
    }
    mock_session$messages[[type]] <- message
  }
)
