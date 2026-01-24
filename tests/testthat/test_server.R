# Tests for server.R

test_that("server initializes state correctly", {
  # Use testServer to test Shiny server logic
  testServer(server, {
    expect_true(!is.null(state))
    expect_true(!is.null(state$current_tab))
    expect_equal(state$current_tab(), "Overview")
    expect_true(!is.null(state$details_open))
    expect_false(state$details_open())
    expect_true(!is.null(state$table_states))
    expect_true(!is.null(state$table_sync_pending))
    expect_true(!is.null(state$table_sync_completed_at))
  })
})

test_that("server table_registry is correct", {
  testServer(server, {
    expect_true(exists("table_registry"))
    expect_true("plots" %in% names(table_registry))
    expect_true("plants" %in% names(table_registry))
    expect_true("communities" %in% names(table_registry))
    expect_true("parties" %in% names(table_registry))
    expect_true("projects" %in% names(table_registry))
    expect_equal(table_registry$plots$table_id, "plot_table")
  })
})