# Tests for server.R

test_that("server initializes state correctly", {
  # Use testServer to test Shiny server logic
  shiny::testServer(server, {
    expect_true(!is.null(state))
    expect_true(!is.null(state$current_tab))
    expect_equal(state$current_tab(), "Home")
    expect_true(!is.null(state$details_open))
    expect_false(state$details_open())
    expect_true(!is.null(state$table_states))
    expect_true(!is.null(state$table_sync_pending))
    expect_true(!is.null(state$table_sync_completed_at))
  })
})

test_that("server table_registry is correct", {
  shiny::testServer(server, {
    expect_true(exists("table_registry"))
    expect_true("plots" %in% names(table_registry))
    expect_true("plants" %in% names(table_registry))
    expect_true("communities" %in% names(table_registry))
    expect_true("parties" %in% names(table_registry))
    expect_true("projects" %in% names(table_registry))
    expect_equal(table_registry$plots$table_id, "plot_table")
  })
})

# ===========================================================================
# map_search_query observer tests
# ===========================================================================

# Small fixture with four rows:
#   - VB.OB.1001 / SITE-A  — unique ob_code and author_obs_code, valid coords
#   - VB.OB.1002 / SITE-B  — valid coords, shares author_obs_code with next row
#   - VB.OB.1003 / SITE-B  — valid coords, same author_obs_code as above (→ multiple)
#   - VB.OB.1004 / ONLY-NA — NA coords (should always be excluded from results)
make_map_obs_fixture <- function() {
  data.frame(
    ob_code         = c("VB.OB.1001", "VB.OB.1002", "VB.OB.1003", "VB.OB.1004"),
    author_obs_code = c("SITE-A",     "SITE-B",     "SITE-B",     "ONLY-NA"),
    latitude        = c(37.5,          38.0,          38.5,          NA_real_),
    longitude       = c(-120.0,        -121.0,        -121.5,        NA_real_),
    stringsAsFactors = FALSE
  )
}

# Helper: run one search query through the real server observer.
# Captures the single sendCustomMessage("map_search_results", …) call and
# returns it as list(type, message), or NULL if nothing was sent.
run_map_search <- function(query, obs = make_map_obs_fixture()) {
  env <- new.env(parent = emptyenv())
  env$msg <- NULL
  shiny::testServer(server, {
    session$sendCustomMessage <- function(type, message) {
      if (type == "map_search_results") {
        env$msg <- list(type = type, message = message)
      }
    }
    # observeEvent(..., ignoreInit = TRUE) skips the FIRST non-NULL invocation.
    # This primer call satisfies that skip; the second setInputs fires the handler.
    session$setInputs(map_search_query = list(query = "__init__", ts = 0))
    map_observations(obs)
    session$setInputs(map_search_query = list(query = query, ts = 1))
  })
  env$msg
}

test_that("map search returns no_data when map_observations is NULL", {
  env <- new.env(parent = emptyenv())
  env$msg <- NULL
  shiny::testServer(server, {
    session$sendCustomMessage <- function(type, message) {
      if (type == "map_search_results") env$msg <- list(type = type, message = message)
    }
    # map_observations() is NULL by default — do NOT set it
    session$setInputs(map_search_query = list(query = "__init__", ts = 0))  # prime ignoreInit
    session$setInputs(map_search_query = list(query = "ABC", ts = 1))
  })
  expect_equal(env$msg$type, "map_search_results")
  expect_equal(env$msg$message$status, "no_data")
})

test_that("map search returns no_data when map_observations is an empty data frame", {
  empty_obs <- data.frame(
    ob_code = character(0), author_obs_code = character(0),
    latitude = numeric(0), longitude = numeric(0),
    stringsAsFactors = FALSE
  )
  msg <- run_map_search("ABC", obs = empty_obs)
  expect_equal(msg$type, "map_search_results")
  expect_equal(msg$message$status, "no_data")
})

test_that("map search returns none when no codes match", {
  msg <- run_map_search("UNKNOWN-CODE")
  expect_equal(msg$message$status, "none")
})

test_that("map search returns single match by ob_code with correct payload", {
  msg <- run_map_search("VB.OB.1001")
  expect_equal(msg$message$status, "single")
  expect_equal(msg$message$ob_code, "VB.OB.1001")
  expect_equal(msg$message$label,   "SITE-A")
  expect_equal(msg$message$lat,     37.5)
  expect_equal(msg$message$lng,     -120.0)
})

test_that("map search returns single match by author_obs_code", {
  msg <- run_map_search("SITE-A")
  expect_equal(msg$message$status,  "single")
  expect_equal(msg$message$ob_code, "VB.OB.1001")
  expect_equal(msg$message$label,   "SITE-A")
})

test_that("map search is case-insensitive", {
  msg_lower <- run_map_search("vb.ob.1001")
  msg_mixed <- run_map_search("Vb.Ob.1001")
  expect_equal(msg_lower$message$status,  "single")
  expect_equal(msg_mixed$message$status,  "single")
  expect_equal(msg_lower$message$ob_code, "VB.OB.1001")
  expect_equal(msg_mixed$message$ob_code, "VB.OB.1001")
})

test_that("map search returns multiple when author_obs_code matches more than one plot", {
  msg <- run_map_search("SITE-B")
  expect_equal(msg$message$status, "multiple")
  expect_length(msg$message$matches, 2)
  codes <- vapply(msg$message$matches, `[[`, character(1), "ob_code")
  expect_setequal(codes, c("VB.OB.1002", "VB.OB.1003"))
})

test_that("map search multiple result items contain required fields", {
  msg <- run_map_search("SITE-B")
  m <- msg$message$matches[[1]]
  expect_true(all(c("lat", "lng", "author_obs_code", "ob_code") %in% names(m)))
})

test_that("map search excludes rows with missing coordinates", {
  msg <- run_map_search("ONLY-NA")
  expect_equal(msg$message$status, "none")
})

test_that("map search result list is capped at 50 matches", {
  large_obs <- data.frame(
    ob_code         = paste0("VB.OB.", seq_len(51)),
    author_obs_code = rep("COMMON-CODE", 51),
    latitude        = rep(37.0, 51),
    longitude       = rep(-120.0, 51),
    stringsAsFactors = FALSE
  )
  msg <- run_map_search("COMMON-CODE", obs = large_obs)
  expect_equal(msg$message$status, "multiple")
  expect_length(msg$message$matches, 50)
})

test_that("map search observer ignores empty queries", {
  env <- new.env(parent = emptyenv())
  env$called <- FALSE
  shiny::testServer(server, {
    session$sendCustomMessage <- function(type, message) {
      if (type == "map_search_results") env$called <- TRUE
    }
    map_observations(make_map_obs_fixture())
    session$setInputs(map_search_query = list(query = "__init__", ts = 0))  # prime ignoreInit
    session$setInputs(map_search_query = list(query = "", ts = 1))
  })
  expect_false(env$called)
})

test_that("map search observer ignores whitespace-only queries", {
  env <- new.env(parent = emptyenv())
  env$called <- FALSE
  shiny::testServer(server, {
    session$sendCustomMessage <- function(type, message) {
      if (type == "map_search_results") env$called <- TRUE
    }
    map_observations(make_map_obs_fixture())
    session$setInputs(map_search_query = list(query = "__init__", ts = 0))  # prime ignoreInit
    session$setInputs(map_search_query = list(query = "   ", ts = 1))
  })
  expect_false(env$called)
})