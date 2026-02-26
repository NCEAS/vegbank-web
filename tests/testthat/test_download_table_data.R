test_that("TABLE_DOWNLOAD_CONFIG has correct structure", {
  expect_true(is.list(TABLE_DOWNLOAD_CONFIG))
  expect_true("plot_table" %in% names(TABLE_DOWNLOAD_CONFIG))

  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  expect_equal(config$resource, "plot-observations")
  expect_equal(config$filename_prefix, "vegbank_plots")
  expect_null(config$has_nested)
  expect_null(config$primary_key)
})

test_that("DOWNLOAD_MAX_RECORDS is set correctly", {
  expect_equal(DOWNLOAD_MAX_RECORDS, 20000L)
})

# GET TABLE FILTER STATE TESTS ===================================================================

test_that("get_table_filter_state extracts search term", {
  # Create a mock state with a function that returns NULL (simulating reactive behavior)
  input <- list(plot_table_search = "oak")
  state <- list(plot_filter = function() NULL)

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_equal(result$search, "oak")
  expect_null(result$filter)
})

test_that("get_table_filter_state extracts cross-resource filter", {
  input <- list(plot_table_search = NULL)
  state <- list(plot_filter = function() list(type = "project", code = "pj.123", label = "Test Project"))

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_null(result$search)
  expect_equal(result$filter$type, "project")
  expect_equal(result$filter$code, "pj.123")
})

test_that("get_table_filter_state handles both search and filter", {
  input <- list(plot_table_search = "pine")
  state <- list(plot_filter = function() list(type = "party", code = "pt.456", label = "Test Party"))

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_equal(result$search, "pine")
  expect_equal(result$filter$type, "party")
  expect_equal(result$filter$code, "pt.456")
  expect_equal(result$filter$label, "Test Party")
})

test_that("get_table_filter_state handles whitespace in search", {
  input <- list(plot_table_search = "  oak  ")
  state <- list(plot_filter = function() NULL)

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_equal(result$search, "  oak  ")
})

test_that("get_table_filter_state handles invalid filter info", {
  input <- list(plot_table_search = NULL)
  state <- list(plot_filter = function() list(type = "project"))

  result <- shiny::isolate(get_table_filter_state("plot_table", input, state))

  expect_null(result$search)
  expect_null(result$filter)
})

# BUILD BUNDLE URL TESTS =========================================================================

test_that("build_bundle_url returns general endpoint with no filter or search", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = NULL, filter = NULL)

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/plot-observations", url))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("search=", url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url uses project-scoped path for project filter (pj.)", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "project", code = "pj.10542", label = "My Project")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/projects/pj.10542/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url uses party-scoped path for party filter (py.)", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "party", code = "py.291", label = "Walker, Marilyn")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/parties/py.291/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url uses community-concepts-scoped path for community concept filter (cc.)", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "community concept", code = "cc.123", label = "Oak Forest")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/community-concepts/cc.123/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url uses plant-concepts-scoped path for plant concept filter (pc.)", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "plant concept", code = "pc.456", label = "Quercus robur")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/plant-concepts/pc.456/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url uses user-datasets-scoped path for dataset citation (ds.)", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "collection-citation", code = "ds.200278", label = "Citation identifier: ds.200278")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/user-datasets/ds.200278/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url uses single-obs path for single-entity citation (ob.)", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "single-entity-citation", code = "ob.2948", label = "Citation identifier: VB.Ob.2948.ACAD143")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/plot-observations/ob.2948", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url appends search to sub-resource path when both filter and search are active", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = "pine",
    filter = list(type = "party", code = "py.291", label = "Walker, Marilyn")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/parties/py.291/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl("search=pine", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url appends search to project sub-resource path", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = "oak",
    filter = list(type = "project", code = "pj.10542", label = "My Project")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/projects/pj.10542/plot-observations", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl("search=oak", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url appends search to single-entity citation path", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = "forest",
    filter = list(type = "single-entity-citation", code = "ob.2948", label = "Citation")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/plot-observations/ob.2948", url, fixed = TRUE))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl("search=forest", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url passes search term for search-only queries", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = "oak", filter = NULL)

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/plot-observations", url))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl("search=", url))
  expect_true(grepl("oak", url))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
  expect_false(grepl("vb_code=", url))
})

test_that("build_bundle_url trims whitespace from search term", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = "  oak  ", filter = NULL)

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("search=oak", url))
})

test_that("build_bundle_url falls back to vb_code param for unknown prefix", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(
    search = NULL,
    filter = list(type = "unknown-type", code = "xx.999", label = "Unknown")
  )

  url <- build_bundle_url(config, filter_state)

  expect_true(grepl("/plot-observations", url))
  expect_true(grepl("bundle=csv", url))
  expect_true(grepl("vb_code=xx.999", url, fixed = TRUE))
  expect_true(grepl(paste0("limit=", DOWNLOAD_MAX_RECORDS), url))
})

test_that("build_bundle_url uses vegbankr base URL", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = NULL, filter = NULL)

  url <- build_bundle_url(config, filter_state)
  base <- vegbankr::vb_get_base_url()

  expect_true(startsWith(url, base))
})

test_that("build_bundle_url ignores empty search string", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = "", filter = NULL)

  url <- build_bundle_url(config, filter_state)

  expect_false(grepl("search=", url))
})

test_that("build_bundle_url ignores whitespace-only search string", {
  config <- TABLE_DOWNLOAD_CONFIG$plot_table
  filter_state <- list(search = "   ", filter = NULL)

  url <- build_bundle_url(config, filter_state)

  expect_false(grepl("search=", url))
})