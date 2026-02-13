# Tests for citation resolution (legacy /cite/ URLs)
# Tests RESOURCE_REGISTRY, resolve_citation(), and UI path redirect

# ==== RESOURCE_REGISTRY ====

test_that("RESOURCE_REGISTRY covers all expected resource types", {
  expect_true("plot-observations" %in% names(RESOURCE_REGISTRY))
  expect_true("community-concepts" %in% names(RESOURCE_REGISTRY))
  expect_true("plant-concepts" %in% names(RESOURCE_REGISTRY))
  expect_true("projects" %in% names(RESOURCE_REGISTRY))
  expect_true("parties" %in% names(RESOURCE_REGISTRY))
  expect_true("user-datasets" %in% names(RESOURCE_REGISTRY))
})

test_that("RESOURCE_REGISTRY entries have correct structure for single entities", {
  single_types <- c("plot-observations", "community-concepts", "plant-concepts", "projects", "parties")
  for (type in single_types) {
    info <- RESOURCE_REGISTRY[[type]]
    expect_true(!is.null(info$tab), info = paste("Missing tab for", type))
    expect_true(!is.null(info$detail_type), info = paste("Missing detail_type for", type))
    expect_true(nzchar(info$tab), info = paste("Empty tab for", type))
    expect_true(nzchar(info$detail_type), info = paste("Empty detail_type for", type))
    # Single entities should not be user-datasets
    expect_false(info$api_type == "user-datasets", info = paste("Expected non-dataset for", type))
  }
})

test_that("RESOURCE_REGISTRY dataset entry has correct structure", {
  info <- RESOURCE_REGISTRY[["user-datasets"]]
  expect_equal(info$tab, "Plots")
  expect_equal(info$api_type, "user-datasets")
  expect_equal(info$detail_type, NULL)  # Datasets use a filter-based view, not a detail overlay
})

test_that("RESOURCE_REGISTRY maps to valid app tabs", {
  valid_tabs <- c("Overview", "Map", "Plots", "Plants", "Communities", "Parties", "Projects", "FAQ", "Cite")
  for (type in names(RESOURCE_REGISTRY)) {
    tab <- RESOURCE_REGISTRY[[type]]$tab
    # The tab field is not set for comm classifications, references, cover-methods, stratum-methods
    # since they only appear in detail overlays
    if (type == "references" || type == "cover-methods" || type == "stratum-methods"
        || type == "community-classifications") {
      next
    }
    expect_true(tab %in% valid_tabs, info = paste("Invalid tab", tab, "for", type))
  }
})

test_that("RESOURCE_REGISTRY detail types match show_detail_view switch cases", {
  # These are the resource_type values accepted by show_detail_view() in detail_view.R
  valid_detail_types <- c(
    "community-classification", "community-concept", "plot-observation",
    "project", "party", "plant-concept", "reference", "cover-method", "stratum-method"
  )
  for (type in names(RESOURCE_REGISTRY)) {
    detail_type <- RESOURCE_REGISTRY[[type]]$detail_type
    if (!is.null(detail_type)) {
      expect_true(detail_type %in% valid_detail_types,
        info = paste("Invalid detail_type", detail_type, "for resource", type)
      )
    }
  }
})


# ==== resolve_citation() ====

# Helper: create a mock vb_resolve response for a given code and resource type
mock_resolve_response <- function(vb_code, vb_resource_type) {
  function(id) {
    list(
      identifier_value = id,
      identifier_type = "accession_code",
      vb_code = vb_code,
      vb_resource_type = vb_resource_type
    )
  }
}

test_that("resolve_citation returns correct structure for plot observation", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("ob.2948", "plot-observations"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.Ob.2948.ACAD143")

      expect_false(is.null(result))
      expect_equal(result$vb_code, "ob.2948")
      expect_equal(result$tab, "Plots")
      expect_equal(result$detail_type, "plot-observation")
      expect_equal(result$resource_info$api_type, "plot-observations")
      expect_equal(result$identifier, "VB.Ob.2948.ACAD143")
    }
  )
})

test_that("resolve_citation returns correct structure for community concept", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("cc.1234", "community-concepts"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.CC.1234.TEST")

      expect_false(is.null(result))
      expect_equal(result$vb_code, "cc.1234")
      expect_equal(result$tab, "Communities")
      expect_equal(result$detail_type, "community-concept")
      expect_equal(result$resource_info$api_type, "community-concepts")
    }
  )
})

test_that("resolve_citation returns correct structure for plant concept", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("pc.5678", "plant-concepts"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.PC.5678.TEST")

      expect_false(is.null(result))
      expect_equal(result$vb_code, "pc.5678")
      expect_equal(result$tab, "Plants")
      expect_equal(result$detail_type, "plant-concept")
      expect_equal(result$resource_info$api_type, "plant-concepts")
    }
  )
})

test_that("resolve_citation returns correct structure for project", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("pj.100", "projects"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.PJ.100.TEST")

      expect_false(is.null(result))
      expect_equal(result$vb_code, "pj.100")
      expect_equal(result$tab, "Projects")
      expect_equal(result$detail_type, "project")
      expect_equal(result$resource_info$api_type, "projects")
    }
  )
})

test_that("resolve_citation returns correct structure for party", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("py.200", "parties"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.PY.200.TEST")

      expect_false(is.null(result))
      expect_equal(result$vb_code, "py.200")
      expect_equal(result$tab, "Parties")
      expect_equal(result$detail_type, "party")
      expect_equal(result$resource_info$api_type, "parties")
    }
  )
})

test_that("resolve_citation returns correct structure for user dataset", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("ds.50", "user-datasets"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.DS.50.TEST")

      expect_false(is.null(result))
      expect_equal(result$vb_code, "ds.50")
      expect_equal(result$tab, "Plots")
      expect_equal(result$detail_type, NULL)
      expect_equal(result$resource_info$api_type, "user-datasets")
      expect_equal(result$identifier, "VB.DS.50.TEST")
    }
  )
})

test_that("resolve_citation returns NULL when vb_resolve throws an error", {
  testthat::with_mocked_bindings(
    vb_resolve = function(id) stop("API connection failed"),
    .package = "vegbankr",
    {
      expect_warning(
        result <- resolve_citation("VB.Ob.999.BAD"),
        "Citation resolution failed"
      )
      expect_null(result)
    }
  )
})

test_that("resolve_citation returns NULL when vb_resolve returns NULL", {
  testthat::with_mocked_bindings(
    vb_resolve = function(id) NULL,
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.XX.000.UNKNOWN")
      expect_null(result)
    }
  )
})

test_that("resolve_citation returns NULL when vb_resolve returns incomplete data", {
  # Missing vb_code
  testthat::with_mocked_bindings(
    vb_resolve = function(id) {
      list(
        identifier_value = id,
        identifier_type = "accession_code",
        vb_resource_type = "plot-observations"
      )
    },
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.Ob.2948.ACAD143")
      expect_null(result)
    }
  )

  # Missing vb_resource_type
  testthat::with_mocked_bindings(
    vb_resolve = function(id) {
      list(
        identifier_value = id,
        identifier_type = "accession_code",
        vb_code = "ob.2948"
      )
    },
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.Ob.2948.ACAD143")
      expect_null(result)
    }
  )
})

test_that("resolve_citation returns NULL for unsupported resource types", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("xx.123", "unknown-type"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.XX.123.TEST")
      expect_null(result)
    }
  )
})

test_that("resolve_citation preserves original accession code in result", {
  testthat::with_mocked_bindings(
    vb_resolve = mock_resolve_response("ob.2948", "plot-observations"),
    .package = "vegbankr",
    {
      result <- resolve_citation("VB.Ob.2948.ACAD143")
      expect_equal(result$identifier, "VB.Ob.2948.ACAD143")
    }
  )
})

test_that("resolve_citation passes accession code to vb_resolve", {
  received_id <- NULL
  testthat::with_mocked_bindings(
    vb_resolve = function(id) {
      received_id <<- id
      list(
        identifier_value = id,
        identifier_type = "accession_code",
        vb_code = "ob.2948",
        vb_resource_type = "plot-observations"
      )
    },
    .package = "vegbankr",
    {
      resolve_citation("VB.Ob.2948.ACAD143")
      expect_equal(received_id, "VB.Ob.2948.ACAD143")
    }
  )
})


# ==== UI Citation URL Redirect ====

test_that("ui() redirects /cite/ paths to query parameter format", {
  # Mock request object for /cite/VB.Ob.2948.ACAD143
  req <- list(PATH_INFO = "/cite/VB.Ob.2948.ACAD143")

  result <- ui(req)

  # Should return HTML with JavaScript redirect
  expect_s3_class(result, "shiny.tag")
  expect_equal(result$name, "html")

  # Extract JavaScript from head
  script_tag <- result$children[[1]]$children[[1]]
  expect_equal(script_tag$name, "script")

  js_code <- as.character(script_tag$children[[1]])
  expect_true(grepl("window\\.location\\.replace", js_code))
  expect_true(grepl("/\\?cite=VB\\.Ob\\.2948\\.ACAD143", js_code))
})

test_that("ui() correctly URL-encodes special characters in citation identifier", {
  # Test with identifier containing spaces and special chars
  req <- list(PATH_INFO = "/cite/VB.Test 123+Special")

  result <- ui(req)

  # Extract JavaScript
  script_tag <- result$children[[1]]$children[[1]]
  js_code <- as.character(script_tag$children[[1]])

  # Should encode space as %20 and + as %2B
  expect_true(grepl("VB\\.Test%20123%2BSpecial", js_code))
})

test_that("ui() safely escapes JavaScript-unsafe characters in redirect URL", {
  # Test with identifier that could break JavaScript if not properly escaped
  # Using a backslash and quote characters
  req <- list(PATH_INFO = '/cite/VB.Test"Quote')

  result <- ui(req)

  # Extract JavaScript
  script_tag <- result$children[[1]]$children[[1]]
  js_code <- as.character(script_tag$children[[1]])

  # The URL should be JSON-escaped, protecting against XSS
  # jsonlite::toJSON will escape the quote as \"
  expect_true(grepl("window\\.location\\.replace", js_code))
  # Should not contain unescaped quotes that would break the JavaScript
  expect_false(grepl('window\\.location\\.replace\\("[^"]*"[^"]*"\\)', js_code))
})

test_that("ui() handles simple plot observation citation path", {
  req <- list(PATH_INFO = "/cite/VB.Ob.22743.INW32086")

  result <- ui(req)

  script_tag <- result$children[[1]]$children[[1]]
  js_code <- as.character(script_tag$children[[1]])

  expect_true(grepl("/\\?cite=VB\\.Ob\\.22743\\.INW32086", js_code))
})

test_that("ui() handles community concept citation path", {
  req <- list(PATH_INFO = "/cite/VB.CC.1234.EXAMPLE")

  result <- ui(req)

  script_tag <- result$children[[1]]$children[[1]]
  js_code <- as.character(script_tag$children[[1]])

  expect_true(grepl("/\\?cite=VB\\.CC\\.1234\\.EXAMPLE", js_code))
})

test_that("ui() handles dataset citation path", {
  req <- list(PATH_INFO = "/cite/VB.DS.50.TESTDATA")

  result <- ui(req)

  script_tag <- result$children[[1]]$children[[1]]
  js_code <- as.character(script_tag$children[[1]])

  expect_true(grepl("/\\?cite=VB\\.DS\\.50\\.TESTDATA", js_code))
})

test_that("ui() does not redirect non-citation paths", {
  # Regular path should return normal UI
  req <- list(PATH_INFO = "/")

  result <- ui(req)

  # Should return full UI, not just redirect HTML
  # The full UI contains many nested tags, not just html > head > script
  expect_true(length(result) > 1 || (length(result) == 1 && length(result[[1]]$children) > 1))
})

test_that("ui() does not redirect paths that start with /cite but aren't citation URLs", {
  # Path like /citation-info should not trigger redirect
  req <- list(PATH_INFO = "/citation-info")

  result <- ui(req)

  # Should return full UI, not redirect
  expect_true(length(result) > 1 || (length(result) == 1 && length(result[[1]]$children) > 1))
})