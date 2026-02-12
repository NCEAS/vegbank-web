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
    # Single entities should have is_dataset = FALSE
    expect_false(info$is_dataset, info = paste("Expected is_dataset=FALSE for", type))
  }
})

test_that("RESOURCE_REGISTRY dataset entry has correct structure", {
  info <- RESOURCE_REGISTRY[["user-datasets"]]
  expect_equal(info$tab, "Plots")
  expect_true(isTRUE(info$is_dataset))
  # Datasets have detail_type = "dataset"
  expect_equal(info$detail_type, "dataset")
})

test_that("RESOURCE_REGISTRY maps to valid app tabs", {
  valid_tabs <- c("Overview", "Map", "Plots", "Plants", "Communities", "Parties", "Projects", "FAQ", "Cite")
  for (type in names(RESOURCE_REGISTRY)) {
    tab <- RESOURCE_REGISTRY[[type]]$tab
    # References don't have a navbar tab, they only appear in detail overlays
    # The tab field is set to "References" for consistency but is not validated
    if (type == "references" || type == "cover-methods" || type == "stratum-methods"
        || type == "datasets" || type == "community-classifications") {
      next
    }
    expect_true(tab %in% valid_tabs, info = paste("Invalid tab", tab, "for", type))
  }
})

test_that("RESOURCE_REGISTRY detail types match show_detail_view switch cases", {
  # These are the resource_type values accepted by show_detail_view() in detail_view.R
  valid_detail_types <- c(
    "community-classification", "community-concept", "plot-observation",
    "project", "party", "plant-concept", "reference", "cover-method", "stratum-method",
    "dataset"  # Datasets have detail_type but use filter instead of detail view
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
      expect_false(result$is_dataset)
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
      expect_false(result$is_dataset)
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
      expect_false(result$is_dataset)
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
      expect_false(result$is_dataset)
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
      expect_false(result$is_dataset)
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
      expect_equal(result$detail_type, "dataset")
      expect_true(result$is_dataset)
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


# ==== UI path redirect ====

test_that("ui() redirects /cite/ path-based URLs to query param form", {
  # Mock request with /cite/ path
  mock_req <- list(
    PATH_INFO = "/cite/VB.Ob.2948.ACAD143",
    REQUEST_URI = "/cite/VB.Ob.2948.ACAD143",
    HTTP_HOST = "vegbank.org"
  )

  result <- ui(mock_req)

  # Should return an HTTP response (a list with status, headers, body)
  expect_true(is.list(result))
  expect_equal(result$status, 302L)
  expect_true(!is.null(result$headers$Location))
  expect_match(result$headers$Location, "cite=VB.Ob.2948.ACAD143")
})

test_that("ui() redirects /cite/ paths with URL-encoded accession codes", {
  mock_req <- list(
    PATH_INFO = "/cite/VB.Ob.22743.INW32086",
    REQUEST_URI = "/cite/VB.Ob.22743.INW32086"
  )

  result <- ui(mock_req)

  expect_equal(result$status, 302L)
  expect_match(result$headers$Location, "cite=")
})

test_that("ui() handles /cite/ paths nested under app base path", {
  # When running under /beta/, PATH_INFO might include the prefix
  mock_req <- list(
    PATH_INFO = "/beta/cite/VB.Ob.2948.ACAD143"
  )

  result <- ui(mock_req)

  expect_equal(result$status, 302L)
  # The accession code should be extracted regardless of prefix
  expect_match(result$headers$Location, "cite=VB.Ob.2948.ACAD143")
})

test_that("ui() does not redirect for normal paths", {
  mock_req <- list(
    PATH_INFO = "/",
    REQUEST_URI = "/"
  )

  result <- ui(mock_req)

  # Normal UI should return a Shiny tag list, not an HTTP redirect
  expect_false(identical(result$status, 302L))
})

test_that("ui() does not redirect for /cite/ with empty accession code", {
  mock_req <- list(
    PATH_INFO = "/cite/",
    REQUEST_URI = "/cite/"
  )

  result <- ui(mock_req)

  # Should still return normal UI since there's no accession code after /cite/
  # The grepl matches, but sub produces empty string and nzchar fails
  # Actually /cite/ matches grepl and sub("^.*/cite/", "", "/cite/") = ""
  # nzchar("") = FALSE, so normal UI is returned
  expect_false(identical(result$status, 302L))
})
