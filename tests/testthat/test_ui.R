# UI Components Tests

test_that("ui function returns a Shiny tag list", {
  # Mock includeMarkdown to avoid file not found error
  with_mocked_bindings(
    includeMarkdown = function(...) {
      shiny::div("Mocked FAQ content")
    },
    .package = "shiny",
    {
      result <- ui(NULL)
      expect_s3_class(result, "shiny.tag.list")
    }
  )
})

test_that("build_navbar returns a Shiny tag (navbar)", {
  # Mock includeMarkdown to avoid file not found error
  with_mocked_bindings(
    includeMarkdown = function(...) {
      shiny::div("Mocked FAQ content")
    },
    .package = "shiny",
    {
      result <- build_navbar()
      expect_s3_class(result, "shiny.tag.list")
    }
  )
})

test_that("build_detail_overlay returns a Shiny tag (overlay)", {
  overlay <- build_detail_overlay()
  expect_s3_class(overlay, "shiny.tag")
})
