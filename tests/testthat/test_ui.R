test_that("ui function returns a Shiny tag list", {
  shiny_ui <- ui(NULL)
  expect_true(inherits(shiny_ui, "shiny.tag.list"))
})

test_that("build_navbar returns a Shiny tag (navbar)", {
  navbar <- build_navbar()
  expect_true(inherits(navbar, "shiny.tag.list"))
})

test_that("build_detail_overlay returns a Shiny tag (overlay)", {
  overlay <- build_detail_overlay()
  expect_true(inherits(overlay, "shiny.tag"))
})
