# Tests for community classification detail view

test_that("build_comm_class_details_view handles NULL data gracefully", {
  result <- build_comm_class_details_view(NULL)

  expect_type(result, "list")
  expect_named(result, c(
    "comm_class_header",
    "comm_class_methods",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))

  # Each component should be a render function
  expect_true(inherits(result$comm_class_header, "shiny.render.function"))
  expect_true(inherits(result$comm_class_methods, "shiny.render.function"))
  expect_true(inherits(result$comm_class_interpretations, "shiny.render.function"))
  expect_true(inherits(result$comm_class_contributors, "shiny.render.function"))
})

test_that("build_comm_class_details_view handles empty data gracefully", {
  result <- build_comm_class_details_view(data.frame())

  expect_type(result, "list")
  expect_setequal(names(result), c(
    "comm_class_header",
    "comm_class_methods",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))
})

test_that("build_comm_class_details_view formats classification data correctly", {
  result <- build_comm_class_details_view(mock_comm_class_data)

  expect_type(result, "list")
  expect_setequal(names(result), c(
    "comm_class_header",
    "comm_class_methods",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))

  # Each component should be a render function
  expect_true(inherits(result$comm_class_header, "shiny.render.function"))
  expect_true(inherits(result$comm_class_methods, "shiny.render.function"))
  expect_true(inherits(result$comm_class_interpretations, "shiny.render.function"))
  expect_true(inherits(result$comm_class_contributors, "shiny.render.function"))
})

test_that("create_comm_class_header_ui renders header with comm_name and observation link", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_header_ui(mock_comm_class_data)
    },
    {
      html <- session$getOutput("test")
      # Should show first interpretation's comm_name
      expect_true(grepl("Oak-Hickory Forest", html$html))
      # Should show CEGL code
      expect_true(grepl("COMM001", html$html))
      # Should show "See observation" link text
      expect_true(grepl("observation", html$html))
    }
  )
})

test_that("create_comm_class_methods_ui renders analysis methods with publication link", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_methods_ui(mock_comm_class_data)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Visual inspection", html$html))
      expect_true(grepl("Table Analysis", html$html))
      expect_true(grepl("Multivariate Analysis", html$html))
      expect_true(grepl("Expert System", html$html))
      expect_true(grepl("Classification Publication", html$html))
      expect_true(grepl("Smith et al. 2020", html$html))
    }
  )
})

test_that("create_comm_class_interpretations_ui renders interpretations with Typal", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_interpretations_ui(mock_comm_class_data)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Oak-Hickory Forest", html$html))
      expect_true(grepl("Mixed Hardwood Forest", html$html))
      expect_true(grepl("High", html$html))
      expect_true(grepl("NVC Authority", html$html))
      # Typal field instead of Type
      expect_true(grepl("Typal", html$html))
      # Zebra striping class
      expect_true(grepl("table-striped", html$html))
    }
  )
})

test_that("create_comm_class_interpretations_ui handles empty interpretations", {
  data_no_interp <- mock_comm_class_data
  data_no_interp$interpretations <- list(data.frame())

  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_interpretations_ui(data_no_interp)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No community interpretations recorded", html$html))
    }
  )
})

test_that("create_comm_class_contributors_ui renders contributors", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_contributors_ui(mock_comm_class_data)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("John Smith", html$html))
      expect_true(grepl("Jane Doe", html$html))
      expect_true(grepl("Author", html$html))
      expect_true(grepl("Classifier", html$html))
    }
  )
})

test_that("create_comm_class_contributors_ui handles empty contributors", {
  data_no_contrib <- mock_comm_class_data
  data_no_contrib$contributors <- list(data.frame())

  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_contributors_ui(data_no_contrib)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("No contributors recorded", html$html))
    }
  )
})

test_that("comm_class_detail_output_names contains all expected output names", {
  expect_equal(comm_class_detail_output_names, c(
    "comm_class_header",
    "comm_class_methods",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))
})
