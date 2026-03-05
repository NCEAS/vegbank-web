# Tests for community classification detail view

test_that("build_comm_class_details_view handles NULL data gracefully", {
  result <- build_comm_class_details_view(NULL)

  expect_type(result, "list")
  expect_named(result, c(
    "comm_class_header",
    "comm_class_details",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))

  # Each component should be a render function
  expect_true(inherits(result$comm_class_header, "shiny.render.function"))
  expect_true(inherits(result$comm_class_details, "shiny.render.function"))
  expect_true(inherits(result$comm_class_interpretations, "shiny.render.function"))
  expect_true(inherits(result$comm_class_contributors, "shiny.render.function"))
})

test_that("build_comm_class_details_view handles empty data gracefully", {
  result <- build_comm_class_details_view(data.frame())

  expect_type(result, "list")
  expect_setequal(names(result), c(
    "comm_class_header",
    "comm_class_details",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))
})

test_that("build_comm_class_details_view formats classification data correctly", {
  # cl.1943: inspection=TRUE, multivariate=FALSE, expert=NA, no publication
  result <- build_comm_class_details_view(mock_comm_class_cl1943)
  expect_type(result, "list")
  expect_setequal(names(result), c(
    "comm_class_header",
    "comm_class_details",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))
  expect_true(inherits(result$comm_class_header, "shiny.render.function"))
  expect_true(inherits(result$comm_class_details, "shiny.render.function"))
  expect_true(inherits(result$comm_class_interpretations, "shiny.render.function"))
  expect_true(inherits(result$comm_class_contributors, "shiny.render.function"))

  # cl.2358: inspection=FALSE, multivariate=TRUE
  result_2358 <- build_comm_class_details_view(mock_comm_class_cl2358)
  expect_type(result_2358, "list")
  expect_setequal(names(result_2358), c(
    "comm_class_header", "comm_class_details",
    "comm_class_interpretations", "comm_class_contributors"
  ))

  # cl.167392: all method flags NA, no dates
  result_167392 <- build_comm_class_details_view(mock_comm_class_cl167392)
  expect_type(result_167392, "list")
  expect_setequal(names(result_167392), c(
    "comm_class_header", "comm_class_details",
    "comm_class_interpretations", "comm_class_contributors"
  ))
})

test_that("create_comm_class_header_ui renders header with cl_code and observation link", {
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_header_ui(mock_comm_class_cl1943)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("cl.1943", html$html))
      expect_true(grepl("ob.3407", html$html))
      # Has start_date → date range rendered
      expect_true(grepl("1998", html$html))
    }
  )

  # cl.167392: no dates → no date range paragraph
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_header_ui(mock_comm_class_cl167392)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("cl.167392", html$html))
      expect_true(grepl("ob.206458", html$html))
    }
  )
})

test_that("create_comm_class_details_ui renders analysis methods correctly", {
  # cl.1943: inspection=TRUE (Yes), table=FALSE (No), multivariate=FALSE (No),
  #           expert=NA (Unspecified), no publication (Unspecified)
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_details_ui(mock_comm_class_cl1943)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Yes", html$html))           # inspection=TRUE
      expect_true(grepl("No", html$html))            # table or multivariate FALSE
      expect_true(grepl("Unspecified", html$html))   # expert=NA and no publication
      expect_true(grepl("Table Analysis", html$html))
      expect_true(grepl("Multivariate Analysis", html$html))
      expect_true(grepl("Expert System", html$html))
      expect_true(grepl("Classification Publication", html$html))
    }
  )

  # cl.2358: inspection=FALSE (No), multivariate=TRUE (Yes)
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_details_ui(mock_comm_class_cl2358)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Yes", html$html))  # multivariate=TRUE
      expect_true(grepl("No", html$html))   # inspection=FALSE
    }
  )

  # cl.167392: all NA → all "Unspecified"
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_details_ui(mock_comm_class_cl167392)
    },
    {
      html <- session$getOutput("test")
      expect_false(grepl("Yes", html$html))
      expect_false(grepl("No", html$html))
      expect_true(grepl("Unspecified", html$html))
    }
  )
})

test_that("create_comm_class_interpretations_ui renders interpretations with Typal", {
  # cl.1943: CEGL001471 – confidence/fit NA, notes present, type=FALSE, nomenclatural=FALSE
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_interpretations_ui(mock_comm_class_cl1943)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Calamovilfa longifolia", html$html))           # comm_name
      expect_true(grepl("Calamovilfa longifolia Grassland", html$html)) # notes
      expect_true(grepl("Typal", html$html))                            # label always present
      expect_true(grepl("table-striped", html$html))                    # table class
    }
  )

  # cl.2358: CEGL002509 – Picea glauca forest, notes "Balsam fir - paper birch forest"
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_interpretations_ui(mock_comm_class_cl2358)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Picea glauca", html$html))
      expect_true(grepl("Balsam fir", html$html))  # notes
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
  # cl.1943: Von Loh Jim + Faber-Langendoen Don, both Classifiers
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_contributors_ui(mock_comm_class_cl1943)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Von Loh, Jim", html$html))
      expect_true(grepl("Faber-Langendoen, Don", html$html))
      expect_true(grepl("Classifier", html$html))
    }
  )

  # cl.167392: single contributor with role "Data aggregator"
  shiny::testServer(
    function(input, output, session) {
      output$test <- create_comm_class_contributors_ui(mock_comm_class_cl167392)
    },
    {
      html <- session$getOutput("test")
      expect_true(grepl("Ramm-Granberg, Tynan", html$html))
      expect_true(grepl("Data aggregator", html$html))
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
    "comm_class_details",
    "comm_class_interpretations",
    "comm_class_contributors"
  ))
})
