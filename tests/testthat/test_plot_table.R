test_that("create_empty_table returns a DataTable object", {
  empty_table <- create_empty_table()
  expect_true(inherits(empty_table, "datatables"))
  expect_true(inherits(empty_table$x$data, "data.frame"))
  expect_equal(colnames(empty_table$x$data), "No.Data.Available")
})

test_that("is_missing_data correctly identifies missing data", {
  # Missing plot data
  expect_true(is_missing_data(NULL, data.frame(), data.frame(), show_notifications = FALSE))
  expect_true(is_missing_data(data.frame(), data.frame(), data.frame(), show_notifications = FALSE))

  # Missing taxa data
  expect_true(is_missing_data(data.frame(a = 1), NULL, data.frame(), show_notifications = FALSE))

  # Missing community data
  expect_true(is_missing_data(data.frame(a = 1), data.frame(b = 2), NULL, show_notifications = FALSE))

  # All data present
  expect_false(is_missing_data(
    data.frame(a = 1),
    data.frame(b = 2),
    data.frame(c = 3),
    show_notifications = FALSE
  ))
})

test_that("clean_column_data handles missing columns and values", {
  test_data <- data.frame(
    col1 = c("value1", NA, ""),
    stringsAsFactors = FALSE
  )

  # Test with existing column
  result <- clean_column_data(test_data, "col1")
  expect_equal(result, c("value1", "Not Provided", "Not Provided"))

  # Test with non-existent column
  result <- clean_column_data(test_data, "col2")
  expect_equal(result, rep("Not Provided", nrow(test_data)))
})

test_that("create_action_buttons generates HTML buttons", {
  test_data <- data.frame(
    id = 1:2,
    stringsAsFactors = FALSE
  )

  buttons <- create_action_buttons(test_data)
  expect_equal(length(buttons), 2)
  expect_true(grepl('onclick="Shiny.setInputValue\\(\'see_details\', 1, ', buttons[1]))
  expect_true(grepl('onclick="Shiny.setInputValue\\(\'show_on_map\', 2, ', buttons[2]))
})

test_that("create_taxa_vectors creates HTML taxa lists", {
  # Create test data
  plot_data <- data.frame(
    observation_id = c(1, 2),
    stringsAsFactors = FALSE
  )

  taxa_data <- data.frame(
    observation_id = c(1, 1, 2),
    int_currplantscinamenoauth = c("Taxon1", "Taxon2", NA),
    maxcover = c(10, 20, NA),
    accessioncode = c("ACC1", "ACC2", "ACC3"),
    stringsAsFactors = FALSE
  )

  result <- create_taxa_vectors(plot_data, taxa_data)
  expect_equal(length(result), 2)
  expect_true(grepl("taxa-list", result[1]))
  expect_true(grepl("Taxon1", result[1]))
  expect_true(grepl("No Taxa Data", result[2]))
})

test_that("create_community_vectors creates HTML community lists", {
  # Create test data
  plot_data <- data.frame(
    obsaccessioncode = c("ACC1", "ACC2"),
    stringsAsFactors = FALSE
  )

  comm_data <- data.frame(
    obsaccessioncode = c("ACC1", "ACC2"),
    commname = c("Community1", NA),
    commconceptaccessioncode = c("COMM1", NA),
    stringsAsFactors = FALSE
  )

  result <- create_community_vectors(plot_data, comm_data)
  expect_equal(length(result), 2)
  expect_true(grepl("comm-list", result[1]))
  expect_true(grepl("Community1", result[1]))
  expect_true(grepl("No Community Data", result[2]))
})

test_that("build_display_data creates correct data frame", {
  # Create test vectors
  author_codes <- c("A1", "A2")
  locations <- c("L1", "L2")
  taxa_html <- c("<taxa1>", "<taxa2>")
  community_html <- c("<comm1>", "<comm2>")
  action_buttons <- c("<btn1>", "<btn2>")

  result <- build_display_data(
    author_codes, locations, taxa_html, community_html, action_buttons
  )

  expect_equal(nrow(result), 2)
  expect_equal(colnames(result),
               c("Actions", "Author Plot Code", "Location", "Top Taxa", "Community"))
  expect_equal(result$Actions, action_buttons)
  expect_equal(result$`Author Plot Code`, author_codes)
  expect_equal(result$Location, locations)
  expect_equal(result$`Top Taxa`, taxa_html)
  expect_equal(result$Community, community_html)
})

test_that("process_table_data returns a DataTable with correct structure", {
  # Skip if no interactive session to avoid Shiny progress bar issues
  skip_if_not(interactive())

  # Create minimal test data
  plot_data <- data.frame(
    observation_id = 1,
    obsaccessioncode = "ACC1",
    authorplotcode = "Plot1",
    stateprovince = "State1",
    stringsAsFactors = FALSE
  )

  taxa_data <- data.frame(
    observation_id = 1,
    int_currplantscinamenoauth = "Taxon1",
    maxcover = 10,
    accessioncode = "ACC1",
    stringsAsFactors = FALSE
  )

  comm_data <- data.frame(
    obsaccessioncode = "ACC1",
    commname = "Community1",
    commconceptaccessioncode = "COMM1",
    stringsAsFactors = FALSE
  )

  testthat::with_mocked_bindings(
    {
      result <- process_table_data(plot_data, taxa_data, comm_data)
      expect_true(inherits(result, "datatables"))
      expect_equal(nrow(result$x$data), 1)
      expect_equal(ncol(result$x$data), 5)
    },
    withProgress = function(expr, ...) expr,
    setProgress = function(...) NULL,
    .package = "shiny"
  )
})
