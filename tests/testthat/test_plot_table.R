test_that("create_empty_table returns a DataTable object", {
  # Test basic functionality
  empty_table <- create_empty_table()
  expect_true(inherits(empty_table, "datatables"))
  expect_true(inherits(empty_table$x$data, "data.frame"))
  expect_equal(colnames(empty_table$x$data), "No.Data.Available")

  # Test with a custom message
  custom_table <- create_empty_table("Custom message")
  expect_true(inherits(custom_table, "datatables"))
  expect_equal(colnames(custom_table$x$data), "No.Data.Available")
  expect_equal(custom_table$x$data$`No.Data.Available`, "Custom message")
})


test_that("clean_column_data handles missing columns and values", {
  test_data <- data.frame(
    col1 = c("value1", NA, ""),
    stringsAsFactors = FALSE
  )

  # Test with existing column
  result <- clean_column_data(test_data, "col1")
  expect_equal(result, c("Value1", "Not provided", "Not provided"))

  # Test with non-existent column
  result <- clean_column_data(test_data, "col2")
  expect_equal(result, c("Not provided", "Not provided", "Not provided"))
})

test_that("create_plot_action_buttons generates correct structure", {
  # Create test data with required structure
  test_data <- data.frame(
    ob_code = c("ob.2948", "ob.2949"),
    latitude = c(40.1, 41.2),
    longitude = c(-74.1, -75.2),
    stringsAsFactors = FALSE
  )

  result <- create_plot_action_buttons(test_data)

  expect_equal(length(result), 2)
  expect_type(result, "list")
  expect_equal(result[[1]]$code, "ob.2948")
  expect_equal(result[[1]]$latitude, 40.1)
  expect_equal(result[[1]]$longitude, -74.1)
  expect_equal(result[[2]]$code, "ob.2949")
  expect_equal(result[[2]]$latitude, 41.2)
  expect_equal(result[[2]]$longitude, -75.2)
})

test_that("create_taxa_vectors creates correct data structure", {
  # Create test data with the correct column structure
  plot_data <- data.frame(
    ob_code = c("ob.2948", "ob.2949"),
    stringsAsFactors = FALSE
  )

  taxa_data <- data.frame(
    ob_code = c("ob.2948", "ob.2948", "ob.2949"),
    int_curr_plant_sci_name_no_auth = c("Taxon1", "Taxon2", NA),
    max_cover = c(10.123, 20.456, NA),
    to_code = c("ACC1", "ACC2", "ACC3"),
    stringsAsFactors = FALSE
  )

  result <- create_taxa_vectors(plot_data, taxa_data)

  expect_equal(length(result), 2)
  expect_type(result, "list")

  # Check first observation (has taxa) - sorted by max_cover descending
  expect_equal(length(result[[1]]), 2)
  expect_equal(result[[1]][[1]]$code, "ACC2") # Higher cover (20.46) comes first
  expect_equal(result[[1]][[1]]$name, "Taxon2")
  expect_equal(result[[1]][[1]]$cover, "20.46") # formatted with 2 decimal places
  expect_equal(result[[1]][[2]]$code, "ACC1") # Lower cover (10.12) comes second
  expect_equal(result[[1]][[2]]$name, "Taxon1")
  expect_equal(result[[1]][[2]]$cover, "10.12")

  # Check second observation (no valid taxa data)
  expect_equal(length(result[[2]]), 0)
})

test_that("create_community_vectors creates correct data structure", {
  # Create test data with the exact column names needed
  plot_data <- data.frame(
    ob_code = c("ob.2948", "ob.2949"),
    stringsAsFactors = FALSE
  )

  comm_data <- data.frame(
    ob_code = c("ob.2948", "ob.2949"),
    comm_name = c("Community1", NA),
    cl_code = c("COMM1", "COMM2"),
    stringsAsFactors = FALSE
  )

  result <- create_community_vectors(plot_data, comm_data)

  expect_equal(length(result), 2)
  expect_type(result, "list")

  # Check first observation (has community data)
  expect_equal(length(result[[1]]), 1)
  expect_equal(result[[1]][[1]]$code, "COMM1")
  expect_equal(result[[1]][[1]]$name, "Community1")

  # Check second observation (has community code but no name)
  expect_equal(length(result[[2]]), 1)
  expect_equal(result[[2]][[1]]$code, "COMM2")
  expect_true(is.na(result[[2]][[1]]$name))
})

test_that("process_plot_data formats display data correctly", {
  # Create minimal test data with required columns
  plot_data <- data.frame(
    ob_code = "ob.2948",
    author_plot_code = "Plot1",
    state_province = "State1",
    stringsAsFactors = FALSE
  )

  taxa_data <- data.frame(
    ob_code = "ob.2948",
    int_curr_plant_sci_name_no_auth = "Taxon1",
    max_cover = 10.5,
    to_code = "TAXA1",
    stringsAsFactors = FALSE
  )

  comm_data <- data.frame(
    ob_code = "ob.2948",
    comm_name = "Community1",
    cl_code = "COMM1",
    stringsAsFactors = FALSE
  )

  # Mock the shiny functions
  with_mocked_bindings(
    incProgress = function(...) NULL,
    .package = "shiny",
    {
      # Call the function
      data_sources <- list(plot_data = plot_data, taxa_data = taxa_data, comm_data = comm_data)
      result <- process_plot_data(data_sources)

      # Verify the output structure
      expect_s3_class(result, "data.frame")
      expect_equal(nrow(result), 1)
      expect_equal(ncol(result), 5)
      expect_equal(
        colnames(result),
        c("Actions", "Author Plot Code", "Location", "Top Taxa by Cover %", "Community")
      )
      expect_equal(result$`Author Plot Code`, "Plot1")
      expect_equal(result$Location, "State1")

      # Check that Actions column contains list data
      expect_type(result$Actions, "list")
      expect_equal(result$Actions[[1]]$code, "ob.2948")

      # Check that Taxa column contains list data
      expect_type(result$`Top Taxa by Cover %`, "list")
      expect_equal(length(result$`Top Taxa by Cover %`[[1]]), 1)
      expect_equal(result$`Top Taxa by Cover %`[[1]][[1]]$name, "Taxon1")
      expect_equal(result$`Top Taxa by Cover %`[[1]][[1]]$cover, "10.50")

      # Check that Community column contains list data
      expect_type(result$Community, "list")
      expect_equal(length(result$Community[[1]]), 1)
      expect_equal(result$Community[[1]][[1]]$name, "Community1")
    }
  )
})

test_that("build_plot_table returns a DataTable with correct structure", {
  # Create minimal test data with all required columns
  plot_data <- data.frame(
    ob_code = "ob.2948",
    author_plot_code = "Plot1",
    state_province = "State1",
    stringsAsFactors = FALSE
  )

  taxa_data <- data.frame(
    ob_code = "ob.2948",
    int_curr_plant_sci_name_no_auth = "Taxon1",
    max_cover = 10.25,
    to_code = "TAXA1",
    stringsAsFactors = FALSE
  )

  comm_data <- data.frame(
    ob_code = "ob.2948",
    comm_name = "Community1",
    cl_code = "COMM1",
    stringsAsFactors = FALSE
  )

  # Mock shiny functionality and the create_table function
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    withProgress = function(expr, ...) force(expr),
    incProgress = function(...) NULL,
    .package = "shiny",
    {
      with_mocked_bindings(
        create_table = function(data_sources, required_sources, process_function, table_config) {
          # Verify inputs
          expect_equal(names(data_sources), c("plot_data", "taxa_data", "comm_data"))
          expect_equal(required_sources, c("plot_data", "taxa_data", "comm_data"))
          expect_equal(names(table_config), c("column_defs", "progress_message"))

          # Verify table config structure
          expect_type(table_config$column_defs, "list")
          expect_equal(length(table_config$column_defs), 5) # 5 column definitions
          expect_equal(table_config$progress_message, "Processing table data:")

          # Check that column definitions have the expected structure
          expect_equal(table_config$column_defs[[1]]$targets, 0) # Actions column
          expect_equal(table_config$column_defs[[1]]$orderable, FALSE)
          expect_equal(table_config$column_defs[[1]]$searchable, FALSE)
          expect_true(inherits(table_config$column_defs[[1]]$render, "JS_EVAL"))

          expect_equal(table_config$column_defs[[4]]$targets, 3) # Taxa column
          expect_equal(table_config$column_defs[[4]]$orderable, FALSE)
          expect_equal(table_config$column_defs[[4]]$searchable, TRUE)
          expect_true(inherits(table_config$column_defs[[4]]$render, "JS_EVAL"))

          expect_equal(table_config$column_defs[[5]]$targets, 4) # Community column
          expect_true(inherits(table_config$column_defs[[5]]$render, "JS_EVAL"))

          # Process data and create a mock DT object
          processed_data <- process_function(data_sources)
          structure(list(x = list(data = processed_data)), class = "datatables")
        },
        .env = pkg_env,
        {
          result <- build_plot_table(plot_data, taxa_data, comm_data)
          expect_s3_class(result, "datatables")
          expect_equal(ncol(result$x$data), 5)
          expect_true(all(c("Actions", "Author Plot Code", "Location", "Top Taxa by Cover %", "Community") %in%
            colnames(result$x$data)))
        }
      )
    }
  )
})

test_that("create_taxa_vectors handles edge cases correctly", {
  plot_data <- data.frame(
    ob_code = c("ob.2947", "ob.2948", "ob.2949"),
    stringsAsFactors = FALSE
  )

  # Test with completely empty taxa data
  taxa_data_empty <- data.frame(
    ob_code = character(0),
    int_curr_plant_sci_name_no_auth = character(0),
    max_cover = numeric(0),
    to_code = character(0),
    stringsAsFactors = FALSE
  )

  result_empty <- create_taxa_vectors(plot_data, taxa_data_empty)
  expect_equal(length(result_empty), 3)
  expect_equal(length(result_empty[[1]]), 0) # Empty list
  expect_equal(length(result_empty[[2]]), 0)
  expect_equal(length(result_empty[[3]]), 0)

  # Test with all NA values
  taxa_data_na <- data.frame(
    ob_code = c("ob.2948", "ob.2949"),
    int_curr_plant_sci_name_no_auth = c(NA, NA),
    max_cover = c(NA, NA),
    to_code = c("ACC1", "ACC2"),
    stringsAsFactors = FALSE
  )

  result_na <- create_taxa_vectors(plot_data, taxa_data_na)
  expect_equal(length(result_na), 3)
  expect_equal(length(result_na[[1]]), 0) # Should be empty due to all NA values
  expect_equal(length(result_na[[2]]), 0)
})

test_that("create_community_vectors handles edge cases correctly", {
  plot_data <- data.frame(
    ob_code = c("ob.2947", "ob.2948", "ob.2949"),
    stringsAsFactors = FALSE
  )

  # Test with completely empty community data
  comm_data_empty <- data.frame(
    ob_code = character(0),
    comm_name = character(0),
    cl_code = character(0),
    stringsAsFactors = FALSE
  )

  result_empty <- create_community_vectors(plot_data, comm_data_empty)
  expect_equal(length(result_empty), 3)
  expect_equal(length(result_empty[[1]]), 0)
  expect_equal(length(result_empty[[2]]), 0)
  expect_equal(length(result_empty[[3]]), 0)

  # Test with all NA values
  comm_data_na <- data.frame(
    ob_code = c("ob.2948", "ob.2949"),
    comm_name = c(NA, NA),
    cl_code = c(NA, NA),
    stringsAsFactors = FALSE
  )

  result_na <- create_community_vectors(plot_data, comm_data_na)
  expect_equal(length(result_na), 3)
  expect_equal(length(result_na[[1]]), 0) # Should be empty due to all NA values
  expect_equal(length(result_na[[2]]), 0)
  expect_equal(length(result_na[[3]]), 0) # No matching ob_code
})
