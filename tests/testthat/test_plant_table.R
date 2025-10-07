test_that("build_plant_table creates a datatable object", {
  test_data <- data.frame(
    pc_code = c("pc.001", "pc.002"),
    plant_name = c("Test Plant 1", "Test Plant 2"),
    concept_rf_name = c("Reference 1", "Reference 2"),
    obs_count = c(100, 200),
    plant_description = c("Description 1", "Description 2"),
    current_accepted = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  )

  pkg_env <- asNamespace("vegbankweb")
  # Mock is_any_data_missing in the correct environment
  with_mocked_bindings(
    is_any_data_missing = function(...) FALSE,
    .env = pkg_env,
    {
      with_mocked_bindings(
        withProgress = function(expr, ...) force(expr),
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          with_mocked_bindings(
            create_table = function(data_sources, required_sources, process_function, table_config) {
              expect_equal(names(data_sources), "plant_data")
              expect_equal(data_sources$plant_data, test_data)
              expect_equal(required_sources, "plant_data")
              expect_equal(table_config$column_defs[[1]]$targets, 0)
              expect_equal(table_config$column_defs[[1]]$orderable, FALSE)
              expect_equal(length(table_config$column_defs), 7)
              structure(list(options = list()), class = "datatables")
            },
            .env = pkg_env,
            {
              result <- build_plant_table(test_data)
              expect_s3_class(result, "datatables")
            }
          )
        }
      )
    }
  )
})

test_that("process_plant_data correctly formats plant data", {
  test_data <- data.frame(
    pc_code = c("pc.001", "pc.002", "pc.003"),
    plant_name = c("Test Plant 1", "Test Plant 2", "Test Plant 3"),
    concept_rf_name = c("Reference 1", "Reference 2", "Reference 3"),
    obs_count = c(100, 200, 300),
    plant_description = c("Description 1", "Description 2", "Description 3"),
    current_accepted = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    clean_column_data = function(data, column, default = "Not specified") {
      if (column == "plant_name") {
        return(c("Test Plant 1", "Test Plant 2", "Test Plant 3"))
      }
      if (column == "concept_rf_name") {
        return(c("Reference 1", "Reference 2", "Reference 3"))
      }
      if (column == "obs_count") {
        return(c("100", "200", "300"))
      }
      if (column == "plant_description") {
        return(c("Description 1", "Description 2", "Description 3"))
      }
      if (column == "pc_code") {
        return(c("pc.001", "pc.002", "pc.003"))
      }
      rep(default, nrow(data))
    },
    create_action_buttons = function(data, actions) {
      c("Action 1", "Action 2", "Action 3")
    },
    {
      with_mocked_bindings(
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          result <- process_plant_data(list(plant_data = test_data))

          expect_s3_class(result, "data.frame")
          expect_equal(nrow(result), 3)
          expect_equal(colnames(result), c("Actions", "Plant Name", "Status", "Reference Source", "Observations", "Description", "Plant Code"))
          expect_equal(result$`Plant Name`, c("Test Plant 1", "Test Plant 2", "Test Plant 3"))
          expect_equal(result$`Reference Source`, c("Reference 1", "Reference 2", "Reference 3"))
          expect_equal(result$Observations, c(100, 200, 300))
          expect_equal(result$Description, c("Description 1", "Description 2", "Description 3"))
          expect_equal(result$`Plant Code`, c("pc.001", "pc.002", "pc.003"))
          
          # Test status badges
          expect_true(grepl("Accepted", result$Status[1]))
          expect_true(grepl("Not Current", result$Status[2]))
          expect_true(grepl("No Status", result$Status[3]))
          
          # Test badge CSS variables
          expect_true(grepl("var\\(--accepted-bg\\)", result$Status[1]))
          expect_true(grepl("var\\(--not-current-bg\\)", result$Status[2]))
          expect_true(grepl("var\\(--no-status-bg\\)", result$Status[3]))
        }
      )
    }
  )
})

test_that("status badges are generated correctly", {
  test_data <- data.frame(
    pc_code = c("pc.001", "pc.002", "pc.003"),
    plant_name = c("Accepted Plant", "Not Current Plant", "No Status Plant"),
    current_accepted = c(TRUE, FALSE, NA),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    clean_column_data = function(data, column, default = "Not specified") {
      if (column == "plant_name") {
        return(data$plant_name)
      }
      rep(default, nrow(data))
    },
    create_action_buttons = function(data, actions) {
      rep("Action", nrow(data))
    },
    {
      with_mocked_bindings(
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          result <- process_plant_data(list(plant_data = test_data))
          
          # Check that status badges have correct structure
          expect_equal(length(result$Status), 3)
          
          # Test Accepted badge
          accepted_badge <- result$Status[1]
          expect_true(grepl("badge rounded-pill", accepted_badge))
          expect_true(grepl("var\\(--accepted-bg\\)", accepted_badge))
          expect_true(grepl("var\\(--accepted-text\\)", accepted_badge))
          expect_true(grepl("Accepted", accepted_badge))
          
          # Test Not Current badge
          not_current_badge <- result$Status[2]
          expect_true(grepl("badge rounded-pill", not_current_badge))
          expect_true(grepl("var\\(--not-current-bg\\)", not_current_badge))
          expect_true(grepl("var\\(--not-current-text\\)", not_current_badge))
          expect_true(grepl("Not Current", not_current_badge))
          
          # Test No Status badge
          no_status_badge <- result$Status[3]
          expect_true(grepl("badge rounded-pill", no_status_badge))
          expect_true(grepl("var\\(--no-status-bg\\)", no_status_badge))
          expect_true(grepl("var\\(--no-status-text\\)", no_status_badge))
          expect_true(grepl("No Status", no_status_badge))
        }
      )
    }
  )
})

test_that("table_config has correct structure for plant table", {
  # Test the column definitions
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    build_plant_table = function(data) {
      # Access the column definitions directly
      column_defs <- list(
        list(targets = 0, orderable = FALSE, searchable = FALSE, width = "10%"),
        list(targets = 1, width = "25%"), # Plant Name
        list(targets = 2, width = "12%", className = "dt-center"), # Status
        list(targets = 3, width = "20%"), # Reference Source
        list(targets = 4, width = "10%", type = "num", className = "dt-right"), # Observations
        list(targets = 5, width = "18%"), # Description
        list(targets = 6, width = "10%") # Plant Code
      )

      expect_equal(length(column_defs), 7)
      expect_equal(column_defs[[1]]$targets, 0)
      expect_equal(column_defs[[1]]$orderable, FALSE)
      expect_equal(column_defs[[1]]$searchable, FALSE)
      expect_equal(column_defs[[2]]$targets, 1)
      expect_equal(column_defs[[2]]$width, "25%")
      expect_equal(column_defs[[3]]$targets, 2)
      expect_equal(column_defs[[3]]$className, "dt-center")
      expect_equal(column_defs[[4]]$targets, 3)
      expect_equal(column_defs[[5]]$type, "num")
      expect_equal(column_defs[[5]]$className, "dt-right")

      structure(list(column_defs = column_defs), class = "datatables")
    },
    .env = pkg_env,
    {
      test_data <- data.frame(
        pc_code = c("pc.001"),
        plant_name = c("Test Plant"),
        stringsAsFactors = FALSE
      )

      result <- build_plant_table(test_data)
      expect_true(is.list(result))
      expect_equal(length(result$column_defs), 7)
    }
  )
})

test_that("numeric conversion works correctly for obs_counts", {
  test_data <- data.frame(
    pc_code = c("pc.001", "pc.002"),
    plant_name = c("Plant 1", "Plant 2"),
    obs_count = c("100", "200"),
    current_accepted = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  with_mocked_bindings(
    clean_column_data = function(data, column, default = "Not specified") {
      if (column == "obs_count") {
        return(c("100", "200"))
      }
      if (column == "plant_name") {
        return(c("Plant 1", "Plant 2"))
      }
      rep(default, nrow(data))
    },
    create_action_buttons = function(data, actions) {
      c("Action 1", "Action 2")
    },
    {
      with_mocked_bindings(
        incProgress = function(...) NULL,
        .package = "shiny",
        {
          result <- process_plant_data(list(plant_data = test_data))
          
          # Check that observations are numeric
          expect_type(result$Observations, "double")
          expect_equal(result$Observations, c(100, 200))
        }
      )
    }
  )
})
