# Tests for detail_view.R

# Create mock objects for testing
mock_output <- local({
  # Create empty output slots that can be assigned to
  environment()
})

mock_session <- list(
  sendCustomMessage = function(type, message) {
    mock_session$messages[[type]] <- message
  },
  messages = list()
)

# Mock test data
mock_plot_data <- list(
  authorobscode = "TEST123",
  authorplotcode = "Plot 123",
  latitude = 40.7128,
  longitude = -74.0060,
  locationnarrative = "Test location",
  stateprovince = "Test State",
  country = "Test Country",
  area = "10x10m",
  permanence = "Permanent",
  elevation = "100m",
  slopeaspect = "North",
  slopegradient = "10%",
  confidentialitytext = "Not confidential",
  obsstartdate = "2023-01-01",
  projectname = "Test Project",
  covertype = "Test Cover",
  stratummethodname = "Test Stratum Method",
  stratummethoddescription = "Test description",
  taxonobservationarea = "100 sq m",
  autotaxoncover = FALSE,
  plotvalidationleveldescr = "High quality"
)

mock_community_data <- list(
  data = list(
    commname = "Test Community",
    commdescription = "Test <em>description</em> with HTML",
    obscount = 42
  )
)

mock_taxon_data <- list(
  data = list(
    authorplantname = "Test Plant",
    int_currplantscinamenoauth = "Scientific name",
    int_currplantscifull = "Scientific name L.",
    int_origplantscinamenoauth = "Original name",
    int_origplantscifull = "Original name Auth.",
    int_currplantcommon = "Common plant",
    int_origplantcommon = "Old common name",
    maxcover = 75,
    taxoninferencearea = 100,
    taxonobservation_id = "TX123",
    int_currplantcode = "CODE1",
    int_origplantcode = "CODE2"
  )
)

test_that("build_plot_obs_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_plot_obs_details_view(NULL)

  # It should return a list with the expected UI components
  expect_type(result, "list")
  expect_named(result, c(
    "plot_id_details", "location_details", "layout_details",
    "environmental_details", "methods_details",
    "plot_quality_details", "taxa_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
})

test_that("build_plot_obs_details_view formats plot data correctly", {
  result <- build_plot_obs_details_view(mock_plot_data)

  # Test structure and types
  expect_type(result, "list")
  expect_named(result, c(
    "plot_id_details", "location_details", "layout_details",
    "environmental_details", "methods_details",
    "plot_quality_details", "taxa_details"
  ))

  # Each component should be a render function
  expect_true(inherits(result$plot_id_details, "shiny.render.function"))
  expect_true(inherits(result$location_details, "shiny.render.function"))
})

test_that("build_community_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_community_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c("community_name", "community_description", "occurence_count"))

  # Each component should be a render function
  expect_true(inherits(result$community_name, "shiny.render.function"))
})

test_that("build_community_details_view formats community data correctly", {
  result <- build_community_details_view(mock_community_data)

  # Test structure and types
  expect_type(result, "list")
  expect_named(result, c("community_name", "community_description", "occurence_count"))

  # Each component should be a render function
  expect_true(inherits(result$community_name, "shiny.render.function"))
  expect_true(inherits(result$community_description, "shiny.render.function"))
  expect_true(inherits(result$occurence_count, "shiny.render.function"))
})

test_that("build_taxon_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_taxon_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "taxon_name", "taxon_scientific", "taxon_common",
    "taxon_coverage", "taxon_identifiers"
  ))

  # Each component should be a render function
  expect_true(inherits(result$taxon_name, "shiny.render.function"))
})

test_that("build_taxon_details_view formats taxon data correctly", {
  result <- build_taxon_details_view(mock_taxon_data)

  # Test structure and types
  expect_type(result, "list")
  expect_named(result, c(
    "taxon_name", "taxon_scientific", "taxon_common",
    "taxon_coverage", "taxon_identifiers"
  ))

  # Each component should be a render function
  expect_true(inherits(result$taxon_name, "shiny.render.function"))
  expect_true(inherits(result$taxon_scientific, "shiny.render.function"))
})

test_that("show_detail_view handles API errors appropriately", {
  # Skip on CRAN
  skip_on_cran()

  # Assign veg_bank_api mock to global environment that returns an error
  local_mocked_bindings(
    veg_bank_api = list(
      get_observation_details = function(accession_code) {
        list(success = FALSE, error = "API Error", message = "Failed to retrieve plot details")
      }
    )
  )

  with_mock_shiny_notifications({
    result <- show_detail_view("plot", "TEST123", output, session)

    # Verify results
    expect_false(result) # Should return FALSE when API call fails

    # Check that an error notification was shown
    notifications <- get_mock_notifications()
    expect_true(length(notifications) > 0)

    # Verify the notification contains the error message
    error_notification <- notifications[[1]]
    expect_equal(error_notification$type, "error")
  })
})

test_that("show_detail_view handles success case for plot details", {
  # Skip on CRAN
  skip_on_cran()

  # Setup test environment
  messages_captured <- list()

  # Assign veg_bank_api mock to global environment
  local_mocked_bindings(
    veg_bank_api = list(
      get_observation_details = function(accession_code) {
        list(success = TRUE, data = mock_plot_data)
      }
    )
  )

  # Create a fake session
  session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )

  # Create a mock output with tracking
  output <- new.env()

  with_mock_shiny_notifications({
    result <- show_detail_view("plot", "TEST123", output, session)

    # Verify results
    expect_true(result)
    expect_true(!is.null(messages_captured$openOverlay))
    expect_true(!is.null(messages_captured$updateDetailType))
    expect_equal(messages_captured$updateDetailType$type, "plot")
  })
})
