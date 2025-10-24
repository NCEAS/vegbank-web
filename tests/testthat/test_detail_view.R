# Tests for show_detail_view router function
# Mock data is provided by helper-detail-mocks.R

test_that("show_detail_view handles API errors appropriately", {
  skip_on_cran()

  test_output <- new.env()
  test_session <- list(
    sendCustomMessage = function(type, message) {},
    messages = list()
  )

  with_mocked_bindings(
    get_plot_observation_details = function(ob_code) list(),
    get_community_concept = function(cc_code) list(),
    get_taxon_observation = function(to_code) list(),
    .package = "vegbankr",
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("plot-observation", "TEST123", test_output, test_session)
        expect_false(result)

        notifications <- get_mock_notifications()
        expect_true(length(notifications) > 0)

        error_notification <- notifications[[1]]
        expect_equal(error_notification$type, "error")
      })
    }
  )
})

test_that("show_detail_view handles success case for plot details", {
  skip_on_cran()

  messages_captured <- list()
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )
  fake_output <- new.env()

  with_mocked_bindings(
    get_plot_observation_details = function(ob_code) mock_plot_data,
    get_community_concept = function(cc_code) list(),
    get_taxon_observation = function(to_code) list(),
    .package = "vegbankr",
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("plot-observation", "TEST123", fake_output, fake_session)
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "plot-observation")
      })
    }
  )
})

test_that("show_detail_view handles project data correctly", {
  skip_on_cran()

  messages_captured <- list()
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )
  fake_output <- new.env()

  with_mocked_bindings(
    get_project = function(pj_code) mock_project_data,
    .package = "vegbankr",
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("project", "PROJ123", fake_output, fake_session)
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "project")
      })
    }
  )
})

test_that("show_detail_view handles community classification data correctly", {
  skip_on_cran()

  messages_captured <- list()
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )
  fake_output <- new.env()

  with_mocked_bindings(
    get_community_classification = function(cl_code) mock_comm_class_data,
    .package = "vegbankr",
    {
      with_mock_shiny_notifications({
        result <- show_detail_view(
          "community-classification",
          "CLASS123",
          fake_output,
          fake_session
        )
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "community-classification")
      })
    }
  )
})

test_that("show_detail_view handles party data correctly", {
  skip_on_cran()

  messages_captured <- list()
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )
  fake_output <- new.env()

  with_mocked_bindings(
    get_party = function(py_code) mock_party_data,
    .package = "vegbankr",
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("party", "PARTY123", fake_output, fake_session)
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "party")
      })
    }
  )
})

test_that("show_detail_view handles reference data correctly", {
  # Skip on CRAN
  skip_on_cran()

  messages_captured <- list()
  fake_session <- list(
    sendCustomMessage = function(type, message) {
      messages_captured[[type]] <<- message
    }
  )
  fake_output <- new.env()

  with_mocked_bindings(
    get_reference = function(rf_code) mock_reference_data,
    .package = "vegbankr",
    {
      with_mock_shiny_notifications({
        result <- show_detail_view("reference", "RF123", fake_output, fake_session)
        expect_true(result)
        expect_true(!is.null(messages_captured$openOverlay))
        expect_true(!is.null(messages_captured$updateDetailType))
        expect_equal(messages_captured$updateDetailType$type, "reference")
      })
    }
  )
})

test_that("show_detail_view works with plant-concept type", {
  mock_get_plant_concept <- function(code) {
    if (code == "pc.12345") {
      return(mock_plant_concept_data)
    }
    data.frame()
  }

  result <- mock_get_plant_concept("pc.12345")
  expect_true(nrow(result) > 0)
  expect_equal(result$pc_code, "pc.12345")
})

test_that("show_detail_view handles missing plant concept data", {
  mock_get_plant_concept_empty <- function(code) {
    data.frame()
  }

  result <- mock_get_plant_concept_empty("nonexistent")
  expect_true(nrow(result) == 0)
})
