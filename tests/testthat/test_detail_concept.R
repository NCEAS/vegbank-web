# Tests for concept detail views (community and plant)

test_that("build_comm_concept_details_view handles NULL data gracefully", {
  # When NULL data is provided
  result <- build_comm_concept_details_view(NULL)

  # It should return a list with placeholder components
  expect_type(result, "list")
  expect_named(result, c(
    "community_concept_header",
    "community_concept_details",
    "community_party_perspective"
  ))

  # Each component should be a render function
  expect_true(inherits(result$community_concept_header, "shiny.render.function"))
})

test_that("build_comm_concept_details_view formats community data correctly", {
  # CEGL007230: fully populated — description, parent, correlations, 5 usages
  result <- build_comm_concept_details_view(mock_comm_concept_cegl007230)
  expect_type(result, "list")
  expect_named(result, c(
    "community_concept_header",
    "community_concept_details",
    "community_party_perspective"
  ))
  expect_true(inherits(result$community_concept_header, "shiny.render.function"))
  expect_true(inherits(result$community_concept_details, "shiny.render.function"))
  expect_true(inherits(result$community_party_perspective, "shiny.render.function"))

  # Brachypodium: no parent, no children, no description, status="undetermined"
  result_b <- build_comm_concept_details_view(mock_comm_concept_brachypodium)
  expect_type(result_b, "list")
  expect_named(result_b, c(
    "community_concept_header",
    "community_concept_details",
    "community_party_perspective"
  ))
  expect_true(inherits(result_b$community_concept_header, "shiny.render.function"))

  # VII: has children, has stop_date (superseded), no parent, obs_count=0
  result_vii <- build_comm_concept_details_view(mock_comm_concept_vii)
  expect_type(result_vii, "list")
  expect_named(result_vii, c(
    "community_concept_header",
    "community_concept_details",
    "community_party_perspective"
  ))
  expect_true(inherits(result_vii$community_concept_header, "shiny.render.function"))
})

test_that("build_comm_concept_details_view header includes copy permalink button", {
  result <- build_comm_concept_details_view(mock_comm_concept_cegl007230)
  mock_session <- shiny::MockShinySession$new()
  html <- htmltools::renderTags(result$community_concept_header(shinysession = mock_session))$html

  expect_true(grepl("vb-copy-permalink", html, fixed = TRUE))
  expect_true(grepl("Copy permalink", html, fixed = TRUE))
  expect_true(grepl("vegbank.org/cite/cc.47882", html, fixed = TRUE))
})

test_that("build_plant_concept_details_view handles valid data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- build_plant_concept_details_view(mock_plant_concept_data)

      expect_type(result, "list")
      expect_equal(
        names(result),
        c("plant_concept_header", "plant_concept_details", "plant_party_perspective")
      )
      expect_s3_class(result$plant_concept_header, "shiny.render.function")
      expect_s3_class(result$plant_concept_details, "shiny.render.function")
      expect_s3_class(result$plant_party_perspective, "shiny.render.function")
    }
  )
})

test_that("build_plant_concept_details_view handles null/empty data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      # Test with NULL data
      result_null <- build_plant_concept_details_view(NULL)
      expect_type(result_null, "list")
      expect_equal(
        names(result_null),
        c("plant_concept_header", "plant_concept_details", "plant_party_perspective")
      )

      # Test with empty data frame
      empty_data <- data.frame()
      result_empty <- build_plant_concept_details_view(empty_data)
      expect_type(result_empty, "list")
      expect_equal(
        names(result_empty),
        c("plant_concept_header", "plant_concept_details", "plant_party_perspective")
      )
    }
  )
})

test_that("create_party_perspective_ui handles valid JSON data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- create_party_perspective_ui(
        mock_plant_concept_data,
        "plant",
        "pc_code",
        "parent_pc_code",
        "plant_link_click"
      )
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_party_perspective_ui handles invalid JSON", {
  invalid_json_data <- mock_plant_concept_data
  invalid_json_data$children <- I(list('{"unclosed":'))
  invalid_json_data$usages <- I(list('{"unclosed":'))

  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      # Should not throw error even with invalid JSON
      result <- create_party_perspective_ui(
        invalid_json_data,
        "plant",
        "pc_code",
        "parent_pc_code",
        "plant_link_click"
      )
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_party_perspective_ui handles NA values", {
  na_data <- mock_plant_concept_data
  na_data$children <- I(list(NA))
  na_data$usages <- I(list(NA))
  na_data$party_label <- NA
  na_data$status <- NA
  na_data$start_date <- NA
  na_data$stop_date <- NA
  na_data$parent_name <- NA

  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- create_party_perspective_ui(
        na_data,
        "plant",
        "pc_code",
        "parent_pc_code",
        "plant_link_click"
      )
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_concept_aliases_ui handles plant usages", {
  result <- create_concept_aliases_ui(mock_plant_concept_data, is_plant = TRUE)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that("create_concept_aliases_ui handles invalid usage data", {
  invalid_data <- mock_plant_concept_data
  invalid_data$usages <- I(list('{"invalid": json}'))
  result <- create_concept_aliases_ui(invalid_data, is_plant = TRUE)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

test_that("create_concept_aliases_ui handles NA/empty usages", {
  na_data <- mock_plant_concept_data
  na_data$usages <- I(list(NA))
  expect_true(inherits(create_concept_aliases_ui(na_data, is_plant = TRUE), "shiny.tag"))

  empty_data <- mock_plant_concept_data
  empty_data$usages <- I(list(NULL))
  expect_true(inherits(create_concept_aliases_ui(empty_data, is_plant = TRUE), "shiny.tag"))
})

test_that("create_concept_aliases_ui sorts usages alphabetically", {
  sorted_data <- mock_plant_concept_data
  sorted_data$usages <- I(list(data.frame(
    class_system = c("Zebra", "Alpha", "Beta"),
    plant_name = c("Last name", "First name", "Second name"),
    stringsAsFactors = FALSE
  )))

  result <- create_concept_aliases_ui(sorted_data, is_plant = TRUE)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})

# ── Community equivalents: create_party_perspective_ui ────────────────────────

test_that("create_party_perspective_ui handles valid community data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      # CEGL007230: has party, parent, correlations, 5 usages, no children
      result <- create_party_perspective_ui(
        mock_comm_concept_cegl007230,
        "community",
        "cc_code",
        "parent_cc_code",
        "comm_link_click"
      )
      expect_s3_class(result, "shiny.render.function")

      # VII: has children (3), no parent, stop_date set
      result_vii <- create_party_perspective_ui(
        mock_comm_concept_vii,
        "community",
        "cc_code",
        "parent_cc_code",
        "comm_link_click"
      )
      expect_s3_class(result_vii, "shiny.render.function")
    }
  )
})

test_that("create_party_perspective_ui handles minimal community data", {
  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      # Brachypodium: no parent, no children, status="undetermined", no stop_date
      result <- create_party_perspective_ui(
        mock_comm_concept_brachypodium,
        "community",
        "cc_code",
        "parent_cc_code",
        "comm_link_click"
      )
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

test_that("create_party_perspective_ui handles NA community values", {
  na_data <- mock_comm_concept_cegl007230
  na_data$children <- I(list(NA))
  na_data$usages <- I(list(NA))
  na_data$party_label <- NA
  na_data$status <- NA
  na_data$start_date <- NA
  na_data$stop_date <- NA
  na_data$parent_name <- NA

  with_mocked_bindings(
    renderUI = function(expr) {
      structure(list(func = expr), class = "shiny.render.function")
    },
    .package = "shiny",
    {
      result <- create_party_perspective_ui(
        na_data,
        "community",
        "cc_code",
        "parent_cc_code",
        "comm_link_click"
      )
      expect_s3_class(result, "shiny.render.function")
    }
  )
})

# ── Community equivalents: create_concept_aliases_ui ─────────────────────────

test_that("create_concept_aliases_ui handles community usages", {
  # CEGL007230 has 5 usages (Scientific, Common, Translated, Code, UID)
  result <- create_concept_aliases_ui(mock_comm_concept_cegl007230, is_plant = FALSE)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))

  # VII has 2 usages (Code, Scientific)
  result_vii <- create_concept_aliases_ui(mock_comm_concept_vii, is_plant = FALSE)
  expect_true(inherits(result_vii, "shiny.tag") || inherits(result_vii, "shiny.tag.list"))
})

test_that("create_concept_aliases_ui handles NA/empty community usages", {
  na_data <- mock_comm_concept_cegl007230
  na_data$usages <- I(list(NA))
  expect_true(inherits(create_concept_aliases_ui(na_data, is_plant = FALSE), "shiny.tag"))

  empty_data <- mock_comm_concept_cegl007230
  empty_data$usages <- I(list(NULL))
  expect_true(inherits(create_concept_aliases_ui(empty_data, is_plant = FALSE), "shiny.tag"))
})

test_that("create_concept_aliases_ui sorts community usages alphabetically", {
  sorted_data <- mock_comm_concept_cegl007230
  sorted_data$usages <- I(list(data.frame(
    class_system = c("Zebra", "Alpha", "Beta"),
    comm_name = c("Last Community", "First Community", "Second Community"),
    stringsAsFactors = FALSE
  )))

  result <- create_concept_aliases_ui(sorted_data, is_plant = FALSE)
  expect_true(inherits(result, "shiny.tag") || inherits(result, "shiny.tag.list"))
})
