test_that("plot schema template returns correct structure", {
  result <- vegbankweb:::PLOT_TABLE_SCHEMA_TEMPLATE

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), length(PLOT_TABLE_FIELDS))
  expect_equal(names(result), PLOT_TABLE_FIELDS)

  # Check nested list columns
  expect_type(result$top_taxon_observations, "list")
  expect_type(result$top_classifications, "list")
})

test_that("serialize_nested_column converts data frames to JSON strings", {
  nested_data <- list(
    data.frame(to_code = "to.1", scientific_name = "Species A", stringsAsFactors = FALSE),
    data.frame(to_code = "to.2", scientific_name = "Species B", stringsAsFactors = FALSE),
    NULL,
    data.frame()
  )

  result <- serialize_nested_column(nested_data)

  expect_type(result, "character")
  expect_equal(length(result), 4)
  expect_false(result[1] == "[]")
  expect_false(result[2] == "[]")
  expect_equal(result[3], "[]")
  expect_equal(result[4], "[]")

  # Verify JSON can be parsed back
  parsed1 <- jsonlite::fromJSON(result[1])
  expect_equal(parsed1$to_code, "to.1")
  expect_equal(parsed1$scientific_name, "Species A")
})

test_that("serialize_nested_column handles NULL and non-list inputs", {
  result_null <- serialize_nested_column(NULL)
  expect_type(result_null, "character")
  expect_equal(length(result_null), 0)

  # Non-list input should generate a warning and return empty arrays
  expect_warning(
    result_non_list <- serialize_nested_column(c("a", "b")),
    "expected a list"
  )
  expect_type(result_non_list, "character")
  expect_equal(result_non_list, c("[]", "[]"))
})

test_that("serialize_taxa_payload encodes totals and rows", {
  payload <- vegbankweb:::serialize_taxa_payload(
    data.frame(name = "Species A", pc_code = "pc.1", stringsAsFactors = FALSE),
    total_count = 4
  )

  parsed <- jsonlite::fromJSON(payload)
  expect_equal(parsed$total, 4)
  expect_equal(parsed$items$name, "Species A")

  payload_no_total <- vegbankweb:::serialize_taxa_payload(NULL, NA)
  parsed_no_total <- jsonlite::fromJSON(payload_no_total)
  expect_null(parsed_no_total$total)
  empty_items <- parsed_no_total$items
  expect_true(
    is.null(empty_items) ||
      (is.data.frame(empty_items) && nrow(empty_items) == 0) ||
      (is.list(empty_items) && length(empty_items) == 0)
  )
})

test_that("format_location_column combines location, coordinates, and elevation", {
  data <- data.frame(
    state_province = c("Virginia", "Maryland", NA, "Texas"),
    country = c("USA", "USA", "Canada", NA),
    stringsAsFactors = FALSE
  )
  lats <- c(38.5, 39.2, 45.0, NA)
  lngs <- c(-78.2, -77.0, -75.0, NA)
  elevs <- c(500, 250, NA, 120)

  result <- format_location_column(data, lats, lngs, elevs)

  expect_equal(
    result[1],
    "Virginia<br><span class=\"text-muted small\">USA</span><br><span class=\"text-muted small\">38.5000, -78.2000</span><br><span class=\"text-muted small\">Elevation: 500m</span>"
  )
  expect_equal(
    result[2],
    "Maryland<br><span class=\"text-muted small\">USA</span><br><span class=\"text-muted small\">39.2000, -77.0000</span><br><span class=\"text-muted small\">Elevation: 250m</span>"
  )
  expect_equal(
    result[3],
    "<span class=\"text-muted small\">Canada</span><br><span class=\"text-muted small\">45.0000, -75.0000</span>"
  )
  expect_equal(
    result[4],
    "Texas<br><span class=\"text-muted small\">Elevation: 120m</span>"
  )
})

test_that("format_location_column handles missing data", {
  data <- data.frame(
    state_province = c(NA, ""),
    country = c(NA, ""),
    stringsAsFactors = FALSE
  )
  lats <- c(NA, NA)
  lngs <- c(NA, NA)
  elevs <- c(NA, NA)

  result <- format_location_column(data, lats, lngs, elevs)

  expect_equal(result[1], "Unspecified")
  expect_equal(result[2], "Unspecified")
})

test_that("process_plot_data returns correctly formatted display data", {
  # The table uses detail="minimal" which returns name/pc_code/max_cover in top_taxon_observations
  # (different from the full detail view's plant_name/cover columns).
  # Build a copy of the shared mock with table-appropriate nested column names,
  # using the same real plant data (same pc_codes, same cover values) as the detail-view mocks.
  plot_data <- mock_plot_observations_multi
  plot_data$top_taxon_observations <- list(
    # ACAD.143 (ob.2948) — Vaccinium dwarf-shrubland: stratum-specific rows only
    data.frame(
      name      = c("Aronia melanocarpa", "Bryophyta", "Danthonia spicata"),
      pc_code   = c("pc.10653", "pc.92211", "pc.22506"),
      max_cover = c(0.1875, 0.5625, 0.0625),
      stringsAsFactors = FALSE
    ),
    # GRSM.225 (ob.3776) — Aesculus flava / Acer saccharum Forest
    data.frame(
      name      = c("Acer saccharum", "Acer pensylvanicum"),
      pc_code   = c("pc.7191", "pc.7186"),
      max_cover = c(62.5, 0.55),
      stringsAsFactors = FALSE
    ),
    # OLYM.Z.681.0003 (ob.206444) — no taxa recorded
    NULL
  )

  result <- process_plot_data(plot_data)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  expect_equal(ncol(result), 7)

  expected_cols <- c("Actions", "VegBank Code", "Author Code", "Location", "Top Taxa", "Communities", "Survey Year")
  expect_equal(names(result), expected_cols)

  # Check action column contains HTML buttons with correct data attributes
  expect_true(all(grepl("<button", result$Actions)))
  expect_true(all(grepl("dt-shiny-action", result$Actions)))
  # ACAD.143 and GRSM.225 have coordinates — map button has dt-map-action class
  expect_true(grepl("dt-map-action", result$Actions[1]))
  expect_true(grepl("dt-map-action", result$Actions[2]))
  # OLYM has no coordinates — map button is rendered but disabled (no dt-map-action class)
  expect_false(grepl("dt-map-action", result$Actions[3]))

  # VegBank Code and Author Code use real observation codes
  expect_equal(
    result$`VegBank Code`,
    vapply(c("ob.2948", "ob.3776", "ob.206444"), htmltools::htmlEscape, character(1), USE.NAMES = FALSE)
  )
  expect_equal(result$`Author Code`, c("ACAD.143", "GRSM.225", "OLYM.Z.681.0003"))

  # Location: ACAD.143 in Maine, GRSM.225 in Tennessee, OLYM has no coordinates
  expect_equal(
    result$Location,
    c(
      "Maine<br><span class=\"text-muted small\">United States</span><br><span class=\"text-muted small\">44.3465, -68.2293</span><br><span class=\"text-muted small\">Elevation: 416m</span>",
      "Tennessee<br><span class=\"text-muted small\">United States</span><br><span class=\"text-muted small\">35.5800, -83.8000</span><br><span class=\"text-muted small\">Elevation: 930m</span>",
      "Unspecified<br><span class=\"text-muted small\">Elevation: 1915m</span>"
    )
  )

  # Survey years differ across parks and eras
  expect_equal(result$`Survey Year`, c("1998", "1998", "2012"))

  # Top Taxa: rows 1-2 have taxa, row 3 (OLYM) has none
  expect_true(grepl("<div", result$`Top Taxa`[1]))
  expect_true(grepl("dt-shiny-action", result$`Top Taxa`[1]))
  expect_true(grepl("<a", result$`Top Taxa`[1]))
  expect_true(grepl("Aronia melanocarpa", result$`Top Taxa`[1]))
  expect_true(grepl("pc.10653", result$`Top Taxa`[1]))
  expect_true(grepl("Acer saccharum", result$`Top Taxa`[2]))
  expect_true(grepl("pc.7191", result$`Top Taxa`[2]))
  # OLYM has no taxa — produces the "No taxa recorded" placeholder
  expect_true(grepl("No taxa recorded", result$`Top Taxa`[3]))

  # Communities column links to real CEGL community codes
  expect_true(all(grepl("<a", result$Communities)))
  expect_true(all(grepl("dt-shiny-action", result$Communities)))
  expect_true(grepl("CEGL005094", result$Communities[1]))  # ACAD.143 Vaccinium dwarf-shrubland
  expect_true(grepl("CEGL007695", result$Communities[2]))  # GRSM.225 Aesculus/Acer forest
  expect_true(grepl("CEGL008260", result$Communities[3]))  # OLYM Alpine sparse vegetation
})

test_that("process_plot_data handles empty data correctly", {
  result <- process_plot_data(NULL)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 7)

  result_empty <- process_plot_data(vegbankweb:::PLOT_TABLE_SCHEMA_TEMPLATE)
  expect_equal(nrow(result_empty), 0)
})

test_that("build_plot_table returns DataTable object", {
  pkg_env <- asNamespace("vegbankweb")

  with_mocked_bindings(
    create_table = function(table_config) {
      expect_type(table_config, "list")

      # Return mock DataTable
      structure(list(x = list(data = data.frame())), class = "datatables")
    },
    .env = pkg_env,
    {
      result <- build_plot_table()
      expect_s3_class(result, "datatables")
    }
  )
})

test_that("plot table spec produces valid config", {
  config <- build_table_config_from_spec(vegbankweb:::PLOT_TABLE_SPEC)

  expect_type(config, "list")
  expect_true("column_defs" %in% names(config))
  expect_true("initial_data" %in% names(config))
  expect_true("ajax" %in% names(config))


  expect_equal(length(config$column_defs), 7)

  # Check actions column (index 0)
  expect_equal(config$column_defs[[1]]$targets, 0)
  expect_false(config$column_defs[[1]]$orderable)
  expect_false(config$column_defs[[1]]$searchable)
  expect_null(config$column_defs[[1]]$render)

  # Check taxon list column (index 4)
  expect_equal(config$column_defs[[5]]$targets, 4)
  expect_false(config$column_defs[[5]]$orderable)
  expect_null(config$column_defs[[5]]$render)

  # Check community list column (index 5)
  expect_equal(config$column_defs[[6]]$targets, 5)
  expect_false(config$column_defs[[6]]$orderable)
  expect_null(config$column_defs[[6]]$render)

  # Check initial data
  expect_s3_class(config$initial_data, "data.frame")
  expect_equal(nrow(config$initial_data), 0)

  # Check AJAX function and data source spec
  expect_true(is.function(config$ajax))
  ajax_env <- environment(config$ajax)
  spec <- ajax_env$data_source_spec

  expect_equal(spec$table_id, "plot_table")
  expect_equal(spec$resource, "plot-observations")
  expect_equal(spec$detail, "minimal")
  expect_equal(spec$query$with_nested, "TRUE")
})

