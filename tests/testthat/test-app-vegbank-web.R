library(shinytest2)

test_that("{shinytest2} recording: vegbank-web-detail-plot-filter-by-community", {
  app <- AppDriver$new(test_path("../.."), name = "vegbank-web-detail-plot-filter-by-community",
    height = 1054, width = 1619)
  app$wait_for_idle()
  app$expect_values(screenshot_args = FALSE)
})


test_that("{shinytest2} recording: vegbank-web", {
  app <- AppDriver$new(test_path("../.."), name = "vegbank-web", height = 1054, width = 1619)
  app$wait_for_idle()
  app$expect_values(screenshot_args = FALSE)
})


# Extracts text from a renderUI output value.
# AppDriver returns renderUI outputs as list(html = "...", deps = list(...));
# this helper collapses the whole structure to a single string for grepl checks.
app_output_html <- function(app, name) {
  paste(unlist(app$get_value(output = name)), collapse = " ")
}


test_that("drills from plot ob.3410 through cl.1946 and cc.42770 to filter plots table", {
  skip_on_cran()

  app <- AppDriver$new(
    test_path("../.."),
    name = "plot-detail-community-filter-chain",
    height = 1054,
    width = 1619,
    timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # --- Step 1: App starts on Home tab ---
  app$wait_for_idle(timeout = 15000)
  expect_equal(app$get_value(input = "page"), "Home")

  # --- Step 2: Navigate to the Plots tab ---
  app$set_inputs(page = "Plots")
  app$wait_for_idle(timeout = 15000)
  expect_equal(app$get_value(input = "page"), "Plots")
  # No cross-resource filter should be active yet
  expect_false(
    grepl("cc.42770", app_output_html(app, "plot_filter_alert"), fixed = TRUE),
    label = "No cc.42770 filter should be active on initial Plots visit"
  )

  # --- Step 3: Paginate to page 5 (default page_length = 100; start = 400 for page 5) ---
  app$set_inputs(
    plot_table_state = list(start = 400L, length = 100L, order = list(), search = ""),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle(timeout = 10000)

  # --- Step 4: Open the detail panel for plot observation ob.3410 ---
  app$set_inputs(plot_link_click = "ob.3410", allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 30000)  # API call required

  expect_true(
    grepl("ob.3410", app_output_html(app, "plot_header"), fixed = TRUE),
    label = "Plot detail header should contain ob.3410"
  )

  # --- Step 5: Navigate into community classification cl.1946 from the plot detail ---
  app$set_inputs(comm_class_link_click = "cl.1946", allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 30000)

  expect_true(
    grepl("cl.1946", app_output_html(app, "comm_class_header"), fixed = TRUE),
    label = "Classification detail header should contain cl.1946"
  )

  # --- Step 6: Navigate into community concept cc.42770 from the classification detail ---
  app$set_inputs(comm_link_click = "cc.42770", allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 30000)

  expect_true(
    grepl("cc.42770", app_output_html(app, "community_concept_header"), fixed = TRUE),
    label = "Community concept header should contain cc.42770"
  )

  # --- Step 7: Click the obs_count link to filter the plots table by cc.42770 ---
  # The link fires obs_count_click with {code, label}; the server extracts the
  # entity type from the "cc." prefix, sets plot_filter, and navigates to Plots.
  app$set_inputs(
    obs_count_click = list(code = "cc.42770", label = "cc.42770"),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle(timeout = 10000)

  # The obs_count handler navigates back to the Plots tab
  expect_equal(
    app$get_value(input = "page"), "Plots",
    label = "obs_count click should navigate to Plots tab"
  )

  # The filter alert should name cc.42770
  filter_alert_html <- app_output_html(app, "plot_filter_alert")
  expect_true(
    grepl("cc.42770", filter_alert_html, fixed = TRUE),
    label = "Plot filter alert should reference cc.42770"
  )

  # The plot table should have been rebuilt (non-null) with the filter applied.
  # NOTE: cc.42770 is captured in the DT AJAX handler closure and is not
  # serialized into the widget JSON, so the filter alert is the correct
  # verification that the table is rendering filtered data.
  plot_table_val <- app$get_value(output = "plot_table")
  expect_false(
    is.null(plot_table_val),
    label = "Plot table output should be non-null when filter is applied"
  )

  # --- Step 8: Close the detail panel ---
  app$set_inputs(close_details = TRUE, allow_no_input_binding_ = TRUE)
  app$wait_for_idle(timeout = 10000)

  # The cc.42770 filter persists on the Plots table after the panel is closed
  expect_true(
    grepl("cc.42770", app_output_html(app, "plot_filter_alert"), fixed = TRUE),
    label = "cc.42770 filter should persist after closing the detail panel"
  )
})
