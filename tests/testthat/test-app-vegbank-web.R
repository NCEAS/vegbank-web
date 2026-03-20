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


test_that("project pj.340: filter plots, navigate to map for ob.2950, search for ob.154622", {
  skip_on_cran()

  app <- AppDriver$new(
    test_path("../.."),
    name = "project-map-filter-search",
    height = 1054,
    width = 1619,
    timeout = 30000
  )
  on.exit(app$stop(), add = TRUE)

  # --- Step 1: App starts on Home ---
  app$wait_for_idle(timeout = 15000)
  expect_equal(app$get_value(input = "page"), "Home")

  # --- Step 2: Navigate to the Projects tab ---
  app$set_inputs(page = "Projects")
  app$wait_for_idle(timeout = 30000)
  expect_equal(app$get_value(input = "page"), "Projects")


  # --- Step 3: Click the obs_count link to cross-filter Plots by pj.340 ---
  # obs_count_click carries {code, label}; the server sets plot_filter and navigates to Plots.
  app$set_inputs(
    obs_count_click = list(code = "pj.340", label = "Acadia National Park"),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle(timeout = 15000)

  expect_equal(
    app$get_value(input = "page"), "Plots",
    label = "obs_count_click should navigate to Plots tab"
  )
  expect_true(
    grepl("pj.340", app_output_html(app, "plot_filter_alert"), fixed = TRUE),
    label = "Plot filter alert should reference pj.340"
  )

  # --- Step 4: Search the DT for ACAD.145 to bring ob.2950 into view ---
  # The server-side table search narrows the pj.340 results to the single row
  # whose author_obs_code matches "ACAD.145".
  app$set_inputs(
    plot_table_state = list(start = 0L, length = 100L, search = "ACAD.145", order = list()),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle(timeout = 30000)

  expect_true(
    isTRUE(app$get_js(
      "!!document.querySelector('.dt-map-action[data-ob-code=\"ob.2950\"]')"
    )),
    label = "Map button for ob.2950 should be rendered after DT search for ACAD.145"
  )

  # --- Step 5: Click the Map button for ob.2950 ---
  # The .dt-map-action click handler reads data-lat/lng/code/ob-code and fires
  # Shiny.setInputValue('show_on_map', payload); the server navigates to the Map tab.
  app$get_js("document.querySelector('.dt-map-action[data-ob-code=\"ob.2950\"]').click()")
  app$wait_for_idle(timeout = 30000)

  expect_equal(
    app$get_value(input = "page"), "Map",
    label = "Clicking the Map button should navigate to the Map tab"
  )

  # --- Step 6: Wait for the map to fully initialize ---
  # The JS onRender callback polls until tiles are loaded and clusters visible,
  # then fires Shiny.setInputValue('map_ready', true).  This triggers
  # map_initialized(TRUE) and maybe_show_map_filter_notice() on the server.
  app$wait_for_value(input = "map_ready", timeout = 90000)
  app$wait_for_idle(timeout = 15000)

  # --- Step 7: Verify the map filter notice notification ---
  # maybe_show_map_filter_notice() calls showNotification(MAP_FILTER_NOTICE_MSG)
  # when navigating to the Map tab while an active Plots filter exists.
  # The notification DOM element persists for 10 seconds.
  notice_text <- app$get_js(
    "var el = document.querySelector('.shiny-notification-content-text'); el ? el.textContent : ''"
  )
  expect_true(
    grepl("Filters applied to the Plots table", notice_text, fixed = TRUE),
    label = "Map filter notice should appear when navigating to Map with an active Plots filter"
  )

  # --- Step 8: Search the map for "5" ---
  # map_search_query is a virtual input {query, ts}; the server looks up the
  # pre-built search index and sends back a map_search_results custom message
  # with status = "multiple" when several plots match.
  app$set_inputs(
    map_search_query = list(query = "5", ts = 0L),
    allow_no_input_binding_ = TRUE
  )
  app$wait_for_idle(timeout = 15000)

  # --- Step 9: Verify search results list contains ob.154622 ---
  # The map_search_results JS handler appends .vb-map-search-match <a> elements
  # to the control's _vbResults node; each item shows "AUTHOR_CODE (ob_code)".
  results_html <- app$get_js(paste0(
    "var ctrl = document.querySelector('.vb-map-search-control');",
    "ctrl && ctrl._vbResults ? ctrl._vbResults.innerHTML : ''"
  ))
  expect_true(
    grepl("ob.154622", results_html, fixed = TRUE),
    label = "Map search results for '5' should include ob.154622"
  )

  # --- Step 10: Click the ob.154622 result ---
  # Clicking a result calls flyAndPopup(lat, lng, makePopupNode(popup_label)), which
  # opens a Leaflet popup at the target coordinates.  No Shiny input is fired;
  # verification is done by inspecting the DOM popup directly.
  app$get_js(paste0(
    "var ctrl = document.querySelector('.vb-map-search-control');",
    "var items = ctrl ? ctrl.querySelectorAll('.vb-map-search-match') : [];",
    "for (var i = 0; i < items.length; i++) {",
    "  if (items[i].textContent.indexOf('ob.154622') !== -1) {",
    "    items[i].click(); break;",
    "  }",
    "}"
  ))
  # flyAndPopup() calls openOn(map) which only closes map._popup, but the ob.2950
  # popup was added via R's leaflet::addPopups() (popup.addTo(map)) and is not
  # tracked as map._popup.  Both popups coexist in the DOM; wait until ANY
  # .leaflet-popup-content node contains "ob.154622" rather than just the first.
  app$wait_for_js(
    paste0(
      "(function() {",
      "  var els = document.querySelectorAll('.leaflet-popup-content');",
      "  for (var i = 0; i < els.length; i++) {",
      "    if (els[i].textContent.indexOf('ob.154622') !== -1) return true;",
      "  }",
      "  return false;",
      "})()"
    ),
    timeout = 10000
  )

  # --- Step 11: Verify the Leaflet popup shows the ob.154622 label ---
  # build_plot_popup_label(author_obs_code, "ob.154622") → "Plot <code> (ob.154622) is here!"
  popup_text <- app$get_js(paste0(
    "(function() {",
    "  var els = document.querySelectorAll('.leaflet-popup-content');",
    "  for (var i = 0; i < els.length; i++) {",
    "    if (els[i].textContent.indexOf('ob.154622') !== -1) return els[i].textContent;",
    "  }",
    "  return '';",
    "})()"
  ))
  expect_true(
    grepl("ob.154622", popup_text, fixed = TRUE),
    label = "Leaflet popup should reference ob.154622 after clicking the search result"
  )
})
