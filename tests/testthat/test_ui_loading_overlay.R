test_that("build_loading_overlay creates proper HTML structure", {
  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Message 1", "Message 2"),
    completion_message = "Done!",
    show_detail = FALSE
  )

  # Check basic structure
  expect_s3_class(overlay, "shiny.tag")
  expect_equal(overlay$name, "div")
  expect_equal(overlay$attribs$id, "test-loading-overlay")
  expect_equal(overlay$attribs$class, "loading-overlay")
})

test_that("build_loading_overlay includes data attributes", {
  messages <- c("Step 1", "Step 2", "Step 3")
  completion <- "Complete!"

  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Processing...",
    messages = messages,
    completion_message = completion,
    show_detail = FALSE
  )

  # Check data attributes exist
  expect_true(!is.null(overlay$attribs$`data-messages`))
  expect_equal(overlay$attribs$`data-completion-message`, completion)

  # Verify messages are JSON encoded
  decoded_messages <- jsonlite::fromJSON(overlay$attribs$`data-messages`)
  expect_equal(decoded_messages, messages)
})

test_that("build_loading_overlay creates unique IDs based on overlay_type", {
  overlay1 <- build_loading_overlay(
    overlay_type = "map",
    default_title = "Loading map...",
    messages = c("Loading..."),
    completion_message = "Done!"
  )

  overlay2 <- build_loading_overlay(
    overlay_type = "download",
    default_title = "Downloading...",
    messages = c("Downloading..."),
    completion_message = "Complete!"
  )

  expect_equal(overlay1$attribs$id, "map-loading-overlay")
  expect_equal(overlay2$attribs$id, "download-loading-overlay")
})

test_that("build_loading_overlay includes detail element when show_detail is TRUE", {
  overlay_with_detail <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!",
    show_detail = TRUE
  )

  # Convert to character and check for detail span
  html <- as.character(overlay_with_detail)
  expect_true(grepl('id="test-loading-detail"', html))
  expect_true(grepl('<span', html))
})

test_that("build_loading_overlay excludes detail element when show_detail is FALSE", {
  overlay_no_detail <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!",
    show_detail = FALSE
  )

  html <- as.character(overlay_no_detail)
  expect_false(grepl('id="test-loading-detail"', html))
})

test_that("build_loading_overlay includes ellipses animation elements", {
  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!"
  )

  html <- as.character(overlay)
  # Ellipses use generic 'loading-ellipses' class, not overlay-type-specific
  expect_true(grepl('class="loading-ellipses"', html))
  # Should have nested divs for animation (checking for the ellipses container)
  expect_true(grepl("loading-ellipses", html))
})

test_that("build_loading_overlay includes pun/message element", {
  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!"
  )

  html <- as.character(overlay)
  expect_true(grepl('id="test-loading-pun"', html))
})

test_that("build_loading_overlay hidden state is managed by CSS class, not inline styles", {
  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!"
  )

  # Positioning and default-hidden state are now managed by the .loading-overlay
  # CSS class (display:none, position:fixed, z-index:1200), so the element should
  # carry NO inline style when visible_on_load is FALSE (the default).
  expect_true(is.null(overlay$attribs$style) || !nzchar(overlay$attribs$style %||% ""))

  # The CSS class must be present so the stylesheet rules apply
  expect_equal(overlay$attribs$class, "loading-overlay")
})

test_that("build_loading_overlay visible_on_load=TRUE adds display:flex inline style", {
  visible_overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!",
    visible_on_load = TRUE
  )

  # Inline display:flex overrides the CSS class's display:none so the overlay
  # appears immediately without a JavaScript call on initial page load.
  expect_true(grepl("display: flex", visible_overlay$attribs$style))
})

test_that("build_loading_overlay visible_on_load=FALSE (default) has no inline display style", {
  hidden_overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!",
    visible_on_load = FALSE
  )

  # No inline style means the CSS class's display:none is in effect
  style_val <- hidden_overlay$attribs$style %||% ""
  expect_false(grepl("display", style_val))
})

test_that("build_map_loading_overlay returns valid overlay", {
  overlay <- build_map_loading_overlay()

  expect_s3_class(overlay, "shiny.tag")
  expect_equal(overlay$attribs$id, "map-loading-overlay")

  # Check messages are plant-themed
  messages_json <- overlay$attribs$`data-messages`
  messages <- jsonlite::fromJSON(messages_json)
  expect_true(any(grepl("seed|branch|root", messages, ignore.case = TRUE)))
})

test_that("build_download_loading_overlay returns valid overlay with detail", {
  overlay <- build_download_loading_overlay()

  expect_s3_class(overlay, "shiny.tag")
  expect_equal(overlay$attribs$id, "download-loading-overlay")

  # Check for detail element
  html <- as.character(overlay)
  expect_true(grepl('id="download-loading-detail"', html))

  # Check messages are download-themed
  messages_json <- overlay$attribs$`data-messages`
  messages <- jsonlite::fromJSON(messages_json)
  expect_true(any(grepl("download|gather|zip", messages, ignore.case = TRUE)))
})

test_that("build_loading_overlay handles empty messages array", {
  expect_error(
    build_loading_overlay(
      overlay_type = "test",
      default_title = "Loading...",
      messages = character(0),
      completion_message = "Done!"
    ),
    NA # Should not error
  )
})

test_that("build_loading_overlay handles special characters in messages", {
  messages <- c("Loading <data>...", "Processing 'items'", 'Handling "quotes"')

  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = "Loading...",
    messages = messages,
    completion_message = "Done!"
  )

  # Messages should be properly JSON encoded
  decoded <- jsonlite::fromJSON(overlay$attribs$`data-messages`)
  expect_equal(decoded, messages)
})

test_that("build_loading_overlay includes title in proper heading element", {
  title <- "Custom Loading Title"

  overlay <- build_loading_overlay(
    overlay_type = "test",
    default_title = title,
    messages = c("Loading..."),
    completion_message = "Done!"
  )

  html <- as.character(overlay)
  expect_true(grepl('<h2 id="test-loading-title"', html))
  expect_true(grepl(title, html, fixed = TRUE))
})

test_that("build_loading_overlay maintains overlay_type consistency", {
  type <- "custom-type-123"

  overlay <- build_loading_overlay(
    overlay_type = type,
    default_title = "Loading...",
    messages = c("Loading..."),
    completion_message = "Done!",
    show_detail = TRUE
  )

  html <- as.character(overlay)

  # All IDs should use the overlay_type
  expect_true(grepl(paste0('id="', type, '-loading-overlay"'), html))
  expect_true(grepl(paste0('id="', type, '-loading-title"'), html))
  expect_true(grepl(paste0('id="', type, '-loading-detail"'), html))
  expect_true(grepl(paste0('id="', type, '-loading-pun"'), html))
  # Ellipses use generic 'loading-ellipses' class (not type-specific)
  expect_true(grepl('class="loading-ellipses"', html))
})

test_that("loading overlay messages are appropriate for context", {
  map_overlay <- build_map_loading_overlay()
  download_overlay <- build_download_loading_overlay()
  overview_overlay <- build_overview_loading_overlay()
  citation_overlay <- build_citation_loading_overlay()

  map_messages <- jsonlite::fromJSON(map_overlay$attribs$`data-messages`)
  download_messages <- jsonlite::fromJSON(download_overlay$attribs$`data-messages`)
  overview_messages <- jsonlite::fromJSON(overview_overlay$attribs$`data-messages`)
  citation_messages <- jsonlite::fromJSON(citation_overlay$attribs$`data-messages`)

  # Each should have multiple messages
  expect_true(length(map_messages) >= 3)
  expect_true(length(download_messages) >= 3)
  expect_true(length(overview_messages) >= 3)
  expect_true(length(citation_messages) >= 3)

  # All four message sets should be distinct
  expect_false(identical(map_messages, download_messages))
  expect_false(identical(map_messages, overview_messages))
  expect_false(identical(map_messages, citation_messages))
  expect_false(identical(overview_messages, citation_messages))
})

test_that("loading overlay completion messages are set", {
  map_overlay <- build_map_loading_overlay()
  download_overlay <- build_download_loading_overlay()
  overview_overlay <- build_overview_loading_overlay()
  citation_overlay <- build_citation_loading_overlay()

  for (overlay in list(map_overlay, download_overlay, overview_overlay, citation_overlay)) {
    expect_true(!is.null(overlay$attribs$`data-completion-message`))
    expect_true(nchar(overlay$attribs$`data-completion-message`) > 0)
  }
})

test_that("build_overview_loading_overlay returns valid overlay", {
  overlay <- build_overview_loading_overlay()

  expect_s3_class(overlay, "shiny.tag")
  expect_equal(overlay$attribs$id, "overview-loading-overlay")
  expect_equal(overlay$attribs$class, "loading-overlay")

  # Not visible by default
  style_val <- overlay$attribs$style %||% ""
  expect_false(grepl("display", style_val))

  # Messages are overview-themed
  messages <- jsonlite::fromJSON(overlay$attribs$`data-messages`)
  expect_true(any(grepl("plot|project|community|overview|statistic|number|contributor", messages, ignore.case = TRUE)))
})

test_that("build_overview_loading_overlay visible=TRUE shows overlay immediately", {
  overlay <- build_overview_loading_overlay(visible = TRUE)

  expect_true(grepl("display: flex", overlay$attribs$style))
})

test_that("build_citation_loading_overlay returns valid overlay", {
  overlay <- build_citation_loading_overlay()

  expect_s3_class(overlay, "shiny.tag")
  expect_equal(overlay$attribs$id, "citation-loading-overlay")
  expect_equal(overlay$attribs$class, "loading-overlay")

  # Not visible by default
  style_val <- overlay$attribs$style %||% ""
  expect_false(grepl("display", style_val))

  # Messages are citation-themed
  messages <- jsonlite::fromJSON(overlay$attribs$`data-messages`)
  expect_true(any(grepl("citation|identifier|archive|reference|record|herbarium|paper", messages, ignore.case = TRUE)))
})

test_that("build_citation_loading_overlay visible=TRUE shows overlay immediately", {
  overlay <- build_citation_loading_overlay(visible = TRUE)

  expect_true(grepl("display: flex", overlay$attribs$style))
})
