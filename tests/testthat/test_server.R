test_that("build_top10_barchart returns a ggplot object", {
  sample_data <- data.frame(
    plotstateprovince = rep(c("State1", "State2"), each = 5)
  )
  p <- build_top10_barchart(sample_data, "plotstateprovince", "Place", "#4F8773")
  expect_s3_class(p, "ggplot")
})

test_that("build_pie_chart returns a plotly object", {
  sample_data <- data.frame(
    interp_current_partyname = rep(c("PartyA", "PartyB"), each = 3)
  )
  p <- build_pie_chart(sample_data, "interp_current_partyname")
  expect_s3_class(p, "plotly")
})

test_that("build_most_recent_date_list returns a UL element", {
  sample_data <- data.frame(obsdateentered = c("2023-09-01 10:00:00", "2023-08-29 12:00:00"))
  ul_tag <- build_most_recent_date_list(sample_data, n = 2)
  expect_true(grepl("<ul>", as.character(ul_tag)))
})

test_that("build_plot_heatmap returns a ggplot object", {
  sample_data <- data.frame(
    longitude = c(-100, -99, -98),
    latitude = c(40, 41, 42)
  )
  p <- build_plot_heatmap(sample_data)
  expect_s3_class(p, "ggplot")
})

test_that("build_taxa_list handles valid data", {
  row_data <- list(taxa = data.frame(
    authorplantname = c(
      "Metrosiderous polymorpha", "Acacia Koa", "Artemisia Tridentata",
      "Clarmontia kakeana", "Haplostachys haplostachya", "Lobelia hypoleuca"
    ),
    maxcover = c(50, 30, 20, 10, 5, 2)
  ))
  html_out <- build_taxa_list(row_data)
  expect_true(grepl("<ol>", html_out))
})

test_that("build_action_buttons returns HTML strings", {
  btns <- build_action_buttons(1)
  expect_true(grepl("action-button", btns))
})

test_that("build_details_view returns a list of outputs", {
  row_data <- data.frame(
    authorobscode = "A1", authorplotcode = "P1",
    taxa = I(list(data.frame(authorplantname = "Sp", cover = 10))),
    stringsAsFactors = FALSE
  )
  outputs <- build_details_view(row_data)
  expect_true("plot_id_details" %in% names(outputs))
  expect_true("taxa_details" %in% names(outputs))
})
