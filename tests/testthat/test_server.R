test_that("build_plot_obs_details_view returns a list of outputs", {
  row_data <- data.frame(
    authorobscode = "A1", authorplotcode = "P1",
    taxa = I(list(data.frame(authorplantname = "Sp", cover = 10))),
    stringsAsFactors = FALSE
  )
  outputs <- build_plot_obs_details_view(row_data)
  expect_true("plot_id_details" %in% names(outputs))
  expect_true("taxa_details" %in% names(outputs))
})
