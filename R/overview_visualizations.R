#' Visualization Functions
#'
#' Functions for creating various visualizations of VegBank data.
#' @keywords internal

#' Build Top Ten Bar Chart
#'
#' Constructs a ggplot bar chart based on the top 10 counts for a specified column.
#'
#' @param data Data frame containing the data.
#' @param column Name of the column to summarize.
#' @param xlab Label for the x-axis.
#' @param color Bar fill color.
#' @return A ggplot object.
#' @importFrom ggplot2 .data ggplot aes geom_bar geom_text coord_flip scale_y_continuous
#' @importFrom ggplot2 labs theme_minimal expansion
#' @importFrom stats reorder var
#' @keywords internal
build_top10_barchart <- function(data, column, xlab, color) {
  top_df <- dplyr::count(data, var = .data[[column]], sort = TRUE) |>
    head(10)
  ggplot2::ggplot(
    top_df,
    ggplot2::aes(
      x = stats::reorder(.data$var, .data$n),
      y = .data$count
    )
  ) +
    ggplot2::geom_bar(stat = "identity", fill = color) +
    ggplot2::geom_text(ggplot2::aes(label = .data$count), hjust = -0.1, size = 3) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.2))) +
    ggplot2::labs(x = xlab, y = "Plot Occurrences") +
    ggplot2::theme_minimal()
}

#' Build Pie Chart
#'
#' Constructs an interactive pie chart using plotly.
#'
#' @param data Data frame containing the data.
#' @param column The column name used for pie chart slices.
#' @param palette Color palette.
#' @param label_top_n Number of top labels to display.
#' @return A plotly pie chart.
#' @importFrom plotly plot_ly config
#' @importFrom grDevices colorRampPalette
#' @keywords internal
build_pie_chart <- function(data, column, palette = c("#a1d99b", "#31a354"), label_top_n = 4) {
  counts <- dplyr::count(data, var = .data[[column]], sort = TRUE) |>
    dplyr::mutate(label = ifelse(dplyr::row_number() <= label_top_n,
      paste0(var, ": ", n), ""
    ))
  colors <- grDevices::colorRampPalette(palette)(nrow(counts))
  plotly::plot_ly(
    counts,
    labels = ~var, values = ~n, type = "pie",
    text = ~label,
    textinfo = "text",
    insidetextorientation = "radial",
    marker = list(colors = colors)
  ) |>
    plotly::config(responsive = TRUE)
}

#' Build Most Recent Date List
#'
#' Creates a list of the most recent dates from the data.
#'
#' @param data Data frame containing date information.
#' @param n Maximum number of dates to display.
#' @param date_column Name of the date column
#' @return A Shiny UI tag containing an unordered list.
#' @importFrom lubridate parse_date_time
#' @importFrom htmltools tags
#' @keywords internal
build_most_recent_date_list <- function(data, n = 16, date_column = "obsdateentered") {
  date_formats <- c("a, d b Y H:M:S z", "d b Y H:M:S", "Y-m-d H:M:S")
  top_dates <- data |>
    dplyr::select(original = dplyr::all_of(date_column)) |>
    dplyr::mutate(parsed = lubridate::parse_date_time(
      data[[date_column]],
      orders = date_formats
    )) |>
    dplyr::filter(!is.na(.data$parsed), !duplicated(.data$parsed)) |>
    dplyr::arrange(dplyr::desc(.data$parsed)) |>
    utils::head(n)
  items <- lapply(top_dates$original, function(d) {
    htmltools::tags$li(class = "list-unstyled", htmltools::tags$strong(d))
  })
  htmltools::tags$ul(items)
}

#' Build Plot Heatmap
#'
#' Constructs a heatmap (using ggplot2) of plot locations using density estimation.
#'
#' @param data Data frame containing longitude and latitude.
#' @return A ggplot object.
#' @importFrom ggplot2 .data ggplot geom_polygon stat_density2d scale_fill_gradient coord_fixed
#' @importFrom ggplot2 xlim labs theme_minimal map_data after_stat aes
#' @keywords internal
build_plot_heatmap <- function(data) {
  na_map <- ggplot2::map_data("world", region = c("USA", "Canada", "Mexico"))
  ggplot2::ggplot() +
    ggplot2::geom_polygon(
      data = na_map,
      ggplot2::aes(
        x = .data$long,
        y = .data$lat, group = .data$group
      ),
      fill = "white", color = "gray70", linewidth = 0.3
    ) +
    ggplot2::stat_density2d(
      data = data,
      ggplot2::aes(
        x = .data$longitude,
        y = .data$latitude,
        fill = ggplot2::after_stat(.data$level)
      ),
      geom = "polygon", color = "black", linewidth = 0.5, contour = TRUE
    ) +
    ggplot2::scale_fill_gradient(low = "lightgreen", high = "darkgreen", na.value = "white") +
    ggplot2::coord_fixed(1.3) +
    ggplot2::xlim(-200, -50) +
    ggplot2::labs(title = "Plot Heatmap", x = "Longitude", y = "Latitude") +
    ggplot2::theme_minimal()
}
