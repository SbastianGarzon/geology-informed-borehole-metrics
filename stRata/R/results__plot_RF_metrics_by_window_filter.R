#' Plot Performance Metrics
#'
#' This function generates and saves plots for specified performance metrics,
#' showing their variation across different `mtry` values.
#' The function includes error bars to represent standard deviation.
#'
#' @param data A data frame containing performance metrics. The data should include columns: `metric_name`, `mtry`, `value`, `SD`, and `window`.
#' @param metrics A character vector of metric names to be plotted.
#' @param plot_titles A character vector of titles corresponding to each metric.
#' @param y_label (Optional) A character vector for custom y-axis labels. Defaults to `plot_titles`.
#' @param path (Optional) A string specifying the directory to save the plots. Defaults to `"plots"`.
#'
#' @return This function does not return a value but saves plots as PNG files in the specified directory.
#' @export
plot_metrics <- function(data, metrics, plot_titles, y_label = plot_titles, path = "plots") {

  # Check that the length of plot_titles matches the number of metrics
  if (length(metrics) != length(plot_titles)) {
    stop("The number of plot titles must match the number of metrics.")
  }

  # Loop through each metric and create a plot
  for (i in seq_along(metrics)) {
    metric <- metrics[i]
    plot_title <- plot_titles[i]

    # Filter the data for the specified metric
    plot_data <- data %>%
      filter(metric_name == metric) %>%
      mutate(window = as.factor(window))  # Convert window to a factor

    # Create the ggplot
    p <- ggplot(plot_data, aes(x = factor(mtry), y = value, group = window, color = window)) +
      geom_point(position = position_dodge(width = 0.5), size = 3) +  # Points for the values
      geom_errorbar(aes(ymin = value - SD, ymax = value + SD),
                    position = position_dodge(width = 0.5),
                    width = 0.2, size = 0.8) +  # Error bars for the SD
      labs(title = paste0(plot_title, " per mtry value"),  # Use custom title
           x = "mtry Values",
           y = plot_title,  # Use custom y-axis label
           color = "Window \n Majority Vote \n (Steps filter)\n - 0.5m",
           caption = "Error bars represent standard deviation") +
      theme_bw() +
      theme(panel.grid.major = element_line(size = 0.5, linetype = 'dashed', colour = "grey"),
            plot.title = element_text(hjust = 0.5))

    # Save the plot
    ggsave(file.path(path,paste0(metric, "_plot.png")), p, width = 10, height = 6, units = "in", dpi = 300)
  }
}
