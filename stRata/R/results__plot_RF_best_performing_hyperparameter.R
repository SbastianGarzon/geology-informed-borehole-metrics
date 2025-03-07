#' Plot Best Performing Hyperparameters
#'
#' This function generates a comparative plot of performance metrics for different hyperparameter (`mtry`) values, highlighting
#' the best-performing hyperparameter configurations.
#'
#' @param metrics_by_hyperparameter A data frame containing performance metrics, including columns `metric_name`, `mtry`, `value`, `SD`, and `window`.
#' @param title A character string specifying the plot title.
#'
#' @return A combined `ggplot2` object comparing the best-performing hyperparameter values across various metrics.
#' @export
best_performing_hyper_plot <- function(metrics_by_hyperparameter,title){

  metrics_by_hyperparameter$mtry <- factor(metrics_by_hyperparameter$mtry)

  # Get the number of unique values in mtry
  n_colors <- length(unique(metrics_by_hyperparameter$mtry))

  # Generate colors: if n_colors > 8, use colorRampPalette to extend, otherwise use brewer.pal
  custom_colors <- if (n_colors > 8) {
    colorRampPalette(RColorBrewer::brewer.pal(8, "Dark2"))(n_colors)
  } else {
    RColorBrewer::brewer.pal(n_colors, "Dark2")
  }

  metrics_table <- metrics_by_hyperparameter %>%
    filter(window==0) %>%
    filter(metric_name %in% c("o_acc",
                              "pb_acc",
                              "Kappa",
                              "o_f1",
                              "w_f1",
                              "pb_f1",
                              "um_f1",
                              "uevs",
                              "tm_f1",
                              "pb_tvs",
                              "o_tvs",
                              "sas",
                              "mean_abs_error_top",
                              "mean_abs_error_centre_mean",
                              "mean_abs_error_centre_median",
                              "mean_abs_error_bottom")) %>%
    mutate(metric_name = factor(metric_name, levels = c("o_acc",
                                                        "pb_acc",
                                                        "Kappa",
                                                        "o_f1",
                                                        "pb_f1",
                                                        "w_f1",
                                                        "um_f1",
                                                        "uevs",
                                                        "tm_f1",
                                                        "pb_tvs",
                                                        "o_tvs",
                                                        "sas",
                                                        "mean_abs_error_top",
                                                        "mean_abs_error_centre_mean",
                                                        "mean_abs_error_centre_median",
                                                        "mean_abs_error_bottom")))

  # Best values for normal metrics (maximum values)

  best_values_normal <- metrics_table %>%
    group_by(metric_name) %>%
    slice_max(order_by = value) %>%
    filter(metric_name != "Accuracy")

  best_values_normal$mtry <- factor(best_values_normal$mtry, levels = levels(as.factor(metrics_table$mtry)))

  # Best values for diff metrics (minimum values)

  best_values_diff <- metrics_table %>%
    group_by(metric_name) %>%
    slice_min(order_by = value) %>%
    filter(metric_name %in% c("mean_abs_error_top",
                              "mean_abs_error_centre_mean",
                              "mean_abs_error_centre_median",
                              "mean_abs_error_bottom"))

  best_values_diff$mtry <- factor(best_values_diff$mtry, levels = levels(as.factor(metrics_table$mtry)))

  metrics_normal <- metrics_table %>%
    filter(!grepl("error", metric_name))

  metrics_diff <- metrics_table %>%
    filter(grepl("error", metric_name))


  # Add the highest values rectangle to the regular metrics plot
  p1 <- ggplot(metrics_normal, aes(x = metric_name, y = value, color = mtry, fill = mtry)) +
    geom_errorbar(aes(ymin = value - SD, ymax = value + SD), width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_rect(data = best_values_normal %>% filter(metric_name %in% unique(metrics_normal$metric_name)),
              aes(xmin = as.numeric(factor(metric_name)) - 0.4,
                  xmax = as.numeric(factor(metric_name)) + 0.4,
                  ymin = 0, ymax = 1, fill = mtry), alpha = 0.2) +
    scale_color_manual(values = custom_colors,name="Hyper-parameter (mtry)") +
    scale_fill_manual(values = custom_colors, guide = "none") +
    scale_x_discrete(labels = c("o_acc"="Accuracy (Overall)", "pb_acc"="Accuracy (per borehole)",
                                "Kappa"="Cohen's Kappa", "o_f1"="F1-Score (Overall)",
                                "pb_f1"="Mean F1-Score (per Borehole)",
                                "w_f1"="Weighted F1",
                                "um_f1"="Unit Match - F1 (per Borehole)",
                                "uevs"="Unit Extent Validation Score",
                                "sas" = "Sequence Alignment Score",
                                "tm_f1"="Transition Match - F1 score",
                                "pb_tvs"="Mean Transition Validation Score (per Borehole)",
                                "o_tvs"="Transition Validation Score (Overall)")) +
    labs(y = "Metric Value (0 to 1 Range)",title = title) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), plot.title = element_text(size=20), axis.title.x = element_blank())

  max_value <- max(metrics_diff$value)

  p2 <- ggplot(metrics_diff, aes(x = metric_name, y = value, color = mtry, fill = mtry)) +
    geom_errorbar(aes(ymin = value - SD, ymax = value + SD), width = 0.2,
                  position = position_dodge(width = 0.5)) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_rect(data = best_values_diff %>% filter(metric_name %in% unique(metrics_diff$metric_name)),
              aes(xmin = as.numeric(factor(metric_name)) - 0.4,
                  xmax = as.numeric(factor(metric_name)) + 0.4,
                  ymin = 0, ymax = max_value+max(SD), fill = mtry), alpha = 0.2) +
    scale_color_manual(values = custom_colors,name="Hyper-parameter (mtry)") +
    scale_fill_manual(values = custom_colors, guide = "none") +
    scale_x_discrete(labels = c("mean_abs_error_top"="Mean absolute error - Top",
                                "mean_abs_error_centre_mean"="Mean absolute error - Centre [mean]",
                                "mean_abs_error_centre_median"="Mean absolute error - Centre [median]",
                                "mean_abs_error_bottom"="Mean absolute error - Bottom")) +
    labs(y = "Metric Value (Meters)") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),axis.title.x = element_blank())

  # Overlay the two plots with a common x-axis
  combined_plot <- cowplot::plot_grid(p1 + theme(legend.position = "top"),
                                      p2 + theme(legend.position = "top"),
                                      ncol = 2, align = "h",rel_widths = c(2.5, 1))

  return(combined_plot)

}
