#' Compute Overall Accuracy Metric
#'
#' This function calculates the overall accuracy of a classification model while ignoring rows where `target_variable` is "NN".
#' A warning is issued if any rows are removed during filtering.
#'
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The overall accuracy as a proportion of correctly classified instances.
#' @export
overall_accuracy_custom_metric <- function(df) {

  total_rows_before <- nrow(df)

  df_filtered <- df %>%
    filter(target_variable != "NN")

  total_rows_after <- nrow(df_filtered)

  if (total_rows_before != total_rows_after) {
    warning(unique(df_filtered$nr),": ",total_rows_before - total_rows_after, " rows with target_variable 'NN' were ignored to compute the accuracy metric")
  }

  mean(df_filtered$target_variable == df_filtered$prediction,na.rm=TRUE)
}

#' Compute per borehole Accuracy Metric
#'
#' This function calculates the mean per borehole accuracy of a classification model while ignoring rows where `target_variable` is "NN".
#' A warning is issued if any rows are removed during filtering.
#'
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The mean per borehole accuracy.
#' @export
mean_per_borehole_accuracy_custom_metric <- function(df) {

  total_rows_before <- nrow(df)

  df_filtered <- df %>%
    filter(target_variable != "NN") # Removes NN from predictions

  total_rows_after <- nrow(df_filtered)

  if (total_rows_before != total_rows_after) {
    warning(unique(df_filtered$nr),": ",total_rows_before - total_rows_after, " rows with target_variable 'NN' were ignored to compute the accuracy metric")
  }

  df_filtered %>% group_by(nr) %>%
    summarise(accuracy = mean(target_variable == prediction,na.rm = TRUE)) %>%
    summarise(mean_accuracy = mean(accuracy)) %>%
    pull(mean_accuracy)

}
