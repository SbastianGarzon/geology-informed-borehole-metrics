#' Summarize and Reshape Random Forest Metrics by Hyperparameter
#'
#' This function processes a data frame of random forest performance metrics (`metrics_by_fold`) by summarizing the mean and standard deviation
#' of metrics for different values of the hyperparameter `mtry`. It then reshapes the data to a long format and separates the metric, window,
#' and statistic for further analysis.
#'
#' @param metrics_by_fold A data frame containing random forest performance metrics, with columns for `mtry`, `fold`, and various metrics (e.g., accuracy, error)
#'                        calculated during cross-validation, along with the standard deviation for each metric.
#'
#' @return A data frame with reshaped and summarized metrics, including columns for `mtry`, `metric_name`, `window`, `mean`, and `SD`.
#'         The data is in long format with separate columns for the metric value and standard deviation.
#'
#' @export
rf_metrics_by_hyper_parameter <- function(metrics_by_fold){

  summary_df <- metrics_by_fold %>%
    group_by(mtry) %>%
    summarise(across(!fold,
                     list(mean = ~ mean(.), SD = ~ sd(.)),
                     .names = "{col}_{fn}"))

  names(summary_df) <- gsub("_mean$", "", names(summary_df))
  names(summary_df) <- gsub("_SD$", "SD", names(summary_df))

  metrics_by_hyper_parameter <- summary_df %>%
    # Gather all metric columns (excluding mtry)
    pivot_longer(
      cols = -mtry,  # Exclude 'mtry' from the long format
      names_to = c("metric_name", "window", "stat"),  # Create columns for metric, window, and stat
      names_pattern = "^(.*?)(?:_window_(\\d+))?(SD)?$",  # Regex to capture metric, window, and optional 'SD'
      values_to = "value"  # Create a column for values
    ) %>%
    # Fill in 'stat' where it's NA to indicate that it's a metric
    mutate(stat = ifelse(stat=="", "value", "SD")) %>%
    # Fill in 'window' where it's NA with an empty string
    mutate(window = ifelse(window=="", 0, window)) %>%
    pivot_wider(
      names_from = stat,  # Use 'stat' to create new columns
      values_from = value,  # Values to fill in the new columns
      values_fill = NA  # Fill missing values with NA
    )

  return(metrics_by_hyper_parameter)
}
