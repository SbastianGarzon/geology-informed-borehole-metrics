#' Compute Cohen's Kappa Statistic
#'
#' This function calculates Cohen's Kappa statistic to measure inter-rater agreement.
#'
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The Cohen's Kappa statistic.
#' @export
kappa_metric <- function(df) {

  df$prediction <- factor(df$prediction)
  df$target_variable <- factor(df$target_variable)

  # Align levels
  levels_combined <- union(levels(df$prediction), levels(df$target_variable))
  df$prediction <- factor(df$prediction, levels = levels_combined)
  df$target_variable <- factor(df$target_variable, levels = levels_combined)

  # Compute confusion matrix and extract Kappa
  conf_matrix <- caret::confusionMatrix(df$prediction, df$target_variable)
  kappa <- conf_matrix$overall["Kappa"]

  return(as.numeric(kappa))
}
