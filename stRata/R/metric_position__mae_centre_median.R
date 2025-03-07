#' Compute Mean Absolute Error Centre (median)
#'
#' This function calculates the absolute error in depth between the predicted and target centre of stratigraphic units
#' It handles exclusions of specific target formations (e.g., "NN")
#'
#' @param borehole A data.table or data frame containing the columns `prediction`, `target_variable`, `depth`, and `original_index`.
#'
#' @return A numeric vector containing the absolute distance between the predicted and target depths.
#' If no valid data is found, it returns `NA`.
#'
#' @export
abs_error_centre_median <- function(borehole) {
  # Convert to data.table
  setDT(borehole)

  # Calculate median depth for predictions and target variables
  bore_pred <- borehole[, .(pred_median = median(depth)), by = prediction]
  bore_target <- borehole[, .(tar_median = median(depth)), by = target_variable]

  # Inner join to combine data
  combined_df <- merge(bore_target, bore_pred, by.x = "target_variable", by.y = "prediction", all = FALSE)
  if (nrow(combined_df) == 0) {
    return(NA)
  }

  # Calculate score and sum distance
  combined_df[, distance := abs(tar_median - pred_median)]

  return(combined_df$distance)
}

#' Mean Absolute Error Centre (Median) Custom Summary
#'
#' This function calculates the mean absolute error for the centre of stratigraphic units.
#' It first computes the absolute error in depth between the predicted and target values using the `abs_error_centre_mean` function per borehole,
#' and then calculates the mean absolute error.
#' @param df A data frame or tibble containing columns `nr`, `prediction`, `target_variable`, `depth`, and `original_index`.
#'
#' @return The mean absolute error for the centre of stratigraphic units.
#'
#' @export
mean_abs_error_centre_median_custom_summary <- function(df) {
  df %>%
    group_by(nr) %>%
    reframe(depth_diff = abs_error_centre_median(pick(prediction, target_variable, depth))) %>%
    reframe(mean_depth_diff_c = mean(depth_diff, na.rm = TRUE)) %>%
    pull(mean_depth_diff_c)
}
