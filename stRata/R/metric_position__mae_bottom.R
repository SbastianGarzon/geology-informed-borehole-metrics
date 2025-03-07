#' Mean Absolute Error Bottom
#'
#' This function calculates the absolute error in depth between the predicted and target bottom of stratigraphic units
#' It handles exclusions of specific target formations (e.g., "NN") and computes the distance between the deepest prediction
#' and target values within each group.
#'
#' @param borehole A data.table or data frame containing the columns `prediction`, `target_variable`, `depth`, and `original_index`.
#'
#' @return A numeric vector containing the absolute distance between the predicted and target depths.
#' If no valid data is found, it returns `NA`.
#'
#' @export
abs_error_bottom <- function(borehole) {
  # Convert to data.table
  setDT(borehole)

  bore_pred <- borehole[, .SD[.N], by = prediction, .SDcols = c("depth","original_index")]
  bore_target <- borehole[, .SD[.N], by = target_variable, .SDcols = c("depth","original_index")]

  if ("NN" %in% bore_target$target_variable) {
    nn_index <- borehole$original_index[which(borehole$target_variable == "NN")-1]
    if (length(nn_index) >= 1) {
      # Exclude the formation directly above 'NN'
      bore_target <- bore_target[!original_index %in% nn_index]
    }
  }

  bore_pred <- bore_pred[seq_len(nrow(bore_pred) - 1)]
  bore_target <- bore_target[seq_len(nrow(bore_target) - 1)]

  # Inner join to combine data
  combined_df <- merge(bore_target, bore_pred, by.x = "target_variable", by.y = "prediction", suffixes = c("_tar", "_pred"))

  if (nrow(combined_df) == 0) {
    return(NA)
  }

  # Calculate distance and sum distances
  combined_df[, distance := abs(depth_tar - depth_pred)]

  return(combined_df$distance)
}

#' Mean Absolute Error Bottom Custom Summary
#'
#' This function calculates the mean absolute error for the bottom of stratigraphic units.
#' It first computes the absolute error in depth between the predicted and target values using the `abs_error_bottom` function per borehole,
#' and then calculates the mean absolute error.
#' @param df A data frame or tibble containing columns `nr`, `prediction`, `target_variable`, `depth`, and `original_index`.
#'
#' @return The mean absolute error for the bottom of stratigraphic units.
#'
#' @export
mean_abs_error_bottom_custom_summary <- function(df){

  df %>%
    group_by(nr) %>%
    reframe(depth_diff = abs_error_bottom(pick(prediction,target_variable,depth,original_index))) %>%
    reframe(mean_v = mean(depth_diff,na.rm = TRUE))%>%
    pull(mean_v)

}
