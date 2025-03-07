#' Majority Vote for Classification
#'
#' This function returns the most frequent value (or class) in a vector `x`. If there are ties, it returns the first occurring value
#' that has the maximum count.
#'
#' @param x A vector of values (e.g., class labels) for which the majority vote needs to be determined.
#'
#' @return A single value representing the most frequent value (class) in the input vector `x`.
#'
#' @export
majority_vote <- function(x) {
  freq_table <- table(x)
  max_count <- max(freq_table)
  candidates <- names(freq_table[freq_table == max_count])
  return(candidates[1])  # Return the first occurring value
}

#' Apply Majority Vote to Rolling Window of Predictions
#'
#' This function applies a majority vote to the `prediction` column of a data frame (`df`)
#' within a rolling window of specified size (`window_size`) for each borehole (`nr`).
#' The majority vote is determined using the `majority_vote` function,
#' which returns the most frequent value within each window.
#' The function is applied to each group defined by `nr` (borehole).
#'
#' @param df A data frame containing a column `prediction` with predicted values, and a grouping variable `nr` (e.g., borehole ID).
#' @param window_size An integer specifying the size of the rolling window over which the majority vote is applied.
#'
#' @return A data frame with the same structure as the input `df`, where the `prediction` column has been updated with the majority vote within each rolling window.
#'         The `prediction` values are centered within the window.
#'
#' @export
apply_majority_vote <- function(df, window_size) {
  df <- df %>%
    group_by(nr) %>%  # Group by borehole
    mutate(prediction = rollapply(prediction,
                                  width = window_size,
                                  FUN = majority_vote,
                                  align = "center",
                                  partial = TRUE,
                                  fill = NA)) %>%
    ungroup()

  return(df)
}
