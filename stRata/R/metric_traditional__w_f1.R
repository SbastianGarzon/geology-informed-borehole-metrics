#' Calculate Weighted F1 Score
#'
#' This function calculates the weighted F1 score for each class in a classification task
#' and returns the overall weighted F1 score.
#'
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The overall weighted F1 score.
#' @export
calculate_w_f1 <- function(df) {
  # Initialize a vector to store weighted f1 score for each class

  df <- df %>% filter(target_variable!="NN")

  classes <- unique(c(as.character(df$target_variable),as.character(df$prediction)))

  w_f1_scores <- numeric(length(classes))
  names(w_f1_scores) <- classes

  # Loop over each class to calculate the w-f1 score
  for (class in classes) {
    # True Positives (TP): Correctly predicted as the class
    tp <- sum(df$target_variable == class & df$prediction == class)

    # False Positives (FP): Predicted as the class but actually not that class
    fp <- sum(df$prediction == class & df$target_variable != class)

    # False Negatives (FN): Actually the class but predicted as something else
    fn <- sum(df$target_variable == class & df$prediction!= class)

    # w_f1 coefficient formula
    if ((2 * tp + fp + fn) == 0) {
      w_f1_scores[class] <- 0
    } else {
      # Weighted f1 score for each class
      w_f1_scores[class] <- (2 * tp) / (2 * tp + fp + fn) * (sum(df$target_variable == class))
    }
  }
  w_f1 <- sum(w_f1_scores)/nrow(df)
  return(w_f1)
}

#' Calculate Weighted F1 Score for custom summary
#'
#' This function serves as a wrapper for `calculate_w_f1`, computing the weighted F1 score.
#'
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The overall weighted F1 score.
#' @export
w_f1_custom_summary <- function(df) {
  calculate_w_f1(df)
}

