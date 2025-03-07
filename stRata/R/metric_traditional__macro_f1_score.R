#' Compute Macro F1 Score
#'
#' This function calculates the macro-averaged F1 score for a classification task.
#'
#' @param predictions A vector of predicted class labels.
#' @param true_labels A vector of true class labels.
#' @return The macro-averaged F1 score.
#' @export
compute_f1_score <- function(predictions, true_labels) {
  # Ensure inputs are factors and align levels
  all_levels <- union(levels(factor(true_labels)), levels(factor(predictions)))
  true_labels <- factor(true_labels, levels = all_levels)
  predictions <- factor(predictions, levels = all_levels)

  # Create confusion matrix
  confusion <- table(true_labels, predictions)

  # Initialize F1 score accumulator
  f1_scores <- numeric(length(all_levels))

  for (i in seq_along(all_levels)) {
    class <- all_levels[i]

    # True positives, false positives, false negatives
    tp <- confusion[class, class]
    fp <- sum(confusion[, class]) - tp
    fn <- sum(confusion[class, ]) - tp

    # Precision and recall
    precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
    recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)

    # F1 score for the current class
    f1_scores[i] <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
  }

  # Macro F1 score
  return(mean(f1_scores))
}

#' Compute Overall F1 Score Summary
#'
#' This function computes the macro-averaged F1 score after filtering out rows where `target_variable` is "NN".
#'
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The macro-averaged F1 score after filtering.
#' @export
overall_f1_score_custom_summary <- function(df){
  df_filtered <- df %>% filter(target_variable != "NN")
  compute_f1_score(df_filtered$prediction, df_filtered$target_variable)
}

#' Compute per borehole F1 Score Summary
#'
#' This function computes the macro-averaged F1 score per borehole
#' @param df A data frame containing `target_variable` (true labels) and `prediction` (predicted labels).
#' @return The mean F1 score per borehole after filtering out "NN" values.
#' @export
per_borehole_f1_score_custom_summary <- function(df){

  df_filtered <- df %>% filter(target_variable != "NN")
  df_filtered %>% group_by(nr) %>%
    reframe(f1_score = compute_f1_score(prediction, target_variable)) %>%
    reframe(mean_f1_score = mean(f1_score, na.rm = TRUE) ) %>%
    pull(mean_f1_score)
}



