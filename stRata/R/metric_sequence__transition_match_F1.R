#' Calculate Transition Match F1 Score
#'
#' This function calculates the Transition Match F1 score based on the transitions in the predicted and target sequences.
#' It evaluates the precision, recall, and F1 score based on the true positives (TP), false positives (FP), and false negatives (FN)
#' in the transition events between units in both the predicted and ground truth sequences.
#'
#' @param target_variable A vector representing the ground truth unit sequence.
#' @param prediction A vector representing the predicted unit sequence.
#'
#' @return A numeric value representing the Formation Match F1 score, which is a measure of the prediction accuracy based on transitions between formations.
#'         The score is between 0 (no match) and 1 (perfect match).
#'
transition_match_f1 <- function(target_variable,prediction) {

  # Extract unique units from both predicted and ground truth sequences
  prediction_sequence <- unlist(extract_sequence(prediction))
  target_sequence <- unlist(extract_sequence(target_variable))

  # Extract transitions

  prediction_transitions <- unlist(extract_transitions(prediction_sequence))
  target_transitions <- unlist(extract_transitions(target_sequence))

  # Calculate True Positives (TP): Transitions that are in both predicted and ground truth
  tp <- length(intersect(target_transitions, prediction_transitions))

  # Calculate False Positives (FP): Transition predicted but not in ground truth
  fp <- length(setdiff(prediction_transitions,target_transitions))

  # Calculate False Negatives (FN): Transitions in ground truth but not predicted
  fn <- length(setdiff(target_transitions,prediction_transitions))

  # Calculate Precision: TP / (TP + FP)
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))

  # Calculate Recall: TP / (TP + FN)
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))

  # Calculate Formation match F1 score: 2 * (Precision * Recall) / (Precision + Recall)

  tm_f1 <- ifelse(precision + recall == 0, 0, 2 * (precision * recall) / (precision + recall))

  return(tm_f1)
}

#' Calculate Transition Match F1 Score Summary
#'
#' This function calculates the mean Transition Match F1 score in the input data frame (`df`).
#' The score is computed by applying the `transition_match_f1` function to the `prediction` and `target_variable` columns for each borehole,
#' excluding rows where the target variable is "NN".
#' The final output is the mean F1 score for all boreholes in the data frame (`df`).
#'
#' @param df A data frame containing the predicted values in the `prediction` column, the ground truth in the `target_variable` column,
#'           and a grouping variable `nr` (e.g., borehole ID).
#'
#' @return A numeric value representing the mean Transition Match F1 score for all boreholes in the data frame (`df`).
#'
#' @export
tm_f1_score_custom_summary <- function(df){

  df %>% group_by(nr) %>%
    filter(target_variable != "NN") %>%
    summarise(tm_f1 = transition_match_f1(prediction, target_variable)) %>%
    summarise(mean_tm_f1 = mean(tm_f1,na.rm=TRUE)) %>%
    pull(mean_tm_f1)
}
