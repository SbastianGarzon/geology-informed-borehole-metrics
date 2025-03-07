#' Compute Unit Match F1 Score
#'
#' This function calculates the Unit Match F1 score, which evaluates how well predicted stratigraphic units match the true stratigraphic units in a borehole.
#' It considers formations as True Positives (TP) if they appear in both the predicted and true sequences.
#' False Positives (FP) are formations predicted but not present in the ground truth, and False Negatives (FN) are formations in the ground truth but not predicted.
#' The metric ignores cases where `target_variable` is "NN".
#'
#' @param target_variable A vector of true stratigraphic unit labels.
#' @param prediction A vector of predicted stratigraphic unit labels.
#' @return The Unit Match F1 score, a value between 0 and 1.
#' @export
unit_match_f1 <- function(target_variable,prediction) {

  # Extract unique formations from both predicted and ground truth sequences
  prediction_unique <- unique(prediction)
  target_unique <- unique(target_variable)

  total_rows_before <- length(target_unique)

  target_unique <- setdiff(target_unique, "NN")

  total_rows_after <- length(target_unique)

  if (total_rows_before != total_rows_after) {
    warning("'NN' were ignored to compute the Unit Match F1 metric")
  }

  # Calculate True Positives (TP): Formations that are in both predicted and ground truth
  tp <- length(intersect(target_unique, prediction_unique))

  # Calculate False Positives (FP): Formations predicted but not in ground truth
  fp <- length(setdiff(prediction_unique,target_unique))

  # Calculate False Negatives (FN): Formations in ground truth but not predicted
  fn <- length(setdiff(target_unique,prediction_unique))

  # Calculate Precision: TP / (TP + FP)
  precision <- ifelse(tp + fp == 0, 0, tp / (tp + fp))

  # Calculate Recall: TP / (TP + FN)
  recall <- ifelse(tp + fn == 0, 0, tp / (tp + fn))

  # Calculate Unit Match F1 score: 2 * (Precision * Recall) / (Precision + Recall)

  um_f1 <- ifelse(precision + recall == 0, 0, 2 * (precision * recall) / (precision + recall))

  return(um_f1)
}

#' Compute Mean Unit Match F1 Score
#'
#' This function calculates the mean Unit Match F1 score for a dataset by grouping it based on `nr`,
#' computing the Unit Match F1 score for each borehole "nr", and then averaging the results.
#' @param df A data frame containing `nr` (grouping variable), `target_variable` (true labels), and `prediction` (predicted labels).
#' @return The mean Unit Match F1 score.
#' @export
um_f1_custom_summary <- function(df){
  df %>%
    group_by(nr) %>%
    summarise(um_f1 = unit_match_f1(target_variable,prediction)) %>%
    summarise(mean_fm_f1 = mean(um_f1)) %>%
    pull(mean_fm_f1)
}


