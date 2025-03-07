#' Sequence Alignment Score (SAS)
#'
#' Calculates the Sequence Alignment Score (SAS) between a predicted and target sequence
#' using the Optimal String Alignment (OSA) method. The score ranges from 0 (no alignment)
#' to 1 (perfect alignment).
#'
#' @param prediction A character vector representing the predicted sequence of categories.
#' @param target A character vector representing the target sequence of categories.
#'
#' @return A numeric value representing the alignment score, ranging from 0 to 1.
#' @export
sequence_alignment_score<- function(prediction,target){

  prediction <- extract_sequence(prediction)[[1]]
  target <- extract_sequence(target)[[1]]

  max_length<- max(length(prediction), length(target))

  unique_categories <- unique(c(prediction, target))
  category_encoding <- setNames(LETTERS[seq_along(unique_categories)], unique_categories)

  # Treath the sequences as strings
  encoded_seq1 <- sapply(prediction, function(x) category_encoding[[x]])
  encoded_seq2 <- sapply(target, function(x) category_encoding[[x]])

  string1 <- paste(encoded_seq1, collapse = "")
  string2 <- paste(encoded_seq2, collapse = "")

  # Calculate the alignment score using the Optimal String Alignment (OSA) method

  alignment <- 1-(stringdist::stringdist(string1, string2, method = "osa")/max_length)
  return(alignment)
}

#' Sequence Alignment Score Custom Summary
#'
#' Computes the mean Sequence Alignment Score (SAS) for each borehole in the dataset.
#' The Sequence Alignment Score is calculated using the `sequence_alignment_score` function,
#' and the mean score is returned for each borehole.
#'
#' @param df A data frame containing at least two columns: `prediction` (predicted sequence)
#'   and `target_variable` (ground truth sequence), and `nr` (borehole identifier).
#'
#' @return A numeric value representing the mean SAS score for the entire dataset.
#' @export
sequence_alignment_custom_summary <- function(df){

  df %>% group_by(nr) %>%
    reframe(sas = sequence_alignment_score(prediction,target_variable)) %>%
    summarise(mean_sas = mean(sas)) %>%
    pull(mean_sas)
}
