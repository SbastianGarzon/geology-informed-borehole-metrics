#' Extract Sequence of Run-Length Encoded Values
#'
#' This function processes a character vector to extract a sequence of run-length encoded
#' values from the unique values of the vector, excluding any "NN" values. The function first
#' ensures the input is a character vector, then applies run-length encoding twice: first to
#' the original vector and then to the filtered unique values.
#'
#' @param x A character vector. This can be any vector of strings where "NN" values will be
#'   excluded from the run-length encoding.
#'
#' @return A list containing a single vector of run-length encoded values, excluding "NN".
#'   The vector represents the sequence of unique values after applying run-length encoding.
#' @export
extract_sequence <- function(x) {
  x <- as.character(x)  # Ensure x is a character vector
  rle_values <- rle(x)$values
  rle_values <- rle(rle_values[rle_values!="NN"])$values
  return(list(rle_values))
}

#' Extract Transitions Between Elements in a Vector
#'
#' This function generates a list of transitions between consecutive elements in a character vector.
#' Each transition is represented as a concatenation of adjacent elements in the original vector.
#'
#' @param x A character vector. This vector should contain the elements between which transitions
#'   are to be extracted.
#'
#' @return A list containing a single character vector of transitions. Each transition is a string
#'   formed by concatenating adjacent elements in the input vector.
#'
#' @export
extract_transitions <- function(x){

  transitions <- list(paste0(x[-length(x)], x[-1]))
  return(transitions)
}

#' Compare Extracted Transitions to Known Transitions
#'
#' This function compares a vector of extracted transitions to a set of known transitions and calculates
#' the proportion of correctly matched transitions. The function returns the proportion of transitions
#' that are found in the set of known transitions.
#'
#' @param x A character vector of extracted transitions. Each transition should be a string representing
#'   the concatenation of adjacent elements.
#' @param known_transitions A character vector of known transitions. This vector contains the transitions
#'   that are considered correct or valid.
#'
#' @return A numeric value representing the proportion of transitions in `x` that are also present in
#'   `known_transitions`.
#'
#' @export
#'
correct_transitions <- function(x,known_transitions){
  num_known_transitions <- sum(unlist(x) %in% known_transitions)
  return(num_known_transitions)

}

#' Transition validation score Custom Summary
#'
#' This function calculates the overall and perborehole transition validation score.
#' @param df A data frame or tibble containing columns `nr`, `prediction`
#'
#' @return A data frame with the mean perborehole transition validation score and the overall transition validation score.
#'
#' @export
transition_validation_custom_summary <- function(df,known_transitions){

  df %>% group_by(nr) %>%
  reframe(correct_transitions = correct_transitions(extract_transitions(unlist(extract_sequence(prediction))),known_transitions), num_transitions = length(unlist(extract_transitions(unlist(extract_sequence(prediction))))), pb_TVS=correct_transitions/num_transitions) %>%
  reframe(mean_pb_TVS = mean(pb_TVS,na.rm=T), o_TVS=sum(correct_transitions)/sum(num_transitions))

}
