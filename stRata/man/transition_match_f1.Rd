% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_sequence__transition_match_F1.R
\name{transition_match_f1}
\alias{transition_match_f1}
\title{Calculate Transition Match F1 Score}
\usage{
transition_match_f1(target_variable, prediction)
}
\arguments{
\item{target_variable}{A vector representing the ground truth unit sequence.}

\item{prediction}{A vector representing the predicted unit sequence.}
}
\value{
A numeric value representing the Formation Match F1 score, which is a measure of the prediction accuracy based on transitions between formations.
The score is between 0 (no match) and 1 (perfect match).
}
\description{
This function calculates the Transition Match F1 score based on the transitions in the predicted and target sequences.
It evaluates the precision, recall, and F1 score based on the true positives (TP), false positives (FP), and false negatives (FN)
in the transition events between units in both the predicted and ground truth sequences.
}
