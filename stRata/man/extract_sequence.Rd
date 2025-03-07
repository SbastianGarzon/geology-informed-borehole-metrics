% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_sequence__transition_validation_score.R
\name{extract_sequence}
\alias{extract_sequence}
\title{Extract Sequence of Run-Length Encoded Values}
\usage{
extract_sequence(x)
}
\arguments{
\item{x}{A character vector. This can be any vector of strings where "NN" values will be
excluded from the run-length encoding.}
}
\value{
A list containing a single vector of run-length encoded values, excluding "NN".
The vector represents the sequence of unique values after applying run-length encoding.
}
\description{
This function processes a character vector to extract a sequence of run-length encoded
values from the unique values of the vector, excluding any "NN" values. The function first
ensures the input is a character vector, then applies run-length encoding twice: first to
the original vector and then to the filtered unique values.
}
