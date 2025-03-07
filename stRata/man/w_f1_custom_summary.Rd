% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_traditional__w_f1.R
\name{w_f1_custom_summary}
\alias{w_f1_custom_summary}
\title{Calculate Weighted F1 Score for custom summary}
\usage{
w_f1_custom_summary(df)
}
\arguments{
\item{df}{A data frame containing \code{target_variable} (true labels) and \code{prediction} (predicted labels).}
}
\value{
The overall weighted F1 score.
}
\description{
This function serves as a wrapper for \code{calculate_w_f1}, computing the weighted F1 score.
}
