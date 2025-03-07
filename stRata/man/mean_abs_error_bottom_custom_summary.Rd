% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_position__mae_bottom.R
\name{mean_abs_error_bottom_custom_summary}
\alias{mean_abs_error_bottom_custom_summary}
\title{Mean Absolute Error Bottom Custom Summary}
\usage{
mean_abs_error_bottom_custom_summary(df)
}
\arguments{
\item{df}{A data frame or tibble containing columns \code{nr}, \code{prediction}, \code{target_variable}, \code{depth}, and \code{original_index}.}
}
\value{
The mean absolute error for the bottom of stratigraphic units.
}
\description{
This function calculates the mean absolute error for the bottom of stratigraphic units.
It first computes the absolute error in depth between the predicted and target values using the \code{abs_error_bottom} function per borehole,
and then calculates the mean absolute error.
}
