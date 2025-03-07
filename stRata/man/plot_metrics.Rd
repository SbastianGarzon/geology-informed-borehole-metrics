% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results__plot_RF_metrics_by_window_filter.R
\name{plot_metrics}
\alias{plot_metrics}
\title{Plot Performance Metrics}
\usage{
plot_metrics(data, metrics, plot_titles, y_label = plot_titles, path = "plots")
}
\arguments{
\item{data}{A data frame containing performance metrics. The data should include columns: \code{metric_name}, \code{mtry}, \code{value}, \code{SD}, and \code{window}.}

\item{metrics}{A character vector of metric names to be plotted.}

\item{plot_titles}{A character vector of titles corresponding to each metric.}

\item{y_label}{(Optional) A character vector for custom y-axis labels. Defaults to \code{plot_titles}.}

\item{path}{(Optional) A string specifying the directory to save the plots. Defaults to \code{"plots"}.}
}
\value{
This function does not return a value but saves plots as PNG files in the specified directory.
}
\description{
This function generates and saves plots for specified performance metrics,
showing their variation across different \code{mtry} values.
The function includes error bars to represent standard deviation.
}
