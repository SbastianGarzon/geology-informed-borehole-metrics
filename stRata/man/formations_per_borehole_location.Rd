% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/preprocessing__data.R
\name{formations_per_borehole_location}
\alias{formations_per_borehole_location}
\title{Generate a Binary Matrix of Formations per Borehole Location}
\usage{
formations_per_borehole_location(unique_boreholes, units_extent)
}
\arguments{
\item{unique_boreholes}{A data frame or tibble containing borehole information. This should include columns \code{nr} (borehole ID), \code{x_28992} (X-coordinate, easting), and \code{y_28992} (Y-coordinate, northing) in the Amersfoort / RD New (EPSG: 28992) coordinate system.}

\item{units_extent}{An \code{sf} object representing the spatial layers of geological formations (units), where each feature corresponds to a different formation. The object should include a \code{layer_name} column with the names of the formations.}
}
\value{
A binary matrix (data frame) where each row corresponds to a borehole and each column corresponds to a geological formation. The cells of the matrix contain binary values: \code{1} if the formation is present at the borehole's location, and \code{0} if it is absent.
}
\description{
This function processes a dataset of borehole locations and identifies which geological formations (units) are present at each location. It returns a binary matrix indicating the presence (1) or absence (0) of each formation for each borehole.
}
\details{
The function iterates over each borehole in the \code{unique_boreholes} dataset, checking which formations intersect with the borehole's location using the \code{units_in_coordinate} function. The results are then converted into a binary matrix, where formations are represented as columns, and boreholes as rows.

Progress is displayed in the console, with updates indicating the current borehole being processed.
}
