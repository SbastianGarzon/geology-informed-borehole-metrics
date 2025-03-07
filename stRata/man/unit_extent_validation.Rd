% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metric_extent__UEVS.R
\name{unit_extent_validation}
\alias{unit_extent_validation}
\title{Compute Unit Extent Validation Score for Borehole Predictions}
\usage{
unit_extent_validation(nr, prediction, units_per_borehole, strat_class_map)
}
\arguments{
\item{nr}{A vector representing the borehole identifier.}

\item{prediction}{A vector of predicted stratigraphic units.}

\item{units_per_borehole}{A data frame where each row corresponds to a borehole and columns represent unit presence (1 if present, 0 otherwise).}

\item{strat_class_map}{A named vector mapping unit names to stratigraphic class groupings.}
}
\value{
A numeric value representing the proportion of predicted units that match expected formations.
}
\description{
This function checks whether the predicted stratigraphic units for a given borehole location
match the expected formations present in that area. It maps unit names to stratigraphic
classes and calculates the proportion of predicted units that exist in the borehole’s unit set.
}
