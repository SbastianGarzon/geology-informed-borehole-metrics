#' Compute Unit Extent Validation Score for Borehole Predictions
#'
#' This function checks whether the predicted stratigraphic units for a given borehole location
#' match the expected formations present in that area. It maps unit names to stratigraphic
#' classes and calculates the proportion of predicted units that exist in the borehole’s unit set.
#'
#' @param nr A vector representing the borehole identifier.
#' @param prediction A vector of predicted stratigraphic units.
#' @param units_per_borehole A data frame where each row corresponds to a borehole and columns represent unit presence (1 if present, 0 otherwise).
#' @param strat_class_map A named vector mapping unit names to stratigraphic class groupings.
#' @return A numeric value representing the proportion of predicted units that match expected formations.
#' @export
unit_extent_validation <- function(nr,prediction,units_per_borehole,strat_class_map){

  #Extract unique values from prediction
  nr <- unique(nr)
  prediction_unique <- unique(prediction)

  #Formations in borehole location
  row <- units_per_borehole[units_per_borehole$nr == nr, ]
  unit_names <- toupper(colnames(row)[which(row == 1)])

  # Transform unit names to stratigraphic class grouping
  transformed_units <- ifelse(is.na(strat_class_map[unit_names]), unit_names, strat_class_map[unit_names])

  #Check if the prediction is in the formations
  common <- prediction_unique[prediction_unique %in% transformed_units]
  prop_common <- length(common)/length(prediction_unique)
  return(prop_common)
}

#' Calculate mean Unit Extent Validation Score per borehole
#'
#' This function serves as a wrapper for `unit_extent_validation`
#'
#' @param df A data frame containing `prediction` (predicted labels).
#' @return A numeric value representing the mean proportion of predicted units per borehole that match expected formations
#' @export
unit_extent_validation_custom_summary <- function(df){

  df %>% group_by(nr) %>%
    reframe(uevs = unit_extent_validation(nr,prediction,units_per_borehole_coord,strat_class_map)) %>%
    reframe(mean_uevs = mean(uevs)) %>%
    pull(mean_uevs)
}
