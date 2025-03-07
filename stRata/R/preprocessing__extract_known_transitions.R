#' Extract Known Transitions
#'
#' This function extracts known transitions between stratigraphic units for each borehole.
#' A transition is defined as the change from one formation to the next along the borehole.
#' The function calculates these transitions, ensuring that boundaries between the same stratigraphic
#' unit are excluded from the final transition list.
#'
#' @param df_labeled A data frame containing borehole data with stratigraphic information.
#'   The data frame should include at least two columns: `nr` (borehole identifier) and `strat_inter`
#'   (the stratigraphic unit/formation).
#'
#' @return A vector containing the unique known transitions (boundary) between different stratigraphic units
#'   that occur in the data.
#' @export
extract_known_transitions <- function(df_labeled){
  # Create a new column to represent the next formation for each borehole
  df <- df_labeled %>%
    filter(strat_inter != "NN") %>%
    group_by(nr) %>%
    mutate(next_formation = lead(strat_inter))

  # Count transitions and create the transition matrix
  transition_df <- df %>%
    filter(!is.na(next_formation)) %>%
    group_by(strat_inter, next_formation) %>%
    summarise(count = n())

  complete_grid <- expand.grid(
    strat_inter = unique(transition_df$strat_inter),
    next_formation = unique(transition_df$next_formation)
  )

  complete_transition_df <- merge(complete_grid, transition_df, all.x = TRUE)
  complete_transition_df$count[is.na(complete_transition_df$count)] <- NA

  complete_transition_df[complete_transition_df$strat_inter==complete_transition_df$next_formation,"count"] <- NA

  transition_df$boundary <- paste0(transition_df$strat_inter,transition_df$next_formation)

  known_transitions <- transition_df$boundary[transition_df$strat_inter!=transition_df$next_formation]

  return(known_transitions)
}
