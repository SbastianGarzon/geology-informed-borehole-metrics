#' Plot Class Proportions Across Folds
#'
#' This function generates a bar plot visualizing the proportion of lithostratigraphic units across different cross-validation folds.
#' It helps to assess the distribution of classes within each fold, ensuring a balanced stratification.
#'
#' @param df_labeled A data frame containing labeled data, including columns `fold` (fold number) and `strat_inter` (lithostratigraphic unit).
#' @param stratigraphic_order A character vector defining the desired order of stratigraphic units.
#' @param color_vector A named vector mapping lithostratigraphic units to specific colors.
#'
#' @return A `ggplot2` object displaying the class proportions across folds.
#' @export
plot_class_prop_per_fold <- function(df_labeled, stratigraphic_order,color_vector) {
  num_folds <- length(unique(df_labeled$fold))

  #Colorblind Friendly Palette
  custom_colors <- RColorBrewer::brewer.pal(n = num_folds, name = "Dark2")

  # Create a list to store data for each fold
  fold_list <- lapply(unique(df_labeled$fold), function(fold_num) {
    fold_data <- df_labeled %>%
      filter(fold == fold_num) %>%  # Filter by fold and exclude "NN"
      group_by(strat_inter) %>%                          # Group by strat_inter
      summarise(Proportion = n() / nrow(.) * 100) %>%     # Calculate proportions
      mutate(Fold = fold_num)            # Add Fold column
    return(fold_data)
  })

  # Combine all the fold data into a single data frame
  fold_data_combined <- do.call(rbind, fold_list)

  fold_data_combined <-  fold_data_combined %>%
    mutate(strat_inter = gsub("NUNA", "NA", strat_inter))

  # Set 'strat_inter' as a factor with levels defined by 'stratigraphic_order'
  fold_data_combined$strat_inter <- factor(fold_data_combined$strat_inter, levels = stratigraphic_order)

  # Extract the strat_inter categories

  color_vector["NA"] <- color_vector["NUNA"]

  strat_inter <-  levels(fold_data_combined$strat_inter)[levels(fold_data_combined$strat_inter) %in% fold_data_combined$strat_inter]
  strat_inter_color <- color_vector[strat_inter]

  strip <- ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = strat_inter_color))

  fold_data_combined <- fold_data_combined %>%
    mutate(Fold=as.factor(Fold))

  # Create and return the bar plot
  plot <- ggplot(fold_data_combined, aes(x = strat_inter, y = Proportion, fill = Fold)) +
    geom_bar(stat = "identity", position = position_dodge2()) +
    labs(title = "Class Proportions Across Folds",
         x = "Lithostratigraphic Unit",
         y = "Proportion (%)") +
    theme_bw() +
    ggh4x::facet_grid2(.~strat_inter,scales = "free_x",
               switch = "x",strip=strip)+
    scale_y_sqrt(breaks = c(seq(0, 1, by = 0.2), seq(2, 26, by = 2))) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
           plot.title = element_text(size=20),
          strip.text=element_text(size=11),
          axis.title.x = element_text(size=16),
          axis.title.y = element_text(size=16)) +
    scale_fill_manual(values = custom_colors)

  return(plot)
}
