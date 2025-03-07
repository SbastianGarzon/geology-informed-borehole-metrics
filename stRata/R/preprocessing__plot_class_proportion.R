#' Plot Class Proportion
#'
#' This function creates a bar plot showing the proportion of each lithostratigraphic unit in the dataset.
#'
#' @param df_labeled A data frame containing labeled data, including the `strat_inter` column representing lithostratigraphic units.
#' @param color_vector A named vector mapping lithostratigraphic units to specific colors.
#' @param stratigraphic_order A character vector defining the desired order of stratigraphic units.
#'
#' @return A `ggplot2` object displaying class proportions.
#' @export
plot_class_proportion <- function(df_labeled,color_vector,stratigraphic_order){

  distribution_classes <- df_labeled %>% group_by(strat_inter) %>% count(strat_inter)

  color_vector["NA"] <- color_vector["NUNA"]

  distribution_classes$prop <- (distribution_classes$n)/(sum(distribution_classes$n))*100

  distribution_classes <- distribution_classes %>%
    mutate(strat_inter = gsub("NUNA", "NA", strat_inter))

  distribution_classes <- distribution_classes %>%
    mutate(strat_inter = factor(strat_inter, levels = stratigraphic_order)) %>%
    arrange(strat_inter)

  plot <- ggplot(distribution_classes, aes(x = strat_inter, y = prop,fill=strat_inter)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_vector) +
    ggh4x::facet_grid2(.~strat_inter,scales = "free_x",
                switch = "x")+
    scale_y_sqrt(breaks = c(seq(0, 1, by = 0.5), seq(2, 25, by = 2)))+
    labs(
      title = "Lithostratigraphic Units Distribution",
      x = "Units",
      y = "Class proportion (%) (Square Root Scale)"
    )+
    theme_bw() +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          plot.title = element_text(size=12),
          strip.text=element_text(size=9),
          axis.title.x = element_text(size=12),
          axis.title.y = element_text(size=12),
          legend.position = "none")

  return(plot)

}
