#' Get Stratigraphic Unit Colors
#'
#' This function reads a color mapping file and returns a named color vector for stratigraphic units.
#'
#' @param path_to_colors A string specifying the path to the CSV file containing color information.
#' @param stratigraphic_order A character vector defining the desired order of stratigraphic units.
#' @return A named vector where stratigraphic unit names are mapped to their corresponding RGB colors.
#' @export
get_strat_colors <- function(path_to_colors,stratigraphic_order){

  strat_colors <- read.csv(path_to_colors,sep = ";")
  strat_colors$RGB_Code <- rgb(strat_colors$R, strat_colors$G, strat_colors$B, maxColorValue = 255)
  strat_colors <- strat_colors[match(stratigraphic_order, strat_colors$Eenheid), ]
  color_vector <- setNames(strat_colors$RGB_Code, strat_colors$Eenheid)

  return(color_vector)

}
