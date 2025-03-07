#' Load basisdata and transform into a GeoDataFrame
#' @param file_path path to basisdata
#' @param crs_target CRS code
#' @param cords_names list with names of x and y parameter
#' @return A GeoDataFrame
#' @export
load_dataset <- function(file_path, crs_target,cords_names=c("x","y"),area_of_interest){

  data_df <- data.table::fread(file_path,na.strings = c("","-"))
  data_sfc <- data_df |> sf::st_as_sf(coords=cords_names, crs= crs_target)
  data_sfc <-data_sfc %>% dplyr::mutate(x_28992 = sf::st_coordinates(.)[,1],
                                      y_28992 = sf::st_coordinates(.)[,2])
  data_sfc$area <- as.logical(st_intersects(data_sfc, area_of_interest))

  data_sfc
}
