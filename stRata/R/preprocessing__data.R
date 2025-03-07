#' Adjust TOP and BASIS to NAP (Normal Amsterdams Peil)
#' @param dgm_extractie geodataframe DGM_extractie
#' @return A GeoDataFrame
#' @export
dgm_to_nap <- function(dgm_extractie){

  # Replacing NA values to "NA" (Naaldwijk Formation)
  dgm_extractie$STRAT <- dgm_extractie$STRAT |> tidyr::replace_na("NA")

  # Adjusting TOP and Basis value to NAP (Normal Amsterdams Peil
  dgm_extractie$TOP <- dgm_extractie$MV-dgm_extractie$TOP
  dgm_extractie$BASIS <- dgm_extractie$MV-dgm_extractie$BASIS
  dgm_extractie
}

#' Assigns "strat_inter" values to basisdata based on dgm_extractie (Normal Amsterdams Peil)
#' @param basisdata dataframe with basisdata
#' @param dgm_extraction dataframe with dgm_extractie
#' @return basis_dgm_roer_combined
#' @export
merge_basis_dgm <- function(basisdata,dgm_extractie,stratigraphic_order){

  # Selecting area of interest
  dgm_extractie_roer <- dgm_extractie[!is.na(dgm_extractie$area),] %>% st_drop_geometry()


  # Selecting boreholes with labeled data

  basis_roer <- basisdata %>% filter(!is.na(area) & nr %in% unique(dgm_extractie_roer$NR))

  # Selecting boreholes that have the same "MV"

  basis_dgm_roer_combined <- basis_roer %>% dplyr::left_join(dgm_extractie_roer%>%select(NR,STRAT,MV,TOP,BASIS),by=c("nr"="NR"),relationship = "many-to-many")

  rm_boreholes <- unique(basis_dgm_roer_combined %>% dplyr::filter(!mv==MV) %>% select(nr) %>% pull(nr))

  print(paste("Removing borehole (Different mv)",rm_boreholes))

  basis_dgm_roer_combined <- basis_dgm_roer_combined %>% dplyr::filter(!nr %in% rm_boreholes)
  basisdata <- basisdata %>% dplyr::filter(!nr %in% rm_boreholes)

  basis_dgm_roer_combined$thickness <- with(basis_dgm_roer_combined,top-bottom)
  basis_dgm_roer_combined$mid <- with(basis_dgm_roer_combined,top-(0.5*thickness))

  basis_dgm_roer_combined <- basis_dgm_roer_combined %>% dplyr::filter(mid>=BASIS)
  basis_dgm_roer_combined <- basis_dgm_roer_combined %>% dplyr::filter(mid<=TOP)

  basis_dgm_roer_combined <-  basis_dgm_roer_combined %>% dplyr::filter(!duplicated(cbind(nr, mid))) %>% st_drop_geometry()

  basisdata <- basisdata %>% dplyr::left_join(basis_dgm_roer_combined %>% dplyr::select(nr,mv,top,bottom,STRAT), by= c("nr", "mv", "top", "bottom")) %>% st_drop_geometry()
  basisdata$strat_inter <- basisdata$STRAT
  basisdata <- basisdata %>% dplyr::filter(!is.na(area)) %>% dplyr::select(-c(area,STRAT))

  basis_dgm_roer_combined <- basisdata %>% dplyr::mutate(strat_inter=factor(strat_inter,levels=stratigraphic_order))

  return(basis_dgm_roer_combined)
}


#' Discretize Intervals
#'
#' This function discretizes intervals for each unique `nr` in the `basis_dgm_roer_combined` dataset.
#' It calculates the highest `top` and lowest `bottom` values for each `nr`, creates sequences
#' between these values at specified intervals, and matches the sequences with the original data.
#'
#' @param basis_dgm_roer_combined A data frame containing the original data with columns `nr`, `top`, and `bottom`.
#' @param interval A numeric value specifying the interval for sequence generation. Default is 0.5.
#'
#' @return A data.table with discretized sample points matched to the original data.
#' @import data.table
#'
#' @export
discretize_intervals <- function(basis_dgm_roer_combined, interval = 0.5) {

  # Convert dataset to data.table
  dt <- as.data.table(basis_dgm_roer_combined)

  # Aggregate rows by 'nr' and calculate the top and bottom values
  aggregated_rows <- dt[, .(
    top = max(top),
    bottom = min(bottom)
  ), by = nr]

  # Function to create the sequences
  create_sequences <- function(top, bottom) {
    seq(top, bottom, by = -interval)
  }

  # Create sequences for each aggregated row
  empty_sample_points <- aggregated_rows[, .(depth = create_sequences(top, bottom)), by = nr]

  # Print the number of generated sample points
  print(nrow(empty_sample_points))

  # Set keys for joining
  setkey(empty_sample_points, nr, depth)
  setkey(dt, nr)

  # Perform a non-equi join to match based on top and bottom ranges
  sample_points <- dt[empty_sample_points, on = .(nr, top >= depth, bottom <= depth), nomatch = 0L,allow.cartesian = TRUE]

  # Set keys for the sample points
  setkey(sample_points, nr, top, bottom)

  # Identify non-duplicated rows based on 'nr', 'top', and 'bottom'
  not_duplicated <- !duplicated(sample_points[, .(nr, top, bottom)])

  # Filter non-duplicated rows
  discretized_sample_points <- sample_points[not_duplicated]

  setnames(discretized_sample_points, "top", "depth") # Change top to depth
  discretized_sample_points[, bottom := NULL]  # Remove "bottom" column

  setorder(discretized_sample_points, nr, -depth)  # Order by 'nr' and 'depth'

  # Return the discretized sample points
  return(discretized_sample_points)
}


#' Copy information from the next and previous rows for multiple columns
#'
#' This function calculates the index of the next and previous rows within each borehole group,
#' copies the information from the next and previous rows for the specified columns, and renames
#' the columns accordingly.
#'
#' @param df Merged dataframe with basisdata
#' @param cols Vector of column names to copy information from the next and previous rows
#' @return Dataframe with copied information from the next and previous rows
#' @export
copy_next_previous_row_info <- function(df, cols) {
  df <- df %>%
    mutate(row_num = row_number()) %>%
    group_by(nr) %>%
    mutate(
           across(all_of(cols), ~lead(.), .names = "next_{.col}"),
           across(all_of(cols), ~lag(.), .names = "previous_{.col}")) %>%
    ungroup() %>%
    select(-row_num)
  return(df)
}

#' Simplify Classes
#'
#' Simplify the classes of a specific column in a data frame using a provided class map.
#'
#' @param data A data frame. The data frame containing the column whose classes need to be simplified.
#' @param column A character string specifying the column name whose classes need to be simplified.
#' @param class_map A named character vector. A mapping of existing class names to their simplified versions.
#'                  The names of the vector represent the existing classes, and the values represent the
#'                  corresponding simplified classes.
#'
#' @return A modified version of the input data frame where the specified column's classes have been simplified
#'         according to the provided class map.
#'
#' @export
simplify_classes <- function(data, column, class_map) {
  to_replace <- data[[column]] %in% names(class_map)
  data[[column]][to_replace] <- class_map[as.character(data[[column]][to_replace])]
  return(data)
}

#' Stratified Group Sampling
#'
#' Function to perform stratified group sampling
#' # Implementation is based on this kaggle kernel:
# https://www.kaggle.com/jakubwasikowski/stratified-group-k-fold-cross-validation
#' This function performs stratified group sampling on a dataset where each group has a target variable. It distributes the groups into folds while ensuring that the class proportions are maintained in each fold.
#'
#' @param df A data frame containing the dataset to be sampled.
#'
#' @return A list containing the indices of groups for training and testing.
#'
#' @export
stratified_group_sampling_k_fold <- function(df,kfolds = 10){

  k <- kfolds
  # Group dataframe by group
  grouped_data <- df %>% group_by(nr)

  # Compute initial class proportions of all dataset

  class_proportions <-table(grouped_data[["strat_inter"]])

  # Compute class counts per group
  y_counts_per_group_df <- grouped_data %>% select(strat_inter) %>% table() %>% as.data.frame.matrix()

  # Define an initial matrix to store the class count per fold
  class_counts_per_fold <- matrix(0, nrow = k, ncol = nrow(class_proportions))
  colnames(class_counts_per_fold) <- levels(unique(grouped_data$strat_inter))

  # Shuffle the order of the dataset
  y_counts_per_group_df <- y_counts_per_group_df[order(sample(rownames(y_counts_per_group_df))),]

  # Organize the boreholes based on the initial sd (Decreasing)
  y_counts_per_group_df <- y_counts_per_group_df[order(apply(y_counts_per_group_df, 1, sd, na.rm = TRUE), decreasing = TRUE), ]

  # Define unique groups for loop

  unique_groups <- unique(rownames(y_counts_per_group_df))

  # Define list of store groups in each fold
  groups_per_fold <- vector("list",k)

  # Loop

  for (group_name in unique_groups) {

    # Subset data for current group
    group_data <- y_counts_per_group_df %>% filter(rownames(y_counts_per_group_df)==group_name) %>% as.matrix()

    min_eval <- Inf
    best_fold <- NULL
    for(fold in 1:k){
      class_counts_temp <- class_counts_per_fold
      class_counts_temp[fold,] <- class_counts_temp[fold,]+group_data
      prop_matrix <- sweep(class_counts_temp, 2, class_proportions, "/")
      column_sds <- apply(prop_matrix, 2, sd, na.rm = TRUE)
      fold_eval <- mean(column_sds,na.rm=TRUE)
      if(fold_eval < min_eval){
        min_eval <- fold_eval
        best_fold <- fold
      }
    }
    class_counts_per_fold[best_fold,] <- class_counts_per_fold[best_fold,] + group_data
    groups_per_fold[[best_fold]] <- c(groups_per_fold[[best_fold]],group_name)

  }

  #index_list <- list(train=train_groups,test=test_groups)
  names(groups_per_fold) <- paste0(1:length(groups_per_fold))
  return(groups_per_fold)

}

#' One-Hot Encoding
#'
#' Function to perform one-hot encoding on a data frame.
#'
#' This function converts categorical variables into a one-hot encoded format,
#' creating dummy variables for each category.
#'
#' @param data A data frame containing the dataset to be one-hot encoded.
#' @return A data frame with one-hot encoded variables.
#' @examples
#' data <- data.frame(
#'   nr = c(1, 2, 1, 3),
#'   category = c("A", "B", "A", "C")
#' )
#' one_hot_encoding(data)
#' @importFrom caret dummyVars
#' @export
one_hot_encoding <- function(data){
  dummy_rf <- caret::dummyVars(" ~ . - nr ", data=data)
  df_rf1_enc <- data.frame(predict(dummy_rf,newdata=data))
  df_rf1_enc$nr <- data$nr
  return(df_rf1_enc)
}

#' Identify Units at a Specific Coordinate
#'
#' This function identifies the units that are present at a given coordinate within a spatial dataset. The function takes in a spatial object containing different units (e.g., geological layers) and checks which of these units intersect with the provided coordinate.
#'
#' @param units An `sf` object representing spatial layers, where each feature corresponds to a different unit. This object should have a `layer_name` column, which contains the names of the units.
#' @param 28992 A numeric value representing the X-coordinate (easting) in the Amersfoort / RD New (EPSG: 28992) coordinate system.
#' @param y_28992 A numeric value representing the Y-coordinate (northing) in the Amersfoort / RD New (EPSG: 28992) coordinate system.
#'
#' @return A character vector containing the names of the units (from the `layer_name` column) that intersect with the provided coordinate. If no units intersect, the function returns an empty vector.
#'
#' @export
units_in_coordinate <- function(units, x_28992, y_28992) {

  coord <- c(x_28992, y_28992)

  # Create a point geometry
  point <- sf::st_point(coord)

  # Set the same CRS as your layers
  point_sf <- sf::st_sfc(point, crs = sf::st_crs(units))

  # Find intersecting units
  units_in_loc <- units$layer_name[sf::st_intersects(units, point_sf, sparse = FALSE)]

  return(units_in_loc)
}

#' Generate a Binary Matrix of Formations per Borehole Location
#'
#' This function processes a dataset of borehole locations and identifies which geological formations (units) are present at each location. It returns a binary matrix indicating the presence (1) or absence (0) of each formation for each borehole.
#'
#' @param unique_boreholes A data frame or tibble containing borehole information. This should include columns `nr` (borehole ID), `x_28992` (X-coordinate, easting), and `y_28992` (Y-coordinate, northing) in the Amersfoort / RD New (EPSG: 28992) coordinate system.
#' @param units_extent An `sf` object representing the spatial layers of geological formations (units), where each feature corresponds to a different formation. The object should include a `layer_name` column with the names of the formations.
#'
#' @return A binary matrix (data frame) where each row corresponds to a borehole and each column corresponds to a geological formation. The cells of the matrix contain binary values: `1` if the formation is present at the borehole's location, and `0` if it is absent.
#'
#' @details
#' The function iterates over each borehole in the `unique_boreholes` dataset, checking which formations intersect with the borehole's location using the `units_in_coordinate` function. The results are then converted into a binary matrix, where formations are represented as columns, and boreholes as rows.
#'
#' Progress is displayed in the console, with updates indicating the current borehole being processed.
#'
#'
#' @importFrom sf st_crs st_point st_sfc st_intersects
#' @export
formations_per_borehole_location<- function(unique_boreholes, units_extent) {

  results_df <- data.frame(nr = unique_boreholes$nr)

  for (i in 1:nrow(units_extent)) {
    # Get the current formation
    formation <- units_extent[i, ]

    formation_name <- as.character(formation$layer_name)
    cat(paste0("Processing formation: ", formation_name),"\r")
    # Check which coordinates are inside this formation
    inside <- st_intersects(unique_boreholes, formation, sparse = FALSE)
    results_df[[formation_name]] <- as.integer(inside)
  }
  return(results_df)
}

