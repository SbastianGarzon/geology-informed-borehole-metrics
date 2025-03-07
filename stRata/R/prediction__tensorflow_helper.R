#' Convert Tensorflow Model Predictions to DataFrame with Predicted Labels
#'
#' This function converts model predictions (in tensor format) into a data frame and associates the predicted labels with the highest probability
#' from the predictions.
#'
#' @param predictions A TensorFlow tensor containing model predictions, with shape `[batch_size, num_classes]`.
#' @param df_pred A data frame containing the original data that the predictions correspond to.
#' @param col_names A character vector of column names representing the class labels corresponding to the predictions.
#'
#' @return A data frame with the original input data (`df_pred`) and an additional column `prediction`,
#'         which contains the predicted class labels based on the highest prediction probability.
#'
#' @export
predictions_to_df <- function(predictions,df_pred,col_names){

  list_data <- list()
  for (i in 1:as.integer(predictions$shape[1])){
    list_data[[i]] <- as.data.frame(predictions[i,,]$numpy())
  }

  df <- do.call(rbind, list_data)

  colnames(df) <- col_names

  print(head(df))

  df <- as.data.frame(df)

  df <- df %>% dplyr::rowwise() %>% dplyr::mutate(prediction = col_names[which.max(dplyr::c_across(dplyr::everything()))])

  df_predictions <- cbind(df_pred,df)

  df_predictions$prediction <- factor(df_predictions$prediction)

  return(df_predictions)

}
