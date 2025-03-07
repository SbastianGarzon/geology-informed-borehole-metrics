#' Build a Bidirectional LSTM Model with Optional Multi-Head Attention
#'
#' This function constructs a deep learning model with a bidirectional LSTM layer and a multi-head attention layer.
#' It also compiles the model with categorical crossentropy loss and a customizable learning rate.
#'
#' @param input_shape An integer specifying the shape of the input data, excluding the batch size.
#' @param target_dim An integer specifying the number of output classes (i.e., the target dimension).
#' @param num_heads An integer (default = 16) specifying the number of attention heads in the multi-head attention layer. If 0, attention is omitted.
#' @param lstm_units An integer (default = 32) specifying the number of units in the LSTM layer.
#' @param learning_rate A numeric value (default = 1e-3) specifying the learning rate for the Adam optimizer.
#'
#' @return A compiled Keras model.
#' @export
build_model <- function(input_shape, target_dim, num_heads = 16, lstm_units = 32,learning_rate=1e-3) {

  tf$keras$backend$clear_session()

  # Define the input layer
  input_layer <- tf$keras$Input(shape = list(NULL, as.integer(input_shape)), ragged = TRUE)

  # Define the LSTM and attention layers
  lstm_layer1 <- tf$keras$layers$Bidirectional(tf$keras$layers$LSTM(units = as.integer(lstm_units), return_sequences = TRUE))(input_layer)

  if (num_heads != 0) {
    attention_layer <- tf$keras$layers$MultiHeadAttention(num_heads = as.integer(num_heads),key_dim = as.integer(16))(lstm_layer1, lstm_layer1)
    output_layer <- tf$keras$layers$Dense(units = as.integer(target_dim), activation = 'softmax')(attention_layer)
  } else {
    output_layer <- tf$keras$layers$Dense(units = as.integer(target_dim), activation = 'softmax')(lstm_layer1)
  }

  # Create and compile the model
  model <- tf$keras$Model(inputs = input_layer, outputs = output_layer)
  model %>% tf$keras$Model$compile(
    loss = 'categorical_crossentropy',
    optimizer = tf$keras$optimizers$legacy$Adam(learning_rate = learning_rate),
    metrics = c(custom_accuracy)
  )

  return(model)
}


#' Train a Keras Model with Early Stopping
#'
#' This function trains a Keras model using the provided training and validation data. It includes early stopping to prevent
#' overfitting. The model training stops when the validation loss does not improve for a specified number of epochs (`patience`).
#' @param model A compiled Keras model to be trained.
#' @param train_data A list containing the training data. It should include `value_ragged_tensor` (input features)
#'        and `target_ragged_tensor` (target labels).
#' @param val_data A list containing the validation data.
#' @param epochs An integer (default = 100) specifying the number of training epochs.
#' @param batch_size An integer (default = 16) specifying the batch size used during training.
#' @param patience An integer (default = 10) specifying the number of epochs with no improvement in validation loss before stopping the training.
#' @return The history object containing the training process metrics (e.g., loss, accuracy).
#' @export
train_model <- function(model, train_data, val_data, epochs = 100, batch_size = 16, patience=10) {

  early_stopping <- tf$keras$callbacks$EarlyStopping(
    monitor = 'val_loss',
    patience = as.integer(patience),
    restore_best_weights = TRUE
  )

  log_dir <- "logs/fit/" # Ensure this path exists

  # Add TensorBoard callback
  tensorboard_callback <- tf$keras$callbacks$TensorBoard(log_dir = log_dir, histogram_freq = 1)

  history <- model %>% tf$keras$Model$fit(
    x = train_data$value_ragged_tensor,
    y = train_data$target_ragged_tensor,
    epochs = as.integer(epochs),
    batch_size = as.integer(batch_size),
    validation_data = list(val_data$value_ragged_tensor, val_data$target_ragged_tensor),
    callbacks = list(early_stopping, tensorboard_callback)
  )

  return(history)
}

#' Preprocess Data for TensorFlow Model
#'
#' This function preprocesses a data frame by separating the input features and target variables, converting them into TensorFlow constants,
#' and then creating ragged tensors for use in TensorFlow models. The function also prepares the necessary column names and splits the data
#' based on provided indices.
#'
#' @param df A data frame containing the input data and target variables. Target variables must start with `strat_inter_`.
#' @param splits_ind An integer vector indicating the row indices used for splitting the data.
#'
#' @return A list containing the following elements:
#' \item{value_ragged_tensor}{A TensorFlow RaggedTensor of the input feature data.}
#' \item{target_ragged_tensor}{A TensorFlow RaggedTensor of the target variable data.}
#' \item{values_names}{A character vector of column names for the input features.}
#' \item{target_names}{A character vector of column names for the target variables}
#' \item{df}{The original input data frame.}
#'
#' @export
preprocess_data <- function(df,splits_ind) {

  # Convert to TensorFlow constants
  values <- as.matrix(select(df, -starts_with("strat_inter")))

  target <- as.matrix(select(df, starts_with("strat_inter")))

  values_names <- colnames(select(df, -starts_with("strat_inter")))

  target_names <- colnames(select(df, starts_with("strat_inter")))

  target_names <- sub("^strat_inter_", "", target_names)

  splits <- as.integer(c(0, which(splits_ind) - 1, length(splits_ind)))

  print(values_names)
  print(target_names)

  values_tensor <- tf$constant(values, dtype = tf$float64)
  target_tensor <- tf$constant(target, dtype = tf$float64)

  row_splits_tensor <- tf$constant(splits, dtype = tf$int64)

  value_ragged_tensor <- tf$RaggedTensor$from_row_splits(values_tensor, row_splits_tensor)
  target_ragged_tensor <- tf$RaggedTensor$from_row_splits(target_tensor, row_splits_tensor)

  return(list(value_ragged_tensor = value_ragged_tensor, target_ragged_tensor = target_ragged_tensor,values_names=values_names,target_names=target_names, df = df))
}

#' Custom Accuracy Metric for Classification
#'
#' This function computes the accuracy of a classification model by comparing the predicted and true labels.
#' It uses the argmax of the predicted and true labels to determine the class with the highest probability and checks if they match.
#' The accuracy is then calculated as the mean of the correct predictions.
#'
#' @param y_true A TensorFlow tensor containing the true labels in one-hot encoded format.
#' @param y_pred A TensorFlow tensor containing the predicted probabilities.
#'
#' @return A TensorFlow tensor representing the mean accuracy, where values range from 0 to 1.
#' @export
custom_accuracy <- function(y_true, y_pred) {
  a <- tf$argmax(y_pred, axis = as.integer(1))
  b <- tf$argmax(y_true, axis = as.integer(1))
  custom_accuracy_score <- tf$math$equal(a, b)
  return(tf$reduce_mean(tf$cast(custom_accuracy_score, tf$float32)))
}

