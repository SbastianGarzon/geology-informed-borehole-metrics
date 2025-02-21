library(vecsets)
library(zoo)
library(randomForest)
library(caret)
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(tictoc)
library(patchwork)
library(data.table)
library(tictoc)
install.packages("../stRata", repos = NULL, type = "source", dependencies = TRUE, ask = FALSE)
library(stRata)
library(caret)
library(recipes)
library(tensorflow)
library(reticulate)


# Hyperparameters

num_heads <- c(8,4,1)
num_lstm <- c(64,32,16)
learning_rate_list <- c(0.001,0.01)
epochs <- 300
patience <- 30
batch_size <- 32

# Experiment name

args <- commandArgs(trailingOnly=TRUE)

experiment_name <- toString(args[[1]])
print(experiment_name)
print(file.path(experiment_name,"df_labeled.csv"))

# Stratigraphic order

stratigraphic_order <- c(
  "AAOP","AAES","NUNA",
  "NA","NASC","NAZA","NAWA1","NAWA2","NAWO","NAWOBE",
  "EC","EC1","EC2",
  "NI","NIGR","NIHO","NIBA",
  "BX","BXKO","BXSI","BXDE","BXWI","BXLM","BXSC","BXBS",
  "BE","BEWY","BERO",
  "HT",
  "KR","KR1","KR2","KRWY",
  "KW",
  "WB",
  "EE",
  "DRGI",
  "DN",
  "UR",
  "UR1","UR2",
  "PE",
  "AP",
  "ST","SY","PZWA","MS","OO","KI","IE","BRVI","VE","RU","TO","DO","LA","HO","MT","VA","AK","DIEP","GE","NN",NA)


# Load Data
df_labeled <- read.csv2(file.path(experiment_name,"df_labeled.csv"),sep = ";")

# Loading preprocessed expected units per borehole location

units_per_borehole_coord <- read.csv2(
  file.path("..","Data","units_per_borehole_coord.csv"),sep=";")

df_labeled <- df_labeled %>% mutate(original_index=row_number())

# Load the CSV
strat_class_map_df <- fread("../Data/strat_class_map.csv",sep=",")

# Convert back to a named vector
strat_class_map <- setNames(
  as.character(strat_class_map_df$value), strat_class_map_df$key)

# Extract known transitions
known_transitions <- stRata::extract_known_transitions(df_labeled)


## Outer loop

##### Nested Loop ########

num_folds <- length(unique(df_labeled$fold))
all_metrics <- c()

result_list <- list()

##### Recipe for the Neural Network ########

df_labeled_rec <- df_labeled %>% select(c(x_28992,y_28992,depth,strat_inter))

recipe <- recipe(strat_inter~., data = df_labeled_rec) %>%
  step_impute_mode(all_nominal(), -all_outcomes()) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_dummy(all_outcomes(), one_hot = TRUE) %>%                  
  step_normalize(c("depth","x_28992","y_28992")) %>%
  prep(training = df_labeled_rec)

# Set the number of threads

tf$compat$v1$config$threading$set_intra_op_parallelism_threads(as.integer(8))
tf$compat$v1$config$threading$set_inter_op_parallelism_threads(as.integer(8))

# Enable XLA

py_run_string("
import tensorflow as tf
tf.config.optimizer.set_jit(True)  # Enable XLA
")


# Reproducible code
set.seed(2014)

tf$compat$v1$reset_default_graph()
Sys.setenv("OMP_NUM_THREADS" = 8)
tf$keras$backend$clear_session()
tf$keras$utils$set_random_seed(as.integer(2013))

# Outer loop
tic("Outer Loop")

folds <- c(1:num_folds)

output_folder <- file.path(experiment_name,"NN1")

for(i in folds){
  
  # Select test fold
  test <- i
  
  # Select the validation fold (next fold in circular order)
  validation <- ifelse(i == length(folds), folds[1], folds[i + 1])
  
  # Select the training folds (remaining folds)
  train <- setdiff(folds, c(test, validation))
  
  df_train <- df_labeled %>% filter(fold %in% train)
  df_validation <- df_labeled %>% filter(fold %in% validation)
  df_test <- df_labeled %>% filter(fold %in% test)
  
  splits_train <- c(FALSE, df_train$nr[-1] != df_train$nr[-nrow(df_train)])
  splits_validation <- c(FALSE, df_validation$nr[-1] != df_validation$nr[-nrow(df_validation)])
  splits_test <- c(FALSE, df_test$nr[-1] != df_test$nr[-nrow(df_test)])
  
  df_train_NN1 <- df_train %>% select(depth,x_28992,y_28992,strat_inter)
  df_validation_NN1 <- df_validation %>% select(depth,x_28992,y_28992,strat_inter)
  df_test_NN1 <- df_test %>% select(depth,x_28992,y_28992,strat_inter)
  
  df_train_NN1 <- bake(recipe, new_data = df_train_NN1) 
  df_validation_NN1 <- bake(recipe, new_data = df_validation_NN1)
  df_test_NN1 <- bake(recipe, new_data = df_test_NN1) 
  
  train_data <- stRata::preprocess_data(df_train_NN1,splits_train)
  val_data <- stRata::preprocess_data(df_validation_NN1,splits_validation)
  test_data <- stRata::preprocess_data(df_test_NN1,splits_test)
  
  model_index <- 1
  
  dir.create(output_folder)
  output_folder_fold <- file.path(output_folder,paste0("fold_",i))
  dir.create(output_folder_fold)
  
  write.csv(train_data$df, file.path(output_folder_fold,paste0("fold_",i,"_train_data.csv")))
  write.csv(val_data$df, file.path(output_folder_fold,paste0("fold_",i,"_val_data.csv")))
  write.csv(test_data$df, file.path(output_folder_fold,paste0("fold_",i,"_test_data.csv")))
  
  ## Inner Loop
  
  for(num_heads_att in num_heads){
    for(num_lstm_units in num_lstm){
      for(learning_rate in learning_rate_list){
        
        message <- paste("Model Index:", model_index, "\n Configuration: Heads =", num_heads_att,
                         " \n LSTM Units =", num_lstm_units,
                         " \n Learning Rate =", learning_rate)
        cat(message)
        
        # Start timing
        tic("Model training")
        model <- build_model(input_shape = train_data$value_ragged_tensor$shape[3],
                             target_dim = train_data$target_ragged_tensor$shape[3],
                             num_heads = num_heads_att,
                             lstm_units = num_lstm_units,
                             learning_rate = learning_rate)
        
        history <- train_model(model, train_data, val_data,epochs = epochs, batch_size = batch_size,patience=patience)
        toc(log=TRUE)
        
        # Convert history metrics into a data frame
        history_df <- data.frame(
          epoch = 1:length(history$history$loss),
          loss = history$history$loss,
          val_loss = history$history$val_loss
        )
        
        # Plot the learning curve
        learning_curve_plot <- ggplot(history_df, aes(x = epoch)) +
          geom_line(aes(y = loss, color = "Training Loss")) +
          geom_line(aes(y = val_loss, color = "Validation Loss")) +
          labs(title = "Learning Curve",
               subtitle = paste("Fold:", i,
                                "| LSTM Units:", num_lstm_units,
                                "| Learning Rate:", learning_rate,
                                "| Num Heads:", num_heads_att,
                                "| Batch size:",batch_size),
               x = "Epoch",
               y = "Loss",
               color="Set")+
          scale_color_manual(values = c("Training Loss" = "blue",
                                        "Validation Loss" = "red")) +
          theme_bw()
        
        # Specify the folder and filename to export
        
        learning_rate_str <- gsub("\\.", "_", sprintf("%.3f", learning_rate))
        output_file <- paste0("fold_",i,"_LSTM_",num_lstm_units,
                              "_Num_heads_",num_heads_att,"_LR_",
                              learning_rate_str,".png")
        
        loss_folder <- file.path(output_folder_fold,"training_val_loss_graph")
        dir.create(loss_folder, showWarnings = FALSE)
        
        output_file <- file.path(loss_folder, output_file)
        
        # Save the plot
        ggsave(output_file, plot = learning_curve_plot, width = 8, height = 6)
        
        # Predict
        
        test_predictions <- model %>% 
          tf$keras$Model$predict(test_data$value_ragged_tensor)
        col_names <- test_data$target_names
        
        predictions_test <- predictions_to_df(
          test_predictions,df_test,col_names = col_names)
        
        # Check if depth exists in predictions_validation
        if (!"depth" %in% names(predictions_test)) {
          # If depth doesn't exist, assign depth values
          predictions_test$depth <- df_labeled$depth[df_test$original_index]
        } else {
          # If depth exists, confirm that the values are the same
          if (identical(df_labeled$depth[df_test$original_index], predictions_test$depth)) {
            print("Depth values match between df_labeled and predictions_validation.")
          }
        }
        
        predictions_test <- predictions_test %>% select(all_of(c("original_index","nr", "depth", col_names,"strat_inter","prediction")))
        
        predictions_test$target_variable <- predictions_test$strat_inter
        
        training_folder <- file.path(output_folder_fold,"predictions_test")
        
        dir.create(training_folder,showWarnings = FALSE)
        
        csv_file <- file.path(training_folder,paste0("/fold_",i,"_LSTM_",num_lstm_units,"_Num_heads_",num_heads_att,"_LR_",learning_rate_str,".csv"))
        
        write.csv(predictions_test, file = csv_file)
        
        tic("Evaluation Metrics - Train and Validation")
        
        # Compute metrics
        metrics_evaluation_test <- metrics_evaluation(predictions_test)
        
        combined_metrics <- data.frame(t(c(metrics_evaluation_test)))
        
        n_epochs <- length(history$history$loss)
        
        n_epochs <- ifelse(n_epochs==epochs,epochs,n_epochs-patience)
        
        loss <- history$history$loss[n_epochs]
        val_loss <- history$history$val_loss[n_epochs]
        accuracy <- history$history$python_function[n_epochs]
        val_accuracy <- history$history$val_python_function[n_epochs]
        
        fold_results <- data.frame(
          fold = i,
          val_loss = val_loss,
          val_acc = val_accuracy,
          loss = loss,
          accuracy = accuracy,
          num_heads = num_heads_att,
          lstm_units = num_lstm_units,
          learning_rate = learning_rate,
          n_epochs = n_epochs
        )
        
        fold_results <- cbind(fold_results, combined_metrics)
        
        result_list[[paste(i,num_heads_att, num_lstm_units, learning_rate, sep = "_")]] <- fold_results
        model_index <- model_index + 1

      }
    }
  }
}

combined_df <- do.call(rbind, result_list)

# Save the results

write.csv2(combined_df, file.path(experiment_name,"NN1_tuning_results.csv"), row.names = FALSE)
