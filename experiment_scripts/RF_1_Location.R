# Load libraries
library(vecsets)
library(zoo)
library(caret)
library(randomForest)
library(ranger)
library(tidyverse)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(tictoc)
library(zoo)
library(patchwork)
library(cowplot)
library(data.table)
library(tictoc)
install.packages("../stRata", repos = NULL, type = "source", 
                 dependencies = TRUE, ask = FALSE)

library(stRata)

start <- Sys.time()

# Extract experiment name from command argument
args <- commandArgs(trailingOnly=TRUE)

experiment_name <- toString(args[[1]])

# Paths for Exports
experiment_path <- file.path(experiment_name)

results_dir <- file.path(experiment_name,"RF1")

dir.create(file.path(results_dir))

# Loading merged dataset

df_labeled <- read.csv2(file.path(experiment_name,"df_labeled.csv"),sep = ";")

# Loading preprocessed expected units per borehole location

units_per_borehole_coord <- read.csv2(
  file.path("..","Data", "units_per_borehole_coord.csv"),sep=";")

# Load the stratigraphic map class
strat_class_map_df <- fread("../Data/strat_class_map.csv")

# Convert back to a named vector
strat_class_map <- setNames(as.character(strat_class_map_df$value), strat_class_map_df$key)

# Extract known transitions
known_transitions <- stRata::extract_known_transitions(df_labeled)

df_labeled <- df_labeled %>% mutate(original_index=row_number())

# Select features for the experiment (SET 1)
df_labeled_sel <- df_labeled %>% select(nr,depth,x_28992,y_28992,original_index)

# One hot encoding
df_labeled_enc <- stRata::one_hot_encoding(df_labeled_sel)
df_labeled_enc$target_variable <- as.factor(df_labeled$strat_inter)
df_labeled_enc$fold <- as.factor(df_labeled$fold)

##### Hyper-parameters ########

mtry <- c(1,2,3)

##### Nested Loop ########

# Number of folds
num_folds <- length(unique(df_labeled$fold))

# List to store metrics
all_metrics <- c()

folds <- c(1:num_folds)

# Set sed for reproducibility
set.seed(2013)

# Outer loop
tic("Outer Loop")
for(i in 1:num_folds){
  
  # Select test fold
  fold_test <- i
  
  # Select the validation fold (next fold in circular order)
  # For RF this fold is ignored
  fold_validation <- ifelse(i == length(folds), folds[1], folds[i + 1])
  
  # Select the training folds (remaining folds)
  folds_train <- setdiff(folds, c(fold_test, fold_validation))
  
  df_train <- df_labeled_enc %>% filter(fold %in% folds_train) 
  df_train_RF1 <- df_train %>% select(depth,x_28992,y_28992,target_variable)
  
  # Select the test fold
  df_test <- df_labeled_enc %>% filter(fold==fold_test) 
  df_test_RF1 <- df_test %>% select(depth,x_28992,y_28992,target_variable)
  
  dir.create(file.path(results_dir,paste0("fold_",i)))
  dir.create(file.path(results_dir,paste0("fold_",i),"predictions_test"))
  
  # Inner Loop
  
  for(m in mtry){
    cat("\n")
    cat("#######################################\n")
    cat("## Training Fold: ", i, "- mtry: ", m, "     ##\n")
    cat("#######################################\n")
    tic("Running time:")
    
    # Train the model using the train set (4 out of 5 folds)
    tic("Trainig time:")
    rf_model <- ranger(target_variable ~ ., data = df_train_RF1,
                       mtry = m,probability = TRUE,num.thread = 20)
    toc(log=TRUE)
    
    # Predict on the test set (1 out of 5 folds)
    predictions <- predict(rf_model, data = df_test_RF1,type="response")
    
    predicted_classes <- colnames(predictions$predictions)[apply(predictions$predictions, 1, which.max)]
    
    df_predictions <- predictions$predictions %>% as.data.frame()
    
    df_test_RF1_metrics <- df_test_RF1
    
    df_test_RF1_metrics$prediction <- predicted_classes
    df_test_RF1_metrics$nr <- df_test$nr
    df_test_RF1_metrics$original_index <- df_test$original_index
    
    # Evaluate metrics
    metrics_eval <- metrics_evaluation(df_test_RF1_metrics)
    
    # Creating dataframe with predictions
    
    df_export <- cbind(df_test_RF1_metrics,df_predictions)
    
    df_export <- df_export %>% select(nr,depth,original_index,colnames(df_predictions),target_variable,prediction)
    
    metrics_eval["fold"] <- i
    metrics_eval["mtry"] <- m
    
    # Save predictions
    
    file_name <-  paste0("fold_",i,"_mtry_",m,".csv")
    write.csv2(df_export,file=file.path(results_dir,paste0("fold_",i),"predictions_test",file_name),row.names = FALSE)
    
    # Store metric results in the list
    all_metrics[[paste0("Fold_", i)]][[paste0("mtry_", m)]] <- metrics_eval
    toc(log=TRUE)
  }
}
toc(log=TRUE)


# Initialize an empty data frame to store results
RF1_metrics_by_fold <- do.call(rbind, lapply(names(all_metrics), function(fold_name) {
  do.call(rbind, lapply(names(all_metrics[[fold_name]]), function(mtry_name) {
    # Extract the metrics for the current fold and mtry
    metrics <- all_metrics[[fold_name]][[mtry_name]]
    
    # Convert to data frame and add fold/mtry columns
    data.frame(
      as.list(metrics),
      row.names = NULL
    )
  }))
}))


RF1_metrics_by_hyperparameter <- stRata::rf_metrics_by_hyper_parameter(RF1_metrics_by_fold)
write.csv2(RF1_metrics_by_fold,file.path(results_dir,"RF1_metrics_by_fold.csv"),row.names = FALSE)
write.csv2(RF1_metrics_by_hyperparameter,file.path(results_dir,"RF1_metrics_by_hyperparameter.csv"),row.names = FALSE)

# Specify the metrics to plot
metrics_to_plot <- c("o_acc", 
                     "pb_acc",
                     "o_f1", 
                     "w_f1",
                     "pb_f1", 
                     "um_f1", 
                     "uevs",
                     "tm_f1",
                     "pb_tvs",
                     "o_tvs",
                     "sas", 
                     "mean_abs_error_top", 
                     "mean_abs_error_centre_mean", 
                     "mean_abs_error_centre_median",
                     "mean_abs_error_bottom")

# Titles for each metric plot
plot_titles <- c("Accuracy (Overall)",
                 "Mean Accuracy (per borehole)", 
                 "F1 Score (Overall)",
                 "Weighted F1 Score",
                 "Mean F1 Score (per borehole)",
                 "Unit Match F1-Score",
                 "Geologic Extent Validation Score",
                 "Transition Match - F1 Score", 
                 "Transition Validation Score (per borehole)",
                 "Transition Validation Score (Overall)",
                 "Sequence alignment Score",
                 "Mean Absolute Error - Top", 
                 "Mean Absolute Error - Centre [mean]", 
                 "Mean Absolute Error - Centre [median]",
                 "Mean Absolute Error - Bottom"
)

RF1_path <- file.path(results_dir,"metrics_by_hyperparameter")

dir.create(RF1_path)

# Call the function with your data, the metrics, and their corresponding titles
plot_metrics(RF1_metrics_by_hyperparameter, metrics_to_plot, plot_titles,path = RF1_path)

# Plot the best performing hyperparameter
best_perf <- best_performing_hyper_plot(RF1_metrics_by_hyperparameter,"RF1-Best performing hyper parameter")

ggplot2::ggsave(file.path(results_dir,"best_performing_hyperparameter.png"),best_perf)

end <- Sys.time()
end-start
