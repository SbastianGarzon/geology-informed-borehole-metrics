#' Evaluate Various Metrics for Borehole Predictions
#'
#' This function computes several performance metrics to evaluate the prediction accuracy for boreholes (`df`).
#' It applies a sliding window approach with various window sizes (3, 5, 7, 9) and calculates multiple metrics, including traditional and geology-inspired metrics.
#' It uses the `apply_majority_vote` function for resampling and evaluates the metrics using custom functions for different summary statistics.
#'
#' @param df A data frame containing the `prediction`, `target_variable`, and `nr` (borehole ID) columns. The data is used to compute a variety of metrics.
#'
#' @return A named vector of computed metrics for each window size (original, window_3, window_5, window_7, window_9).
#' These include overall accuracy, F1-score, Unit match F1-score, transition match F1-score,
#' and various other metrics.
#'
#' @export
metrics_evaluation <- function(df){

  tictoc::tic("Computing metrics")
  df$nrs <- df$nr

  tictoc::tic("Apply window")
  resampled_dfs <- list(
    original = df,
    window_3 = apply_majority_vote(df,3),
    window_5 = apply_majority_vote(df,5),
    window_7= apply_majority_vote(df,7),
    window_9= apply_majority_vote(df,9)
  )
  tictoc::toc(log = TRUE)

  unique_nrs <- unique(df$nrs)

  tic("Overall Accuracy")
  #### Overall Accuracy
  o_acc <- lapply(resampled_dfs, overall_accuracy_custom_metric)
  toc(log = TRUE)
  #### Mean per borehole Accuracy

  tic("Overall Accuracy (per borehole)")
  pb_acc <- lapply(resampled_dfs, mean_per_borehole_accuracy_custom_metric)
  toc(log = TRUE)

  #### Overall F1-Score
  tic("Overall F1-Score")
  o_f1 <- lapply(resampled_dfs, overall_f1_score_custom_summary)
  toc(log = TRUE)

  ### Mean per borehole F1-Score
  tic("Overall F1-Score (per borehole)")
  pb_f1 <- lapply(resampled_dfs, per_borehole_f1_score_custom_summary)
  toc(log = TRUE)

  # Kappa
  o_kappa <- kappa_metric(df)

  ### Weighted F1-Score
  tic("Weighted F1-Score")
  w_f1 <- lapply(resampled_dfs,w_f1_custom_summary)
  toc(log= TRUE)

  #### Unit Match F1-Score (UM-F1)
  tic("Unit Match F1-Score")
  um_f1 <- lapply(resampled_dfs, um_f1_custom_summary)
  toc(log = TRUE)

  #### Unit Extent Validation Score (UEVS)
  tic("Unit Extent Validation Score")
  uevs <- lapply(resampled_dfs, function(df) {
    unit_extent_validation_custom_summary(df)
  })
  toc(log = TRUE)

  #### Transition Match F1-Score (TM-F1)
  tic("Transition Match F1-Score")
  results_tm_f1 <- lapply(resampled_dfs, tm_f1_score_custom_summary)
  toc(log = TRUE)

  #### Transition Validation Score (TVS)
  tic("Transition Validation Score")
  results_tvs <- lapply(resampled_dfs, function(df) {
    transition_validation_custom_summary(df, known_transitions)
  })
  toc(log = TRUE)

  #### Sequence Alignment Score  (SAS)
  tic("Sequential Alignment Score")
  sas <- lapply(resampled_dfs, sequence_alignment_custom_summary)
  toc(log = TRUE)

  #### Mean Absolute Error - Top  (MAE-Top)
  tic("Mean Absolute Error - Top")
  mean_abs_error_top <- lapply(resampled_dfs, mean_abs_error_top_custom_summary)
  toc(log = TRUE)

  #### Mean Absolute Error Centre (mean) - (MAE-Centre-mean)
  tic("Mean Absolute Error Centre (mean) - (MAE-Centre-mean)")
  mean_abs_error_centre_mean <- lapply(resampled_dfs,mean_abs_error_centre_mean_custom_summary)
  toc(log = TRUE)

  #### Mean Absolute Error - Centre (median) - (MAE-Centre)
  tic("Mean Absolute Error - Centre (median) - (MAE-Centre)")
  mean_abs_error_centre_median <- lapply(resampled_dfs,mean_abs_error_centre_median_custom_summary)
  toc(log = TRUE)

  #### Mean Absolute Error Bottom - (MAE-Bottom)
  tic("Mean Absolute Error Bottom - (MAE-Bottom)")
  mean_abs_error_bottom <- lapply(resampled_dfs,mean_abs_error_bottom_custom_summary)
  toc(log = TRUE)

  toc(log = TRUE)

  return(c(  o_acc = unlist(o_acc$original),
             o_acc_window_3=unlist(o_acc$window_3),
             o_acc_window_5=unlist(o_acc$window_5),
             o_acc_window_7=unlist(o_acc$window_7),
             o_acc_window_9=unlist(o_acc$window_9),

             pb_acc = unlist(pb_acc$original),
             pb_acc_window_3=unlist(pb_acc$window_3),
             pb_acc_window_5=unlist(pb_acc$window_5),
             pb_acc_window_7=unlist(pb_acc$window_7),
             pb_acc_window_9=unlist(pb_acc$window_9),

             Kappa = o_kappa,

             w_f1 = unlist(w_f1$original),
             w_f1_window_3 = unlist(w_f1$window_3),
             w_f1_window_5 = unlist(w_f1$window_5),
             w_f1_window_7 = unlist(w_f1$window_7),
             w_f1_window_9 = unlist(w_f1$window_9),

             o_f1 = unlist(o_f1$original),
             o_f1_window_3=unlist(o_f1$window_3),
             o_f1_window_5=unlist(o_f1$window_5),
             o_f1_window_7=unlist(o_f1$window_7),
             o_f1_window_9=unlist(o_f1$window_9),

             pb_f1 = unlist(pb_f1$original),
             pb_f1_window_3=unlist(pb_f1$window_3),
             pb_f1_window_5=unlist(pb_f1$window_5),
             pb_f1_window_7=unlist(pb_f1$window_7),
             pb_f1_window_9=unlist(pb_f1$window_9),

             um_f1=unlist(um_f1$original),
             um_f1_window_3=unlist(um_f1$window_3),
             um_f1_window_5=unlist(um_f1$window_5),
             um_f1_window_7=unlist(um_f1$window_7),
             um_f1_window_9=unlist(um_f1$window_9),

             uevs = unlist(uevs$original),
             uevs_window_3= unlist(uevs$window_3),
             uevs_window_5 = unlist(uevs$window_5),
             uevs_window_7 = unlist(uevs$window_7),
             uevs_window_9 = unlist(uevs$window_9),

             tm_f1 = unlist(results_tm_f1$original),
             tm_f1_window_3=unlist(results_tm_f1$window_3),
             tm_f1_window_5=unlist(results_tm_f1$window_5),
             tm_f1_window_7=unlist(results_tm_f1$window_7),
             tm_f1_window_9=unlist(results_tm_f1$window_9),

             pb_tvs = unlist(results_tvs$original$mean_pb_TVS),
             pb_tvs_window_3 = unlist(results_tvs$window_3$mean_pb_TVS),
             pb_tvs_window_5= unlist(results_tvs$window_5$mean_pb_TVS),
             pb_tvs_window_7= unlist(results_tvs$window_7$mean_pb_TVS),
             pb_tvs_window_9= unlist(results_tvs$window_9$mean_pb_TVS),

             o_tvs = unlist(results_tvs$original$o_TVS),
             o_tvs_window_3 = unlist(results_tvs$window_3$o_TVS),
             o_tvs_window_5= unlist(results_tvs$window_5$o_TVS),
             o_tvs_window_7= unlist(results_tvs$window_7$o_TVS),
             o_tvs_window_9= unlist(results_tvs$window_9$o_TVS),

             sas = unlist(sas$original),
             sas_window_3 = unlist(sas$window_3),
             sas_window_5 = unlist(sas$window_5),
             sas_window_7 = unlist(sas$window_7),
             sas_window_9 = unlist(sas$window_9),

             mean_abs_error_top= unlist(mean_abs_error_top$original),
             mean_abs_error_top_window_3=unlist(mean_abs_error_top$window_3),
             mean_abs_error_top_window_5=unlist(mean_abs_error_top$window_5),
             mean_abs_error_top_window_7=unlist(mean_abs_error_top$window_7),
             mean_abs_error_top_window_9=unlist(mean_abs_error_top$window_9),

             mean_abs_error_centre_mean = unlist(mean_abs_error_centre_mean$original),
             mean_abs_error_centre_mean_window_3 = unlist(mean_abs_error_centre_mean$window_3),
             mean_abs_error_centre_mean_window_5 = unlist(mean_abs_error_centre_mean$window_5),
             mean_abs_error_centre_mean_window_7 = unlist(mean_abs_error_centre_mean$window_7),
             mean_abs_error_centre_mean_window_9 = unlist(mean_abs_error_centre_mean$window_9),

             mean_abs_error_centre_median = unlist(mean_abs_error_centre_median$original),
             median_center_depth_diff_window_3 = unlist(mean_abs_error_centre_median$window_3),
             median_center_depth_diff_window_5 = unlist(mean_abs_error_centre_median$window_5),
             median_center_depth_diff_window_7 = unlist(mean_abs_error_centre_median$window_7),
             median_center_depth_diff_window_9 = unlist(mean_abs_error_centre_median$window_9),

             mean_abs_error_bottom = unlist(mean_abs_error_bottom$original),
             mean_abs_error_bottom_window_3 = unlist(mean_abs_error_bottom$window_3),
             mean_abs_error_bottom_window_5 = unlist(mean_abs_error_bottom$window_5),
             mean_abs_error_bottom_window_7 = unlist(mean_abs_error_bottom$window_7),
             mean_abs_error_bottom_window_9 = unlist(mean_abs_error_bottom$window_9)
  ))
}
