run_h2o_automl <- function(target, predictors, data, runtime = 60) {
  # define target and predictors
  y <- target
  x <- predictors
  
  # Run Auto ML
  aml <- h2o.automl(x = x, y = y, training_frame = data, max_runtime_secs =  runtime, seed = 1)
  
  # Get the leaderboard
  lb <- aml@leaderboard

  
  # Return both aml and leaderboard
  return(list(aml = aml, leaderboard = lb))
}
  

###############################################

calc_metrics <- function(model, data, fitted_values) {
  # function to calculate R-squared, RMSE, MAE, and MAPE
  
  # get model id and algorithm as string
  model_id <- model@model_id[1]
  algo <- model@algorithm
  
  # calculate performance object
  perf <- h2o.performance(model, data)
  
  # extract actual and predicted values as data frames
  data <- as.data.frame(data)
  actual <- data$hec
  predicted <- as.data.frame(fitted_values)
  
  # calculate R-squared
  rsq <- h2o.r2(perf)
  
  # calculate RMSE
  rmse <- h2o.rmse(perf)
  
  # calculate MAE
  mae <- h2o.mae(perf)
  
  # calculate MAPE
  mape <- (mae / mean(actual)) * 100
  
  # return a data frame with metrics
  metrics_df <- data.frame(Model = model_id,
                           Algorithm = algo,
                           R_squared = rsq,
                           RMSE = rmse,
                           MAE = mae,
                           MAPE = mape)
  
  # calculate metrics of current approach
  current_actual <- data$hec
  current_predicted <- data$hec_pred_current_method
  current_algo = 
    
    # calculate R-squared
    rsq <- cor(current_predicted, current_actual)^2
  
  # calculate RMSE
  rmse <- sqrt(mean((current_predicted - current_actual)^2))
  
  # calculate MAE
  mae <- mean(abs(current_predicted - current_actual))
  
  # calculate MAPE
  mape <- mean(abs((actual - current_predicted)/actual)) * 100
  
  # create a data frame with the metrics
  metrics_df_current <- data.frame(Model = "Current Method",
                                   Algorithm = "Based on HEPI",
                                   R_squared = rsq,
                                   RMSE = rmse,
                                   MAE = mae,
                                   MAPE = mape)
  
  metrics_df <- metrics_df |>
    bind_rows(metrics_df_current)
  
  return(metrics_df)
}


###############################################
save_models <- function(model_object) {
# Save best models
  best_models <- list()
  algorithms <- c("Overall", "XGBoost", "Stacked Ensemble", "GBM", "Deep Learning", "GLM", "Base Model")
  
  for (algo in algorithms) {
  if (algo == "Overall") {
    best_models[[algo]] <- h2o.get_best_model(aml, criterion = "RMSE")
  } else {
    best_models[[algo]] <- h2o.get_best_model(aml, algorithm = tolower(algo), criterion = "RMSE")
  }
  h2o.saveModel(best_models[[algo]], file.path(paste0("best_model_", tolower(algo), "_", Sys.Date(), ".bin")))
  }
}
