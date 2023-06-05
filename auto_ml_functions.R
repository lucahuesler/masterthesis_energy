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

calc_metrics <- function(model, data, fitted_values, target = "HEC")  {
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
  
  # calculate CV(RMSE)
  cv_rmse <- (rmse / mean(actual)) 

  
  # return a data frame with metrics
  metrics_df <- data.frame(Model = model_id,
                           Algorithm = algo,
                           R_squared = rsq,
                           RMSE = rmse,
                           MAE = mae,
                           MAPE = mape,
                           CV = cv_rmse)
  
  # calculate metrics of current approach
  if (target == "HEC") {
    current_actual <- data$hec
    current_predicted <- data$hec_pred_current_method
  } else if (target == "HEPI") {
    current_actual <- data$hepi
    current_predicted <- data$hepi_pred_current_method
  } else {
    print("Define target")
  }

    
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


###############################################
#' Plot Best Metrics for a Model
#'
#' This function generates a plot showing the best metrics for a given model.
#'
#' @param metrics_list_element The element from the list of metrics, containing the test metrics for a specific model.
#' @keywords plot visualization metrics
#' @import tidyverse ggplot2
#'
#' @examples
#' plot_best_metrics(models_by_building_class[[1]])
#'
#' @export
#' Plot Best Metrics for a Model
#'
#' This function generates a plot showing the best metrics for a given model.
#'
#' @param metrics_list_element The element from the list of metrics, containing the test metrics for a specific model.
#' @keywords plot visualization metrics
#' @import tidyverse ggplot2
#'
#' @examples
#' plot_best_metrics(models_by_building_class[[1]])
#'
#' @export
plot_best_metrics <- function(metrics_list_element) {
  # Modify the metrics dataframe to match the column names in your example
  metrics <- metrics_list_element$test_metrics %>%
    rename(
      Algorithm = Algorithm...2,
      algo = Algorithm...8
    ) %>%
    mutate(Algorithm = if_else(Algorithm == "Based on HEPI", "Current Approach", Algorithm))
  
  # Define the metrics to consider
  metrics_to_consider <- c("RMSE", "R_squared", "MAPE", "MAE")
  
  # Initialize an empty dataframe to store the best metrics
  best_metrics_per_algo <- data.frame(stringsAsFactors = FALSE)
  
  # Iterate over the metrics and find the best metric for each algorithm
  for (metric in metrics_to_consider) {
    best_metric <- metrics %>%
      group_by(Algorithm) %>%
      filter({{metric}} == min({{metric}})) %>%
      arrange({{metric}}) %>%
      slice_head(n = 1)
    
    best_metrics_per_algo <- bind_rows(best_metrics_per_algo, best_metric)
  }
  
  # Create named vector of colors
  algorithm_colors <- c(
    "gbm" = "#d73027", 
    "xgboost" = "#f46d43", 
    "deeplearning" = "#fdae61", 
    "drf" = "#fee090", 
    "stackedensemble" = "#e6f598",
    "Current Approach" = "#abd9e9",
    "glm" = "#74add1"
  )
  
  # Reshape the data to long format
  best_metrics_per_algo_long <- pivot_longer(
    best_metrics_per_algo,
    cols = c("RMSE", "R_squared", "MAPE", "MAE"),
    names_to = "Metric",
    values_to = "Value"
  )
  
  # Reorder the levels of Algorithm by the corresponding metric value
  best_metrics_per_algo_long$Algorithm <- factor(
    best_metrics_per_algo_long$Algorithm,
    levels = levels(reorder(best_metrics_per_algo_long$Algorithm, best_metrics_per_algo_long$Value))
  )
  
  # Create the plot
  p <- ggplot(best_metrics_per_algo_long, aes(x = Algorithm, y = Value, fill = Algorithm)) +
    geom_col(position = position_dodge()) +
    facet_wrap(~ Metric, scales = "free_y") +
    labs(x = NULL, y = "Value", fill = "") +
    theme_bw() +
    scale_fill_manual(values = algorithm_colors) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Display the plot
  print(p)
}





