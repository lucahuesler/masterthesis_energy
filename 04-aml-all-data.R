# Automated machine learning

## Modelling with all data
### Prepare

library(h2o)
library(tidymodels)
library(tidyverse)
source("auto_ml_functions.R")
h2o.init()

# read data
energy_modelling <- read_rds("data/energy_modelling.rds") |>
  select(egid, 
         hepi, 
         hec, 
         survey_year,
         num_residents_mean, 
         heated_area_m2, 
         year_of_installation, 
         efficiency_of_installation, 
         energy_usage_of_installation, 
         municipality_name, 
         building_class, 
         construction_year, 
         meters_above_sealevel, 
         energy_production_solar_mwh, 
         energy_consumed_hot_water_mwh,
         retrofitted,
         hdd,
         foreign_ratio,
         household_1_person_ratio,
         elderly_ratio,
         youth_ratio,
         residence_less_1_year_ratio,
         hepi_pred_current_method,
         hec_pred_current_method,
         stand_alone)


### Fit & predict

all_metrics <- data.frame()

# Variable selections
vars <- names(energy_modelling)

predictors_hec_all <- vars[-which(vars %in% c("egid", "hec", "hepi", "hec_pred_current_method", "hepi_pred_current_method"))]

predictors_hec_without_retrofit <- predictors_hec_all[-which(predictors_hec_all == "retrofitted")]

predictors_without_social <- predictors_hec_all[-which(predictors_hec_all %in% c("foreign_ratio","household_1_person_ratio","elderly_ratio","youth_ratio","residence_less_1_year_ratio"))]

predictors_hec_without_standalone <- predictors_hec_all[-which(predictors_hec_all == "stand_alone")]

predictors_basic <- predictors_without_social[-which(predictors_without_social %in% c("meters_above_sealevel","energy_production_solar_mwh","energy_consumed_hot_water_mwh","retrofitted", "stand_alone", "survey_year", "municipality_name", "energy_usage_of_installation"))]


variable_selections <- list(
  predictors_hec_all = predictors_hec_all,
  predictors_hec_without_retrofit = predictors_hec_without_retrofit,
  predictors_without_social = predictors_without_social,
  predictors_hec_without_standalone = predictors_hec_without_standalone,
  predictors_basic = predictors_basic
)

models <- lapply(names(variable_selections), function(name) {
  predictors <- variable_selections[[name]]
  
  # Prepare data
  df <- energy_modelling %>%
    select(egid, 
           hepi, 
           hec, 
           survey_year, 
           num_residents_mean, 
           heated_area_m2, 
           year_of_installation, 
           efficiency_of_installation, 
           energy_usage_of_installation, 
           municipality_name, 
           building_class, 
           construction_year, 
           meters_above_sealevel, 
           energy_production_solar_mwh, 
           energy_consumed_hot_water_mwh,
           retrofitted,
           hdd,
           foreign_ratio,
           household_1_person_ratio,
           elderly_ratio,
           youth_ratio,
           residence_less_1_year_ratio,
           hepi_pred_current_method,
           hec_pred_current_method,
           stand_alone)
  
  # Split data
  df_h2o <- as.h2o(df)
  split <- h2o.splitFrame(df_h2o, seed = 1)
  train <- split[[1]]
  test <- split[[2]]
  
  # Run AutoML
  aml_results <- run_h2o_automl(target = "hec",
                                predictors = predictors,
                                data = train,
                                runtime = 5000)
  
  # Get the leaderboard for this variable selection
  leaderboard <- h2o.get_leaderboard(aml_results$aml, extra_columns = "algo") |>
    as.data.frame() |>
    mutate(variable_selection = name)
  
  # Loop over the leaderboard and predict on the corresponding test set
  for (i in 1:nrow(leaderboard)) {
    # Extract the model_id, algorithm, and variable selection columns
    model_id <- as.character(leaderboard[i, "model_id"])
    algorithm <- as.character(leaderboard[i, "algo"])
    variable_selection <- as.character(leaderboard[i, "variable_selection"])
    print(paste0(model_id, " / ", algorithm, " / ", variable_selection))
    
    # Get the model from the H2O AutoML object
    model <- h2o.getModel(model_id)
    
    # Make predictions on the test dataset
    predictions <- h2o.predict(model, test)
    
    # Call the calc_metrics function to get the metrics
    metrics <- calc_metrics(model, test, predictions, target = "HEC") |>
      bind_cols(Algorithm = algorithm) |>
      bind_cols(Variable_Selection = variable_selection)
    
    # Add the metrics to the data frame
    all_metrics <- bind_rows(all_metrics, metrics)
  }
  
  ## Model evaluation
  # Retrieve the best model
  best_model <- h2o.get_best_model(aml_results$aml)
  
  # Predict with the best model
  best_model_predictions <- h2o.predict(best_model, test)
  
  # Combine data frames
  test_preds <- test |>
    h2o.cbind(best_model_predictions) |>
    as.data.frame()
  
  # Calculate aggregated errors
  aggregated_error_curr_method <- 1 - sum(test_preds$hec_pred_current_method) / sum(test_preds$hec)
  aggregated_error_best_model <- 1 - sum(test_preds$predict) / sum(test_preds$hec)
  
  # Return the AutoML results
  return(list(variable_selection = name,
              aml_results = aml_results,
              train_metrics = leaderboard,
              test_metrics = all_metrics,
              aggregated_error_curr_method = aggregated_error_curr_method,
              aggregated_error_best_model = aggregated_error_best_model,
              test_preds = test_preds,
              algo_best_model = best_model@algorithm))
})

# Combine metrics and aggregated errors for all variable selections
train_metrics <- map_dfr(models, ~ .x$train_metrics)
test_metrics <- map_dfr(models, ~ .x$test_metrics)
aggregated_errors <- map_dfr(models, ~ data.frame(variable_selection = .x$variable_selection,
                                                  aggregated_error_curr_method = .x$aggregated_error_curr_method,
                                                  aggregated_error_best_model = .x$aggregated_error_best_model))

# Save models and results to RDS files
saveRDS(models, paste0("models/models_all_data_", Sys.Date(), ".rds"))
saveRDS(train_metrics, paste0("models/train_metrics_all_data_", Sys.Date(), ".rds"))
saveRDS(test_metrics, paste0("models/test_metrics_all_data_", Sys.Date(), ".rds"))
saveRDS(aggregated_errors, paste0("models/aggregated_errors_all_data_", Sys.Date(), ".rds"))
