library(h2o)
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
         construction_period,
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


# split per construction period
data_by_survey_year <- split(energy_modelling, energy_modelling$survey_year)

### Fit & predict

all_metrics_hec_subset_survey_year <- data.frame()

# Run AutoML for each survey year separately
models_by_survey_year <- lapply(names(data_by_survey_year), function(name) {
  
  # Get the name of the current element in the list
  df_name <- name
  
  # Retrieve the corresponding data frame
  df <- data_by_survey_year[[name]]
  
  # Prepare data
  df <- df %>%
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
           construction_period,
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
  # variable selection
  vars <- names(energy_modelling)
  
  # predictor variables for HEC overall
  predictors_hec_all <- vars[-which(vars %in% c("egid", "hec", "hepi", "hec_pred_current_method", "hepi_pred_current_method"))]
  
  # without social indicators
  predictors_without_social <- predictors_hec_all[-which(predictors_hec_all %in% c("foreign_ratio","household_1_person_ratio","elderly_ratio","youth_ratio","residence_less_1_year_ratio"))]
  
  aml_results <- run_h2o_automl(target = "hec", 
                                predictors = predictors_without_social, 
                                data = train, 
                                runtime = 7200)
  
  # Get the leaderboard for this subset
  leaderboard <- h2o.get_leaderboard(aml_results$aml, extra_columns = "algo") |>
    as.data.frame() |>
    mutate(approach = df_name)
  
  # Loop over the leaderboard and predict on the corresponding test set
  for (i in 1:nrow(leaderboard)) {
    # Extract the model_id, algorithm, and approach columns
    model_id <- as.character(leaderboard[i, "model_id"])
    algorithm <- as.character(leaderboard[i, "algo"])
    approach <- as.character(leaderboard[i, "approach"])
    print(paste0(model_id, " / ", algorithm, " / ", approach))
    
    # Get the model from the H2O AutoML object
    model <- h2o.getModel(model_id)
    
    # Make predictions on the test dataset
    predictions <- h2o.predict(model, test)
    
    # Call the calc_metrics function to get the metrics
    metrics <- calc_metrics(model, test, predictions, target = "HEC") |>
      bind_cols(Algorithm = algorithm) |>
      bind_cols(Approach = approach)
    
    # Add the metrics to the data frame
    all_metrics_hec_subset_survey_year <- bind_rows(all_metrics_hec_subset_survey_year, metrics)
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
  aggregated_error_curr_method <- 1 - sum(test_preds$hec_pred_current_method)/sum(test_preds$hec)
  aggregated_error_best_model <- 1 - sum(test_preds$predict)/sum(test_preds$hec)
  
  # Return the AutoML results
  results <- list(survey_year = name,
                  aml_results = aml_results,
                  train_metrics = leaderboard,
                  test_metrics = all_metrics_hec_subset_survey_year,
                  aggregated_error_curr_method = aggregated_error_curr_method,
                  aggregated_error_best_model = aggregated_error_best_model,
                  test_preds = test_preds,
                  algo_best_model = best_model@algorithm)
  
  # Save the results to a file
  saveRDS(results, paste0("models/subset_survey_year/results_", name, "_",   Sys.Date(),".rds"))
  
  return(results)
})

# Combine metrics of each subset into one data frame
subset_survey_year_train_metrics <- map_dfr(models_by_survey_year, ~ .x$train_metrics)
subset_survey_year_test_metrics <- map_dfr(models_by_survey_year, ~ .x$test_metrics)

# Combine aggregated errors
survey_year <- map_dfr(models_by_survey_year, ~ data.frame(survey_year = .x$survey_year))
aggregated_error_curr_method <- map_dfr(models_by_survey_year, ~ data.frame(aggregated_error_curr_method = .x$aggregated_error_curr_method))
aggregated_error_best_model <- map_dfr(models_by_survey_year, ~ data.frame(aggregated_error_best_model = .x$aggregated_error_best_model))

aggregated_errors_survey_year <- survey_year |>
  bind_cols(aggregated_error_best_model) |>
  bind_cols(aggregated_error_curr_method)

# save to rds
saveRDS(models_by_survey_year, paste0("models/subset_survey_year/models_", Sys.Date(), ".rds"))

