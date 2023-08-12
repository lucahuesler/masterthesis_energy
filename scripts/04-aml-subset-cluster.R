# Automated machine learning

## Modelling per cluster
### Prepare

library(h2o)
library(tidymodels)
library(tidyverse)
source("auto_ml_functions.R")
h2o.init(max_mem_size = "8g")

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



### Clustering

#### Define k with elbow method


# Convert the data frame to an H2OFrame
energy_modelling_h2o <- as.h2o(energy_modelling)

# Specify the columns to be used in the clustering
columns_for_clustering <- c("num_residents_mean",
                            "construction_year",
                            "stand_alone",
                            "heated_area_m2",
                            "survey_year",
                            "municipality_name")

# Scale the columns
for (col in columns_for_clustering) {
  energy_modelling_h2o[[col]] <- h2o.scale(energy_modelling_h2o[[col]])
}

# Convert the 'retrofitted' column to a factor
energy_modelling_h2o$retrofitted <- as.factor(energy_modelling_h2o$retrofitted)

# Initialize an empty vector to hold the total within sum of squares for each k
total_within_ss <- numeric()

# Specify the maximum k
max_k <- 10

# Run k-means for k from 1 to max_k
for (k in 1:max_k) {
  # Run k-means
  kmeans_model <- h2o.kmeans(training_frame = energy_modelling_h2o, 
                             x = columns_for_clustering, 
                             k = k, 
                             seed = 1234)
  
  # Get the total within sum of squares and add it to the vector
  total_within_ss[k] <- h2o.tot_withinss(kmeans_model)
}

# Plot the total within sum of squares for each k
plot(1:max_k, total_within_ss, type = "b", xlab = "Number of clusters (k)", ylab = "Total within sum of squares", main = "Elbow Method")


#### Run kmeans



# Perform k-means clustering
k <- 8 # Specify the number of clusters
kmeans_model <- h2o.kmeans(training_frame = energy_modelling_h2o, 
                           x = columns_for_clustering, 
                           k = k, 
                           seed = 1234)

# Print the model summary
print(kmeans_model)


# Get the cluster assignment
cluster_assignment <- h2o.predict(kmeans_model, energy_modelling_h2o)

# Add the cluster assignment to the original data frame
energy_modelling$cluster <- as.vector(cluster_assignment$predict)






# Load the ggplot2 package
library(ggplot2)

# Create the scatter plot
ggplot(energy_modelling, aes(x = construction_year, y = num_residents_mean, color = factor(cluster))) +
  geom_point(alpha = 0.6) +
  scale_color_discrete(name = "Cluster") +
  labs(x = "Construction Year", y = "Number of Residents Mean", title = "Scatter Plot of Construction Year vs Number of Residents Mean") +
  theme_minimal()





### Fit & Predict


# split per cluster
data_by_cluster <- split(energy_modelling, energy_modelling$cluster)




all_metrics_hec_subset_cluster <- data.frame()

# Run AutoML for each cluster separately
models_by_cluster <- lapply(names(data_by_cluster), function(name) {
  
  # Get the name of the current element in the list
  df_name <- name
  
  # Retrieve the corresponding data frame
  df <- data_by_cluster[[name]]
  
  # Prepare data
  df <- df %>%
    select(egid,
           cluster,
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
  # variable selection
  vars <- names(energy_modelling)
  
  # predictor variables for HEC overall
  predictors_hec_all <- vars[-which(vars %in% c("egid", "hec", "hepi", "hec_pred_current_method", "hepi_pred_current_method"))]
  
  
  # without social indicators
  predictors_without_social <- predictors_hec_all[-which(predictors_hec_all %in% c("foreign_ratio","household_1_person_ratio","elderly_ratio","youth_ratio","residence_less_1_year_ratio"))]
  
  
  aml_results <- run_h2o_automl(target = "hec", 
                                predictors = predictors_without_social, 
                                data = train, 
                                runtime = 900)
  
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
    all_metrics_hec_subset_cluster <- bind_rows(all_metrics_hec_subset_cluster, metrics)
  }
  
  ## Model evaluation
  # Retrieve the best model
  best_model <- h2o.get_best_model(aml_results$aml)
  
  # Predict with the best model
  best_model_predictions <- h2o.predict(model, test)
  # Combine data frames
  test_preds <- test |>
    h2o.cbind(best_model_predictions) |>
    as.data.frame()
  
  # Calculate aggregated errors
  aggregated_error_curr_method <- 1 - sum(test_preds$hec_pred_current_method)/sum(test_preds$hec)
  aggregated_error_best_model <- 1 - sum(test_preds$predict)/sum(test_preds$hec)
  
  # Return the AutoML results
  results <- list(name = name,
                  aml_results = aml_results,
                  train_metrics = leaderboard,
                  test_metrics = all_metrics_hec_subset_building_class,
                  aggregated_error_curr_method = aggregated_error_curr_method,
                  aggregated_error_best_model = aggregated_error_best_model,
                  test_preds = test_preds,
                  algo_best_model = best_model@algorithm)
  
  # Save the results to a file
  saveRDS(results, paste0("models/subset_cluster/results_", name, "_",   Sys.Date(),".rds"))
  
  return(results)
})



# Combine metrics of each subset into one data frame
subset_cluster_train_metrics <- map_dfr(models_by_cluster, ~ .x$train_metrics)
subset_cluster_test_metrics <- map_dfr(models_by_cluster, ~ .x$test_metrics)

# Combine aggregated errors
cluster <- map_dfr(models_by_cluster, ~ data.frame(cluster = .x$cluster))
aggregated_error_curr_method <- map_dfr(models_by_cluster, ~ data.frame(aggregated_error_curr_method = .x$aggregated_error_curr_method))
aggregated_error_best_model <- map_dfr(models_by_cluster, ~ data.frame(aggregated_error_best_model = .x$aggregated_error_best_model))

aggregated_errors_cluster <- cluster |>
  bind_cols(aggregated_error_best_model) |>
  bind_cols(aggregated_error_curr_method)

# save to rds
saveRDS(models_by_cluster, paste0("models/models_by_cluster_", Sys.Date(), ".rds"))

