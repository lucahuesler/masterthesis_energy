# Script for training models as plain code (e.g. to run on server)

# Load necessary libraries for energy modeling analysis
library(tidyverse)    # Data wrangling and visualization
library(tidymodels)   # Modeling and machine learning
library(doParallel)   # Parallel processing
library(skimr)       # Summary statistics
library(plotly)      # Interactive visualization
library(sf)          # Spatial data handling
library(agua)        # Wrapper for machine learning pipelines
library(gt)          # Table generation
library(rules)       # Association rule mining
library(baguette)    # Bagged clustering
library(ggridges)    # Ridgeline plots
library(viridis)     # Color scales
library(hrbrthemes)  # Themes for ggplot2
library(finetune)    # Hyperparameter tuning
library(ggrepel)     # Text labels for ggplot2
library(vip)         # Variable importance plots
library(shapviz)     # Shapley values plots
library(DALEXtra)    # Explainable machine learning

# Read in data for energy modeling analysis
energy_modelling <- read_rds("data/energy_modelling.rds")


#| label: train-test-split
#| include: false

# Set the random number stream using `set.seed()` so that the results can be 
# reproduced later. 
set.seed(501)

# Save the split information for an 80/20 split of the data
energy_split <- initial_split(energy_modelling, 
                              prop = 0.001, 
                              strata = hec)

# Creating train and test set
energy_train <- training(energy_split)
energy_test  <-  testing(energy_split)


# Creating a validation set (20% of training data)
set.seed(234)
energy_validation <- validation_split(energy_train,
                                      prop = 0.90,
                                      strata = hec)

dim(energy_train)
dim(energy_validation)

# creating folds for cross validation
set.seed(1502)
energy_folds <- 
  vfold_cv(energy_train, strata = hec, repeats = 3)


#| label: preprocessing-base-recipe-hec
#| include: false

# Specify basic recipe (applicable to all models)
recipe_base_hec <- recipe(hec ~ .,
                          data = energy_train) |> 
  step_select(egid, 
              hepi, 
              hec, 
              survey_year, 
              num_residents, 
              num_floors, 
              building_area_m2, 
              gross_floor_area_m2, 
              heated_area_m2, 
              year_of_installation, 
              efficiency_of_installation, 
              energy_usage_of_installation, 
              solar_system, 
              solar_system_area_m2, 
              solar_system_usage, 
              solar_system_area_m2, 
              photovoltaic_system, 
              photovoltaic_system_power_kw, 
              municipality_code, 
              building_class, 
              construction_year, 
              coordinate_e, 
              coordinate_n, 
              num_dwellings, 
              meters_above_sealevel, 
              energy_production_solar_mwh, 
              energy_consumed_hot_water_mwh,
              retrofitted,
              retrofit_investment_costs,
              hdd,
              hepi_pred_current_method,
              hec_pred_current_method) |>
  # remove variables from predictors that are only used for info or later comparison 
  update_role(egid, hepi, hepi_pred_current_method, hec_pred_current_method, new_role = "id") |>
  # Create dummy variables for nominal predictors
  step_dummy(all_nominal()) |>
  # Remove variables that contain only one value
  step_zv(all_numeric_predictors()) |>
  # Impute means for variables with NA
  step_impute_mean(num_residents) |>
  step_impute_mean(year_of_installation)

# Specify recipe with normalized numeric predictors
recipe_base_hec_norm <- recipe_base_hec |>
  step_normalize(all_numeric_predictors())


# Add polynomials and interaction
recipe_base_hec_poly <- 
  recipe_base_hec_norm %>% 
  step_poly(all_numeric_predictors()) %>% 
  step_interact(~ all_numeric_predictors():all_numeric_predictors())



### Defining models

# MLR model
lm_basic_spec <- 
  linear_reg() |> 
  set_engine("lm")

# MLR glmnet spec
lm_glmnet_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_engine("glmnet")

# decision tree
dt_spec <- 
  decision_tree() |> 
  set_engine("rpart") |> 
  set_mode("regression")

# k nearest neighbour
knn_spec <-
  nearest_neighbor(neighbors = tune(), dist_power = tune(), weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

# random forest
rf_spec <- rand_forest() |>
  set_engine("ranger") |>
  set_mode("regression")

# xgboost
xgb_spec <- 
  boost_tree(tree_depth = tune(), learn_rate = tune(), loss_reduction = tune(), 
             min_n = tune(), sample_size = tune(), trees = tune()) %>% 
  set_engine("xgboost") %>% 
  set_mode("regression")

# svm radial
svm_r_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# svm poly
svm_p_spec <-
  svm_poly(cost = tune(), degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")


# articifial neural net
nnet_spec <- 
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) %>% 
  set_engine("nnet", MaxNWts = 2600) %>% 
  set_mode("regression")

# auto ml
auto_ml_spec <- auto_ml() |>
  set_engine("h2o") |>
  set_mode("regression") 
  

### Define workflows


#### Workflow for nonlinear models
#> this workflow is for nonlinear models that require the predictors to be in the same units

normalized <- 
  workflow_set(
    preproc = list(normalized = recipe_base_hec_norm), 
    models = list(neural_network = nnet_spec, 
                  SVM_radial = svm_r_spec, 
                  SVM_poly = svm_p_spec, 
                  KNN = knn_spec)
  )


#### Workflow with only basic preprocessing
#> this workflow is for models that do not need normalizing

no_pre_proc <- 
  workflow_set(
    preproc = list(simple = recipe_base_hec), 
    models = list(lm_basic = lm_basic_spec, 
                  random_forest = rf_spec, 
                  xgboost = xgb_spec
    )
  )

no_pre_proc


#### Models with non linear terms and interactions
with_features <- 
  workflow_set(
    preproc = list(full_quad = recipe_base_hec_poly), 
    models = list(lm_glmnet = lm_glmnet_spec, KNN = knn_spec)
  )


#### Combine all workflows

all_workflows <- 
  bind_rows(no_pre_proc, normalized) %>% 
  # Make the workflow ID's a little more simple: 
  mutate(wflow_id = gsub("(simple_)|(normalized_)", "", wflow_id))

all_workflows


### Tuning


#| label: hyperparameter-tuning
#| include: false

#> Almost all of the members of all_workflows contain tuning parameters. To evaluate their performance, we can use the standard tuning or resampling functions (e.g., tune_grid() and so on). The workflow_map() function will apply the same function to all of the workflows in the set; the default is tune_grid()


grid_ctrl <-
  control_grid(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = FALSE
  )

grid_results <-
  all_workflows %>%
  workflow_map(
    seed = 1503,
    resamples = energy_validation,
    grid = 25,
    control = grid_ctrl
  )



# without knn
grid_results_subset <- grid_results %>%
  subset(wflow_id != "KNN")

autoplot(
  grid_results_subset,
  rank_metric = "rmse",  # <- how to order models
  metric = "rmse",       # <- which metric to visualize
  select_best = TRUE     # <- one point per workflow
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
  #lims(y = c(3.5, 9.5)) +
  theme(legend.position = "none") +
  ylim(18000, 28000) +
  ggtitle("Benchmark") +
  ylab("RMSE")


#### Improve effiecency: Racing approach

#| label: race-results
#| include: false

race_ctrl <-
  control_race(
    save_pred = TRUE,
    parallel_over = "everything",
    save_workflow = TRUE
  )

race_results <-
  all_workflows %>%
  workflow_map(
    "tune_race_anova",
    seed = 1503,
    resamples = energy_folds,
    grid = 25,
    control = race_ctrl
  )

race_results



#| label: race-autoplot
#| include: true

autoplot(
  race_results,
  rank_metric = "rmse",  
  metric = "rmse",       
  select_best = TRUE    
) +
  geom_text(aes(y = mean - 1/2, label = wflow_id), angle = 90, hjust = 1) +
  lims(y = c(3.0, 9.5)) +
  theme(legend.position = "none")


#### Compare grid search and race approach
matched_results <- 
  rank_results(race_results, select_best = TRUE) %>% 
  select(wflow_id, .metric, race = mean, config_race = .config) %>% 
  inner_join(
    rank_results(grid_results, select_best = TRUE) %>% 
      select(wflow_id, .metric, complete = mean, 
             config_complete = .config, model),
    by = c("wflow_id", ".metric"),
  ) %>%  
  filter(.metric == "rmse")

matched_results %>% 
  ggplot(aes(x = complete, y = race)) + 
  geom_abline(lty = 3) + 
  geom_point() + 
  geom_text_repel(aes(label = model)) +
  coord_obs_pred() + 
  labs(x = "Complete Grid RMSE", y = "Racing RMSE") 




