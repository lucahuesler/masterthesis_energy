if (!require(pacman)) install.packages("pacman")
pacman::p_load(h2o, tidyverse, tidymodels, vip, shapviz, DALEXtra, agua)

# prepare data
energy_modelling <- read_rds("data/energy_modelling.rds")


# split
# data split
energy_split <- initial_split(energy_modelling, 
                              prop = 0.8, 
                              strata = hec)


energy_train <- training(energy_split)
energy_test  <-  testing(energy_split)

dim(energy_train)


# recipe
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
              stand_alone,
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


# prepare train data
trained_rec <- prep(recipe_base_hec, training = energy_train)
train_data <- bake(trained_rec, new_data = energy_train)

# train with h20
h2o.init()

energy_train_h2o <- as.h2o(energy_train)

aml <- h2o.automl(
  y = "hec",
  training_frame = energy_train_h2o,
  max_models = 20,
  seed = 1,
  max_runtime_secs = 360 # stop after 6 minutes
)


# View the AutoML leaderboard
lb <- aml@leaderboard
print(lb, n = nrow(lb))

# Get the leader model
leader_model <- h2o.getModel(aml@leader@model_id)
View(leader_model)

# Variable importance
varimp_df <- h2o.varimp(leader_model)
View(varimp_df)
varimp_plot <- h2o.varimp_plot(leader_model)
varimp
