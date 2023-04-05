# Auto ML

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, tidymodels, vip, shapviz, DALEXtra, agua)


energy_modelling <- read_rds("data/energy_modelling.rds")

energy_modelling <- energy_modelling |>
  select(egid, 
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
            hec_pred_current_method)

# settings
theme_set(theme_bw())
h2o_start()


# data split
energy_split <- initial_split(energy_modelling, 
                              prop = 0.8, 
                              strata = hec)


energy_train <- training(energy_split)
energy_test  <-  testing(energy_split)

dim(energy_train)


# auto ml spec
# run for a maximum of 120 seconds
auto_spec <-
  auto_ml() %>%
  set_engine("h2o", max_runtime_secs = 120, seed = 1) %>%
  set_mode("regression")


# recipe
recipe_automl_hec <- recipe(hec ~ .,
                          data = energy_train) |> 
  # remove variables from predictors that are only used for info or later comparison 
  update_role(egid, hepi, hepi_pred_current_method, hec_pred_current_method, new_role = "id") |>
  # Create dummy variables for nominal predictors
  step_dummy(all_nominal()) |>
  # Remove variables that contain only one value
  step_zv(all_numeric_predictors()) |>
  # Impute means for variables with NA
  step_impute_mean(num_residents) |>
  step_impute_mean(year_of_installation) |>
  step_normalize(all_predictors())



# bake recipe for test data
energy_test_baked <- recipe_automl_hec %>%
  prep() %>%
  bake(new_data = energy_test)

# workflow
auto_wflow <-
  workflow() %>%
  add_model(auto_spec) %>%
  add_recipe(recipe_automl_hec)


# fit models
auto_fit <- fit(auto_wflow, data = energy_train)

extract_fit_parsnip(auto_fit)

saveRDS(auto_fit, paste0("output/auto_fit", Sys.time(), ".rds"))

# predict on test

# bake recipe for test data
energy_test_baked <- recipe_automl_hec %>%
  prep() %>%
  bake(new_data = energy_test)

pred <- predict(auto_fit, new_data = energy_test)

energy_test_pred <- energy_test |>
  select(egid, hec) |>
  bind_cols(pred)


# plot
ggplot(energy_test_pred, aes(x = hec, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual HEC", y = "Predicted HEC", title = "Actual vs. Predicted HEC") +
  theme_bw()

# rank results
rank_results(auto_fit) %>%
  filter(.metric == "r2") %>%
  arrange(rank)

# check metrics
metric_set <- metric_set(rmse, mape, mae, rsq)

autoML_metrics <- auto_fit %>%
  collect_metrics()


# current approach
energy_train_current_method <- energy_train |>
  dplyr::inner_join(energy_modelling, by = c("egid"), suffix = c("", "_y")) |>
  dplyr::select(egid, survey_year, hec, hepi, hepi_pred_current_method, hec_pred_current_method, heated_area_m2)

custom_metrics <- metric_set(rmse, mape, mae, rsq)

energy_train_metrics_current_method <- energy_train_current_method |>
  custom_metrics(hec, hec_pred_current_method)


energy_train_metrics_current_method



auto_fit

