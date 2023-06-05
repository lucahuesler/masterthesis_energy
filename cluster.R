# load necessary packages
library(tidyverse)
library(factoextra)
library(cluster)

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

# Specify the columns to be used in the clustering
columns_for_clustering <- c("num_residents_mean",
                            "construction_year",
                            "stand_alone",
                            "heated_area",
                            "survey_year",
                            "municipality_name")

# select columns for clustering
df_for_clustering <- df %>% select(all_of(columns_for_clustering))

# handle categorical variables, for example by converting them to factor levels
df_for_clustering <- mutate_if(df_for_clustering, is.character, as.factor)

# handle missing values, for example by replacing them with mean values
df_for_clustering <- mutate_if(df_for_clustering, is.numeric, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# scale the data
df_for_clustering <- scale(df_for_clustering)

# compute the distance matrix
dist_matrix <- dist(df_for_clustering, method = "euclidean")

# perform hierarchical clustering
hclust_result <- hclust(dist_matrix, method = "ward.D2")

# plot the dendrogram
plot(hclust_result, cex = 0.6)
