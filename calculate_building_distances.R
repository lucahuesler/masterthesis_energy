library(sf)
library(nngeo)
library(tidyverse)


building_polygons_sf <- st_read("A:/Durchfuehrung/08_Energie/02_Auswertung/01_Anfragen/2022/20221024_HSLU/gebaeudegrundriss_v2.csv", options = "GEOM_POSSIBLE_NAMES=geom_text", stringsAsFactors = FALSE, quiet = TRUE)

building_polygons_sf_clean <- building_polygons_sf |>
  filter(!st_is_empty(building_polygons_sf)) 


# Find distance to nearest polygon (Parallel processing)
# Here, we set k = 2 because the nearest neighbor will always be the point itself (with distance 0). We are interested in the second nearest neighbor. The returnDist = TRUE option tells the function to return the distances as well.

start <- Sys.time()
result <- st_nn(building_polygons_sf_clean, building_polygons_sf_clean, k = 2, parallel = 4, returnDist = TRUE)
end <- Sys.time()
end - start


building_polygons_sf_clean$nearest_distance <- sapply(result$dist, `[`, 2)

View(building_polygons_sf_clean)


saveRDS(building_polygons_sf_clean, "A:/Durchfuehrung/08_Energie/02_Auswertung/01_Anfragen/2022/20221024_HSLU/buildings_distances.rds")
