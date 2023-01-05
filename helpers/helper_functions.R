library(httr)
library(jsonlite)


get_mas_height_buildings <- function(gkode, gkodn) {
  # get meters above sealevel from api3.geo.admin.ch
  url <- paste0("https://api3.geo.admin.ch/rest/services/height?easting=",gkode,"&northing=",gkodn)
  res = GET(url)
  height <- res$content
  data = fromJSON(rawToChar(res$content))
  height <- as.numeric(data$height)

}

coords <- gwr_building %>%
  select(egid, gkode, gkodn)

heights <- mapply(get_mas_height_buildings, coords$gkode, coords$gkodn) %>%
  data_frame()

heights <- heights %>%
  rename(meters_above_sealevel = ".") %>%
  mutate(meters_above_sealevel = as.numeric(meters_above_sealevel))

coords_heights <- coords %>%
  bind_cols(heights)

gwr_building_heights <- gwr_building %>% 
  left_join(coords_heights)  %>%
  write_delim("data/gwr_building_heights.csv", delim = ";")



