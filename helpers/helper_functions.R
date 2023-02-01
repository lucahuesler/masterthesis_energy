# library(httr)
# library(jsonlite)
# 
# 
# get_mas_height_buildings <- function(gkode, gkodn) {
#   # get meters above sealevel from api3.geo.admin.ch
#   url <- paste0("https://api3.geo.admin.ch/rest/services/height?easting=",gkode,"&northing=",gkodn)
#   res = GET(url)
#   height <- res$content
#   data = fromJSON(rawToChar(res$content))
#   height <- as.numeric(data$height)
# 
# }
# 
# coords <- gwr_building %>%
#   select(egid, gkode, gkodn)
# 
# # heights <- mapply(get_mas_height_buildings, coords$gkode, coords$gkodn) %>%
# #   data_frame()
# 
# heights <- heights %>%
#   rename(meters_above_sealevel = ".") %>%
#   mutate(meters_above_sealevel = as.numeric(meters_above_sealevel))
# 
# coords_heights <- coords %>%
#   bind_cols(heights)
# 
# gwr_building_heights <- gwr_building %>% 
#   left_join(coords_heights)  %>%
#   write_delim("data/gwr_building_heights.csv", delim = ";")


# renaming
library(tidyverse)

rename_statpop <- function(df){
  df <- df %>%
    rename(Total_pop = BTOT,
           Swiss_pop = B11,
           Foreign_pop = B12,
           EU_pop = B13,
           Eur_pop = B14,
           Non_Eur_pop = B15,
           No_nation_pop = B16,
           Born_Swiss_pop = B21,
           Born_comm_pop = B22,
           Born_canton_pop = B23,
           Born_other_canton = B24,
           Born_Swiss_unalloc = B25,
           Born_abroad_pop = B26,
           Born_EU_pop = B27,
           Born_Eur_abroad_pop = B28,
           Born_Non_Eur_abroad_pop = B29,
           Born_abroad_unalloc = B30,
           Male_pop = BMTOT,
           Male_0_4 = BM01,
           Male_5_9 = BM02,
           Male_10_14 = BM03,
           Male_15_19 = BM04,
           Male_20_24 = BM05,
           Male_25_29 = BM06,
           Male_30_34 = BM07,
           Male_35_39 = BM08,
           Male_40_44 = BM09,
           Male_45_49 = BM10,
           Male_50_54 = BM11,
           Male_55_59 = BM12,
           Male_60_64 = BM13,
           Male_65_69 = BM14,
           Male_70_74 = BM15,
           Male_75_79 = BM16,
           Male_80_84 = BM17,
           Male_85_89 = BM18,
           Male_90plus = BM19,
           Female_pop = BWTOT,
           Female_0_4 = BW01,
           Female_5_9 = BW02,
           Female_10_14 = BW03,
           Female_15_19 = BW04,
           Female_20_24 = BW05,
           Female_25_29 = BW06,
           Female_30_34 = BW07,
           Female_35_39 = BW08,
           Female_40_44 = BW09,
           Female_45_49 = BW10,
           Female_50_54 = BW11,
           Female_55_59 = BW12,
           Female_60_64 = BW13,
           Female_65_69 = BW14,
           Female_70_74 = BW15,
           Female_75_79 = BW16,
           Female_80_84 = BW17,
           Female_85_89 = BW18,
           Female_90plus = BW19,
           Residence_less_1_year = B41,
           Residence_1_5_years = B42,
           Residence_6_10_years = B43,
           Residence_more_10_years = B44,
           Residence_since_birth = B45,
           Residence_unknown = B46,
           year_before_same_municipality = B51,
           year_beforesame_canton = B52,
           year_before_diff_canton = B53,
           year_before_foreign = B54,
           year_before_not_born = B55,
           year_before_unknown = B56,
           household_total = PTOT,
           household_1_person = P01,
           household_2_person = P02,
           household_3_person = P03,
           household_4_person = P04,
           household_5_person = P05,
           household_6_person = P06,
           household_plausibility = PI) |>
    rename_all(tolower)
}
