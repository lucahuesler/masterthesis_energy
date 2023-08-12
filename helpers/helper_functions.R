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



library(tidyverse)
library(ggthemes)

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


library(ggplot2)

create_residual_plot <- function(model_list, model_index, model_type) {
  # Extract actual values and predicted values
  actual <- model_list[[model_index]]$test_preds$hec
  predicted <- model_list[[model_index]]$test_preds$predict
  
  # Calculate residuals
  residuals <- actual - predicted
  
  # Create a data frame with the values
  df <- data.frame(Actual = actual, Residuals = residuals)
  
  # Create the residual plot
  p <- ggplot(df, aes(x = Actual, y = Residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    xlab("Actual HEC") +
    ylab("Residuals") +
    ggtitle(paste0("", model_type, "")) +
    scale_x_continuous(labels = label_number_auto()) +
    scale_y_continuous(labels = label_number_auto()) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 10)
    )
  
  return(p)
}


library(ggplot2)
library(scales)
library(tidyr)

create_density_plot <- function(model_list, model_index, model_type, logarithmic = FALSE) {
  # Extract actual values and predicted values
  actual <- model_list[[model_index]]$test_preds$hec
  predicted <- model_list[[model_index]]$test_preds$predict
  
  # Create a data frame with the values
  df <- data.frame(Actual = actual, Predicted = predicted)
  
  if (logarithmic) {
    # Apply logarithmic transformation to the values
    df <- df %>%
      mutate(Actual = log(Actual),
             Predicted = log(Predicted))
    label <- "Log(HEC)"
  } else {
    label <- "HEC"
  }
  
  title_label <- case_when(
    model_type == "2016" ~ "2016",
    model_type == "2018" ~ "2018",
    model_type == "2020" ~ "2020",
    model_type == "1110" ~ "SFH",
    model_type == "1121" ~ "MFH 2 apartments",
    model_type == "1122" ~ "MFH 3+ apartments",
    model_type == "8011" ~ "Before 1919",
    model_type == "8012" ~ "1919-1945",
    model_type == "8013" ~ "1946-1960",
    model_type == "8014" ~ "1961-1970",
    model_type == "8015" ~ "1971-1980",
    model_type == "8016" ~ "1981-1985",
    model_type == "8017" ~ "1986-1990",
    model_type == "8018" ~ "1991-1995",
    model_type == "8019" ~ "1996-2000",
    model_type == "8020" ~ "2001-2005",
    model_type == "8021" ~ "2006-2010",
    model_type == "8022" ~ "2011-2015",
    model_type == "8023" ~ "After 2016",
    TRUE ~ model_type
  )
  
  # Combine actual and predicted values into a single column
  df <- tidyr::gather(df, key = "Variable", value = "Value", Actual, Predicted)
  
  # Create the density plot with overlay
  p <- ggplot(df, aes(x = Value, fill = Variable)) +
    geom_density(alpha = 0.5) +
    xlab(label) +
    ylab("Density") +
    ggtitle(paste0("", title_label, "")) +
    theme(legend.title = element_blank(),
          legend.position = c(0.9, 0.9),
          legend.justification = c(1, 1),
          legend.box.background = element_rect(color = "black", fill = "white")) +
    scale_x_continuous(labels = comma) +
    scale_y_continuous(labels = comma) +
    coord_cartesian(xlim = c(0, 300000))  # Set the x-axis limits
  
  return(p)
}

create_actual_predicted_plot <- function(model_list, model_index, model_type) {
  # Extract actual values and predicted values
  actual <- model_list[[model_index]]$test_preds$hec
  predicted <- model_list[[model_index]]$test_preds$predict
  
  # Create a data frame with the values
  df <- data.frame(Actual = actual, Predicted = predicted)
  
  # Create the scatter plot of actual vs predicted
  p <- ggplot(df, aes(x = Actual, y = Predicted)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    xlab("Actual HEC") +
    ylab("Predicted HEC") +
    ggtitle(paste0("", model_type, "")) +
    scale_x_continuous(labels = scales::comma) +
    scale_y_continuous(labels = scales::comma) +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      plot.title = element_text(size = 10)
    )
  
  return(p)
}
