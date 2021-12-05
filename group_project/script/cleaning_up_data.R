## created by Ruth Vergara reyes 

### Load Libraries 
library(tidyverse)
library(here)
library(dplyr)
library(janitor)

## Loading Data ################################################################ 

Temperature_data <- read_csv(here("group_project/data/temperature_data_water.csv"))


## Cleaning up the data ########################################################
Temp_data_clean <- Temperature_data %>%
  clean_names() %>%
  select(-date, -time, -seconds_since_1970)%>%
  separate(
    col = date_time,  
    into = c('date', 'time'),  
    sep = " ") %>%
  select(-time) %>%
  separate(
    col = date,
    into = c('year', 'month', 'day'), 
    sep = "-"  ) %>%
  filter(year != '2013',
         year != '2014', 
         year != '2021') %>%
  rename(average_water_temp_C = water_temperature_6_0m_agincourt_reef_weather_station_level0_value_avg)%>%
  unite(col = "year_month",   # the name of the NEW col #
        c(year, month),       # the columns to unite #
        sep = "_",            # lets put a . in the middle #
        remove = FALSE) %>%   # keep the original columns #
  write_csv(here("group_project", "data", "Cleaned_up_temperature_data.csv"))


  

