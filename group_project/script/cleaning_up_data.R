## created by Ruth Vergara reyes 

### Load Libraries 
library(tidyverse)
library(here)
library(dplyr)
library(janitor)

## Loading Data ################################################################ 

Temperature_data <- read_csv(here("group_project/data/temperature_data_water.csv"))
Zooplankton_data <- read_csv(here("group_project/data/Zooplankton_Abundance_Latest_raw_data.csv"))
Phytoplankton_data <- read_csv(here("group_project/data/Phytoplankton_Abundance_Latest_raw_data.csv"))

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
  rename(average_water_temp_C = water_temperature_6_0m_agincourt_reef_weather_station_level0_value_avg)

Zooplankton_cleaned_data

