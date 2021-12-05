## created by Ruth Vergara reyes 

### Load Libraries 
library(tidyverse)
library(here)
library(dplyr)
library(janitor)

## Loading Data ################################################################ 

Temperature_data <- read_csv(here("group_project/data/temperature_data_water.csv"))
Zooplankton_data <- read_csv(here("group_project/data/Zooplankton_abundance_raw_data.csv"))
Phytoplankton_data <- read_csv(here("group_project/data/Phytoplankton_abundance_raw_data.csv"))


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
    sep = "-"  ) %>%                                                            # separating the dates_time into separate columns 
  filter(year != '2013',
         year != '2014', 
         year != '2021') %>%                                                    # getting rid of years we will not look at 
  rename(average_water_temp_C = water_temperature_6_0m_agincourt_reef_weather_station_level0_value_avg)%>%
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "_",                                                              # lets put a . in the middle 
        remove = FALSE) %>%                                                     # keep the original columns 
  write_csv(here("group_project", "data", "Cleaned_up_temperature_data.csv"))


Phytoplankton_cleaned_data <- Phytoplankton_data %>%
  clean_names() %>%
  select(-fid: -longitude, 
         -time_24hr,
         -family: -species,
         -caab_code:-taxon_start_date,
         -phyto_comments: -geom) %>%
  arrange(year) %>%
  filter(year != '2013',
         year != '2014', 
         year != '2021') %>%
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a . in the middle 
        remove = FALSE) %>%
  add_column(plankton_type = "phytoplankton") %>%
  write_csv(here("group_project", "data", "Cleaned_up_Phytoplankton_data.csv"))

Zooplankton_clean_data <- Zooplankton_data %>%
  clean_names() %>%
  select(-fid: -longitude, 
         -time_24hr,
         -family: -species,
         -caab_code:-taxon_start_date,
         -zoop_comments: -geom,
         -sex,
         -life_stage,
         -taxon_eco_group)%>%
  arrange(year) %>%
  filter(year != '2013',
         year != '2014', 
         year != '2021') %>%
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a . in the middle 
        remove = FALSE) %>%
  add_column(plankton_type = "zooplankton") %>%
  write_csv(here("group_project", "data", "Cleaned_up_Zooplankton_data.csv"))

### merging the two plankton data sets into one ###

full_plankton_data <- full_join(Phytoplankton_cleaned_data, Zooplankton_clean_data) %>%
  arrange(year)%>%
  select(-day) %>%
  write_csv(here("group_project", "data", "Complete_plankton_data.csv"))


  

