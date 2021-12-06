## created by Ruth Vergara reyes 

### Load Libraries 
library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(viridis)  
library(RColorBrewer)
library(ggplot2)
library(ggsci)

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
         year != '2021') %>%                                                    # getting rid of years we will not look at 
  rename(average_water_temp_C = water_temperature_6_0m_agincourt_reef_weather_station_level0_value_avg)%>%
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a . in the middle 
        remove = FALSE) %>%                                                     # keep the original columns 
    write_csv(here("group_project", "data", "Cleaned_up_temperature_data.csv"))


Phytoplankton_cleaned_data <- Phytoplankton_data %>%
  clean_names() %>%
  select(-fid: -longitude, 
         -time_24hr,
         -family: -species,
         -caab_code:-taxon_start_date,
         -phyto_comments: -geom) %>%
  arrange(year, 
          taxon_group) %>%
  filter(year != '2013',
         year != '2014', 
         year != '2021', 
         year != '2015',
         year != '2016', 
         year != '2017') %>%
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
         year != '2021', 
         year != '2015',
         year != '2016', 
         year != '2017') %>%
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
  filter(taxon_group != 'Radiozoa',
         taxon_group != 'NOCTILUCA', 
         taxon_group != 'Silicoflagellate',
         taxon_group != 'Other')%>%
  write_csv(here("group_project", "data", "Complete_plankton_data.csv"))

## Making the Graphs to analysis data ##########################################
# Plankton Abundance 
ggplot(data= full_plankton_data,
        mapping = aes(x = year_month,
                       y = taxon_per_m3,
                       group = plankton_type,
                       fill = year_month))+
  geom_col()+
  facet_wrap(~plankton_type,
             scales = "free",
             ncol = 2)+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 13))+
  labs(x = "",
       y = "Number of plankton per cubic meter",
       title = "Abundance of Plankton",
       subtitle = "The graph shows us the abunancy of phytoplankton and Zooplankton between June 2018 - August 2020",
       caption = "Created by Ruth Vergara Reyes, using data from 'The Australian Continuous Plankton Recorder (AusCPR)' ")+
  scale_fill_jco()+                                                       ### how I got my color theme ##
  scale_x_discrete(guide = guide_axis(angle = 60))                              ### how i customized my axis scales ##
ggsave(here("group_project", "output", "Abundance_of_plankton.png"),
       width = 10, height = 7)   

# Sea Temperature Graph 
ggplot(data = Temp_data_clean,
       mapping = aes(x = year,
                     y = average_water_temp_C,
                     color = year))+
  geom_point(size = 3)+
  facet_wrap(~month)+
  theme_light()+
  theme(legend.position = "none",
        axis.title = element_text(size = 10),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 13))+
  labs(x = "",
       y = "Temperature (C)",
       title = "Average Monthly Temperature of Seawater",
       subtitle = "The graphs bellow show what the monthly average seawater temperature was from 2014 - 2020.
The data was recorded off the coast of Northeastern Australia.",
       caption = "Created by Ruth Vergara Reyes, using data from 'The Australian Continuous Plankton Recorder (AusCPR)' ")+
  scale_color_jco() +                                       ### how I got my color theme ##
  scale_x_discrete(guide = guide_axis(angle = 60))                              ### how i customized my axis scales ##

