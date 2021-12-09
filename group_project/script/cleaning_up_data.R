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
abundancy_data <- read_csv(here("group_project/data/total_abundancy_data.csv"))


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
  arrange(year_month,
          taxon_group)%>%
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
  arrange(year_month,
          taxon_group)%>%
  write_csv(here("group_project", "data", "Cleaned_up_Zooplankton_data.csv"))

### merging the two plankton data sets into one ###

full_plankton_data <- full_join(Phytoplankton_cleaned_data, Zooplankton_clean_data) %>%
  arrange(year_month)%>%
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
  facet_wrap(~month,
             scales = "free")+
  theme_classic()+
  theme(legend.position = "bottom",
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
ggsave(here("group_project", "output", "temp_seawater_2014_2020.png"),
       width = 10, height = 7) 

### Making the abundancy graph ###
abundancy_data_clean <- abundancy_data%>%
  select(-...4, -...5) %>%
  arrange(date) %>%
  filter(date != "2018-09-07",
         date != "2019-11-12",
         date != "2020-02-16",
         date != "2020-08-18",
         date != "2020-12-02",
         date != "2018-08-09",
         date != "2019-04-17") %>%
  separate(
    col = date,
    into = c('year', 'month', 'day'), 
    sep = "-"  ) %>%
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a . in the middle 
        remove = FALSE) %>%
  add_column(time = if_else(.$month == "08", "Before Summer", "After Summer"))
  
  
  

ggplot(data = abundancy_data_clean,
       mapping = aes(x = year_month,
                     y = abundancy,
                     fill = time))+
  geom_col()+
  facet_wrap(~type,
             scale = "free")+
  scale_fill_simpsons()+
  theme_classic()+
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.title = element_text(size = 11),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 14))+
  labs(x = "",
       y = "Abundancy",
       title = "Abundancy Before and After Heatwaves",
       subtitle = "The graph show how phytoplankton and zooplankton abundancy is effected after a summer that had a record number of heatwaves occuring.",
       caption = "Created by Ruth Vergara Reyes, using data from 'The Australian Continuous Plankton Recorder (AusCPR)' ")+                                      ### how I got my color theme ##
  scale_x_discrete(guide = guide_axis(angle = 45))                              ### how i customized my axis scales ##
ggsave(here("group_project", "output", "comparing_summer_2019/2020.png"),
       width = 10, height = 7) 

