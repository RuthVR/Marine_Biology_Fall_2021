### This is the official code that my group and I used to analysis the data. 
### Created by: Ruth Vergara Reyes 
### Updated on : December 11, 2021

### Data Libraries ###
library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggsci)              # my color palette that i will be using

### Loading Data ###
Temperature_raw_data <- read_csv(here("group_project/data/raw_data/temperature_data_water.csv"))
Zooplankton_raw_data <- read_csv(here("group_project/data/raw_data/Zooplankton_abundance_raw_data.csv"))
Phytoplankton_raw_data <- read_csv(here("group_project/data/raw_data/Phytoplankton_abundance_raw_data.csv"))
abundancy_data <- read_csv(here("group_project/data/cleaned_data_sets/Abundancy_data.csv"))

### Cleaning and Analyzing the Temperature Data ###
 
Clean_Temp_data <- Temperature_raw_data %>%                                         # Calling the data
  clean_names() %>%                                                             # making column names all have one format
  rename(average_water_temp_C =
    water_temperature_6_0m_agincourt_reef_weather_station_level0_value_avg) %>% # renaming a column                                                             # giving the names of columns to be all same format
  select(-date, -time, -seconds_since_1970) %>%                                 # getting rid of columns not need for our purposes 
  separate(
    col = date_time,  
    into = c('date', 'time'),  
    sep = " ") %>%                                                              # separating a date_time column into two columns
  select(-time) %>%                                                             # getting rid of data not needed for our purposes 
  separate(
    col = date,
    into = c('year', 'month', 'day'),  
    sep = "-"  )  %>%                                                           # separating date column
  unite(col = "year_month",                                                     # name of new column
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a / in the middle 
        remove = FALSE) %>%                                                     # keep the original columns                                                            # separating the date into separate columns 
  filter(year != '2013',
         year != '2021',
         year_month != '2014/10',
         year_month != '2020/09') %>%                                           # getting rid of years we will not look at and the NAs 
  write_csv(here("group_project", "data","cleaned_data_sets",
                 "Cleaned_up_temperature_data.csv"))                            #saving new data set as an csv file 

# making the graph
ggplot(data = Clean_Temp_data,
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

## Cleaning and Analyzing Plankton Abundance Data 
Cleaned_Phytoplankton_data <- Phytoplankton_raw_data %>%                        # cleaning phytoplankton data set
  clean_names() %>%                                                             # getting every column name to be in the same format 
  select(-fid: -longitude, 
         -time_24hr,
         -family: -species,
         -caab_code:-taxon_start_date,
         -phyto_comments: -geom) %>%                                            # getting rid of data we are not looking at 
  arrange(year, 
          taxon_group) %>%                                                      # arranging the data 
  filter(year != '2013',
         year != '2014', 
         year != '2021', 
         year != '2015',
         year != '2016', 
         year != '2017') %>%                                                    # getting rid of year we will not be look 
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a / in the middle 
        remove = FALSE) %>%
  add_column(plankton_type = "phytoplankton") %>%                               # adding a column
  arrange(year_month,
          taxon_group)                                                          # arranging the data 

Clean_Zooplankton_data <- Zooplankton_raw_data %>%                              # cleaning the zooplankton data set same sets as above were used 
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
  unite(col = "year_month",                                                     
        c(year, month),                                                        
        sep = "/",                                                              
        remove = FALSE) %>%
  add_column(plankton_type = "zooplankton") %>%
  arrange(year_month,
          taxon_group)

## merging the two plankton data sets into one ###

full_plankton_data <- full_join(Cleaned_Phytoplankton_data, Clean_Zooplankton_data) %>%
  arrange(year_month)%>%
  select(-day) %>%                                                              # taken out because it is repetitive information located in another column
  filter(taxon_group != 'Radiozoa',
         taxon_group != 'NOCTILUCA', 
         taxon_group != 'Silicoflagellate',
         taxon_group != 'Other')%>%                                             # taking out plankton types that have little to no data
  write_csv(here("group_project", "data", 
                 "cleaned_data_sets", "Complete_plankton_data.csv"))            # making the data into a csv file 

# Editing the abundance data to make it easier to graph 
abundancy_graph_data <- abundancy_data %>%                                      
  arrange(date) %>%
  separate(
    col = date,
    into = c('month', 'day','year'), 
    sep = "/"  )%>%                                                             # separating the date column
  arrange(year,month)%>%
  unite(col = "year_month",                                                     # the name of the NEW col 
        c(year, month),                                                         # the columns to unite 
        sep = "/",                                                              # lets put a / in the middle 
        remove = FALSE) %>%
  filter(year_month != '2020/12')

## making the graphs for the abundance data sets 

ggplot(data= abundancy_graph_data,
       mapping = aes(x = year_month,
                     y = abundancy,
                     fill = year_month))+
  geom_col()+
  facet_wrap(~type,
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
  scale_fill_jco()+                                                             # how I got my color theme ##
  scale_x_discrete(guide = guide_axis(angle = 60))                              # how i customized my axis scales ##

ggsave(here("group_project", "output", "Abundance_of_plankton_2018_2020.png"),
       width = 10, height = 7)   


## making a graph to look at just before and after the most recent summer

Before_After_summer_data <- abundancy_graph_data%>%                             # will be extracting the data need for next graph from data set 
  filter(year != '2018',
         month != '11',
         month != '4',
         month != '2',
         day != '18') %>%                                                       # removing the dates we do not want to look at for the graph 
  mutate(month = recode(month, 
                            "5" = "May",
                            "8" = "August"))                                    # renaming the characters in the month column

# making the before and after summer graph 
ggplot(data = Before_After_summer_data,                                         
       mapping = aes(x = month,
                     y = abundancy,
                     fill = year))+                                             # setting the diameters of the graph 
  geom_col()+
  facet_wrap(~type,
             scale = "free")+                                                   # creating two separate graph based on plankton type
  scale_fill_aaas()+                                                            # color palate 
  theme_classic()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.title = element_text(size = 11),
        title = element_text(size = 18),
        plot.subtitle = element_text(size = 14))+                               # defining the elements of the graph 
  labs(x = "",
       y = "Abundancy",
       title = "Abundancy Before and After Heatwaves",                          # making the labels for the graph
       subtitle = "The graph show how phytoplankton and zooplankton abundancy were effected after a summer
that had a record number of heatwaves occur. The summer that we are looking at is for the year 2019-2020",
       caption = "Created by Ruth Vergara Reyes, using data from 'The Australian Continuous Plankton Recorder (AusCPR)' ")                                     ### how I got my color theme ##
ggsave(here("group_project", "output", "comparing_summer_2019_2020.png"),
       width = 10, height = 7)                                                  # saving the graph




