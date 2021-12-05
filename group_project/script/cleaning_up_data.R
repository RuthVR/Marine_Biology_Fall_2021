## created by Ruth Vergara reyes 

### Load Libraries 
library(tidyverse)
library(here)
library(dplyr)
library(janitor)

## Loading Data ################################################################ 

Temperature_data <- read_csv(here("group_project/data/water_temp_data_2012_2021.csv"))
Zooplankton_data <- read_csv(here("group_project/data/Zooplankton_Abundance_Latest_raw_data.csv"))
Phytoplankton_data <- read_csv(here("group_project/data/Phytoplankton_Abundance_Latest_raw_data.csv"))

## Cleaning up the data ########################################################
