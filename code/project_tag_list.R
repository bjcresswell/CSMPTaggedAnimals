### PROJECT TAG LIST WRANGLING ###

# This script loads our list of animals tagged for the CSMP project -> 2 separate dfs, one for teleosts and one for sharks
# Then pairs up the IDs with the metadata from the regional_tag_list (contains lats/longs etc)
## This script is designed to be called on from external Rmd files
## If you want to run from within this script:
## setwd("code")

getwd()
#rm(list=ls())
#setwd("../code")


# Packages
library(writexl)
library(readxl)
library(lubridate)
library(tidyverse)


# Load the 2 files and only retain the ID column
# Sharks
project_IDs_sharks <- read_excel("../data/csmp_sharks.xlsx", trim_ws = TRUE) %>% 
  dplyr::select(ID)
# Teleosts
project_IDs_teleosts <- read_excel("../data/csmp_teleosts.xlsx", trim_ws = TRUE) %>% 
  dplyr::select(ID)

# Bind together
project_IDs <- project_IDs_sharks %>% 
  rbind(project_IDs_teleosts)


# Then merge with the regional tag list to extract the rest of the metadata

# Need to get the regional list in first:
source("../code/regional_tag_list.R")


# List of total project tags
project_tags_all <- project_IDs %>% 
  merge(regional_tag_list, by = 'ID') # Adds in other variables we might need
# 137 tags total (inc sensor tags)

# List of project tags in sharks
project_tags_sharks <- project_tags_all %>% 
  filter(Org_type == 'Elasmobranch') #%>% 
  #distinct(transmitter_id)
#write.csv(project_sharks, file = '../data/RData/project_sharks.csv')
# 114 tags in sharks total (inc sensor tags)

# List of project tags in  teleosts
project_tags_teleosts <- project_tags_all %>% 
  filter(Org_type == 'Teleost')# %>% 
  #distinct(transmitter_id)
# 23 tags in telosts total (inc sensor tags)



# Just one entry per animal
## Should just be able to get rid of one of the sensor transmitters (e.g. pressure)

# All animals
project_serials <- 
  project_tags_all %>%
  filter(Type != "press")
# 112 total

project_serials_sharks <- 
  project_tags_sharks %>%
  filter(Type != "press")
# 99 total

project_serials_teleosts <- 
  project_tags_teleosts %>%
  filter(Type != "press")
# 13 total
  
# 137 - 112 = 25 animals with tags in: 10 teleost and 15 sharks
# Can check:
sensortag_organisms <- 
  project_tags_all %>% 
  filter(Type %in% c("press", "temp"))

# By reef

## Osprey
project_serials %>% 
  filter(Location == 'Osprey') %>% 
  droplevels() %>% 
  summary()
## 50 - 13 teleosts, 37 elasmobranchs

## Bougainville 
project_serials %>% 
  filter(Location == 'Bougainville') %>% 
  droplevels() %>% 
  summary()
# 9


## Holmes
project_serials %>% 
  filter(str_detect(Location, 'Holmes')) %>% 
  droplevels() %>% 
  summary()


## Flinders
project_serials %>% 
  filter(str_detect(Location, 'Flin')) %>% 
  droplevels() %>% 
  summary()










