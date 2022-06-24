### PROJECT TAG LIST WRANGLING ###

# This script loads our list of animals tagged for the CSMP project -> 2 separate dfs, one for teleosts and one for sharks
# Then pairs up the IDs with the metadata from the regional_tag_list (contains lats/longs etc)


# Set wd
setwd("../code")

# Or if you want to run from within this script:
#setwd("code")

getwd()
#rm(list=ls())


# Packages
library(writexl)
library(readxl)
library(lubridate)
library(tidyverse)


# Load the 2 files and only retain the ID column
# Sharks
project_serials_sharks <- read_excel("../data/csmp_sharks.xlsx", trim_ws = TRUE) %>% 
  dplyr::select(ID)
# Teleosts
project_serials_teleosts <- read_excel("../data/csmp_teleosts.xlsx", trim_ws = TRUE) %>% 
  dplyr::select(ID)

# Bind together
project_serials <- project_serials_sharks %>% 
  rbind(project_serials_teleosts) 


# Then merge with the regional tag list to extract the rest of the metadata

# Need to get the regional list in first:
source("../code/regional_tag_list.R")


project_tag_list <- project_serials %>% 
  merge(regional_tag_list, by = 'ID') # Adds in other variables we might need


# List of project sharks
project_sharks <- project_tag_list %>% 
  filter(Org_type == 'Elasmobranch') %>% 
  distinct(transmitter_id)
write.csv(project_sharks, file = '../data/RData/project_sharks.csv')

# List of project teleosts
project_teleosts <- project_tag_list %>% 
  filter(Org_type == 'Teleost') %>% 
  distinct(transmitter_id)
