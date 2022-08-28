### REGIONAL TAG LIST WRANGLING ###

# This script loads the list of known tags deployed in QLD/GBR/CSMP
# Metadata is from a file from AB for a few reasons:
# - The db format from the IMOS site is unsuitable for use in R.
# - The df of just the project tagged animals only contains info for those animals and no details on lat/long etc.
# - It's pretty comprehensive in terms of metadata

# The only precaution that will be needed is to make sure this file is updated manually over time (as new tags get added to the wider db)

# Set wd
setwd("~/OneDrive - James Cook University/Ben PhD/Data & analysis/CSMPTaggedAnimals/code")
#rm(list=ls())


# Packages
library(writexl)
library(readxl)
library(lubridate)
library(tidyverse)


# These are in 2 tabs in the same spreadsheet so need to load one at a time and get into a format for row binding:

# Teleosts:
regional_teleost_list <- read_excel('../data/Master_tagged_animals_BC.xlsx', 2, trim_ws = TRUE) %>% # imports the spreadsheet with the list of tagged teleosts
  mutate(transmitter_id=paste(Freq, Space, ID, sep = '-'), .after=ID) %>% # Creates a new column for the complete Tag ID
  dplyr::select(!25) %>% # Gets rid of the notes column
  mutate(Org_type = factor('Teleost')) # Column with Teleost vs Shark -> may need later

regional_teleost_list %$% 
  summary(factor(Common_name))

# Sharks:
regional_shark_list <- read_excel('../data/Master_tagged_animals_BC.xlsx', 1, trim_ws = TRUE) %>% # imports the spreadsheet with the list of tagged teleosts
  mutate(transmitter_id=paste(Freq, Space, ID, sep = '-'), .after=ID) %>% # Creates a new column for the complete Tag ID
  dplyr::select(!25) %>% # Gets rid of the sat-tag column
  mutate(Org_type = factor('Elasmobranch')) # Column with Teleost vs Shark -> may need later


# Combine
regional_tag_list <- rbind(regional_teleost_list, regional_shark_list)

# Checks
#glimpse(regional_tag_list)
#glimpse(regional_teleost_list)
#glimpse(regional_shark_list)

# All variables that should be numeric are numeric so now just need to...

# Convert any character variables to factors
regional_tag_list <- regional_tag_list %>% 
  mutate_if(is.character, as.factor) # Sort out variables

