## Wrangling Ri - Osprey ##

## This script is designed to be called on from the main wrangling Ri Rmd

setwd('code')

ri_osprey <- alldata %>% 
  filter(Location == 'Osprey')

ri_osprey <- ri_osprey %>% 
  mutate(tag_date = as.Date(Date), .keep = 'unused')


# Need to figure out days at large
## This is time from tagging to the end of the study period (in this case, removal of an station/receiver or installation/array)
## To get the dates of the VR2W retrievals:
osprey_dep2 <- read_excel('../data/receiver_list_dep2.xlsx', trim_ws = TRUE) %>%
  tibble() %>% # Make into tibble format
  mutate_if(is.character, as.factor) %>%  # Sort out variables
  filter(installation_name == 'Osprey')

# Check recovery date(s)
osprey_dep2 %$% 
  summary(recoverydatetime_timestamp)

# Calculate days at large:
osprey_DAL <- 
  os_grs %>% 
  mutate(detection_date = as.Date(detection_timestamp)) %>% 
  dplyr::select(transmitter_id, tag_date, detection_date) %>% 
  distinct() %>% 
  group_by(transmitter_id, tag_date) %>% 
  mutate(detected_days = n()) %>% 
  dplyr::select(!detection_date) %>% 
  distinct() %>% 
  mutate(deployment_end = date('2022-03-03')) %>% 
  mutate(deployment_days = as.integer(deployment_end - tag_date))