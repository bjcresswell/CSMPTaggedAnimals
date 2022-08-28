##  Residency index - station ##

## This script wrangles out residency indices (Ri) for each individual per station
## Mainly going to be interesting for the teleosts, but shark values are retained also
## This script is designed to be called on from the main Ri Rmd document. It you want to run code here, from within this script you need to run this first:
## setwd("code")


#getwd()
#rm(list=ls())
#setwd('../code')

# Load main data file
load(file = "../data/RData/alldata.RData")

# And for other required data, run the project_tags_all script:
source("../code/project_tags_list.R")

# Select only variables and organisms required for this analysis
station_ri <- alldata %>%
  filter(Org_type != 'NA') %>%                        # We don't want the random detections included in this analysis
  filter(Scientific_name !='Galeocerdo cuvier') %>%   # Also don't want the visitors from outside the array included in this analysis  droplevels() %>% 
  dplyr::select(transmitter_id, 
                #receiver_name,                                                       # VR2W serial # - don't want this in
                detection_timestamp,                                                  # 
                installation_name, station_name, station_name_long,                   # Variables relating to detecting receiver
                #deploymentdatetime_timestamp, recoverydatetime_timestamp,            # Will add in deployment dates later (for whole proj, not individual deps)
                Common_name, Scientific_name, Date, 
                Location, Site, Sex, Org_type) %>%                                    # Leave in the orrganism and tagging info
  mutate(tag_date = as.Date(Date), 
         detection_date = as.Date(detection_timestamp),                               # One row/observation for every day the organism detected in array
         tag_loc = Location, 
         tag_site = Site, 
         .keep = 'unused') %>% 
  distinct()

head(station_ri)


## The above df provides raw data to determine number of days detected (each row is one day) -> we want to summarise this to counts per individual per station
## Also provides info about beginning of the deployment (starts the date the animal was tagged.)
## The end of the deployment is when the receiver was retrieved from the water
## The number of days between those two dates is the "days at large" value for the organism

## Reload the latest receiver metadata file, if not already loaded (contains the VR2 retrieval dates)
vr2ws_dep2 <- read_excel('../data/receiver_list_dep2.xlsx', trim_ws = TRUE) %>%
  tibble() %>% # Make into tibble format
  mutate(recovery_date = as.Date(recoverydatetime_timestamp), .keep = 'unused') %>% 
  mutate_if(is.character, as.factor) # Sort out variables

## Check
glimpse(vr2ws_dep2)

## Merge in just the retrieval dates to the main df
station_ri <- 
  station_ri %>% 
  merge(vr2ws_dep2[c(4,10)], by='station_name') %>% 
  arrange(transmitter_id)

## Check
head(station_ri)

# Calculate days at large and Ri by station
station_ri <-
  station_ri %>%
  group_by(station_name, transmitter_id, tag_date) %>%
  mutate(detectiondays_station = n()) %>%
  #ungroup() %>%
  dplyr::select(!c(detection_date)) %>%
  distinct() %>%
  mutate(deploymentdays_station = as.integer(recovery_date - tag_date),
         ri_station = detectiondays_station/deploymentdays_station)

# So each station has an Ri value per individual organism. 
# Note that there will be some duplication of transmitter IDs as a few organisms went between installations, so these have more than one Ri assigned to them:
station_ri %$% 
  summary(duplicated(transmitter_id))

# Next, we should check missing IDs against project tag list
missing_from_ri <- 
  project_tags_all %>% 
  anti_join(station_ri, by = "transmitter_id")
## There should be 9 actual animals) missing - same as identified by tag_asst.Rmd so we already knew about these (see that Rmd for more info)
## Plus: half of the sensor tags (the temp ones)


# SINCE I WENT BACK AND WRANGLED OUT THE EXTRA SENSOR TAGS AT THE BEGINNING, DON'T THINK WE NEED THIS ANY MORE:

# Now need to deal with duplicate serials, as T/P tagged organisms will have tags counted twice

# First need to pull out the T/P entries into a separate df:

sensor_station_ri <-                                                   # Extract just the T/P tags from the main df
  project_tags_all %>%                                       # Need to get the serial number and tag info
  dplyr::select('transmitter_id', 'Type', 'Serial') %>%    # back out of the project_tags_all
  merge(station_ri, by = 'transmitter_id') %>%        # and merge 
  filter(Type != 'pinger') %>%                             # Then pull out just the T/P entries - 44 of them which will become 22
  group_by(Serial, station_name) %>%                                     # Group by the Serial #
  slice_max(ri_station, n = 1, with_ties = FALSE)     # And remove the lower of the Ri values (or duplicate)


# And need to do the reverse on station_ri:

nonsensor_station_ri <- 
  project_tags_all %>%                                       # Need to get the serial number and tag info
  dplyr::select('transmitter_id', 'Type', 'Serial') %>%    # back out of the project_tags_all
  merge(station_ri, by = 'transmitter_id') %>%        # and merge 
  filter(Type == 'pinger')

# And then combine back together
#station_ri <- nonsensor_station_ri %>% 
#  bind_rows(sensor_station_ri)


#save(station_ri, file = "../data/RData/station_ri.RData")
#write_csv(station_ri, file = "../output/station_Ri.csv")


