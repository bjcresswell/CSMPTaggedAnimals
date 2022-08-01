## Wrangling Ri ##

## This script is designed to be called on from the main wrangling Ri Rmd

setwd('code')

# Select only variables and organisms required for this analysis
ri_df <- alldata %>%
  filter(Org_type != 'NA') %>%                        # We don't want the random detections included in this analysis
  filter(Scientific_name !='Galeocerdo cuvier') %>%   # Also don't want the visitors from outside the array included in this analysis
  droplevels() %>% 
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

head(ri_df)

## The above df provides raw data to determine number of days detected  (each row is one day)
## Also provides info about beginning of the deployment (starts the date the animal was tagged.)
## The end of the deployment is when the receiver(s) were retrieved from the water
## The number of days between those two dates is the "days at large" value for the organism

## Reload the latest receiver metadata file, if not already loaded (contains the VR2 retrieval dates)
vr2ws_dep2 <- read_excel('../data/receiver_list_dep2.xlsx', trim_ws = TRUE) %>%
  tibble() %>% # Make into tibble format
  mutate(recovery_date = as.Date(recoverydatetime_timestamp), .keep = 'unused') %>% 
  mutate_if(is.character, as.factor) # Sort out variables

## Check
glimpse(vr2ws_dep2)

## Merge in just the retrieval dates to the main df
ri_df <- ri_df %>% 
  merge(vr2ws_dep2[c(4,10)], by='station_name') %>% 
  arrange(transmitter_id)

## Check
head(ri_df)


# Calculate days at large by station
station_ri <-
  ri_df %>% 
  group_by(station_name, transmitter_id, tag_date) %>% 
  mutate(detectiondays_station = n()) %>% 
  #ungroup() %>% 
  dplyr::select(!c(detection_date)) %>% 
  distinct() %>% 
  mutate(deploymentdays_station = as.integer(recovery_date - tag_date),
         ri_station = detectiondays_station/deploymentdays_station)


# Calculate days at large by installation
installation_ri <- station_ri %>% 
  group_by(installation_name, transmitter_id) %>% 
  mutate(detectiondays_installation = sum(detectiondays_station)) %>%
  group_by(installation_name) %>% 
  mutate(recovery_date = min(recovery_date)) %>% 
  dplyr::select(!c(station_name, station_name_long, ri_station,
                   detectiondays_station, deploymentdays_station)) %>%   # Remove all the station-level info
  distinct() %>% 
  mutate(deploymentdays_installation = as.integer(recovery_date - tag_date),
         ri_installation = detectiondays_installation/deploymentdays_installation)

  
save(station_ri, file = "../data/RData/station_ri.RData")
save(installation_ri, file = "../data/RData/installation_ri.RData")
