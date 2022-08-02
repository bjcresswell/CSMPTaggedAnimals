## Wrangling Ri ##

## This script is designed to be called on from the main wrangling Ri Rmd
## If you want to run from within this script:
## setwd("code")

#getwd()
rm(list=ls())
#setwd('../code')

# Load main data file
load(file = "../data/RData/alldata.RData")

# And for other required data, run the project_tag_list script:
source("../code/project_tag_list.R")



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
## Not going to progress this any further unless required.

# Calculate days at large by installation
installation_ri <- ri_df %>% 
  dplyr::select(!c(station_name, station_name_long)) %>% 
  group_by(installation_name) %>% 
  mutate(recovery_date = min(recovery_date)) %>% 
  group_by(installation_name, transmitter_id) %>% 
  distinct() %>% 
  mutate(detectiondays_installation = n()) %>% 
  dplyr::select(!c(detection_date)) %>% 
  distinct() %>% 
  mutate(deploymentdays_installation = as.integer(recovery_date - tag_date),
         ri_installation = detectiondays_installation/deploymentdays_installation) %>% 
  mutate(ri_installation = case_when(ri_installation > 1 ~ 1,
                                     TRUE ~ ri_installation))

# There is some duplication in here as T/P organisms will be counted twice
# So need to deal with this but before we do, we should check missing IDs against project tag list
missing_from_ri <- 
  project_tag_list %>% 
  anti_join(installation_ri, by = "transmitter_id")
## 12 transmitter_IDs (9 actual animals) missing - same as identified by tag_asst.Rmd so we already knew about these (see that Rmd for more info)

# First need to pull out the T/P entries into a separate df:

tp_ri <-                                                   # Extract just the T/P tags from the main df
project_tag_list %>%                                       # Need to get the serial number and tag info
  dplyr::select('transmitter_id', 'Type', 'Serial') %>%    # back out of the project_tag_list
  merge(installation_ri, by = 'transmitter_id') %>%        # and merge 
  filter(Type != 'pinger') %>%                             # Then pull out just the T/P entries - 44 of them which will become 22
  group_by(Serial) %>%                                     # Group by the Serial #
  slice_max(ri_installation, n = 1, with_ties = FALSE)     # And remove the lower of the Ri values (or duplicate)
  

# And need to do the reverse on installation_ri:

installation_ri <- 
  project_tag_list %>%                                       # Need to get the serial number and tag info
  dplyr::select('transmitter_id', 'Type', 'Serial') %>%    # back out of the project_tag_list
  merge(installation_ri, by = 'transmitter_id') %>%        # and merge 
  filter(Type == 'pinger')

# And then combine back together
## Original installation_ri was 128 obs. Minus 44 + 22 = 106 obs

installation_ri <- installation_ri %>% 
  bind_rows(tp_ri)


#save(station_ri, file = "../data/RData/station_ri.RData")
#save(installation_ri, file = "../data/RData/installation_ri.RData")

write_csv(installation_ri, file = "../output/Installation_Ri.csv")

# Split up into relevant groups.

# 1. Taxa

## Grey reef sharks
ri_greys <- 
  installation_ri %>% 
  filter(Scientific_name == 'Carcharhinus amblyrhynchos')

## Silver tips
ri_silvers <- 
  installation_ri %>% 
  filter(Scientific_name == 'Carcharhinus albimarginatus')

## GTs
ri_gts <- 
  installation_ri %>% 
  filter(Scientific_name == 'Caranx ignobilis')

## Lugubris
ri_lugub <- 
  installation_ri %>% 
  filter(Scientific_name == 'Caranx lugubris')

## Trout
ri_trout <- 
  installation_ri %>% 
  filter(grepl('laev', Scientific_name))














