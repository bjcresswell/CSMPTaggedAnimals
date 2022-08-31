##  Residency index - installation ##


## This script wrangles out residency indices (Ri) for each individual per location.  Ri here is calculated at the installation level (rather than individual station)
## Mainly going to be interesting for the shark data, but teleost values are retained also
## This script is designed to be called on from the main Ri Rmd document. It you want to run code here, from within this script you need to run this first:
##setwd("code")

getwd()
#rm(list=ls())
setwd('../code')

# Load main data file
load(file = "../data/RData/alldata.RData")

# And for other required data, run the project_tag_list script:
source("../code/project_tag_list.R")

# Select only variables and organisms required for this analysis
installation_ri <-
  alldata %>%
  filter(Org_type != 'NA') %>%                        # We don't want the random detections included in this analysis
  filter(Scientific_name !='Galeocerdo cuvier') %>%   # Also don't want the visitors from outside the array included in this analysis
  droplevels() %>% 
  dplyr::select(transmitter_id, ID, Serial,
                #receiver_name,                                                       # VR2W serial # - don't want this in
                detection_timestamp,                                                  # 
                installation_name, station_name, station_name_long,                   # Variables relating to detecting receiver -> need to leave station info in for now to pair up with retrievals
                #deploymentdatetime_timestamp, recoverydatetime_timestamp,            # Will add in deployment dates later (accurate ones which we'll apply to whole proj, not individual deps)
                Common_name, Scientific_name, Date, 
                Location, Site, Sex, Org_type) %>%                                    # Leave in the organism and tagging info
  mutate(tag_date = as.Date(Date), 
         detection_date = as.Date(detection_timestamp),                               # One row/observation for every day the organism detected in array
         tag_subinstallation = Location, 
         tag_station = Site, 
         .keep = 'unused') %>% 
  mutate(tag_installation = case_when(grepl("Boug", tag_subinstallation) ~ "Bougainville",  # Need a variable that records the tagging installation name
                                      grepl("Flin", tag_subinstallation) ~ "Flinders",
                                      grepl("Holm", tag_subinstallation) ~ "Holmes",
                                      grepl("Ospr", tag_subinstallation) ~ "Osprey")) %>% 
  distinct()




head(installation_ri)

alldata  %$% 
  summary(Common_name)

## The above df provides raw data to determine number of days detected  (each row is one day) -> we want to summarise this to counts per individual per installation
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
installation_ri <- 
  installation_ri %>% 
  merge(vr2ws_dep2[c(4,10)], by='station_name') %>%   # Merges just with station name and recovery date
  arrange(transmitter_id)

## Check
head(installation_ri)

# Calculate days at large and Ri by installation
installation_ri <- 
  installation_ri %>% 
  dplyr::select(!c(station_name, station_name_long)) %>% 
  group_by(installation_name) %>% 
  mutate(recovery_date = min(recovery_date)) %>%                                            # Applies the earliest recovery date to all receivers
  group_by(installation_name, transmitter_id) %>% 
  distinct() %>% 
  mutate(detectiondays_installation = n()) %>% 
  dplyr::select(!c(detection_date)) %>% 
  distinct() %>% 
  mutate(deploymentdays_installation = as.integer(recovery_date - tag_date),
         ri_installation = detectiondays_installation/deploymentdays_installation) %>% 
  mutate(ri_installation = case_when(ri_installation > 1 ~ 1,                               # Some individuals have Ri>1 (impossible), caused by the rounding down of recovery date above
                                     TRUE ~ ri_installation))

# So each installation has an Ri value per individual organism. 
#Note that there will be some duplication of transmitter IDs as a few organisms went between installations, so these have more than one Ri assigned to them:
installation_ri %$% 
  summary(duplicated(Serial))
# 103 total, 3 of which duplicated. 

# Duplicates should just be sharks. Check...
installation_ri %>% 
  group_by(Serial) %>% 
  filter(n()>1)


# Next, we should check missing IDs against project tag list
missing_from_ri <- 
  project_tags_all %>% 
  anti_join(installation_ri, by = "Serial")
## 12 transmitter_IDs (9 actual animals) missing - same as identified by tag_asst.Rmd so we already knew about these (see that Rmd for more info)



# SINCE I WENT BACK AND WRANGLED OUT THE EXTRA SENSOR TAGS AT THE BEGINNING, DON'T THINK WE NEED THIS ANY MORE:

# Now we need to deal with duplicate serials, as T/P tagged organisms will have tags counted twice

# First need to pull out the T/P entries into a separate df:
sensor_installation_ri <-                                                   # Extract just the T/P tags from the main df
project_tags_all %>%                                       # Need to get the serial number and tag info
  dplyr::select('transmitter_id', 'Type') %>%    # back out of the project_tag_list
  merge(installation_ri, by = 'transmitter_id') %>%        # and merge 
  filter(Type != 'pinger') %>%                             # Then pull out just the T/P entries - 44 of them which will become 22
  group_by(Serial) %>%                                     # Group by the Serial #
  slice_max(ri_installation, n = 1, with_ties = FALSE)     # And remove the lower of the Ri values (or duplicate)
  

# And need to do the reverse on installation_ri:

nonsensor_installation_ri <- 
  project_tags_all %>%                                       # Need to get the serial number and tag info
  dplyr::select('transmitter_id', 'Type') %>%    # back out of the project_tag_list
  merge(installation_ri, by = 'transmitter_id') %>%        # and merge 
  filter(Type == 'pinger')

# And then combine back together
## Original installation_ri was 128 obs. Minus 44 + 22 = 106 obs

installation_ri2 <- nonsensor_installation_ri %>% 
  bind_rows(sensor_installation_ri)

#Check
installation_ri
anti_join(installation_ri, installation_ri2, by = "transmitter_id")


# Relevel installation_name by North to South
installation_ri <- installation_ri %>% 
  mutate(installation_name = fct_relevel(installation_name, c('Osprey', 'Bougainville', 'Holmes', 'Flinders'))) 

#save(installation_ri, file = "../data/RData/installation_ri.RData")
write_csv(installation_ri, file = "../output/Installation_Ri.csv")



#######
