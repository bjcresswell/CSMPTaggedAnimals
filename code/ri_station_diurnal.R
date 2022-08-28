## Day - Night Ri ##



#getwd()
#rm(list=ls())
#setwd('../code')

# Load main data file
load(file = "../data/RData/alldata.RData")

# And for other required data, run the project_tag_list script:
source("../code/project_tag_list.R")

# Select only variables and organisms required for this analysis
diurnal_ri <- alldata %>%
  filter(Org_type != 'NA') %>%                        # We don't want the random detections included in this analysis
  filter(Scientific_name !='Galeocerdo cuvier') %>%   # Also don't want the visitors from outside the array included in this analysis  droplevels() %>% 
  dplyr::select(transmitter_id, 
                #receiver_name,                                                       # VR2W serial # - don't want this in
                detection_timestamp,                                                  # 
                installation_name, station_name, station_name_long,                   # Variables relating to detecting receiver
                #deploymentdatetime_timestamp, recoverydatetime_timestamp,            # Will add in deployment dates later (for whole proj, not individual deps)
                Common_name, Scientific_name, Date, 
                Location, Site, Sex, Org_type) %>%                                    # Leave in the orrganism and tagging info
  mutate(hour = hour(detection_timestamp)) %>% 
  mutate(tod = case_when(hour %in% (6:18) ~ "Day",
                         TRUE ~ "Night")) %>% 
  mutate(tag_date = as.Date(Date), 
         detection_date = as.Date(detection_timestamp),                               # One row/observation for every day the organism detected in array
         tag_loc = Location, 
         tag_site = Site, 
         .keep = 'unused') %>%
    dplyr::select(!hour) %>% 
  distinct
  

## Reload the latest receiver metadata file, if not already loaded (contains the VR2 retrieval dates)
vr2ws_dep2 <- read_excel('../data/receiver_list_dep2.xlsx', trim_ws = TRUE) %>%
  tibble() %>% # Make into tibble format
  mutate(recovery_date = as.Date(recoverydatetime_timestamp), .keep = 'unused') %>% 
  mutate_if(is.character, as.factor) # Sort out variables



## Merge in just the retrieval dates to the main df
diurnal_ri <- 
  diurnal_ri %>% 
  merge(vr2ws_dep2[c(4,10)], by='station_name') %>% 
  arrange(transmitter_id)


# Split out into day vs night
day_ri <-
  diurnal_ri %>%
  filter(tod == 'Day') %>% 
  distinct()

night_ri <-
  diurnal_ri %>%
  filter(tod == 'Night') %>% 
  distinct()


# Calculate days at large and Ri by day vs night
## Day

day_ri <- 
day_ri %>% 
  group_by(station_name, transmitter_id, tag_date) %>%
  mutate(detectiondays_station = n()) %>%
  #ungroup() %>%
  dplyr::select(!c(detection_date)) %>%
  distinct() %>%
  mutate(deploymentdays_station = as.integer(recovery_date - tag_date),
         ri_station = detectiondays_station/deploymentdays_station)

night_ri <-
  night_ri %>% 
  group_by(station_name, transmitter_id, tag_date) %>%
  mutate(detectiondays_station = n()) %>%
  #ungroup() %>%
  dplyr::select(!c(detection_date)) %>%
  distinct() %>%
  mutate(deploymentdays_station = as.integer(recovery_date - tag_date),
         ri_station = detectiondays_station/deploymentdays_station)

# Bind back to one df
daynight_ri <- 
  day_ri %>% 
  bind_rows(night_ri)

# Now filter out by taxa/organism type etc

alldata %>% 
  filter(Common_name == "Footballer trout")


# Teleost Ri day vs night - this is effectively installation Ri (presence vs absence over the whole of Osprey)

teleost_ri_dn <- 
  daynight_ri %>% 
  filter(Org_type == "Teleost") %>% 
  group_by(tod) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station))
  

# GT Ri day vs night
gt_ri_dn <- 
daynight_ri %>% 
  filter(Scientific_name == 'Caranx ignobilis') %>% 
  group_by(tod) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station))

gt_ri_dn %>% 
  ggplot(aes(x = tod, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs(y= "Ri", x = "Time of day")
# Looking pretty even


# Black trevally Ri day vs night 
lugub_ri_dn <- 
daynight_ri %>% 
  filter(Scientific_name == 'Caranx lugubris') %>% 
  group_by(tod) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station))

lugub_ri_dn %>% 
  ggplot(aes(x = tod, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs(y= "Ri", x = "Time of day")
# Pretty similar - maybe more present in the day time

# Trout Ri day vs night - probably the most interesting
trout_ri_dn <- 
daynight_ri %>% 
  filter(Common_name == "Footballer trout") %>% 
  group_by(tod) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station))

trout_ri_dn %>% 
  ggplot(aes(x = tod, y = mean)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  labs(y= "Ri", x = "Time of day")

# Note the small Ri for day time is caused by 2 observations at just after 6am.
# Can check by running:
alldata %>%
  filter(Common_name == "Footballer trout") %>% 
  dplyr::select(transmitter_id, 
                #receiver_name,                                                       # VR2W serial # - don't want this in
                detection_timestamp,                                                  # 
                installation_name, station_name, station_name_long,                   # Variables relating to detecting receiver
                #deploymentdatetime_timestamp, recoverydatetime_timestamp,            # Will add in deployment dates later (for whole proj, not individual deps)
                Common_name, Scientific_name, Date, 
                Location, Site, Sex, Org_type) %>%                                    # Leave in the orrganism and tagging info
  mutate(hour = hour(detection_timestamp)) %>% 
  mutate(tod = case_when(hour %in% (6:18) ~ "Day",
                         TRUE ~ "Night")) %>% 
  filter(tod == "Day")

