## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(writexl)
library(readxl)
library(lubridate)
library(magrittr)
library(tidyverse)
#library(VTrack) # Current R version doesn't like VTrack - may need to install an earlier version..


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#rm(list=ls())
load(file = "../data/RData/alldata.RData")


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
glimpse(alldata)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
alldata %>% 
  dplyr::select(project_name, site_code, installation_name, station_name, station_name_long, Location, Site, station_latitude, station_longitude, Lat, Long) %>% 
  distinct()

summary(alldata$Site) %>% 
  as.data.frame()


## ----filter-movers-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
movers <- alldata %>% 
  filter(transmitter_id %in% c("A69-9001-64103", 
                             "A69-9001-60580", 
                             "A69-9001-64080", 
                             "A69-9001-63847", 
                             "A69-9001-4172",
                             "A69-9001-63947")) %>% 
  droplevels() %>% 
  mutate(transmitter_id = fct_relevel(transmitter_id, "A69-9001-64103", after = Inf)) %>%  # Have to move this individual to the end so their osprey obs doesn't overlap with 64080
  mutate(shark_id = factor(case_when(grepl('4172', transmitter_id) ~ 'Tiger 1',           # Create meaningful ID for later on
                                     grepl('63847', transmitter_id) ~ 'Tiger 2',
                                     grepl('63947', transmitter_id) ~ 'Tiger 3',
                                     grepl('64080', transmitter_id) ~ 'Silver 1',
                                     grepl('60580', transmitter_id) ~ 'Silver 2',
                                     grepl('64103', transmitter_id) ~ 'Grey 1'))) %>% 
  mutate(sharkloc = factor(case_when(grepl('4172', transmitter_id) ~ 'Tiger 1 - Saunders Reef',           # Create meaningful ID for later on
                                     grepl('63847', transmitter_id) ~ 'Tiger 3 - Cap. Bunkers',
                                     grepl('63947', transmitter_id) ~ 'Tiger 2 - Orpheus Island',
                                     grepl('64080', transmitter_id) ~ 'Silver 1 - Holmes Reef',
                                     grepl('60580', transmitter_id) ~ 'Grey 2 - Flinders Reef',
                                     grepl('64103', transmitter_id) ~ 'Grey 1 - Osprey Reef')))
  


## ----check-id-filter-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Simultaneously can check how many obs of each
movers %$% 
  summary(transmitter_id) 
movers %$% 
  summary(shark_id)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ext_dets <- read_excel('../data/outside_array_fromAB.xlsx', trim_ws = TRUE) %>% 
transmute(transmitter_id = factor(paste(`code space`, ID, sep = '-')),      # Going to make this the same structure as the tag_locs object below..
         Scientific_name = factor(Scientific_name),
         Location = factor(location),
         Site = factor(Site),
         Lat = lat,
         Long = long,
         Date = as.Date(date, format = "%d.%m.%Y")) %>% 
mutate(shark_id = factor(case_when(grepl('4172', transmitter_id) ~ 'Tiger 1',           # Create meaningful ID for later on
                              grepl('63847', transmitter_id) ~ 'Tiger 2',
                              grepl('63947', transmitter_id) ~ 'Tiger 3',
                              grepl('64080', transmitter_id) ~ 'Silver 1',
                              grepl('60580', transmitter_id) ~ 'Silver 2',
                              grepl('64103', transmitter_id) ~ 'Grey 1'))) %>% 
  mutate(sharkloc = factor(case_when(grepl('4172', transmitter_id) ~ 'Tiger 1 - Saunders Reef',           # Create meaningful ID for later on
                                     grepl('63847', transmitter_id) ~ 'Tiger 3 - Cap. Bunkers',
                                     grepl('63947', transmitter_id) ~ 'Tiger 2 - Orpheus Island',
                                     grepl('64080', transmitter_id) ~ 'Silver 1 - Holmes Reef',
                                     grepl('60580', transmitter_id) ~ 'Grey 2 - Flinders Reef',
                                     grepl('64103', transmitter_id) ~ 'Grey 1 - Osprey Reef')))
ext_dets 


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tag_locs <- movers %>% 
  dplyr::select(transmitter_id, Scientific_name, shark_id, sharkloc, Location, Site, Lat, Long, Date) %>% 
  distinct()
 
  tag_locs


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
movers <- 
  movers %>% 
  mutate(transmitter_id = transmitter_id,
         Scientific_name = Scientific_name,
         shark_id = shark_id,
         sharkloc = sharkloc,
         Location = installation_name,
         Site = station_name,
         Lat = station_latitude,
         Long = station_longitude,
         Date = as.Date(detection_timestamp, format = "%d.%m.%Y"),
        .keep ='none') %>% 
  bind_rows(tag_locs, ext_dets) %>% 
  arrange(transmitter_id, Date)                                     # Need to make sure all detections are ordered by individual and also by date of detection
movers



## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
movers <- 
  movers %>% 
  mutate(Sublocation = factor(case_when(grepl("F1|F2|F3", Site) ~ 'Flinders - South',
                                 grepl("F4|F5|F6", Site) ~ 'Flinders - North',
                                 grepl("H1|H2", Site) ~ 'Holmes - West',
                                 grepl("H3|H4", Site) ~ 'Holmes - East'))) %>%  
  mutate(Sublocation = coalesce(Sublocation, Location))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mover_sum_location <- 
  movers %>% 
  group_by(transmitter_id) %>%                                                     # Create a blank row per change in id. W/out this the  new 'to/from' vars will be mixed up across individuals
  group_modify(~ add_row(.x, .before=0)) %>%                                       # Put the row before each new transmitter_id. If you want it after then specify: group_modify(~ add_row(.x, .after = Inf))
  mutate(To_Location = lead(Location), .after = Location) %>%                      # Start creating to/from vars -> first for the locations (NB could do other way round using 'lag' instead of 'lead')
  rename(From_Location = Location) %>%                                             # 
  rename(From_Sublocation = Sublocation) %>%                                       # And for sublocations
  mutate(To_Sublocation = lead(From_Sublocation), .after = From_Sublocation) %>%   # 
  mutate(From_Lat = Lat,                                                           # And for lats/longs 
         From_Long = Long, .keep = "unused") %>%                                   #
  mutate(To_Lat = lead(From_Lat), .after = From_Lat) %>%                           #     
  mutate(To_Long = lead(From_Long), .after = From_Long) %>%                        # 
  mutate(From_Site = Site, .after = To_Sublocation,                                # And for sites
           To_Site = lead(From_Site), .keep = "unused") %>%                        # 
  filter(!is.na(From_Location),                                                    # Get rid of lines with NAs in the From column (these will basically be the line before the tagging event so obviously can't know this)
         !is.na(To_Location)) %>%                                                  # ..plus the one after the final entry for each animal (we don't know this either)
  filter(From_Sublocation != To_Sublocation,                                       # Finally, remove entries if the to/from sublocations or overall locations are the same (means no blue-water/large movement)
         From_Location != To_Location)                                         # Have to do both of these due to mismatch in names for tagging vs receiver locs
 
head(mover_sum_location)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tiger_locmovs <- mover_sum_location %>% 
  filter(str_detect(Scientific_name, 'Galeo'))

other_locmovs <- mover_sum_location %>% 
  filter(str_detect(Scientific_name, 'Carch'))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mover_sum_sublocation <- 
  movers %>% 
  group_by(transmitter_id) %>%                                                     # Create a blank row per change in id. W/out this the  new 'to/from' vars will be mixed up across individuals
  group_modify(~ add_row(.x, .before=0)) %>%                                       # Put the row before each new transmitter_id. If you want it after then specify: group_modify(~ add_row(.x, .after = Inf))
  mutate(To_Location = lead(Location), .after = Location) %>%                      # Start creating to/from vars -> first for the locations (NB could do other way round using 'lag' instead of 'lead')
  rename(From_Location = Location) %>%                                             # 
  rename(From_Sublocation = Sublocation) %>%                                       # And for sublocations
  mutate(To_Sublocation = lead(From_Sublocation), .after = From_Sublocation) %>%   # 
  mutate(From_Lat = Lat,                                                           # And for lats/longs 
         From_Long = Long, .keep = "unused") %>%                                   #
  mutate(To_Lat = lead(From_Lat), .after = From_Lat) %>%                           #     
  mutate(To_Long = lead(From_Long), .after = From_Long) %>%                        # 
  mutate(From_Site = Site, .after = To_Sublocation,                                # And for sites
           To_Site = lead(From_Site), .keep = "unused") %>%                        # 
  filter(!is.na(From_Location),                                                    # Get rid of lines with NAs in the From column (these will basically be the line before the tagging event so obviously can't know this)
         !is.na(To_Location)) %>%                                                  # ..plus the one after the final entry for each animal (we don't know this either)
  filter(From_Sublocation != To_Sublocation)                                       # Finally, remove entries if the to/from sublocations are the same (means no blue-water movement)

(mover_sum_sublocation)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
mover_sum_location %>% 
  filter(transmitter_id == 'A69-9001-60580')

mover_sum_sublocation %>% 
  filter(transmitter_id == 'A69-9001-60580')

#check <- movers %>%   filter(transmitter_id == 'A69-9001-60580')


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tiger_sublocmovs <- mover_sum_sublocation %>% 
  filter(str_detect(Scientific_name, 'Galeo'))

other_sublocmovs <- mover_sum_sublocation %>% 
  filter(str_detect(Scientific_name, 'Carch'))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
tag_locs_tigers <-
  tag_locs %>% 
  filter(Scientific_name == "Galeocerdo cuvier")

tag_locs_others <- 
  tag_locs %>% 
  filter(Scientific_name != "Galeocerdo cuvier")

