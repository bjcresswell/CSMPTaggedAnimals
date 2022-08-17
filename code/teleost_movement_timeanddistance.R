## ----setup-, include=FALSE----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
options(knitr.duplicate.label = 'allow')


## ----packages, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
# Packages
library(sp)
library(rgdal)
library(rgeos)
#library(tibbletime) # Time series management - don't think we need. lubridate prob better.
library(adehabitatHR)
library(leaflet)
library(ggmap)    # for fortifying shapefiles
library(readxl)
library(scatterpie)
library(plotrix)   # Calculates SE
library(collapse)
library(magrittr)
library(tidyverse)


## ----housekeeping, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----
# Housekeeping
#rm(list=ls())
#getwd()


## ----load-data, message=FALSE, warning=FALSE----------------------------------
load(file = "../data/RData/alldata.RData")


## ----teleost-filter-----------------------------------------------------------
teleosts <- 
  alldata %>% 
  filter(Org_type == 'Teleost')


## ----df-check, echo=FALSE, message=FALSE, warning=FALSE-----------------------
teleosts %>% 
  dplyr::select(transmitter_id) %>% 
  distinct() 


## ----check-proj-teleosts, echo=FALSE, message=FALSE, warning=FALSE------------
# Can check against project_teleosts df
source('project_tag_list.R')
project_teleosts %>% 
  group_by(Serial) %>% 
  distinct()


## ----teleost-abacus, echo=FALSE, message=FALSE, warning=FALSE-----------------
teleosts %>% 
  ggplot(aes(x = detection_timestamp, y = station_name)) + 
  xlab("Date") + 
  ylab("Station") +
  geom_point() +
  facet_wrap(~ transmitter_id)


## ----teleosts-mutate-daynight, echo=FALSE, message=FALSE, warning=FALSE-------
teleosts <- 
  teleosts %>%
  mutate(hour = hour(detection_timestamp),
         minute = minute(detection_timestamp),
         time = hm(paste(hour, minute))) %>% 
  mutate(tod = case_when(hour %in% (6:18) ~ "Day",
         TRUE ~ "Night"))


## ----tab-plot, echo=FALSE, message=FALSE, warning=FALSE-----------------------
# Day
teleosts %>% 
  filter(tod == 'Day') %>% 
  dplyr::select(station_name, station_name_long) %>% 
  group_by(station_name, station_name_long) %>% 
  mutate(stationobs = n()) %>% 
  distinct %>% 
  arrange(station_name) %>% 
  #print() %>% 
  ggplot(aes(x = station_name, y = stationobs)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ggtitle("Day") +
  labs(y= "No observations", x = "Osprey station code")

# Night
teleosts %>% 
  filter(tod == 'Night') %>% 
  dplyr::select(station_name, station_name_long) %>% 
  group_by(station_name, station_name_long) %>% 
  mutate(stationobs = n()) %>% 
  distinct %>% 
  arrange(station_name) %>% 
  #print() %>% 
  ggplot(aes(x = station_name, y = stationobs)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  #geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  ggtitle("Night") +
  labs(y= "No observations", x = "Osprey station code")


## -----------------------------------------------------------------------------
# Day
teleosts %>% 
  filter(tod == 'Day') %>% 
  dplyr::select(station_name, station_name_long) %>% 
  group_by(station_name, station_name_long) %>% 
  mutate(stationobs = n()) %>% 
  distinct %>% 
  arrange(station_name) %>% 
  print() 


# Night
teleosts %>% 
  filter(tod == 'Night') %>% 
  dplyr::select(station_name, station_name_long) %>% 
  group_by(station_name, station_name_long) %>% 
  mutate(stationobs = n()) %>% 
  distinct %>% 
  arrange(station_name) %>% 
  print()



## -----------------------------------------------------------------------------
#trout <- 
  teleosts %>% 
  filter(Common_name == "Footballer trout") %>% 
  mutate(detection_date = as.Date(detection_timestamp)) %>% 
  dplyr::select(transmitter_id, detection_date) %>% 
  distinct() %>% 
  arrange(detection_date) %>% 
  ggplot(aes(x = detection_date, y = transmitter_id)) + 
  xlab("Date") + 
  ylab("Trout") +
  geom_point()







