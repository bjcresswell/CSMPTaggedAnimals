## Basic plotting of Ri stats ##

setwd('code')
rm(list=ls())

load(file = "../data/RData/station_ri.RData")
load(file = "../data/RData/installation_ri.RData")



installation_ri %>% 
  filter(installation_name = Osprey)