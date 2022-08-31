## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------
# Packages
library(knitr)
library(kableExtra)
library(sp)
library(rgdal)
library(rgeos)
library(adehabitatHR)
library(leaflet)
library(ggmap)    # for fortifying shapefiles
library(readxl)
library(scatterpie)
library(plotrix)   # Calculates SE
library(collapse)
library(magrittr)
library(tidyverse)


## ----housekeeping, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
# Housekeeping
#rm(list=ls())
#getwd()


## ----load-data, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
source("ri_installation.R")
source("ri_station.R")


## ----teleost-inst-ri-plot, echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------
teleost_installation_ri <-   
installation_ri %>% 
  filter(Org_type == 'Teleost') %>% 
  group_by(Scientific_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print

#teleost_installation_ri %>% write.csv("../output/ri_installation_teleosts.csv")

teleost_installation_ri_plot <- 
teleost_installation_ri %>% 
  ggplot(aes(x = Scientific_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,.7), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  theme(panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent")) +
  #ggtitle("Installation Ri (Osprey) for teleost taxa") +
  labs(y= "Ri (mean ± SE)", x = "Taxa")
#ggsave(teleost_installation_ri_plot, filename = "../output/inst_ri_teleost_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)



## ----gt-ri, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
station_ri_gts <- 
  station_ri %>% 
  filter(Scientific_name == 'Caranx ignobilis')
  
station_ri_gts %>% 
  kbl() %>%
  kable_minimal()


## ----cl-ri, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
station_ri_lugub <- 
  station_ri %>% 
  filter(Scientific_name == 'Caranx lugubris') 

station_ri_lugub %>% 
  kbl() %>%
  kable_minimal()


## ----pl-ri, echo=FALSE, message=FALSE, warning=FALSE--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
station_ri_trout <- 
  station_ri %>% 
  filter(grepl('laev', Scientific_name)) 

station_ri_trout %>% 
  kbl() %>%
  kable_minimal()


## ----gt-bar, echo=FALSE, message=FALSE, warning=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
station_ri_stats_gts <- 
  station_ri_gts %>% 
  group_by(Scientific_name, station_name, station_name_long) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print

station_ri_stats_gts %>%  write.csv("../output/ri_station_gts.csv")

ri_gts_bar <- 
station_ri_stats_gts %>% 
  ggplot(aes(x = station_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,0.7), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  theme(panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom") +
  labs(y= "Ri (mean ± SE)", x = "Osprey station code")

ri_gts_bar

ggsave(ri_gts_bar, filename = "../output/ri_gts_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----lug-bar, echo=FALSE, message=FALSE, warning=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------
station_ri_stats_lugub <- 
station_ri_lugub %>% 
  group_by(Scientific_name, station_name, station_name_long) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print

station_ri_stats_lugub %>%   write.csv("../output/ri_station_lugub.csv")

ri_lugub_bar <- 
station_ri_stats_lugub %>% 
  ggplot(aes(x = station_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,0.8), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  theme(panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom") +
  labs(y= "Ri (mean ± SE)", x = "Osprey station code")

ri_lugub_bar

ggsave(ri_lugub_bar, filename = "../output/ri_lugub_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----trout-bar, eval=FALSE, message=FALSE, warning=FALSE, include=FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------
station_ri_stats_trout <- 
station_ri_trout %>% 
  group_by(station_name, station_name_long) %>% 
  summarise(mean = mean(ri_station), sd = sd(ri_station), se = std.error(ri_station)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print

station_ri_stats_trout %>% 
  write.csv("../output/ri_station_trout.csv")

ri_trout_bar <- 
station_ri_stats_trout %>% 
  ggplot(aes(x = station_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,0.1), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  theme(panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom") +
  labs(y= "Ri (mean ± SE)", x = "Osprey station code")

ri_trout_bar

ggsave(ri_trout_bar, filename = "../output/ri_trout_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


