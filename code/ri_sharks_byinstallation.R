## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ----packages, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------------------------------------------------------------------------------------------
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
library(patchwork)
library(collapse)
library(magrittr)
library(tidyverse)


## ----housekeeping, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE--------------------------------------------------------------------------------------
# Housekeeping
#rm(list=ls())
#getwd()


## ----load-data, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE-----------------------------------------------------------------------------------------
source("ri_installation.R")


## ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
installation_ri %$% 
  summary(installation_name)

installation_ri <- installation_ri %>% 
  mutate(installation_name = fct_relevel(installation_name, c('Osprey', 'Bougainville', 'Holmes', 'Flinders'))) 


## ----osprey-ri, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sharkri_osprey <- 
  installation_ri %>% 
  filter(installation_name == 'Osprey')


## ----osprey-ri, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sharkri_bougainville <- 
  installation_ri %>% 
  filter(installation_name == 'Bougainville')


## ----osprey-ri, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sharkri_holmes <- 
  installation_ri %>% 
  filter(installation_name == 'Holmes')


## ----osprey-ri, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------
sharkri_flinders <- 
  installation_ri %>% 
  filter(installation_name == 'Flinders')


## ----grs-stats-1, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------
osprey_stats <- 
  sharkri_osprey %>% 
  filter(Org_type == "Elasmobranch") %>% 
  group_by(Scientific_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print() 

#osprey_stats %>%   write.csv("../output/sharkri_osprey.csv")

ri_osprey_bar <- 
  osprey_stats %>% 
  ggplot(aes(x = Scientific_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black",fill = "steelblue", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  #cale_x_discrete(labels = c("Silvers", "Greys"))+
  scale_x_discrete(labels = NULL)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c( 0.4, 0.8)) +
  theme(#panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        #axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  annotate("text", x = 1, y = 0.9, label = "Osprey", size = 3, fontface = 2) +
  labs(y= "", x = "")

ri_osprey_bar
  
#ggsave(ri_osprey_bar, filename = "../output/ri_osprey_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----boug-stats, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----------------------------------------------------------------------------------------
bougainville_stats <- 
  sharkri_bougainville %>% 
  filter(Org_type == "Elasmobranch") %>% 
  group_by(Scientific_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print() 

#bougainville_stats %>%   write.csv("../output/sharkri_bougainville.csv")

ri_bougainville_bar <- 
  bougainville_stats %>% 
  ggplot(aes(x = Scientific_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black",fill = "springgreen", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  #scale_x_discrete(labels = c("Silvers", "Greys"))+
  scale_x_discrete(labels = NULL)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c( 0.4, 0.8)) +
  theme(#panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        #axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  #annotate("text", x = 1, y = 0.9, label = "Bougainville", size = 3, fontface = 2) +
  annotate("text", x = 1.2, y = 0.9, label = "Bougainville", size = 3, fontface = 2) +
  labs(y= "", x = "")

ri_bougainville_bar

#ggsave(ri_bougainville_bar, filename = "../output/ri_bougainville_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----grs-stats-1, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------
holmes_stats <- 
  sharkri_holmes %>% 
  filter(Org_type == "Elasmobranch") %>% 
  group_by(Scientific_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print() 

#holmes_stats %>%   write.csv("../output/sharkri_holmes.csv")

ri_holmes_bar <- 
  holmes_stats %>% 
  ggplot(aes(x = Scientific_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black",fill = "yellow", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  #scale_x_discrete(labels = c("Silvers", "Greys"))+
  scale_x_discrete(labels = NULL)+
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c( 0.4, 0.8)) +
  theme(#panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        #axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  annotate("text", x = 1, y = 0.9, label = "Holmes", size = 3, fontface = 2) +
  labs(y= "", x = "")

ri_holmes_bar

#ggsave(ri_holmes_bar, filename = "../output/ri_holmes_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----grs-stats-1, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------
flinders_stats <- 
  sharkri_flinders %>% 
  filter(Org_type == "Elasmobranch") %>% 
  group_by(Scientific_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print() 

#flinders_stats %>%   write.csv("../output/sharkri_flinders.csv")

ri_flinders_bar <- 
  flinders_stats %>% 
  ggplot(aes(x = Scientific_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black",fill = "coral", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_x_discrete(labels = c("Silvers", "Greys"))+
  scale_y_continuous(expand = c(0,0), limits = c(0,1), breaks = c( 0.4, 0.8)) +
  theme(#panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        #axis.line = element_line(),
        panel.background = element_rect(fill = "white", colour = "black"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  annotate("text", x = 1, y = 0.9, label = "Flinders", size = 3, fontface = 2) +
  labs(y= "", x = "")

ri_flinders_bar

#ggsave(ri_flinders_bar, filename = "../output/ri_flinders_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----combined, fig.width=10, fig.height=12-----------------------------------------------------------------------------------------------------------------------------

ri_combined_bar <- 
ri_osprey_bar/ri_bougainville_bar/ri_holmes_bar/ri_flinders_bar


ri_combined_bar


ggsave(ri_combined_bar, filename = "../output/ri_combined_bar.png", width = 100, height = 160, units = 'mm', dpi = 300)




## ----grs-stats-2, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------
grs_sex_stats <- 
  ri_greys %>% 
  group_by(Sex) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print()

grs_sex_stats %>% 
  write.csv("../output/ri_greys_sex.csv")

grs_ri_sex_bar <-
  grs_sex_stats %>% 
  ggplot(aes(x = Sex, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,1), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  scale_x_discrete(labels = c('Female', 'Male')) +
  theme(axis.text.y = element_blank(), # Can get rid of this as plotting next to the installation plot
        axis.title.y = element_blank(), # Ditto
        axis.ticks.y = element_blank(), # Ditto
        panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "blue"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  labs(y= "Ri (mean ± SE)", x = "Sex")

grs_ri_sex_bar

ggsave(grs_ri_sex_bar, filename = "../output/ri_greys_sex_bar.png", width = 80, height = 100, units = 'mm', dpi = 600)



## ----silvers-stats-2, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE-----------------------------------------------------------------------------------
silvers_sex_stats <- 
  ri_silvers %>% 
  group_by(Sex) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print()

silvers_ri_sex_bar <-
  silvers_sex_stats %>% 
  ggplot(aes(x = Sex, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,1), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  scale_x_discrete(labels = c('Female', 'Male')) +
  theme(axis.text.y = element_blank(), # Can get rid of this as plotting next to the installation plot
        axis.title.y = element_blank(), # Ditto
        axis.ticks.y = element_blank(), # Ditto
        panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  labs(y= "Ri (mean ± SE)", x = "Sex")

ggsave(silvers_ri_sex_bar, filename = "../output/ri_silvers_sex_bar.png", width = 80, height = 100, units = 'mm', dpi = 600)
silvers_ri_sex_bar



## ----grs-ri-plots, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE--------------------------------------------------------------------------------------
sex_riplot <- 
silvers_ri_sex_bar + grs_ri_sex_bar +
  plot_layout(widths = c(2,1))
ggsave(greys_riplot, filename = "../output/riplot_greys.png", width = 160, height = 100, units = 'mm', dpi = 600)

