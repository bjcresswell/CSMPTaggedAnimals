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
library(patchwork)
library(collapse)
library(magrittr)
library(tidyverse)


## ----housekeeping, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
# Housekeeping
#rm(list=ls())
#getwd()


## ----load-data, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------------------------------------------------------------------------------------------------------------------------------------------------
source("ri_installation.R")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
installation_ri %$% 
  summary(installation_name)

installation_ri <- installation_ri %>% 
  mutate(installation_name = fct_relevel(installation_name, c('Osprey', 'Bougainville', 'Holmes', 'Flinders'))) 


## ----greys-ri, echo=FALSE, message=FALSE, warning=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------
ri_greys <- 
  installation_ri %>% 
  filter(Scientific_name == 'Carcharhinus amblyrhynchos')

ri_greys_table <- 
  ri_greys %>% 
  head() %>% 
  kbl() %>%
  kable_minimal()


## ----silvers-ri, echo=FALSE, message=FALSE, warning=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------
ri_silvers <- 
  installation_ri %>% 
  filter(Scientific_name == 'Carcharhinus albimarginatus')

ri_silvers_table <-  
  ri_silvers %>% 
  head() %>% 
  kbl() %>%
  kable_minimal()
# Note: this kable code makes for a decent table when knitted but horrible to try to produce stand-alone table


## ----grs-stats-1, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
grs_installation_stats <- 
  ri_greys %>% 
  group_by(installation_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print() 

grs_installation_stats %>% 
  write.csv("../output/ri_greys.csv")

ri_greys_bar <- 
  grs_installation_stats %>% 
  ggplot(aes(x = installation_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,1), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  theme(panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  labs(y= "Ri (mean ± SE)", x = "Installation (reef)")

ri_greys_bar
  
ggsave(ri_greys_bar, filename = "../output/ri_greys_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----grs-stats-2, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE----------------------------------------------------------------------------------------------------------------------------------------------
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
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  labs(y= "Ri (mean ± SE)", x = "Sex")

grs_ri_sex_bar

ggsave(grs_ri_sex_bar, filename = "../output/ri_greys_sex_bar.png", width = 80, height = 100, units = 'mm', dpi = 600)



## ----grs-ri-plots, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
greys_riplot <- 
ri_greys_bar + grs_ri_sex_bar +
  plot_layout(widths = c(2,1))
ggsave(greys_riplot, filename = "../output/riplot_greys.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----silvers-stats-1, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------------------------------------------------------------------------------------------------------------------------------------------
silvers_installation_stats <- 
  ri_silvers %>% 
  group_by(installation_name) %>% 
  summarise(mean = mean(ri_installation), sd = sd(ri_installation), se = std.error(ri_installation)) %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print() 

silvers_installation_stats %>% 
  write.csv("../output/ri_silvers.csv")

ri_silvers_bar <- 
  silvers_installation_stats %>% 
  ggplot(aes(x = installation_name, y = mean)) +
  geom_bar(stat="identity", position="dodge", color="black", width = 0.95) +
  geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) +
  scale_y_continuous(expand = c(0,0), limits = c(-0.02,1), breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  theme(panel.grid = element_line(colour = "#ECEFF1"),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "bottom")  +
  labs(y= "Ri (mean ± SE)", x = "Installation (reef)")
ggsave(ri_silvers_bar, filename = "../output/ri_silvers_bar.png", width = 160, height = 100, units = 'mm', dpi = 600)


## ----silvers-stats-2, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------------------------------------------------------------------------------------------------------------------------------------------
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



## ----silver-ri-plots, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE------------------------------------------------------------------------------------------------------------------------------------------
silvers_riplot <- 
ri_silvers_bar + silvers_ri_sex_bar +
  plot_layout(widths = c(2,1))

ggsave(silvers_riplot, filename = "../output/riplot_silvers.png", width = 160, height = 100, units = 'mm', dpi = 600)

