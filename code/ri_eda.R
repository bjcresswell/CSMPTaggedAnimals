## Basic plotting of Ri stats ##

setwd('code')
rm(list=ls())

load(file = "../data/RData/station_ri.RData")
load(file = "../data/RData/installation_ri.RData")

ri_ambly <- 
installation_ri %>% 
  filter(Scientific_name == 'Carcharhinus amblyrhynchos')

# Boxplots

ri_ambly %>% 
  ggplot(aes(x = Sex, y = ri_installation)) +
  geom_boxplot()


ri_ambly %>% 
  ggplot(aes(x = installation_name, y = ri_installation)) +
  geom_boxplot()


installation_ri %>% 
  ggplot(aes(x = Scientific_name, y = ri_installation)) +
  geom_boxplot()
