# Installation colours

# So let's go with: green, yellow, red, blue
installation_palette <- c("steelblue", "springgreen",  "yellow", "coral")

### If a stand alone legend is required for this colour scheme, here it is...

### Make a dummy plot in ggplot
leg_plot <- 
  ggplot() +
  geom_point(data = installation_sum, aes(x = point_long, y = point_lat, fill = installation_name), stroke = 0.5, pch = 21, size = 10) +
  #scale_fill_brewer(palette = "Dark2") +
  #scale_fill_viridis_d(direction = -1) +
  #scale_fill_manual(values=wes_palette(name="Moonrise3", n = 4, type = "discrete")) +
  scale_shape_manual(name = '') +
  scale_fill_manual(name = 'NULL', values =  installation_palette) +
  theme(legend.key = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank()) +
  coord_quickmap(ylim = c(-18, -13.5), xlim = c(143.5, 149)) 

### Plot legend separately using ggpubr
library(ggpubr)
legend <- 
  leg_plot %>% 
  get_legend() %>% 
  as_ggplot() +
  #annotate(geom = element_rect(size = 1, fill = "transparent")) #+
  theme(plot.background = element_rect(fill = "transparent", colour = "transparent"))

### Save
ggsave(legend, filename = '../output/installation_legend.png', width = 40, height = 64, units = 'mm', dpi = 300)



