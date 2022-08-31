


leg_plot_bigmov <-
  ggplot() +
  geom_point(data = tag_locs_tigers, aes(x = Long, y = Lat, shape = Scientific_name, fill = sharkloc), size = 4) +
  geom_point(data = tag_locs_others, aes(x = Long, y = Lat, shape = Scientific_name, fill = sharkloc), size = 4) +
  scale_shape_manual(name = 'Shark ID', values = c(22, 24, 21)) +
  scale_fill_brewer(name = 'Shark ID', palette = "Dark2", direction = -1) +
  #theme(legend.position = 'none') +
  guides(fill = guide_legend(override.aes = list(pch = c(24,24,22,21,21,21)))) +
  guides(shape = "none") +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.border = element_rect(colour = "black", fill = "transparent"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", colour = "transparent"),
        legend.key = element_blank(),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(),
        legend.position = "left") +
  coord_quickmap(ylim = c(-23.8, -10), xlim = c(143, 153))


leg_plot_bigmov





legend_bigmov <-
  leg_plot_bigmov %>%
  get_legend() %>%
  as_ggplot() +
  #annotate(geom = element_rect(size = 1, fill = "transparent")) #+
  theme(plot.background = element_rect(fill = "transparent", colour = "transparent"))

legend_bigmov


#ggsave(legend_bigmov, filename = 'output/bigmover_legend.png', width = 62, height = 100, units = 'mm', dpi = 400)
