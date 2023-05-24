
# Custom map theme
source('./scripts/plots/custom_theme.R')

# Make an outline of the countries for the plot
data(coastsCoarse)
map_outline_df <- fortify(coastsCoarse)

library(rworldmap)
data(coastsCoarse)
sPDF <- getMap()[getMap()$ADMIN!='Antarctica',]



#------------------------------------------------------------------------------
Fw_max_df_filt <- Fw_max_df %>% filter(Fw_max > 0.025)
bioclim_stack_df_wdist2 <- left_join(bioclim_stack_df_wdist, Fw_max_df_filt, by=c('x','y')) %>% 
  filter(!is.na(Fw_max))


# /----------------------------------------------------------------------------#
#/  Map minimum distances
closest_tower_map <- ggplot() +
  
  geom_polygon(data=sPDF, aes(long, lat, group=group), fill="grey90") +  
  geom_tile(data = bioclim_stack_df_wdist2, aes(x = x, y = y, fill = closest_tower)) +
  
  # geom_tile(data = subset(bioclim_stack_df_wdist2, min_dist > 0.4), aes(x = x, y = y), fill = "blue") +
  
  # geom_text_repel(data = towers_coords_df_acquired, 
  #                 aes(x = LON, y =  LAT, label = ID), 
  #                 point.padding = NA, arrow = arrow(angle = 45, length = unit(1.25, 'mm'), ends = "last", type = "open"), 
  #                 segment.size = 0.5, color = "black", size = 4) +
  
  geom_point(data = towers_coords_df_all, 
             aes(x = LON, y = LAT), size = 1.5, color = "blue") + 
  
  geom_path(data = map_outline_df, aes(long, lat, group = group), color = 'grey20', size = 0.3) +
  
  coord_equal() +
  
  # scale_fill_distiller(palette = "YlOrRd", trans = "reverse") +  # limits = c(0.8, 0), 
  scale_y_continuous(limits=c(-60, 85)) + 
  
  
  #labs(title = "Global Dissimilarity of the Tower Network") + 
  theme_map(8) + #theme_map(8) + #line_plot_theme
  theme(legend.position = 'right')


closest_tower_map


#----------------------------------------------------------
ggsave(plot = closest_tower_map, file = "closest_tower_map.png", 
       path = "./output/figures", 
       width = 188, height = 100, dpi = 600, units = "mm")



# 
# 
# # /------------------------------------------------------------
# #/ Map clustering of regions
# map_clusters <- ggplot() +
# 
#   geom_tile(data = bioclim_stack_df_k, aes(x = x, y = y, fill = as.factor(k))) +
#   geom_text(data = towers_coords_df_acquired, aes(x = LON, y =  LAT + 2, label = ID)) +
#   geom_point(data = towers_coords_df_acquired, aes(x = LON,  y = LAT),
#              shape = 21, size = 2, stroke = 1.1, color = "black")
