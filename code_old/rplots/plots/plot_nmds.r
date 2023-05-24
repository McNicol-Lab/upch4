# https://stackoverflow.com/questions/14711470/plotting-envfit-vectors-vegan-package-in-ggplot2

# /----------------------------------------------------------------------------#
#/  Plot MDS of all towers (acquired and not acquired)                 ---------

mds_plot <- ggplot() + 
  
  # Add the species labels (climatic variables)
  geom_text(data = species.scores, 
            aes(x = NMDS1, y = NMDS2, label = species), 
            alpha = 0.5, color = "black", size = 3) +
  
  # Add the point markers (tower sites)
  geom_point(data = data.scores, 
             aes(x = NMDS1, y = NMDS2), size = 2.5) + 
  
  # /--------------------------------------------------------------------------#
  #/ Add the site labels (tower ID's)
  # geom_text_repel(data = data.scores, 
  #                 aes(x = NMDS1, y = NMDS2, label = towers_coords_df_all$SITE_ID, color = as.factor(grp)),
  #                 size = 2, vjust = 0, hjust = 0, show.legend = FALSE) + 
  
  labs(title = "Relative Dissimiarity of All Towers") + 
  scale_colour_manual(values = c("No" = "red", "Yes" = "blue")) +
  coord_equal() +
  line_plot_theme +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))


mds_plot


# Save settings for the MDS plot 
ggsave(plot = mds_plot, file = "mds_plot_sm.png", path = "./outputs", 
       width = 180, height = 100, dpi = 600, units = "mm")
