



# Filter out towers in the same pixel (climatically identical)
towers_coords_df_all <- c %>%
  distinct(wc2.0_bio_5m_01, tmp_avgr, .keep_all = TRUE)

# # Get just the data of the climatic variables 
# towers_coords_df_all_mds <- normalize(towers_coords_df_all[, 17: ncol(towers_coords_df_all)], method = "range", range = c(0, 1), margin = 2L, on.constant = "quiet")

# Run nMDS
nmdsinput = bioclim_stack_df_wdist2[,1:11]
# nmdsinput = towers_coords_df_all[, 17: ncol(towers_coords_df_all)]

sol <- vegan::metaMDS(comm = nmdsinput, 
                      distance = "euclidean", 
                      engine = "isoMDS", 
                      wascores = TRUE, 
                      autotransform = TRUE)

# Add "no" to the Data_acquired column where there is a missing place
# towers_coords_df_all$Data_acquired[towers_coords_df_all$Data_acquired == ""]    <- "No"
# towers_coords_df_all$Data_acquired[towers_coords_df_all$Data_acquired == "yes"] <- "Yes"

# Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores <- as.data.frame(vegan::scores(sol))

# Create a column of site names, from the rownames of data.scores
data.scores$site <- rownames(data.scores) 

# Add the grp variable created earlier
# data.scores$grp <- towers_coords_df_all$Data_acquired 
data.scores <- bind_cols(data.scores, towers_coords_df_all[, c('ID','IGBP','COUNTRY')])


# Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores <- as.data.frame(vegan::scores(sol, "species"))

# Create a column of species, from the rownames of species.scores
species.scores$species <- rownames(species.scores)  




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~``

# NMDS.log<-log(dune+1)
# sol <- metaMDS(NMDS.log)

NMDS = data.frame(MDS1 = sol$points[,1], MDS2 = sol$points[,2])

# Add species loadings and save them as data frame. Directions of arrows cosines are stored in list vectors and matrix arrows. 
# To get coordinates of the arrows those direction values should be multiplied by square root of r2 values that are stored in vectors$r. More straight forward way is to use function scores() then add new column containing species names.

vec.sp <- envfit(sol$points, nmdsinput, perm=1000)
vec.sp.df <- as.data.frame(vec.sp$vectors$arrows * sqrt(vec.sp$vectors$r))
vec.sp.df$species <- rownames(vec.sp.df)

names(vec.sp.df) <- c('MDS1','MDS2','species')






# /----------------------------------------------------------------------------#
#/  Plot MDS of all towers (acquired and not acquired)                 ---------



mds_plot = ggplot(data = NMDS, aes(MDS1, MDS2)) + 
  
  
  # geom_point(aes(data = MyMeta, color = MyMeta$amt))+
  geom_segment(data=vec.sp.df, aes(x=0, xend=MDS1, y=0, yend=MDS2),
               arrow = arrow(length = unit(0.5, "cm")), colour="grey80") + 
  
  geom_text(data=vec.sp.df, aes(x=MDS1, y=MDS2, label=species), colour="grey80", size=3) +
  
  
  # Add the point markers (tower sites)
  geom_point(data = data.scores,  aes(x = NMDS1, y = NMDS2, color=IGBP), alpha=0.6, size = 2.5) + 
  geom_text(data = data.scores, aes(x = NMDS1, y = NMDS2, label=ID), color='black', size = 2.5) + 
  
  
  
  labs(title = "Relative dissimiarity of FLUXNET-CH4 towers") + 
  xlim(-1, 1) + ylim(-1, 1) +
  # scale_colour_manual(values = c("No" = "red", "Yes" = "blue")) +
  coord_equal() +
  line_plot_theme +
  theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))



# Save settings for the MDS plot 
ggsave(plot = mds_plot, file = "mds_plot_dbv2.png", path = "./output/figures", 
       width = 180, height = 140, dpi = 300, units = "mm")





#=====================================================================================================

# # Calculate number of columns
# num_col = ncol(towers_coords_df_wdist_all)
# 
# # Convert into long format, filter, then wide format
# towers_coords_df2_wdist_all <- towers_coords_df_wdist_all %>%
#   gather(key = "tower_to", value = "dist", (num_bio + 14):num_col) %>%
#   filter(SITE_ID != tower_to) %>%
#   spread(key = "tower_to", value = "dist", fill = NA)
# 
# # Find the minimum distance between the towers
# towers_coords_df2_wdist_all$min_dist <- apply(towers_coords_df2_wdist_all[, (num_bio + 14):num_col], MARGIN = 1, FUN = min, na.rm = TRUE)

#=====================================================================================================

# # Convert data frame into a matrix
# towers_coords_matrix_wdist_all <- as.matrix(towers_coords_df_wdist_all[1:61, (num_bio + 14):num_col])
# 
# # Calculates mds
# towers_all = isoMDS(towers_coords_matrix_wdist_all)
# plot(towers_all$points, type = "n", pch = 20, cex = 3, col = adjustcolor("black", alpha = 0.3), xlab = "X", ylab = "Y") 
# text(towers_all$points, labels = towers_coords_df_all$SITE_ID, cex = 0.75)

#=====================================================================================================