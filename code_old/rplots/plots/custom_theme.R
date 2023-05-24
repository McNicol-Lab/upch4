base_size = 8

#======================================================================================================================================

# Custom map ggplot theme to make it super pretty to my own desire
theme_map <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 12, hjust = 0.5),
          # plot.background = element_blank(), #element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
           
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill='white'),
          panel.background = element_rect(fill='white'),
          panel.border = element_blank(),
          # Axis
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position= c(0.15, 0.25),
          legend.box = "vertical",
          legend.text = element_text(size = 8),
          # legend.box.spacing = unit(22, 'mm'),
          legend.spacing = unit(2, "mm"),
          legend.key.size = unit(2, "mm"),
          # legend.box.background = element_rect(color = "black", size = 1.25),
          legend.background = element_blank())
}

#======================================================================================================================================

# Custom histogram ggplot theme
theme_hist <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5', size = 2),
          
          # Axis
          axis.title = element_text(size = 40, face = 'bold'),
          axis.text = element_text(size = 36),
          
          # Legend
          legend.position = 'none')
}

#======================================================================================================================================

# Custom MDS ggplot theme
theme_mds <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 8, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5', size = 2),
          
          # Axis
          axis.text.x = element_blank(),  
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(),  
          axis.title.x = element_text(size=8, face = 'bold'), 
          axis.title.y = element_text(size=8, face = 'bold'),
          
          # Legend
          legend.title = element_blank(),
          legend.key = element_blank(),
          legend.position= c(0.09, 0.09),
          legend.box = "vertical",
          legend.text = element_text(size = 36),
          legend.spacing = unit(2, "mm"),
          legend.key.size = unit(2, "mm"),
          legend.box.background = element_rect(color = "black", size = 1.25),
          legend.background = element_blank())

}

#===================================================================================================

# Custom single climatic ggplot theme
theme_single <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5', size = 2),
          
          # Axis
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          
          # Legend
          legend.title = element_text(size = 36, face = 'bold', hjust = 0.5),
          legend.key = element_blank(),
          legend.position= c(0.1, 0.2),
          legend.box = "vertical",
          legend.text = element_text(size = 36),
          legend.spacing = unit(20, "mm"),
          legend.key.size = unit(20, "mm"),
          legend.box.margin = margin(5, 5, 5, 5, 'mm'),
          legend.box.background = element_rect(color = "black", size = 1.25),
          legend.background = element_blank())
}

#==================================================================================================

# Custom KS-Test ggplot theme
theme_ks <- function(base_size) {
  theme_bw(base_size = base_size) +
    theme(plot.title = element_text(face = 'bold', size = 56, hjust = 0.5),
          plot.background = element_rect(fill = 'white'),
          plot.margin = margin(5, 5, 5, 5, "mm"),
          
          # Panel
          panel.grid.minor = element_line(color = '#D0D0D0'),
          panel.grid.major = element_line(color = '#D0D0D0'),
          panel.background = element_rect(fill = '#F5F5F5', size = 2))
}