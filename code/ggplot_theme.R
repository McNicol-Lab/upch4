my_theme <- theme_bw() +
  theme(panel.border = element_blank(), 
        axis.title=element_text(size=14), axis.text=element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        strip.text = element_text(face="bold", size=10),
        strip.background = element_rect(fill='white', colour='white',size=1),
        plot.margin=unit(c(1,2,1.5,1.2),"cm"))
