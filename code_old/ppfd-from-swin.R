## Approximate PPFD_IN from SW_IN
## Gavin McNicol
## August 2020

library(tidyverse)
library(broom)

rm(list=ls())

# ggplot theme
mytheme <- theme_bw() +
  theme(panel.border = element_blank(),
        axis.title=element_text(size=14), axis.text=element_text(size=14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position = "bottom",
        strip.text = element_text(face="bold", size=10),
        strip.background = element_rect(fill='white', colour='white',size=1))

# read training data
data <- read.csv("/Volumes/LACIE SHARE/Stanford CH4/June 2020 Upscaling/Model Datasets/Final/200629_FWET_rfe_train.csv") # local path

# set up linear fits
dfID = data %>% group_by(ID) %>%
  do(fitID = lm(PPFD_IN ~ SW_IN, data = .))

# get the coefficients by group in a tidy data_frame
dfIDCoef = tidy(dfID, fitID)

dfIDCoef %>% 
  filter(term == "SW_IN") %>% 
  ungroup() %>% 
  write.csv("/Volumes/LACIE SHARE/Stanford CH4/June 2020 Upscaling/Grids/Terraclimate/ppfd_vs_swin_lm.csv")

dfIDCoef %>% 
  filter(term == "SW_IN") %>% 
  ungroup() %>% 
  summarize(meanestimate = mean(estimate),
            sdestimate = sd(estimate))

data %>% 
  ggplot(aes(SW_IN, PPFD_IN, color = ID)) + 
  # geom_point() +
  geom_smooth() +
  mytheme
ggsave("/Volumes/LACIE SHARE/Stanford CH4/June 2020 Upscaling/Grids/Terraclimate/ppfd_vs_swin.pdf",
       width = 15, height = 15, units = c("cm"), dpi = 300)


data %>% 
  group_by(ID) %>% 
  mutate(factor = PPFD_IN/SW_IN) %>% 
  dplyr::select(factor) %>% 
  ungroup() %>% 
  summarize(factor = mean(factor, na.rm=TRUE)) 
  
