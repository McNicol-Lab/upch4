# Global CH4 Sums and Uncertaintines from 500 Bootstrap Models
# Gavin McNicol

rm(list=ls())

library(tidyverse)

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

loc <- "/Volumes/LACIE SHARE/Stanford CH4/June 2020 Upscaling/Predictions/Gridded/individual_bootstrap/"
setwd(loc)
files <- list.files()
data <- lapply(files, read_csv)

# reset wd
setwd("/Volumes/LACIE SHARE/Stanford CH4/upch4/")

length(data)

all_data <- bind_rows(data) %>% 
  dplyr::select(-X1)

# histogram of annual global sums by-member
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  mutate(Year = 2000+ceiling(timestep/12)) %>% 
  group_by(member, Year, bootstrap) %>% 
  summarize(TgCH4year = sum(TgCH4month),
            length = n()) %>% 
  ggplot(aes(TgCH4year)) +
  geom_histogram() +
  facet_wrap(~member, ncol = 1) +
  mytheme

# annual overall member-average global sum and confidence intervals
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  mutate(Year = 2000+ceiling(timestep/12)) %>% 
  group_by(member, Year, bootstrap) %>% 
  summarize(TgCH4year = sum(TgCH4month),
            length = n()) %>% 
  group_by(Year) %>% 
  summarize(mean = mean(TgCH4year),
            sd = sd(TgCH4year)*1.96) %>% 
  write.csv("results/final_product_eval/annual_sum_95ci.csv",
            row.names = F)

# monthly overall member-average global sum and confidence intervals
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  group_by(timestep) %>% 
  summarize(mean = mean(TgCH4month),
            sd = sd(TgCH4month)*1.96) %>% 
  write.csv("results/final_product_eval/monthly_sum_95ci.csv",
            row.names = F)

# monthly overall member-average global sum and confidence intervals
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  group_by(member, timestep) %>% 
  summarize(mean = mean(TgCH4month),
            sd = sd(TgCH4month)*1.96) %>% 
  write.csv("results/final_product_eval/member_monthly_sum_95ci.csv",
            row.names = F)

  

data[[1]] %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  mutate(Year = 2000+ceiling(timestep/12)) %>% 
  group_by(bootstrap, Year) %>% 
  mutate(cum_TgCH4year = cumsum(TgCH4month)) %>% 
  summarize(annual_cum_TgCH4year = max(cum_TgCH4year)) %>% 
  group_by(Year) %>% 
  summarize(sd = sd(annual_cum_TgCH4year))
  summarize()
  summarize(TgCH4year = sum(TgCH4month),
            length = n()) %>% 
  group_by(Year) %>% 
  summarize(TgCH4year_mean = mean(TgCH4year),
            TgCH4year_ci = sd(TgCH4year)*1.96)
  
  
data[[1]] %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  group_by(timestep) %>% 
  summarize(TgCH4month = mean(TgCH4month)) %>% 
  mutate(Year = 2000+ceiling(timestep/12)) %>% 
  group_by(Year) %>% 
  summarize(sum = sum(TgCH4month))
