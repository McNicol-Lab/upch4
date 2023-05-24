# Global CH4 Sums and Uncertaintines from 500 Bootstrap Models
# Gavin McNicol

rm(list=ls())

library(tidyverse)

# ggplot theme
source("code/ggplot_theme.R")

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
  geom_histogram(aes(fill = factor(Year))) +
  facet_wrap(~member, ncol = 1) +
  scale_fill_manual(values = c(rep("dark grey", 7), rep("light grey", 8))) +
  mytheme
ggsave("results/final_product_eval/bootstrap_annual_sum_histogram.png",
       width = 12, height = 22, units = c("cm"), dpi = 300)

# annual overall member-average global sum and confidence intervals
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  mutate(Year = 2000+ceiling(timestep/12)) %>% 
  group_by(member, Year, bootstrap) %>% 
  summarize(TgCH4year = sum(TgCH4month),
            length = n()) %>% 
  group_by(Year) %>% 
  summarize(mean = mean(TgCH4year),
            sd = sd(TgCH4year),
            ci = sd*1.96) 
  write.csv("results/final_product_eval/annual_sum_95ci.csv",
            row.names = F)
  
# overall member-average and uncertainty
all_data %>% 
    dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
    mutate(Year = 2000+ceiling(timestep/12)) %>% 
    group_by(member, Year, bootstrap) %>% 
    summarize(TgCH4year = sum(TgCH4month),
              length = n()) %>% 
    group_by(member) %>% 
    summarize(mean = mean(TgCH4year),
              sd = sd(TgCH4year),
              ci = sd*1.96) %>% 
  write.csv("results/final_product_eval/member_annual_sum_95ci.csv",
            row.names = F)

# overall average and uncertainty
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  mutate(Year = 2000+ceiling(timestep/12)) %>% 
  group_by(member, Year, bootstrap) %>% 
  summarize(TgCH4year = sum(TgCH4month),
            length = n()) %>% 
  ungroup() %>% 
  summarize(mean = mean(TgCH4year),
            sd = sd(TgCH4year),
            ci = sd*1.96) %>% 
  write.csv("results/final_product_eval/overall_annual_sum_95ci.csv",
            row.names = F)

# monthly overall member-average global sum and confidence intervals
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  group_by(timestep) %>% 
  summarize(mean = mean(TgCH4month),
            sd = sd(TgCH4month),
            ci = sd*1.96) 
  write.csv("results/final_product_eval/monthly_sum_95ci.csv",
            row.names = F)

# monthly overall member-average global sum and confidence intervals
all_data %>% 
  dplyr::select(bootstrap = b, member = m, timestep = t, TgCH4month = sum_TgCH4month) %>% 
  group_by(member, timestep) %>% 
  summarize(mean = mean(TgCH4month),
            sd = sd(TgCH4month),
            ci = sd*1.96) 
  write.csv("results/final_product_eval/member_monthly_sum_95ci.csv",
            row.names = F)


  

