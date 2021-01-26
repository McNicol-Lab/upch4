# Upscaling and GCP timeseries plots
# Gavin McNicol

rm(list=ls())

source("code/ggplot_theme.R")
library(tidyverse)

# read upscaling sums and uncertaintines
files <- list.files("results/final_product_eval/", ".csv")
upscaling <- lapply(paste("results/final_product_eval/",files, sep = ""), read_csv)
names(upscaling) <- c(files)

upscaling[["member_monthly_sum_95ci.csv"]] %>% 
  mutate(plot_group = ifelse(member %in% c(1,2,3,4), "CRU-TS", "MERRA2")) %>% 
  ggplot(aes(timestep, mean, color = factor(member))) + 
  geom_ribbon(aes(ymin = mean + ci, ymax = mean - ci), fill = "light grey", color = 'light grey')  +
  geom_line() +
  facet_wrap(~plot_group, ncol = 1) +
  my_theme


upscaling[["monthly_sum_95ci.csv"]] %>% 
  ggplot(aes(timestep, mean)) + 
  geom_ribbon(aes(ymin = mean + ci, ymax = mean - ci), fill = "light grey")  +
  geom_line() +
  # facet_wrap(~member, ncol = 1) +
  my_theme

