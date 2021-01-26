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

# read in gcp sums 
gcp <- read_csv("results/final_product_eval/gcp_ch4_model_sum_2000_2017_indiv.csv") %>% 
  dplyr::select(-X1, - units) %>% # units refer to original product, not sums 
  mutate(Tg_month = flux,
         model = sub("\\_.*", "", model), # model name from file name
         timestep = rep(1:156, n()/156)) %>% 
  dplyr::select(model, timestep, Tg_month)

# check on GCP global sums 
gcp %>% 
  group_by(model) %>% 
  filter(timestep %in% c(1:12)) %>% 
  summarize(sum = sum(Tg_month))

# merge with upscaling data
all <- upscaling[["member_monthly_sum_95ci.csv"]] %>% 
  mutate(model = paste("upscaling", member, sep =  "_")) %>% 
  dplyr::select(model, timestep, Tg_month = mean) %>% 
  bind_rows(gcp)

all %>% 
  # mutate(plot_group = ifelse(member %in% c(1,2,3,4), "CRU-TS", "MERRA2")) %>% 
  ggplot(aes(timestep, Tg_month, color = factor(model))) + 
  # geom_ribbon(aes(ymin = mean + ci, ymax = mean - ci), fill = "light grey", color = 'light grey')  +
  geom_line() +
  # facet_wrap(~plot_group, ncol = 1) +
  my_theme
  

