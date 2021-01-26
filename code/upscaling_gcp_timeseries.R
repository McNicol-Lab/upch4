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
gcp <- read_csv("results/final_product_eval/gcp_monthly_sum.csv") %>% 
  mutate(type = "gcp")

# check on GCP global sums 
gcp %>% 
  group_by(model) %>% 
  filter(timestep %in% c(1:12)) %>% 
  summarize(sum = sum(tg_month))

# merge with upscaling data
all <- upscaling[["member_monthly_sum_95ci.csv"]] %>% 
  mutate(model = paste("m", member, sep =  "_"),
         type = "upscaling") %>% 
  dplyr::select(type, model, timestep, tg_month = mean, ci) %>% 
  bind_rows(gcp) %>% 
  mutate(ci = ifelse(is.na(ci), 0, ci), 
         Year = ceiling(timestep/12) + 2000) %>% 
  group_by(type, model, Year) %>% 
  mutate(month = 1:n(),
         dec_year = Year + month/12)

all %>% 
  ggplot(aes(dec_year, tg_month, color = factor(model), alpha = factor(model))) + 
  geom_line() +
  # geom_ribbon(aes(ymin = tg_month + ci, ymax = tg_month - ci, fill = factor(model)))  +
  # facet_wrap(~type, ncol = 1) +
  scale_y_continuous(limits = c(0, 30)) +
  scale_x_continuous(limits = c(2001, 2015)) +
  scale_color_manual(values = c(rep("grey", 9), rep("red", 4), rep("blue", 4), rep("grey", 4))) +
  scale_alpha_manual(values = c(rep(0.4, 9), rep(0.8, 8), rep(0.4, 4))) +
  # scale_fill_manual(values = c(rep(NA, 9), rep("red", 4), rep("blue", 4), rep(NA, 4))) + 
  labs(x = "Year", y = expression( atop( "Wetland Emissions", paste("(Tg "*CH[4]*" month"^{-1}*")") ) ) ) +
  my_theme
ggsave("results/final_product_eval/upscaling_gcp_2001_2015_timeseries.png",
       width = 24, height = 20, units = c("cm"), dpi = 300)



