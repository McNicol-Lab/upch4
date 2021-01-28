# Visualize GCP-upscaling model differences as spatially-explicit metrics 
# Gavin McNicol

rm(list=ls())

library(tidyverse)
library(raster)
library(tabularaster)

source("code/ggplot_theme.R")

# check the spread of GCP model estimates
gcp_sums <- read.csv("results/final_product_eval/gcp_monthly_sum.csv")

gcp_sums %>% 
  group_by(model) %>% 
  summarize(mean = mean(tg_month)) %>% 
  arrange(mean)

# TRIPLEX-GHG is the lowest, LPJ-WSL is intermediate, JULES is highest

# look at gcp models
files <- list.files("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/")

# get the three models and subset 2001-2015 from 2000-2017
triplexghg <- brick(paste("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/", files[12], sep = "")) %>% 
  subset(13:192)
lpjwsl <- brick(paste("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/", files[8], sep = "")) %>% 
  subset(13:192)
jules <- brick(paste("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/", files[5], sep = "")) %>% 
  subset(13:192)

# convert them to dataframes
triplexghg_df <- triplexghg %>% 
  as_tibble(xy = TRUE)

lpjwsl_df <- lpjwsl %>% 
  as_tibble(xy = TRUE)

jules_df <- jules %>% 
  as_tibble(xy = TRUE)

# read in upscaling members
files <- list.files("/Volumes/LACIE SHARE/Stanford CH4/June 2020 Upscaling/Predictions/Gridded/mgCH4_weighted/")

# get all eight upscaling members
m <- list()
for (i in 1:8) {
  m[[i]] <- brick(paste("/Volumes/LACIE SHARE/Stanford CH4/June 2020 Upscaling/Predictions/Gridded/mgCH4_weighted/", files[i], sep = ""))
}

# make sure the extents match
triplexghg_crop <- crop(triplexghg[[1]], m[[1]])

# resample each raster brick to match the gcp models (0.5-degree)
for(i in 1:8) {
  m[[i]] <- resample(m[[i]], triplexghg_crop, method = "bilinear")
}

# convert each brick to a dataframe
m_df <- list()
for (i in 1:8) {
  m_df[[i]] <- m[[i]] %>% as_tibble(xy = TRUE)
} 

# check structure
str(m_df)
max(m_df[[1]]$dimindex) # 180 months in `dimindex` variable

# add a model column
for (i in 1:8) {
  m_df[[i]] <-  m_df[[i]] %>% 
    mutate(model = paste("model", i, sep = "_"))
} 



