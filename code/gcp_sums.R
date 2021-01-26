# Compute GCP ensemble global sums
# Gavin McNicol

rm(list=ls())

library(tidyverse)
library(raster)
library(ncdf4)

files <- list.files("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/")
gcp_models <- stack(paste("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/", files, sep = ""))

# get the units for each model
units <- list()
for (i in 1:length(files)) {
  units[[i]] <- nc_open(paste("/Volumes/LACIE SHARE/Stanford CH4/CH4 Models/gcp-ch4_ncdfs/prescribed/", files[i], sep = ""))
}

# create vector for 2001-2015 out of 2000-2017
full <- rep(1:216, 13)
true <- rep(c(rep(0, 12), 13:(12+180), rep(0, 24)), 13)
length(true) == length(full)

index <- cbind(full, true) %>% 
  as_tibble() %>%
  mutate(run = 1:n()) %>% 
  filter(true != 0) %>% 
  dplyr::select(run) %>% 
  pull()

gcp_models <- subset(gcp_models, index)

tg_month <- list()
for (i in 1:nlayers(gcp_models)){
  tg_month[[i]] <-  cellStats( gcp_models[[i]] * area(gcp_models[[1]]) * 10^6, sum ) * 10^-12
}

gcp_tg_month <- unlist(tg_month) 
timestep <- rep( c(1:180), 13)
model <- rep(files, each = 180)

gcp_tg <- cbind(timestep, gcp_tg_month) %>% 
  cbind(model) %>% 
  as_tibble() %>% 
  mutate(model =  sub("\\_.*", "", model),
         gcp_tg_month = as.numeric(gcp_tg_month)) %>% 
  dplyr::select(model, timestep, tg_month = gcp_tg_month)

# check looks reasonble
gcp_tg %>% 
  group_by(model) %>% 
  filter(timestep %in% c(1:12)) %>% 
  summarize(sum = sum(tg_month))

write.csv(gcp_tg, "results/final_product_eval/gcp_monthly_sum.csv", 
          row.names = F)



