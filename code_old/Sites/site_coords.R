## FLUXNET CH4 Sites and metadata, plus rice sites from Zutao
## Gavin McNicol
## January 2021

library(tidyverse)

# get site data
fluxnet_ch4 <- read.csv("data/sites/fluxnet-ch4.csv") %>% 
  mutate(ID = paste(substr(ID,1,2),substr(ID,4,6),sep=""))
rice <- read.csv("data/sites/rice.csv") %>% 
  mutate(ID = paste(substr(Site.ID,1,2),substr(Site.ID,4,6),sep="")) %>% 
  rename( LAT = Latitude, LON = Longitude)

# get simple coordinate list
site.coords <- fluxnet_ch4 %>% 
  full_join(rice, by = "ID") %>% 
  mutate(LAT = ifelse(is.na(LAT.x), LAT.y, LAT.x),
         LON = ifelse(is.na(LON.x), LON.y, LON.x)) %>% 
  dplyr::select(ID, Site.ID, LAT, LON)

write.csv(site.coords, "data/sites/all_site_coords.csv", row.names = FALSE)

