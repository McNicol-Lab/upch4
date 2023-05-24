## Prepare MODIS Data
## Gavin McNicol
## February 2021

# load packages
library(tidyverse)
library(lubridate)
library(oce)

# clear workspace
rm(list=ls())

# ggplot2 theme
source("code/ggplot_theme.R")

# read in extracted MODIS data (Credit Zutao Ouyang, Stanford)
loc <- "data/modis/"
files <- list.files(loc)

# set modis product names (for gather values)
modis.names <- c("LSTD", "EVI", "LAI", "LSWI", "NDSI", "NDVI", "NDWI", "LSTN", "SRWI")

# test reading one file to check dimensions
modis1 <- read.csv(paste(loc, files[1], sep = ""))
dim(modis1) 

# there are 867 8-day measurements (in the LSTD file), for 86 sites

# read all modis data, gather site observations, and simplify IDs
modis <- list()
for (i in 1:length(files)) {
  modis[[i]] <- read.csv(paste(loc, files[i], sep = "")) %>% 
    gather(key = ID, value = "modis.name", 2:87) %>% 
    mutate(ID = toupper( paste(substr(ID, 1, 2), substr(ID, 4, 6), sep = "")) ) %>% 
    as_tibble()
}

# assign names to list elements
names(modis) <- modis.names

# check details of each modis product
str(modis)

# 1) NDSI is daily
# 2) LAI time series is slightly shorter than other products

# Will not be able to bind columns with different lengths

# remove LAI and NDSI then rejoin 
modis.1.names <- c("LSTD", "EVI", "LSWI", "NDVI", "NDWI", "LSTN", "SRWI")
modis.1 <- modis[modis.1.names]
modis.1 <- modis.1 %>% 
  bind_cols() %>% 
  dplyr::select(1, 2, 3, 6, 9, 12, 15, 18, 21) %>%  # subset only data columns
  as_tibble() 
names(modis.1) <- c("Date", "ID", modis.1.names)

# join LAI using dates
modis.2 <- modis.1 %>% left_join(modis$LAI)
names(modis.2)[10] <- "LAI"

# join NDSI
modis <- modis.2 %>% right_join(modis$NDSI)
names(modis)[11] <- "NDSI"

# Convert Date into Year/DOY, correct MARC name, remove LSTD (including LSTN only)
modis <- modis %>% 
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>% 
  mutate(ID = ifelse(ID == "MARC", "MAERC",ID),
         DOY = yday(Date),
         Date = as_date(Date),
         Week = ceiling(DOY/7),
         Week = ifelse(Week == 53, 52, Week),
         Week = as.factor(Week),
         Year = as.integer(substr(Date, 1,4 )),
         Month = as.numeric(substr(Date, 6, 7))) %>% 
  dplyr::select(ID, Date, Year, Month, Week, DOY, NDSI, NDVI, EVI, LAI, NDWI, SRWI, LSWI, LSTN) 

# summarize NAs before despiking
modis_nas <- modis %>%
  group_by(ID) %>%
  summarize_all(list(~sum(is.na(.)))) 

# despike outlier values for each product 
modis_dsp <- modis %>% 
  mutate(NDSI_dsp = despike(NDSI, reference = 'trim', min = 0, max = 100, replace = "NA"),
         NDVI_dsp = despike(NDVI, reference = 'trim', min = -1, max = 1, replace = "NA"),
         EVI_dsp = despike(EVI, reference = 'trim', min = -1, max = 1, replace = "NA"),
         LAI_dsp = despike(LAI, reference = 'trim', min = 0, max = 5, replace = "NA"),
         NDWI_dsp = despike(NDWI, reference = 'trim', min = 0, max = 1, replace = "NA"),
         SRWI_dsp = despike(SRWI, reference = 'trim', min = -1, max = 3, replace = "NA"),
         LSWI_dsp = despike(LSWI, reference = 'trim', min = 0, max = 1, replace = "NA"),
         LSTN_dsp = despike(LSTN, reference = 'trim', min = -60, max = 50, replace = "NA")) %>% 
dplyr::select(ID, Date, Year, Month, Week, DOY, NDSI, NDVI, EVI, LAI, NDWI, SRWI, LSWI, LSTN,
              NDSI_dsp, NDVI_dsp, EVI_dsp, LAI_dsp, NDWI_dsp, SRWI_dsp, LSWI_dsp, LSTN_dsp) 

# summarize NAs after despiking
modis_dsp_nas <- modis_dsp %>%
  group_by(ID) %>%
  summarize_all(list(~sum(is.na(.)))) 

# calculate removed values
modis_dsp_nas %>% 
  mutate(NDSI = NDSI - NDSI_dsp,
         NDVI = NDVI - NDVI_dsp,
         EVI = EVI - EVI_dsp,
         LAI = LAI - LAI_dsp,
         NDWI = NDWI - NDWI_dsp,
         SRWI = SRWI - SRWI_dsp,
         LSWI = LSWI - LSWI_dsp,
         LSTN = LSTN - LSTN_dsp) %>% 
  dplyr::select(-ID) %>% 
  summarize_all(list(~sum(.)))

# rename without `_dsp` suffix
modis_dsp <- modis_dsp %>% 
  dplyr::select(ID, Date, Year, Month, Week, DOY, 
                NDSI = NDSI_dsp, 
                NDVI = NDVI_dsp, 
                EVI = EVI_dsp,
                LAI = LAI_dsp,
                NDWI = NDWI_dsp,
                SRWI = SRWI_dsp,
                LSWI = LSWI_dsp,
                LSTN = LSTN_dsp)
 
# calculate mean  seasonal cycle (msc), then weekly means, then fill weekly gaps with msc
modis_gapfilled <- modis_dsp %>% 
  group_by(ID, Month) %>% 
  mutate(NDSI_msc = mean(NDSI, na.rm = TRUE),
         NDVI_msc = mean(NDVI, na.rm = TRUE),
         EVI_msc = mean(EVI, na.rm = TRUE), 
         LAI_msc = mean(LAI, na.rm = TRUE),
         NDWI_msc = mean(NDWI, na.rm = TRUE),
         SRWI_msc = mean(SRWI, na.rm = TRUE),
         LSWI_msc = mean(LSWI, na.rm = TRUE),
         LSTN_msc = mean(LSTN, na.rm =TRUE)) %>% 
  group_by(ID, Year, Month, Week) %>% 
  summarize(NDSI = mean(NDSI, na.rm = TRUE),
            NDVI = mean(NDVI, na.rm = TRUE),
            EVI = mean(EVI, na.rm = TRUE),
            LAI = mean(LAI, na.rm = TRUE),
            NDWI = mean(NDWI, na.rm = TRUE),
            SRWI = mean(SRWI, na.rm = TRUE),
            LSWI = mean(LSWI, na.rm = TRUE),
            LSTN = mean(LSTN, na.rm = TRUE),
            NDSI_msc = NDSI_msc[1],
            NDVI_msc = NDVI_msc[1],
            EVI_msc = EVI_msc[1],
            LAI_msc = LAI_msc[1],
            NDWI_msc = NDWI_msc[1],
            SRWI_msc = SRWI_msc[1],
            LSWI_msc = LSWI_msc[1],
            LSTN_msc = LSTN_msc[1]) %>% 
  mutate(NDSI_F = ifelse(is.na(NDSI), NDSI_msc, NDSI),
         NDVI_F = ifelse(is.na(NDVI), NDVI_msc, NDVI),
         EVI_F = ifelse(is.na(EVI), EVI_msc, EVI),
         LAI_F = ifelse(is.na(LAI), LAI_msc, LAI),
         NDWI_F = ifelse(is.na(NDWI), NDWI_msc, NDWI),
         SRWI_F = ifelse(is.na(SRWI), SRWI_msc, SRWI),
         LSWI_F = ifelse(is.na(LSWI), LSWI_msc, LSWI),
         LSTN_F = ifelse(is.na(LSTN), LSTN_msc, LSTN)) 

# if snow is on the ground, set water indices to zero (frozen)
modis_frozen <- modis_gapfilled %>% 
  group_by(ID,Year) %>% 
  mutate(NDWI_msc = ifelse(NDSI_msc > 0, quantile(NDWI_msc, 0.05, na.rm=TRUE), NDWI_msc),
         SRWI_msc = ifelse(NDSI_msc > 0, quantile(SRWI_msc, 0.05, na.rm=TRUE), SRWI_msc),
         LSWI_msc = ifelse(NDSI_msc > 0, quantile(LSWI_msc, 0.05, na.rm=TRUE), LSWI_msc),
         
         NDWI_F = ifelse(NDSI_F > 0, quantile(NDWI_F, 0.05, na.rm=TRUE), NDWI_F),
         SRWI_F = ifelse(NDSI_F > 0, quantile(SRWI_F, 0.05, na.rm=TRUE), SRWI_F),
         LSWI_F = ifelse(NDSI_F > 0, quantile(LSWI_F, 0.05, na.rm=TRUE), LSWI_F))

# summarize NAs after snow corrections
modis_frozen %>%
  ungroup() %>% 
  summarize_all(list(~sum(is.na(.))/n())) %>% view()

# get mean, min, max, amplitude
modis_frozen <- modis_frozen %>% 
  group_by(ID, Year) %>% 
  mutate(NDSI_mean = mean(NDSI_F,na.rm=TRUE),
         NDVI_mean = mean(NDVI_F,na.rm=TRUE),
         EVI_mean = mean(EVI_F,na.rm=TRUE),
         LAI_mean = mean(LAI_F,na.rm=TRUE),
         NDWI_mean = mean(NDWI_F,na.rm=TRUE),
         SRWI_mean = mean(SRWI_F,na.rm=TRUE),
         LSWI_mean = mean(LSWI_F,na.rm=TRUE),
         LSTN_mean = mean(LSTN_F,na.rm=TRUE),
         
         NDSI_min = min(NDSI_F,na.rm=TRUE),
         NDVI_min = min(NDVI_F,na.rm=TRUE),
         EVI_min = min(EVI_F,na.rm=TRUE),
         LAI_min = min(LAI_F,na.rm=TRUE),
         NDWI_min = min(NDWI_F,na.rm=TRUE),
         SRWI_min = min(SRWI_F,na.rm=TRUE),
         LSWI_min = min(LSWI_F,na.rm=TRUE),
         LSTN_min = min(LSTN_F,na.rm=TRUE),
         
         NDSI_max = max(NDSI_F,na.rm=TRUE),
         NDVI_max = max(NDVI_F,na.rm=TRUE),
         EVI_max = max(EVI_F,na.rm=TRUE),
         LAI_max = max(LAI_F,na.rm=TRUE),
         NDWI_max = max(NDWI_F,na.rm=TRUE),
         SRWI_max = max(SRWI_F,na.rm=TRUE),
         LSWI_max = max(LSWI_F,na.rm=TRUE),
         LSTN_max = max(LSTN_F,na.rm=TRUE),
         
         NDSI_amp = NDSI_max-NDSI_min,
         NDVI_amp = NDVI_max-NDVI_min,
         EVI_amp = EVI_max-EVI_min,
         LAI_amp = LAI_max-LAI_min,
         NDWI_amp = NDWI_max-NDWI_min,
         SRWI_amp = SRWI_max-SRWI_min,
         LSWI_amp = LSWI_max-LSWI_min,
         LSTN_amp = LSTN_max-LSTN_min)

# get IDs
site.names <- modis_frozen %>%
  ungroup() %>% 
  mutate(ID = factor(ID)) %>% 
  dplyr::select(ID) %>% 
  pull() %>% unique()

# get names
names <- names(modis)[7:14]
names_F <- paste(names, "_F", sep = "")
names_msc <- paste(names, "_msc", sep = "")

for (i in 1:length(names)){
  
  # visualize 1-45
  modis_frozen %>%
    filter(ID %in% site.names[1:45]) %>%
    dplyr::select(Month, value_F = names_F[i], value_msc = names_msc[i]) %>% 
    ggplot(aes(Month, value_F)) +
    geom_point(size = 1, alpha = 0.3) +
    geom_line(aes(Month, value_msc), col = 'purple', size = 2) +
    facet_wrap(~ID, scales = 'free', ncol = 9) +
    my_theme
  ggsave(paste("data/qcqa/modis/", names[i], "_1.pdf", sep = ""),
         width = 50, height = 30, units = c("cm"), dpi = 300)
  
  # visualize 46-86
  modis_frozen %>%
    filter(ID %in% site.names[46:86]) %>%
    dplyr::select(Month, value_F = names_F[i], value_msc = names_msc[i]) %>% 
    ggplot(aes(Month, value_F)) +
    geom_point(size = 1, alpha = 0.3) +
    geom_line(aes(Month, value_msc), col = 'purple', size = 2) +
    facet_wrap(~ID, scales = 'free', ncol = 9) +
    my_theme
  ggsave(paste("data/qcqa/modis/", names[i], "_2.pdf", sep = ""),
         width = 50, height = 30, units = c("cm"), dpi = 300)
  
}

# check data for _mean through _amp
modis_frozen %>% 
  ggplot(aes(Year, LSTN_amp)) +
  geom_point() +
  facet_wrap(~ID) +
  mytheme

write.csv(modis_frozen, "data/modis/modis_processed.csv",
          row.names = FALSE)

