## setwd('H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces')


### Packages
library(dplyr)
library(RSQLite)
library(tictoc)
library(readr)
library(lubridate)


### connect to eBird db
ebird_db <- src_sqlite("D:/All eBird Data/ebird.sqlite", create=FALSE)
all_ebird <- tbl(ebird_db, "ebird")

### read in study site csv
study_sites <- read_csv("Data/Final_study_sites/Final_study_sites.csv")

study_sites.char <- study_sites$LOCALITY_ID

### select study site data
bird_data <- all_ebird %>%
  select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, COMMON_NAME,
         ALL_SPECIES_REPORTED, DURATION_MINUTES, EFFORT_DISTANCE_KM, LATITUDE, LONGITUDE, CATEGORY,
         STATE, COUNTRY, COUNTY, NUMBER_OBSERVERS, GROUP_IDENTIFIER, PROTOCOL_TYPE,
         LOCALITY, OBSERVATION_COUNT, EFFORT_AREA_HA, LATITUDE, LONGITUDE) %>%
  filter(LOCALITY_ID %in% study_sites.char) %>%
  collect(n=Inf)

bird_data$OBSERVATION_DATE <- as.Date(bird_data$OBSERVATION_DATE, format="%Y-%m-%d")
bird_data$YEAR <- year(bird_data$OBSERVATION_DATE)


bird_data <- bird_data %>%
  filter(YEAR %in% c(2013, 2014, 2015, 2016))



### remove all but the study site data
rm(list=setdiff(ls(), c("bird_data")))

### save study site data as RData file
save.image("Data/Bird_data/bird_data.RData")

