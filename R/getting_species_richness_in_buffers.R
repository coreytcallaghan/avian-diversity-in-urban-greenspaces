## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")


### Packages
library(dplyr)
library(RSQLite)
library(tictoc)
library(readr)
library(lubridate)


### connect to eBird db
ebird_db <- src_sqlite("D:/All eBird Data/ebird.sqlite", create=FALSE)
all_ebird <- tbl(ebird_db, "ebird")

### read in study site data
Final_study_sites <- read_csv("Data/Final_study_sites/Final_study_sites.csv")

### the first step was to get all the unique localities in the eBird database
### this was then exported
### it was read into ArcMap
### I then joined by the various buffer sizes - 5/15/25 and one of 100
### each one was exported as a separate text file from ArcMap
### this was turned into excel/csv
### this was read in below and then using those locality IDs
### I re-called the ebird database and calculated the species richness in each buffer zone

unique_localities <- all_ebird %>%
  select(LOCALITY_ID, LATITUDE, LONGITUDE) %>%
  group_by(LOCALITY_ID) %>%
  summarise(LATITUDE=mean(LATITUDE), LONGITUDE=mean(LONGITUDE)) %>%
  collect(n=Inf)

write.csv(unique_localities, "H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces/Data/unique_localities.csv", row.names=FALSE)


### calculate the number of species in each buffer surrounding each hotspot

### five km buffer

# read file in
X5_km_buffer_locations <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces/Data/5_km_buffer_locations.csv")

five_locations <- X5_km_buffer_locations$LOCALITY_I

five_birds <- all_ebird %>%
  select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, COMMON_NAME,
         CATEGORY) %>%
  filter(LOCALITY_ID %in% five_locations) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  collect(n=Inf)

five_birds$OBSERVATION_DATE <- as.Date(five_birds$OBSERVATION_DATE, format="%Y-%m-%d")
five_birds$YEAR <- year(five_birds$OBSERVATION_DATE)

five_birds <- five_birds %>%
  filter(YEAR %in% c(2013, 2014, 2015, 2016))

five_km_buffer_SR <- X5_km_buffer_locations %>%
  rename(LOCALITY_ID = LOCALITY_I) %>%
  inner_join(., five_birds, by="LOCALITY_ID") %>%
  group_by(LOCALITY_I_1) %>%
  summarise(five_km_buffer_SR = length(unique(COMMON_NAME)))

Final_study_sites <- five_km_buffer_SR %>%
  rename(Polygon_id = LOCALITY_I_1) %>%
  inner_join(., Final_study_sites, by="Polygon_id")

rm(list=setdiff(ls(), c("Final_study_sites", "ebird_db", "all_ebird")))

### 15 km buffer
# read file in
X15_km_buffer_locations <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces/Data/15_km_buffer_locations.csv")

fifteen_locations <- X15_km_buffer_locations$LOCALITY_I

fifteen_birds <- all_ebird %>%
  select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, COMMON_NAME,
         CATEGORY) %>%
  filter(LOCALITY_ID %in% fifteen_locations) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  collect(n=Inf)

fifteen_birds$OBSERVATION_DATE <- as.Date(fifteen_birds$OBSERVATION_DATE, format="%Y-%m-%d")
fifteen_birds$YEAR <- year(fifteen_birds$OBSERVATION_DATE)

fifteen_birds <- fifteen_birds %>%
  filter(YEAR %in% c(2013, 2014, 2015, 2016))

fifteen_km_buffer_SR <- X15_km_buffer_locations %>%
  rename(LOCALITY_ID = LOCALITY_I) %>%
  inner_join(., fifteen_birds, by="LOCALITY_ID") %>%
  group_by(LOCALITY_I_1) %>%
  summarise(fifteen_km_buffer_SR = length(unique(COMMON_NAME)))

Final_study_sites <- fifteen_km_buffer_SR %>%
  rename(Polygon_id = LOCALITY_I_1) %>%
  inner_join(., Final_study_sites, by="Polygon_id")

rm(list=setdiff(ls(), c("Final_study_sites", "ebird_db", "all_ebird")))

### 25km buffer
# read file in
X25_km_buffer_locations <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces/Data/25_km_buffer_locations.csv")

twentyfive_locations <- X25_km_buffer_locations$LOCALITY_I

twentyfive_birds <- all_ebird %>%
  select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, COMMON_NAME,
         CATEGORY) %>%
  filter(LOCALITY_ID %in% twentyfive_locations) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  collect(n=Inf)

twentyfive_birds$OBSERVATION_DATE <- as.Date(twentyfive_birds$OBSERVATION_DATE, format="%Y-%m-%d")
twentyfive_birds$YEAR <- year(twentyfive_birds$OBSERVATION_DATE)

twentyfive_birds <- twentyfive_birds %>%
  filter(YEAR %in% c(2013, 2014, 2015, 2016))

twentyfive_km_buffer_SR <- X25_km_buffer_locations %>%
  rename(LOCALITY_ID = LOCALITY_I) %>%
  inner_join(., twentyfive_birds, by="LOCALITY_ID") %>%
  group_by(LOCALITY_I_1) %>%
  summarise(twentyfive_km_buffer_SR = length(unique(COMMON_NAME)))

Final_study_sites <- twentyfive_km_buffer_SR %>%
  rename(Polygon_id = LOCALITY_I_1) %>%
  inner_join(., Final_study_sites, by="Polygon_id")

rm(list=setdiff(ls(), c("Final_study_sites", "ebird_db", "all_ebird")))

### 100km buffer
# read file in
X100_km_buffer_locations <- read_csv("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces/Data/100_km_buffer_locations.txt")


hundred_locations <- X100_km_buffer_locations$LOCALITY_I

hundred_birds <- all_ebird %>%
  select(OBSERVATION_DATE, OBSERVER_ID, LOCALITY_ID, SAMPLING_EVENT_IDENTIFIER, COMMON_NAME,
         CATEGORY) %>%
  filter(LOCALITY_ID %in% hundred_locations) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  collect(n=Inf)

hundred_birds$OBSERVATION_DATE <- as.Date(hundred_birds$OBSERVATION_DATE, format="%Y-%m-%d")
hundred_birds$YEAR <- year(hundred_birds$OBSERVATION_DATE)

hundred_birds <- hundred_birds %>%
  filter(YEAR %in% c(2013, 2014, 2015, 2016))

hundred_km_buffer_SR <- X100_km_buffer_locations %>%
  rename(LOCALITY_ID = LOCALITY_I) %>%
  inner_join(., hundred_birds, by="LOCALITY_ID") %>%
  group_by(LOCALITY_I_1) %>%
  summarise(hundred_km_buffer_SR = length(unique(COMMON_NAME)))

Final_study_sites <- hundred_km_buffer_SR %>%
  rename(Polygon_id = LOCALITY_I_1) %>%
  inner_join(., Final_study_sites, by="Polygon_id")


write.csv(Final_study_sites, "Data/Final_study_sites/Final_study_sites.csv", row.names=FALSE)




