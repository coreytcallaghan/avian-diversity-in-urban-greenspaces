## setwd('H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Github work/avian diversity in greenspaces')


## Necessary packages 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(data.table)
library(lubridate)
library(RSQLite)
library(dbplyr)
library(ggmap)
library(data.table)
library(geosphere)

### connect to all ebird sqlite file db
### this needs to be a hard connection
ebird_db <- src_sqlite("D:/All eBird Data/ebird.sqlite", create=FALSE)
all_ebird <- tbl(ebird_db, "ebird")


### return the locality_ids that have greater than 1000 checklists total
localities <- all_ebird %>%
  select(LOCALITY_ID, OBSERVATION_DATE, LATITUDE, LONGITUDE, SAMPLING_EVENT_IDENTIFIER) %>%
  distinct() %>%
  filter(OBSERVATION_DATE > "2013-06-01") %>%
  group_by(LOCALITY_ID) %>%
  summarise(LATITUDE=mean(LATITUDE),
            LONGITUDE=mean(LONGITUDE),
            Number_of_checklists=n(SAMPLING_EVENT_IDENTIFIER)) %>%
  filter(Number_of_checklists>500) %>%
  collect(n=Inf) 
  

### get locality names
localities_list <- localities$LOCALITY_ID


localities_names <- all_ebird %>%
  select(LOCALITY_ID, LOCALITY, LOCALITY_TYPE) %>%
  filter(LOCALITY_ID %in% localities_list) %>%
  distinct() %>%
  collect(n=Inf)
  

localities <- merge(localities, localities_names, by="LOCALITY_ID")

### read in dataframe of top cities
top_cities <- read_csv("Data/ALL top urban areas.csv")



### calculate lat/long for each city/urban area
### this can be a pain and kick lots of warnings of
### it not calculating due to the google API query
lonlat <- geocode(top_cities$city_country)


### need to merge the files coordinates together with the cities
top_cities <- cbind(top_cities, lonlat)


### calculate the nearest cities for each of the localities
library(RANN)

closest <- as.data.frame(nn2(top_cities[, 11:12], localities[, 3:2], 1))

localities <- cbind(localities, closest)

city_id <- as.data.frame(top_cities$city_country)
city_id$id <- 1:nrow(city_id)

locality_closest_city <- localities %>%
  rename(id=nn.idx) %>%
  inner_join(., city_id, by="id") %>%
  rename(city_country = 'top_cities$city_country') %>%
  inner_join(., top_cities, by="city_country") %>%
  select(LOCALITY_ID, LOCALITY, Number_of_checklists, LATITUDE, LONGITUDE, city_country, lon, lat, Country, Urban_area, LOCALITY_TYPE)

### now calculate the geodesic distance from each hotspot to the nearest city
setDT(locality_closest_city)[, distance.km := distGeo(matrix(c(lon, lat), ncol=2),
                                                       matrix(c(LONGITUDE, LATITUDE), ncol=2))/1000]



## remove all object besides dataframes necessary
rm(list=setdiff(ls(), c("locality_closest_city")))


## save df as Rdata file
save.image("Data/locality_closest_city.RData")

























 



  