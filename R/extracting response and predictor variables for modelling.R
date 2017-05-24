## setwd('H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Github work/avian diversity in greenspaces')


## Necessary packages 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(data.table)


### Reading in study sites

possible_study_sites <- read_csv("Data/possible.study.sites.csv")


### Reading in bird data
bird_data <- read_csv("Data/Bird_Data_Corey.csv")


### Reading in predictor variables

#### for buffers
setwd("Data/ebird_buffer_stats")

temp=list.files(pattern="*.csv")
myfiles=lapply(temp, read_csv)

habitat_variables <- bind_rows(myfiles)

rm(temp)
rm(myfiles)

## setwd('H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Github work/avian diversity in greenspaces')



#### for hotspots

setwd("Data/ebird_hotspot_stats")

temp=list.files(pattern="*.csv")
myfiles=lapply(temp, read_csv)

hotspot_variables <- bind_rows(myfiles)

rm(temp)
rm(myfiles)


## setwd('H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Github work/avian diversity in greenspaces')

## Now, lets' combine data to have __response variables__ & __predictor variables__

### Forming predictor variables file for the buffers
predictor_variables.buffers <- habitat_variables %>%
  # get rid of 'name'
  select(-one_of('name')) %>%
  # transpose file
  dcast(LOCALITY_ID ~ file.name, value.var="value") %>%
  # remove 'NA' column
  select(-one_of('NA')) %>%
  # join with possible study sites file from above
  inner_join(., possible_study_sites, by="LOCALITY_ID")

### Taking a mean of the included evi year values
predictor_variables.buffers$evi_5 <- rowMeans(predictor_variables.buffers[, c('evi_2013_5', 'evi_2014_5', 'evi_2015_5', 'evi_2016_5')])

predictor_variables.buffers$evi_15 <- rowMeans(predictor_variables.buffers[, c('evi_2013_15', 'evi_2014_15', 'evi_2015_15', 'evi_2016_15')])

predictor_variables.buffers$evi_25 <- rowMeans(predictor_variables.buffers[, c('evi_2013_25', 'evi_2014_25', 'evi_2015_25', 'evi_2016_25')])



### Forming predictor variables file for the hotspots
predictor_variables.hotspots <- hotspot_variables %>%
  # transpose file
  dcast(LOCALITY_ID ~ file.name, value.var="value")

### Taking a mean of the included evi year values
predictor_variables.hotspots$evi_hs <- rowMeans(predictor_variables.hotspots[, c('evi_2013_hs', 'evi_2014_hs', 'evi_2015_hs', 'evi_2016_hs')])

### making one single predictor variables file
predictor_variables <- merge(predictor_variables.buffers, predictor_variables.hotspots, by="LOCALITY_ID")

rm(predictor_variables.buffers)
rm(predictor_variables.hotspots)
rm(habitat_variables)
rm(hotspot_variables)


### Forming response variables file

## I want to have two different response variables; __species richness__ and __species diversity__

#### Let's create a file with the response variables for each checklist

# Counts how many 'x's per checklist
X_missing <- bird_data %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION_COUNT=="X"))

# here is where you add all the columns needed for the analysis (that don't vary within checklist)
sampling_event_info <- bird_data %>%
  select(SAMPLING_EVENT_IDENTIFIER, LOCALITY, LOCALITY_ID, OBSERVATION_DATE,
         PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, 
         DURATION_MINUTES, OBSERVATION_DATE) %>%
  distinct()

# Formatting Date
sampling_event_info$OBSERVATION_DATE <- as.Date(sampling_event_info$OBSERVATION_DATE, format="%m/%d/%Y")

# combines species and subspecies into one count
bird_data.new <- bird_data %>%
  filter(CATEGORY %in% c("species","issf")) %>% 
  group_by(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME) %>%
  summarise(COUNT_SPP = sum(as.numeric(as.character(OBSERVATION_COUNT)))) %>%
  rename(OBSERVATION_COUNT = COUNT_SPP) %>% 
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER")%>%
  inner_join(., X_missing, by="SAMPLING_EVENT_IDENTIFIER")


## First, calculate species richness and abundance
species_richness <- bird_data.new %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(species_richness=length(unique(COMMON_NAME)))

## Second, calculate species diversity
species_diversity <- bird_data.new %>%
  filter(!is.na(OBSERVATION_COUNT)) %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(species_diversity=diversity(OBSERVATION_COUNT))

# Merging the species richness and species diversity dataframes with sampling event info
response_variables <- sampling_event_info %>%
  inner_join(., species_diversity, by="SAMPLING_EVENT_IDENTIFIER") %>%
  inner_join(., species_richness, by="SAMPLING_EVENT_IDENTIFIER") %>%
  ## filtering data to highest quality
  filter(ALL_SPECIES_REPORTED == 1, 
         PROTOCOL_TYPE !="eBird - Casual Observation",
         PROTOCOL_TYPE !="Historical") %>%
  filter(OBSERVATION_DATE>= "2011-01-01" & OBSERVATION_DATE <= "2016-12-31") 

rm(species_diversity)
rm(species_richness)
rm(bird_data)
rm(bird_data.new)
rm(sampling_event_info)
rm(X_missing)

rm(possible_study_sites)

save.image("Data/modelling_data.RData")
