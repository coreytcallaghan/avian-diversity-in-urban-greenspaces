## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")

## Necessary packages 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(data.table)
library(lubridate)


### Reading in study sites

Final_study_sites <- read_csv("Data/Final_study_sites/Final_study_sites.csv")


### Reading in bird data
load("Data/Bird_data/bird_data.RData")


### Reading in predictor variables

#### for buffers
setwd("Data/ebird_buffer_stats")

temp=list.files(pattern="*.csv")
myfiles=lapply(temp, read_csv)

habitat_variables <- bind_rows(myfiles)

rm(temp)
rm(myfiles)

## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")


#### for hotspots

setwd("Data/ebird_Hotspot_stats")

temp=list.files(pattern="*.csv")
myfiles=lapply(temp, read_csv)

hotspot_variables <- bind_rows(myfiles)

rm(temp)
rm(myfiles)


## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")

## Now, lets' combine data to have __response variables__ & __predictor variables__

### Forming predictor variables file for the buffers
predictor_variables.buffers <- habitat_variables %>%
  rename(LOCALITY_ID = LOCALITY_I) %>%
  # transpose file
  dcast(LOCALITY_ID ~ file.name, value.var="value") %>%
  # join with possible study sites file from above
  inner_join(., Final_study_sites, by="LOCALITY_ID")

### Taking a mean of the included evi year values
predictor_variables.buffers$evi_5 <- rowMeans(predictor_variables.buffers[, c('evi_2013_5', 'evi_2014_5', 'evi_2015_5', 'evi_2016_5')])

predictor_variables.buffers$evi_15 <- rowMeans(predictor_variables.buffers[, c('evi_2013_15', 'evi_2014_15', 'evi_2015_15', 'evi_2016_15')])

predictor_variables.buffers$evi_25 <- rowMeans(predictor_variables.buffers[, c('evi_2013_25', 'evi_2014_25', 'evi_2015_25', 'evi_2016_25')])



### Forming predictor variables file for the hotspots
predictor_variables.hotspots <- hotspot_variables %>%
  rename(LOCALITY_ID = LOCALITY_I) %>%
  # transpose file
  dcast(LOCALITY_ID ~ file.name, value.var="value")

### Taking a mean of the included evi year values
predictor_variables.hotspots$evi_hs <- rowMeans(predictor_variables.hotspots[, c('evi_2013_hs', 'evi_2014_hs', 'evi_2015_hs', 'evi_2016_hs')])

### making one single predictor variables file
predictor_variables <- merge(predictor_variables.buffers, predictor_variables.hotspots, by="LOCALITY_ID")

predictor_variables <- predictor_variables %>%
  rename(Polygon_id = LOCALITY_ID) %>%
  .[2:65]


rm(predictor_variables.buffers)
rm(predictor_variables.hotspots)
rm(habitat_variables)
rm(hotspot_variables)


### Forming response variables file
## I want to have two different response variables:  __species richness__ and __species diversity__
#### Let's create a file with the response variables for each checklist

# Counts how many 'x's per checklist
X_missing <- bird_data %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(number_X = sum(OBSERVATION_COUNT=="X"))

# here is where you add all the columns needed for the analysis (that don't vary within checklist)
sampling_event_info <- bird_data %>%
  select(SAMPLING_EVENT_IDENTIFIER, LOCALITY, LOCALITY_ID, OBSERVATION_DATE,
         PROTOCOL_TYPE, ALL_SPECIES_REPORTED, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, 
         DURATION_MINUTES, OBSERVATION_DATE, GROUP_IDENTIFIER) %>%
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


# Apply some filtering criteria
bird_data.new <- bird_data.new %>%
  filter(ALL_SPECIES_REPORTED==1) %>%
  filter(PROTOCOL_TYPE %in% c("eBird - Exhaustive Area Count", "eBird - Stationary Count", "eBird - Traveling Count")) %>%
  filter(OBSERVATION_DATE>= "2013-01-01" & OBSERVATION_DATE <= "2016-12-31")

# filter out group_identifier data to eliminate duplicated checklists
duplicated <- bird_data.new %>%
  drop_na(GROUP_IDENTIFIER) %>%
  select(GROUP_IDENTIFIER, SAMPLING_EVENT_IDENTIFIER) %>%
  distinct(.keep_all=TRUE) %>%
  group_by(GROUP_IDENTIFIER) %>%
  # randomly sample one checklist for each group_identifier
  sample_n(., 1) %>%
  .$SAMPLING_EVENT_IDENTIFIER

duplicated_data <- bird_data.new %>%
  filter(SAMPLING_EVENT_IDENTIFIER %in% duplicated)

## now, append the selected checklists for each group_identifier
## with the non group_identifier checklists from the data

# first select all data without a group_identifier
bird_data.new <- bird_data.new %>%
  filter(!grepl("G", GROUP_IDENTIFIER)) %>%
  bind_rows(., duplicated_data)

##############################################################################
############ apply distance and duration caps ################################
##############################################################################
bird_data.new <- bird_data.new %>%
  filter(DURATION_MINUTES >= 5 & DURATION_MINUTES <=240) %>%
  filter(EFFORT_DISTANCE_KM <= 10)

## Calculate species richness and diversity

## First, calculate species richness
species_richness <- bird_data.new %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(checklist_species_richness=length(unique(COMMON_NAME))) %>%
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER") %>%
  select(-one_of("LOCALITY"))

## Second, calculate species diversity
species_diversity <- bird_data.new %>%
  filter(number_X == 0) %>%
  group_by(SAMPLING_EVENT_IDENTIFIER) %>%
  summarise(species_diversity=diversity(OBSERVATION_COUNT)) %>%
  inner_join(., sampling_event_info, by="SAMPLING_EVENT_IDENTIFIER") %>%
  select(-one_of("LOCALITY"))


## adding month to the response variables files
detach("package:data.table", unload=TRUE)
species_diversity$month <- month(species_diversity$OBSERVATION_DATE)
species_richness$month <- month(species_richness$OBSERVATION_DATE)

species_richness <- species_richness %>%
  inner_join(., Final_study_sites, by="LOCALITY_ID")

species_diversity <- species_diversity %>%
  inner_join(., Final_study_sites, by="LOCALITY_ID")


### remove all but the two response variables and the predictor variables file, and final study sites
rm(list=setdiff(ls(), c("species_richness", "species_diversity", "predictor_variables", "Final_study_sites")))

save.image("Data/modelling_data.RData")




