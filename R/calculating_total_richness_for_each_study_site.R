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

### Reading in species classification spreadsheet
species_class <- read_csv("Data/species_class.csv")


### Reading in bird data
load("Data/Bird_data/bird_data.RData")

### get richness per locality_id
length(unique(bird_data$LOCALITY_ID))

location_richness <- bird_data %>%
  inner_join(., Final_study_sites, by="LOCALITY_ID") %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  group_by(Polygon_id) %>%
  summarise(species_richness = length(unique(COMMON_NAME)))

richness_class <- bird_data %>%
  inner_join(., Final_study_sites, by="LOCALITY_ID") %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  inner_join(., species_class, by="COMMON_NAME") %>%
  group_by(Polygon_id, classification) %>%
  summarise(richness=length(unique(COMMON_NAME)))

richness_class <- dcast(richness_class, Polygon_id~classification, value.var="richness")


Final_study_sites <- Final_study_sites %>%
  inner_join(., location_richness, by="Polygon_id") %>%
  inner_join(., richness_class, by="Polygon_id")

write.csv(Final_study_sites, "Data/Final_study_sites/Final_study_sites.csv", row.names=FALSE)
