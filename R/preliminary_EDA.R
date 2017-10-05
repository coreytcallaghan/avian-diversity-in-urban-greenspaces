## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")

## Necessary packages 
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(vegan)
library(data.table)
library(lubridate)

## load data
load("Data/modelling_data.RData")


#####################
#####################
### ANALYSIS 1 ######
#####################
#####################

## Number of sites
length(unique(Final_study_sites$Polygon_id))

## Number of cities
length(unique(Final_study_sites$Urban_area))

## comparing richness and checklists among dataset 1
location_richness <- bird_data %>%
  inner_join(., Final_study_sites, by="LOCALITY_ID") %>%
  group_by(Polygon_id) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  summarise(species_richness = length(unique(COMMON_NAME)),
            checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

## test for correlation between richness and checklists
hist(location_richness$species_richness)
hist(location_richness$checklists)
hist(sqrt(location_richness$checklists))

cor.test(location_richness$species_richness, sqrt(location_richness$checklists))


## mean, sd, range of species richness of dataset 1
Final_study_sites$species_richness <- as.numeric(Final_study_sites$species_richness)

summary_richness <- Final_study_sites %>%
  group_by(Polygon_id) %>%
  summarise(SR=mean(species_richness)) %>%
  summarise(mean=mean(SR),
            sd=sd(SR),
            min=min(SR),
            max=max(SR))



## histogram of species richness response variable
hist(species_richness$checklist_species_richness)

## try a log transform
hist(log(species_richness$checklist_species_richness))

## Hard to say for sure, but looks like it might be fine untransformed.
## Looks mostly normal with a slight tail

## Number of data points by location
test <- species_richness %>%
  group_by(Polygon_id) %>%
  summarise(N=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  arrange(N) %>%
  ggplot(., aes(x=Polygon_id, y=N))+
  geom_bar(stat='identity')+
  coord_flip()

## Obviously a problem given there is substantially more data from one site compared to the others!
## Need to account for this in the modelling, but first let's remove the smallest of sample sizes


### Reading in study sites
Final_study_sites <- read_csv("Data/Final_study_sites/Final_study_sites.csv")


### plot between richness and hotspot size
Final_study_sites %>%
  group_by(Polygon_id) %>%
  summarise(Hotspot_size = mean(Area_ha),
            species_richness = mean(species_richness)) %>%
  ggplot(., aes(x=species_richness, y=(Hotspot_size))) +
  geom_point()+
  theme_bw()