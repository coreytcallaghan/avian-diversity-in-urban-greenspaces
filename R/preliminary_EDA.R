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


### plot between richness and hotspot size
Final_study_sites %>%
  group_by(Polygon_id) %>%
  summarise(Hotspot_size = mean(Area_ha),
            species_richness = mean(species_richness)) %>%
  ggplot(., aes(x=species_richness, y=log(Hotspot_size))) +
  geom_point()+
  theme_bw()