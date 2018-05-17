### A script that is used to respond to the reviewer's comments about 
### habitat heterogeneity

## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")


## First, load the RData file in
load("Data/modelling_data.RData")

### Read in bird data
load("Data/Bird_data/bird_data.RData")

### Packages
library(dplyr)
library(ggplot2)
library(vegan)

### Prepare data for modelling of "FINAL DATASET"

## collapse all sites to polygon ids
analysis_all <- predictor_variables
analysis_all$species_richness <- as.numeric(analysis_all$species_richness)
analysis_all <- na.exclude(analysis_all)

## get final dataset

### select only the necessary response and predictor variables to include in the analysis
model_data <- analysis_all %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, gls_tree_hs, gls_water_hs,
                evi_5, gls_tree_5, gls_water_5, evi_25, gls_tree_25, gls_water_25, Urban_area, Country, 
                hundred_km_buffer_SR, species_richness, L, W, R, Number_of_checklists)


model_data %>%
  mutate(heterogeneity = diversity(.[4:6])) %>%
  dplyr::select(heterogeneity, Area_ha, species_richness) %>%
  rename(`Habitat heterogeneity` = heterogeneity) %>%
  rename(`Species richness` = species_richness) %>%
  rename(`Area (Ha)` = Area_ha) %>%
  pairs()
           
ggplot(model_data, aes(x=Area_ha, y=heterogeneity))+
  geom_point()+
  geom_smooth(method="lm")


ggplot(model_data, aes(x=species_richness, y=heterogeneity))+
  geom_point()+
  geom_smooth(method="lm")





