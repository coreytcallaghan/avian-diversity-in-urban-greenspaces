#########################################################
#########################################################
################  ANALYSIS 2 ############################
#########################################################
#########################################################


## setwd("H:/Dissertation/Dissertation Chapters/Data Chapters/Relationship between avian diversity and urban environments/Analysis/avian diversity in greenspaces")


## First, load the RData file in
load("Data/modelling_data.RData")

### Read in bird data
load("Data/Bird_data/bird_data.RData")

### Packages
library(dplyr)
library(ggplot2)
library(lme4)
library(arm)
library(MuMIn)
library(effects)


### Prepare data for modelling of "FINAL DATASET"

## collapse all sites to polygon ids
analysis_all <- predictor_variables
analysis_all$species_richness <- as.numeric(analysis_all$species_richness)
analysis_all <- na.exclude(analysis_all)



################################
################################
## assess Species Richness first
################################


## join the file above with the bird data file
tmp.model_data_SR <- Final_study_sites %>%
  dplyr::select(Polygon_id, LOCALITY_ID) %>%
  right_join(., analysis_all, by="Polygon_id") %>%
  dplyr::select(LOCALITY_ID) %>%
  inner_join(., species_richness, by="LOCALITY_ID") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, checklist_species_richness, OBSERVATION_DATE, PROTOCOL_TYPE, ALL_SPECIES_REPORTED,
         DURATION_MINUTES, month, Polygon_id) %>%
  inner_join(., analysis_all, by="Polygon_id")


species_richness_summary <- tmp.model_data_SR %>%
  group_by(Polygon_id) %>%
  summarise(lists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

## remove any greenspaces which didn't have at least 50 checklists
model_data_SR <- tmp.model_data_SR %>%
  group_by(Polygon_id) %>%
  summarise(lists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  inner_join(., tmp.model_data_SR, by="Polygon_id") %>%
  filter(lists>=50)

data_summary_SR <- model_data_SR %>%
  group_by(Polygon_id) %>%
  summarise(lists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

mean(data_summary_SR$lists)
sd(data_summary_SR$lists)
min(data_summary_SR$lists)
max(data_summary_SR$lists)


### select only the necessary response and predictor variables to include in the analysis
model_data_SR <- model_data_SR %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, gls_tree_hs, gls_water_hs,
                evi_5, gls_tree_5, gls_water_5, evi_25, gls_tree_25, gls_water_25, DURATION_MINUTES, 
                Urban_area, Country, hundred_km_buffer_SR, checklist_species_richness, SAMPLING_EVENT_IDENTIFIER, month,
                Polygon_id)

month <- data.frame(month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                    id=1:12)

### change month to a character vector for modelling
model_data_SR$month <- as.character(model_data_SR$month)


### scale and center the predictor variables
model_data_SR[1:12] <- scale(model_data_SR[1:12])

hist(model_data_SR$checklist_species_richness)

### global model
global.mod_SR <- lmer(checklist_species_richness ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + gls_tree_hs + gls_water_hs +
                     evi_5 + gls_tree_5 + gls_water_5  + evi_25 + gls_tree_25 + gls_water_25 + offset(DURATION_MINUTES) + (1|Polygon_id/month) +
                     (1|Urban_area) + (1|Country), REML=FALSE, data=model_data_SR, na.action="na.fail")

#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod_SR, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod_SR)              
hist(E, xlab="Residuals", main="")

summary(global.mod_SR)
display(global.mod_SR)

plot(Effect("Area_ha", global.mod_SR))

#### Automated Approach ####
### Paralleslised ###
library(snow)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 10), type = clusterType))

clusterExport(clust, c("model_data_SR", "lmer"))


system.time(model.set_SR <- pdredge(global.mod_SR, cluster=clust)) #### Computes all possible subsets of global model

stopCluster(clust)

model.set_SR

top.models_SR <- get.models(model.set_SR, subset=delta<3) #### selects all models with deltaAic < 3

length(top.models_SR) ## how many top models there are

my.models_SR <- model.sel(top.models_SR) #### Ranks these models based on AICc

print.data.frame(my.models_SR, digits=2) #### Prints the top models (selected above)

Averaged_models_SR <- model.avg(top.models_SR) #### Employs a model averaging technique for the top models using a full(zero method)

summary(Averaged_models_SR)
confint(Averaged_models_SR, full=TRUE)

r2_table_SR <- as.data.frame(t(sapply(top.models_SR, r.squaredGLMM)))

mean(r2_table_SR$R2m)
sd(r2_table_SR$R2m)

mean(r2_table_SR$R2c)
sd(r2_table_SR$R2c)

################################
################################
## Assess Species Diversity 2nd
################################

## join the file above with the bird data file
tmp.model_data_SD <- Final_study_sites %>%
  dplyr::select(Polygon_id, LOCALITY_ID) %>%
  right_join(., analysis_all, by="Polygon_id") %>%
  dplyr::select(LOCALITY_ID) %>%
  inner_join(., species_diversity, by="LOCALITY_ID") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, species_diversity, OBSERVATION_DATE, PROTOCOL_TYPE, ALL_SPECIES_REPORTED,
                DURATION_MINUTES, month, Polygon_id) %>%
  inner_join(., analysis_all, by="Polygon_id")


species_diversity_summary <- tmp.model_data_SD %>%
  group_by(Polygon_id) %>%
  summarise(lists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

## remove any greenspaces which didn't have at least 50 checklists
model_data_SD <- tmp.model_data_SD %>%
  group_by(Polygon_id) %>%
  summarise(lists=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  inner_join(., tmp.model_data_SD, by="Polygon_id") %>%
  filter(lists>=50)

data_summary_SD <- model_data_SD %>%
  group_by(Polygon_id) %>%
  summarise(lists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

mean(data_summary_SD$lists)
sd(data_summary_SD$lists)
min(data_summary_SD$lists)
max(data_summary_SD$lists)


### select only the necessary response and predictor variables to include in the analysis
model_data_SD <- model_data_SD %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, gls_tree_hs, gls_water_hs,
                evi_5, gls_tree_5, gls_water_5, evi_25, gls_tree_25, gls_water_25, DURATION_MINUTES, 
                Urban_area, Country, hundred_km_buffer_SR, species_diversity, SAMPLING_EVENT_IDENTIFIER, month,
                Polygon_id)

month <- data.frame(month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                    id=1:12)

### change month to a character vector for modelling
model_data_SD$month <- as.character(model_data_SD$month)


### scale and center the predictor variables
model_data_SD[1:12] <- scale(model_data_SD[1:12])

hist(model_data_SD$species_diversity)

### global model
global.mod_SD <- lmer(species_diversity ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + gls_tree_hs + gls_water_hs +
                        evi_5 + gls_tree_5 + gls_water_5  + evi_25 + gls_tree_25 + gls_water_25 + offset(DURATION_MINUTES) + (1|Polygon_id/month) +
                        (1|Urban_area) + (1|Country), REML=FALSE, data=model_data_SD, na.action="na.fail")


#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod_SD, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod_SD)              
hist(E, xlab="Residuals", main="")

summary(global.mod_SD)
display(global.mod_SD)

plot(Effect("Area_ha", global.mod_SD))

#### Automated Approach ####
### Paralleslised ###
library(snow)
clusterType <- if(length(find.package("snow", quiet = TRUE))) "SOCK" else "PSOCK"
clust <- try(makeCluster(getOption("cl.cores", 10), type = clusterType))

clusterExport(clust, c("model_data_SD", "lmer"))


model.set_SD <- pdredge(global.mod_SD, cluster=clust) #### Computes all possible subsets of global model

stopCluster(clust)

model.set_SD

top.models_SD <- get.models(model.set_SD, subset=delta<3) #### selects all models with deltaAic < 3

length(top.models_SD) ## how many top models there are

my.models_SD <- model.sel(top.models_SD) #### Ranks these models based on AICc

print.data.frame(my.models_SD, digits=2) #### Prints the top models (selected above)

Averaged_models_SD <- model.avg(top.models_SD) #### Employs a model averaging technique for the top models using a full(zero method)

summary(Averaged_models_SD)
confint(Averaged_models_SD, full=TRUE)


r2_table_SD <- as.data.frame(t(sapply(top.models_SD, r.squaredGLMM)))

mean(r2_table_SD$R2m)
sd(r2_table_SD$R2m)

mean(r2_table_SD$R2c)
sd(r2_table_SD$R2c)

