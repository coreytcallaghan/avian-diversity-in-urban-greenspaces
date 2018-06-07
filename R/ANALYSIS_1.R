#########################################################
#########################################################
################  ANALYSIS 1 ############################
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

### Preliminary EDA for RESULTS section of paper
## Number of sites
length(unique(analysis_all$Polygon_id))

## Number of cities
length(unique(analysis_all$Urban_area))

## comparing richness and checklists among dataset 1
location_richness <- Final_study_sites %>%
  dplyr::select(Polygon_id, LOCALITY_ID) %>%
  right_join(., analysis_all, by="Polygon_id") %>%
  inner_join(., bird_data, by="LOCALITY_ID") %>%
  group_by(Polygon_id) %>%
  filter(CATEGORY %in% c("species", "issf")) %>%
  summarise(species_richness = length(unique(COMMON_NAME)),
            checklists=length(unique(SAMPLING_EVENT_IDENTIFIER)))

mean(location_richness$species_richness)
sd(location_richness$species_richness)
min(location_richness$species_richness)
max(location_richness$species_richness)

mean(location_richness$checklists)
sd(location_richness$checklists)
min(location_richness$checklists)
max(location_richness$checklists)

## test for correlation between richness and checklists
hist(location_richness$species_richness)
hist(location_richness$checklists)
hist(sqrt(location_richness$checklists))

cor.test(location_richness$species_richness, sqrt(location_richness$checklists))


#####################################################
############# MODELLING #############################
#####################################################

### test for correlation among predictor variables
global_predictors <- analysis_all %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, evi_5, evi_15, evi_25,
                gls_tree_hs, gls_tree_5, gls_tree_15, gls_tree_25, gls_water_hs, gls_water_5, gls_water_15, gls_water_25)

global_predictors <- round(cor(global_predictors), 2)

global_predictors.2 <- analysis_all %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, evi_5, evi_25,
                gls_tree_hs, gls_tree_5, gls_tree_25, gls_water_hs, gls_water_5, gls_water_25)

global_predictors.2 <- round(cor(global_predictors.2), 2)

library(corrplot)
corrplot(global_predictors)
corrplot(global_predictors.2)

### removed the 15 km buffer due to collinearity issues

### select only the necessary response and predictor variables to include in the analysis
model_data <- analysis_all %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, gls_tree_hs, gls_water_hs,
                evi_5, gls_tree_5, gls_water_5, evi_25, gls_tree_25, gls_water_25, Urban_area, Country, 
                hundred_km_buffer_SR, species_richness, L, W, R, Number_of_checklists, LATITUDE)

### scale and center the predictor variables
model_data[1:12] <- scale(model_data[1:12])

### global model
global.mod <- lmer(species_richness ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + gls_tree_hs + gls_water_hs +
                     evi_5 + gls_tree_5 + gls_water_5  + evi_25 + gls_tree_25 + gls_water_25 + LATITUDE + offset(Number_of_checklists) +
                     (1|Urban_area) + (1|Country), REML=FALSE, data=model_data, na.action="na.fail")


#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod)              
hist(E, xlab="Residuals", main="")

summary(global.mod)
display(global.mod)

plot(Effect("Area_ha", global.mod))

#### Automated Approach ####
model.set <- dredge(global.mod) #### Computes all possible subsets of global model

model.set

top.models <- get.models(model.set, subset=delta<3) #### selects all models with deltaAic < 2

length(top.models) ## how many top models there are

#### only four top models in this case

my.models <- model.sel(top.models) #### Ranks these models based on AICc

print.data.frame(my.models, digits=2) #### Prints the top models (selected above)

Averaged_models <- model.avg(top.models) #### Employs a model averaging technique for the top models using a full(zero method)

summary(Averaged_models)
confint(Averaged_models, full=TRUE)


r2_table <- as.data.frame(t(sapply(top.models, r.squaredGLMM)))

mean(r2_table$R2m)
sd(r2_table$R2m)

mean(r2_table$R2c)
sd(r2_table$R2c)

##########################################################
##########################################################
### 'standardised' species richness analysis #############
##########################################################

### global model
global.mod_standard <- lmer(species_richness/hundred_km_buffer_SR ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + 
                            gls_tree_hs + gls_water_hs + evi_5 + gls_tree_5 + gls_water_5 + evi_25 + gls_tree_25 + gls_water_25 + LATITUDE +
                            offset(Number_of_checklists) + (1|Urban_area) + (1|Country), REML=FALSE, data=model_data, na.action="na.fail")


#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod_standard, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod_standard)              
hist(E, xlab="Residuals", main="")

summary(global.mod_standard)
display(global.mod_standard)

plot(Effect("Area_ha", global.mod_standard))

#### Automated Approach ####
model.set_standard <- dredge(global.mod_standard) #### Computes all possible subsets of global model

model.set_standard

top.models_standard <- get.models(model.set_standard, subset=delta<3) #### selects all models with deltaAic < 3
#### only four top models in this case

my.models_standard <- model.sel(top.models_standard) #### Ranks these models based on AICc

print.data.frame(my.models_standard, digits=2) #### Prints the top four models (selected above)

Averaged_models_standard <- model.avg(top.models_standard) #### Employs a model averaging technique for the top four models using a full(zero method)

summary(Averaged_models_standard)
confint(Averaged_models_standard, full=TRUE)

r2_table_standard <- as.data.frame(t(sapply(top.models_standard, r.squaredGLMM)))

mean(r2_table_standard$R2m)
sd(r2_table_standard$R2m)

mean(r2_table_standard$R2c)
sd(r2_table_standard$R2c)

############################################################
###### Running a model for waterbirds only #################
############################################################
### global model
global.mod_waterbirds <- lmer(W ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + gls_tree_hs + gls_water_hs +
                     evi_5 + gls_tree_5 + gls_water_5  + evi_25 + gls_tree_25 + gls_water_25 + LATITUDE + offset(Number_of_checklists) +
                     (1|Urban_area) + (1|Country), REML=FALSE, data=model_data, na.action="na.fail")


#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod_waterbirds, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod_waterbirds)              
hist(E, xlab="Residuals", main="")

summary(global.mod_waterbirds)
display(global.mod_waterbirds)

plot(Effect("Area_ha", global.mod_waterbirds))

#### Automated Approach ####
model.set_waterbirds <- dredge(global.mod_waterbirds) #### Computes all possible subsets of global model

model.set_waterbirds

top.models_waterbirds <- get.models(model.set_waterbirds, subset=delta<3) #### selects all models with deltaAic < 2

length(top.models_waterbirds) ## how many top models there are

#### only four top models in this case

my.models_waterbirds <- model.sel(top.models_waterbirds) #### Ranks these models based on AICc

print.data.frame(my.models_waterbirds, digits=2) #### Prints the top models (selected above)

Averaged_models_waterbirds <- model.avg(top.models_waterbirds) #### Employs a model averaging technique for the top models using a full(zero method)

summary(Averaged_models_waterbirds)
confint(Averaged_models_waterbirds, full=TRUE)


r2_table_waterbirds <- as.data.frame(t(sapply(top.models_waterbirds, r.squaredGLMM)))

mean(r2_table_waterbirds$R2m)
sd(r2_table_waterbirds$R2m)

mean(r2_table_waterbirds$R2c)
sd(r2_table_waterbirds$R2c)


############################################################
###### Running a model for landbirds only #################
############################################################
### global model
global.mod_landbirds <- lmer(L+R ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + gls_tree_hs + gls_water_hs +
                                evi_5 + gls_tree_5 + gls_water_5  + evi_25 + gls_tree_25 + gls_water_25 + LATITUDE + offset(Number_of_checklists) +
                                (1|Urban_area) + (1|Country), REML=FALSE, data=model_data, na.action="na.fail")


#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod_landbirds, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod_landbirds)              
hist(E, xlab="Residuals", main="")

summary(global.mod_landbirds)
display(global.mod_landbirds)

plot(Effect("Area_ha", global.mod_landbirds))

#### Automated Approach ####
model.set_landbirds <- dredge(global.mod_landbirds) #### Computes all possible subsets of global model

model.set_landbirds

top.models_landbirds <- get.models(model.set_landbirds, subset=delta<3) #### selects all models with deltaAic < 2

length(top.models_landbirds) ## how many top models there are

#### only four top models in this case

my.models_landbirds <- model.sel(top.models_landbirds) #### Ranks these models based on AICc

print.data.frame(my.models_landbirds, digits=2) #### Prints the top models (selected above)

Averaged_models_landbirds <- model.avg(top.models_landbirds) #### Employs a model averaging technique for the top models using a full(zero method)

summary(Averaged_models_landbirds)
confint(Averaged_models_landbirds, full=TRUE)



#### Calculate the R2 for each of the top models and then average the R2 and provide sd of R2 values across the top models

r2_table_landbirds <- as.data.frame(t(sapply(top.models_landbirds, r.squaredGLMM)))

mean(r2_table_landbirds$R2m)
sd(r2_table_landbirds$R2m)

mean(r2_table_landbirds$R2c)
sd(r2_table_landbirds$R2c)



###########################################################
#### Making figures and/or tables for paper presentation ##
###########################################################


## figure of raw species richness versus greenspace size plotted
library(scales)

ggplot(analysis_all, aes(x=Area_ha, y=species_richness))+
  geom_point()+
  theme_bw()+
  xlab("Log(Greenspace Area (Ha))")+
  ylab("Species Richness")+
  scale_x_log10(labels=comma)+
  theme(axis.text.x=element_text(size=12, color="black"))+
  theme(axis.text.y=element_text(size=12, color="black"))+
  theme(axis.title.y=element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  geom_smooth(method=lm)


## figure of additionally important patterns
library(reshape2)



plotting <- analysis_all %>% 
  dplyr::select(W, R, L, gls_water_hs, gls_tree_hs, Area_ha) %>%
  mutate(L=L+R) %>%
  dplyr::select(-one_of("R")) %>%
  melt(., id.vars=c("W", "L")) %>%
  rename(predictor=variable) %>%
  rename(predictor_value=value) %>%
  melt(., id.vars=c("predictor", "predictor_value"))


  plotting$predictor <- gsub("gls_water_hs", "Percent Water", plotting$predictor)
  plotting$predictor <- gsub("gls_tree_hs", "Percent Tree", plotting$predictor)
  plotting$predictor <- gsub("Area_ha", "Greenspace Area (Ha)", plotting$predictor)
  
  
  ggplot(plotting, aes(x=predictor_value, y=value, color=variable, linetype=variable))+
    geom_point()+
    ylab("Species Richness")+
    xlab("Predictor Value")+
    geom_smooth(method=lm, show.legend=FALSE)+
    theme(axis.text.x=element_text(size=12, color="black"))+
    theme(axis.text.y=element_text(size=12, color="black"))+
    theme(axis.title.y=element_text(size=16))+
    theme(axis.title.x=element_text(size=16))+
    theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
    theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
    theme_bw()+
    guides(color=FALSE)+
    facet_grid(~predictor, scales="free", labeller=as_labeller(facet_names))



a <- as.data.frame(coefficients(Averaged_models, full=TRUE))
b <- as.data.frame(confint(Averaged_models, full=TRUE))
model_results <- cbind(a, b)
names(model_results)[1] <- "estimate"
names(model_results)[2] <- "lwr"
names(model_results)[3] <- "upr"
model_results$variable <- row.names(model_results)
row.names(model_results) <- NULL


ggplot(model_results, aes(x=variable, y=estimate, ymin=lwr, ymax=upr))+
  geom_linerange(position=position_dodge(width=0.8))+
  geom_point(color="blue")+
  geom_hline(yintercept=0, color="red")+
  coord_flip()+
  theme_bw()+
  xlab('Model Variable')+
  ylab('Estimate')+
  theme(axis.text.x=element_text(size=12))+
  theme(axis.text.y=element_text(size=12))+
  theme(axis.title.y=element_text(size=20))+
  theme(axis.title.x=element_text(size=20))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())




## A figure which shows the proportional richness at a greenspace associated with greenspace area

### select only the necessary response and predictor variables to include in the analysis
model_data <- analysis_all %>%
  dplyr::select(Area_ha, distance.to.coast_km, distance_km, evi_hs, gls_tree_hs, gls_water_hs,
                evi_5, gls_tree_5, gls_water_5, evi_25, gls_tree_25, gls_water_25, Urban_area, Country, 
                hundred_km_buffer_SR, species_richness, L, W, R, Number_of_checklists, LATITUDE)
library(scales)
ggplot(model_data, aes(x=Area_ha, y=species_richness/hundred_km_buffer_SR))+
  geom_point()+
  theme_bw()+
  xlab("Log(Greenspace Area (Ha))")+
  ylab("Proportional Species Richness")+
  scale_x_log10(labels=comma)+
  theme(axis.text.x=element_text(size=12, color="black"))+
  theme(axis.text.y=element_text(size=12, color="black"))+
  theme(axis.title.y=element_text(size=16))+
  theme(axis.title.x=element_text(size=16))+
  theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())+
  theme(panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank())+
  geom_smooth(method=lm)


















