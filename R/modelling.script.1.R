## First, load the RData file in
load("Data/modelling_data.RData")


### Packages
library(dplyr)
library(ggplot2)
library(lme4)
library(mgcv)
library(gamm4)
library(MuMIn)
library(effects)




## we will want to sample randomly, based on the fact the number of data points is vastly uneven among locations
## Not sure how to do this in running the final models
## i.e. I'm not sure how to do it multiple times and then combine them into one 'modelling result'
## but, because the models take yonks to fit when using the entire dataframe
## I will just create a sample dataframe to run the models on and get the framework up and running

sample_data <- modelling.data %>%
  group_by(LOCALITY_ID) %>%
  sample_n(size=20)


#################################################################################################
##### The plan is to have four 'global models' which include the four spatial distances ########


###### MODEL 1: Just the hotspot and corresponding hotspot variables
hotspot_model <- gamm4(log(species_richness) ~ s(DURATION_MINUTES) + s(Lat, Long) + month + gls_water_hs + 
                       gls_tree_hs + evi_hs + distance.to.coast_km + Hotspot_Size_ha + Size_km,
                       family=gaussian(), data=sample_data, 
                       random=~(1|LOCALITY_ID/Ecoregion))


## we get the warning message that predictor variables are on different scales.
## I guess I should rescale the predictor variables before running the model?
## What do you think about rescaling predictor variables?


par(mfrow=c(1,2))
plot(hotspot_model$gam)


par(mfrow=c(2,2))
gam.check(hotspot_model$gam)

par(mfrow=c(1,1))
plot(fitted(hotspot_model$gam, residuals(hotspot_model$gam)))
## it doesn't look too bad based on the checks.


summary.gam(hotspot_model$gam)
anova.gam(hotspot_model$gam)
## Well, the months are all significant, which is to be expected! I think I set the model up wrong.
## I'm not really interested in month as a factor in the model, I want to account for month in the model
## perhaps I need to allow the slope to vary through month as well, thus 'accounting' for month?




hotspot_model.2 <- gamm4(log(species_richness) ~ s(DURATION_MINUTES) + s(Lat, Long) + gls_water_hs + 
                         gls_tree_hs + evi_hs + distance.to.coast_km + Hotspot_Size_ha + Size_km,
                       family=gaussian(), data=sample_data, 
                       random= ~ (1|LOCALITY_ID/month) + (1|LOCALITY_ID/Ecoregion))

# I think i specified the random effects correctly...?


par(mfrow=c(1,2))
plot(hotspot_model.2$gam)


par(mfrow=c(2,2))
gam.check(hotspot_model.2$gam)
## it doesn't look too bad based on the checks.


summary.gam(hotspot_model.2$gam)
anova.gam(hotspot_model.2$gam)
















####### Model 2: Incorporating the 5 km buffer into the model

five_km_model <- gamm4(log(species_richness) ~ s(DURATION_MINUTES) + s(Lat, Long) + month + gls_water_hs +
                       gls_tree_hs + evi_hs + distance.to.coast_km + Hotspot_Size_ha + Size_km +
                       gls_water_5 + gls_tree_5 + evi_5, family=gaussian(), data=modelling.data,
                       random=~(1|LOCALITY_ID/Ecoregion))





####### Model 3: Incorporating the 15 km buffer into the model

fifteen_km_model <- gamm4(log(species_richness) ~ s(DURATION_MINUTES) + s(Lat, Long) + month + gls_water_hs +
                         gls_tree_hs + evi_hs + distance.to.coast_km + Hotspot_Size_ha + Size_km +
                         gls_water_15 + gls_tree_15 + evi_15, family=gaussian(), data=modelling.data,
                         random=~(1|LOCALITY_ID/Ecoregion))



####### Model 4: Incorporating the 25 km buffer into the model

fifteen_km_model <- gamm4(log(species_richness) ~ s(DURATION_MINUTES) + s(Lat, Long) + month + gls_water_hs +
                            gls_tree_hs + evi_hs + distance.to.coast_km + Hotspot_Size_ha + Size_km +
                            gls_water_25 + gls_tree_25 + evi_25, family=gaussian(), data=modelling.data,
                            random=~(1|LOCALITY_ID/Ecoregion))





