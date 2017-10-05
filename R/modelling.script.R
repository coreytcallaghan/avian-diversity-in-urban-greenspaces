## First, load the RData file in
load("Data/modelling_data.RData")
     
     
### Packages
library(dplyr)
library(ggplot2)
library(lme4)
library(arm)
library(MuMIn)
library(effects)
library(mgcv)


#########################################################
#########################################################
################  ANALYSIS 1 ############################
#########################################################
#########################################################

## collapse all sites to polygon ids
analysis_all <- predictor_variables
analysis_all$species_richness <- as.numeric(analysis_all$species_richness)
analysis_all <- na.exclude(analysis_all)

global.mod <- lmer(species_richness ~ Area_ha + distance.to.coast_km + distance_km + evi_hs + gls_tree_hs + gls_water_hs +
                     (1|Urban_area) + (1|Country), REML=FALSE, data=analysis_all, na.action="na.fail")
                   

#### Plotting fitted vs residual values of model (homogeneity) ####
plot(global.mod, add.smooth = FALSE, which = 1)

#### Checking for normality of residuals of null model
E <- resid(global.mod)              
hist(E, xlab="Residuals", main="")

summary(global.mod)
display(global.mod)

plot(Effect("Area_ha", global.mod))

#### Automated Approach ####
model.set_1 <- dredge(global.mod) #### Computes all possible subsets of global model

model.set_1

top.models_1 <- get.models(model.set_1, subset=delta<4) #### selects all models with deltaAic < 4
#### only four top models in this case

my.models_1 <- model.sel(top.models_1) #### Ranks these models based on AICc

print.data.frame(my.models_1, digits=2) #### Prints the top four models (selected above)

Averaged_models_1 <- model.avg(top.models_1) #### Employs a model averaging technique for the top four models using a full(zero method)

summary(Averaged_models_1)
confint(Averaged_models_1, full=TRUE)



###########################################################
#### Plotting Parameter Estimates #########################
###########################################################

a <- as.data.frame(coefficients(Averaged_models_1, full=TRUE))
b <- as.data.frame(confint(Averaged_models_1, full=TRUE))
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


























## gam function

fit.gam <- function(hotspot, data) {
  
  if (!hotspot %in% data$LOCALITY_ID) {stop(paste0(hotspot, " hotspot is not in the data"))}
  message(paste0("Analysing the ",hotspot," hotspot"))
  
  select_hotspot <- function(y) {
    
    modelling.data%>%filter(LOCALITY_ID == y)
  }
  
  dat <- select_hotspot(hotspot)

  # set k
  k <- length(unique(dat$month))
  
  # fit model
  require(mgcv)
  predictors <- as.list(c('gls_water_hs',
                          'gls_tree_hs',
                          'evi_hs',
                          'Hotspot_Size_ha',
                          'evi_5',
                          'evi_15',
                          'evi_25',
                          'gls_tree_5',
                          'gls_tree_15',
                          'gls_tree_25',
                          'gls_water_5',
                          'gls_water_15',
                          'gls_water_25'))
  
  pen.list = list()
  for (i in predictors) {
    pen.list[[i]]=list(diag(1))
  }
  
  
  model.fit <- gam(formula = log(species_richness) ~
                      s(DURATION_MINUTES) +
                      s(month, bs='cc', k=k) +
                      gls_water_hs +
                      gls_tree_hs +
                      evi_hs +
                      Hotspot_Size_ha +
                      evi_5 +
                      evi_15 +
                      evi_25 +
                      gls_tree_5 +
                      gls_tree_15 +
                      gls_tree_25 +
                      gls_water_5 +
                      gls_water_15 +
                      gls_water_25,
            data=dat,
            family=gaussian(),
            paraPen=pen.list
    )
  
  return(model.fit)
}









