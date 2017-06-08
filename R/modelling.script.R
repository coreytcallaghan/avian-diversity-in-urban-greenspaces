## First, load the RData file in
load("Data/modelling_data.RData")


### Packages
library(dplyr)
library(ggplot2)
library(mgcv)

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









