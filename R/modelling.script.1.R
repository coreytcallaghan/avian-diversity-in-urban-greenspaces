## First, load the RData file in
load("Data/modelling_data.RData")


### Packages
library(lme4)
library(arm)
library(MuMIn)
library(effects)

### Now we have the data to start modelling, but first let's look at the quantity of the data by site

response_variables %>%
  group_by(LOCALITY_ID) %>%
  summarise(N=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  arrange(N)


### Let's remove any locations with <20 checklists - based on previous work

filtered_locations <- response_variables %>%
  group_by(LOCALITY_ID) %>%
  summarise(N=length(unique(SAMPLING_EVENT_IDENTIFIER))) %>%
  arrange(N) %>%
  filter(N>=20) %>%
  .$LOCALITY_ID

response_variables <- response_variables %>%
  filter(LOCALITY_ID %in% filtered_locations)

predictor_variables <- predictor_variables %>%
  filter(LOCALITY_ID %in% filtered_locations)