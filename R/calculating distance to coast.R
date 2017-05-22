#### packages
library(readr)
library(rgdal)
library(rgeos)

#### read file in
possible_study_sites <- read_csv("Data/possible.study.sites.csv")

#### calculate nearest distance to coast
pts <- possible_study_sites[, c('Long', 'Lat')]



wgs.84    <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
mollweide <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

sp.points <- SpatialPoints(pts, proj4string=CRS(wgs.84))




coast  <- readOGR(dsn="Data/ne_10m_coastline",layer="ne_10m_coastline",p4s=wgs.84)
coast.moll <- spTransform(coast,CRS(mollweide))
point.moll <- spTransform(sp.points,CRS(mollweide))


possible_study_sites$distance.to.coast_km <- (sapply(1:length(point.moll), function(i)gDistance(point.moll[i],coast.moll)))/1000



write.csv(possible_study_sites, "Data/possible.study.sites.csv", row.names=FALSE)



