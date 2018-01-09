

require(raster)
require(rgdal)
require(tidyverse)

rm(list = ls())
options(scipen = 999)

load('_rData/_run6/clustereddata.rData')

occ <- cbind.data.frame(occ, labelRF) %>% dplyr::select(Lon, Lat, labelRF)

coordinates(occ) <- ~ Lon + Lat

plot(occ)
spplot(occ)

writeOGR(obj = occ, dsn = '_shp', layer = 'occ_run6', driver = 'ESRI Shapefile')
