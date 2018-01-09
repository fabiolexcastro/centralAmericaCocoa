
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(gtools)
require(rgeos)

# Initial setup
rm(list = ls())
setwd('Z:/_cam')
options(scipen = 999)
cat('\f')

# Load data
load('_rData/_run6/clustereddata.rData'); rm(labelRF, no.clusters, clusteredpresdata)
occ <- dplyr::select(occ, Lon, Lat)
imp30 <- raster('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_change_2030.asc')

# Farmers
frms_imp30 <- raster::extract(imp30, occ[,1:2]) %>% cbind(occ, .)

frm_adaptc <- filter(frms_imp30, . == 1)
frm_adaptc <- sample(x = row.names(frm_adaptc), size = 1) %>% as.numeric() %>% occ[.,] %>% mutate(impact = 'Adaptacion Incremental' )

frm_transf <- filter(frms_imp30, . == 3)
frm_transf <- sample(x = row.names(frm_transf), size = 1) %>% as.numeric() %>% occ[.,] %>% mutate(impact = 'Transformacion')

frm_resili <- filter(frms_imp30, . == 5)
frm_resili <- sample(x = row.names(frm_resili), size = 1) %>% as.numeric() %>% occ[.,] %>% mutate(impact = 'Resilicencia')

df <- rbind(frm_adaptc, frm_transf, frm_resili)

coordinates(df) <- ~ Lon + Lat

writeOGR(df, dsn = '_shp/_frms', layer = 'farmers', driver = 'ESRI Shapefile')

