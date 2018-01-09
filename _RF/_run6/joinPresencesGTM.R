
# Load libraries
require(raster)
require(rgdal)
require(dplyr)
require(readr)

# Initial Setup
rm(list = ls())
options(scipen = 999)
setwd('Z:/_cam')

# Load Data
occ1 <- read_csv('_points/_csv/4_occ_swd.csv') %>% dplyr::select(Lon:Lat)
occ2 <- read_csv('_points/_new/_second/coordsAllWGS.csv')
occ  <- rbind(occ1, occ2)

shp <- shapefile('_shp/_base/all_countries.shp')
gtm <- shapefile('_shp/_base/_admCountries/GTM_adm1.shp')

raster::extract(shp, occ2[,1:2])
int <- raster::extract(gtm, occ2[,1:2]); unique(int$NAME_0)

write.csv(occ, '_points/_csv/6_occ_swd.csv', row.names = F)
