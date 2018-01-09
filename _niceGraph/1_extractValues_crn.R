
# Load libraries
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(gtools)

# Initial setup
rm(list = ls())
options(scipen = 999)
setwd('Z:/_cam')
myproj <- '+proj=longlat +datum=WGS84 +no_defs'
cat('\f')

# Load data
load('_rData/_run6/clustereddata.rData'); rm(clusteredpresdata, clusterdata, labelRF, no.clusters)

vrs <- c('prec', 'tmax', 'tmean', 'tmin')
crn <- list.files('_raster/_climate/_current/_asc', full.names = TRUE, pattern = '.asc$') %>% 
          mixedsort() %>% 
          grep(paste0(vrs, collapse = '|'), ., value = T) %>% 
          stack()

# Extract values to current
crn_vls <- raster::extract(crn, occ[,1:2]) %>% 
              cbind(occ[,1:2], .) %>% 
              tbl_df() %>% 
              select(-c(Lon, Lat))
crn_vls <- colMeans(crn_vls, na.rm = T)
crn_vls <- t(data.frame(crn_vls))

write.csv(crn_vls, '_workspace/_graphPointsCocoa/_current/zonal_current_mean.csv', row.names = F)
