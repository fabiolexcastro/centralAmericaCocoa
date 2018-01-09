
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)

# Initial setup
rm(list = ls())
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam')
} else {
  if(OSys == 'Windows'){
    setwd('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}
cat('\f')

# Load data
run <- '_run6'
years <- c('_2030', '_2050')

for(i in 1:length(years)){
  
  print(years[i])
  
  lyrs <- paste0('_RF/', run, '/_results/_process/_percentil0_5/_mixed/', years[i]) %>% 
              list.files(full.names = T, pattern = '.asc$') %>%
              stack()
  
  lyr_modal <- raster::modal(lyrs)
  
  writeRaster(lyr_modal, paste0('_RF/', run, '/_results/_process/_percentil0_5/_mixed/', 'RF_5Classes_unc', years[i], '.asc'), overwrite = T)
  
}


imp30 <- raster('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_change_2030.asc')
imp50 <- raster('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_change_2050.asc')

plot(imp30)
plot(imp50)


