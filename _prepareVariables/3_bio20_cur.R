
# Calculate bioclimatic 20 to Central America
# Bunn & Castro
# CIAT, 2017

# Load Libraries

library(rgdal)
library(raster)
library(maptools)
library(maps)
library(mapdata)
library(tidyverse)
library(stringr)
library(gtools)

# Files

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

# Necessary function

bio20 <- function(files, path_output, threshold, mask){
  
  layers <- list.files(files, full.names = T, pattern = '.asc$') %>%
               mixedsort() %>%
               grep('prec', ., value = T)
  
  preclayers <- stack(layers)
  precbin <- reclassify(preclayers, c(-Inf, threshold, 1, threshold, Inf, NA))
  names(precbin) <- names(preclayers)
  twoyears   <- addLayer(precbin, precbin)
  allperiods <- stack()
  
  print('Stack ...')
  
  for(i in 1:12){
    
    oneyear <- twoyears[[i:(i+11)]]
    drymonths <- cumsum(oneyear)
    maxnumber <- max(drymonths, na.rm = T)
    allperiods <- addLayer(allperiods, maxnumber)
    
    rm(maxnumber)
    rm(drymonths)
    
  }
  
  bio_20 <- max(allperiods, na.rm = T)
  bio_20[is.na(bio_20)] <- 0
  bio_20 <- raster::mask(bio_20, mask)
  
  print('Write Raster')
  
  writeRaster(bio_20, paste(path_output, 'bio_20.asc', sep = '/'))
  
  rm(allperiods)
  
}

bio20(files = paste0(path, '/_raster/_climate/_current/_asc'),
      path_output = paste0(path, '/_raster/_climate/_current/_asc'),
      threshold = 100,
      mask = raster(paste0(path, '/_raster/_climate/_current/_asc/bio_1.asc')))



