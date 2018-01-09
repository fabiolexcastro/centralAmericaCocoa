# CIAT, 2016 Updated: February 2017
# Author: Bunn & Castro
# Target: means to GCM

# Load libraries

library(tidyverse)
library(raster)
library(rgdal)
library(dismo)
library(gtools)
library(sp)
library(rgeos)
library(stringr)

set.seed(1234)

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

run <- '_run5'
years <- c('_2030', '_2050')
NumberOfClusters <- 5

# Cluster

for(i in 1:length(years)){
  
  print(years[i])
  
  layers         <- paste0(path, '/_RF/', run, '/_results/_raw/', years[i])  %>%
                            list.files(full.names = T, pattern = '.asc$') %>%
                            grep('Clust', ., value = T) %>%
                            stack()
                              
  cluster_raster <- raster::modal(layers)

  uncertainty        <- (modal(layers, freq = T) / nlayers(layers))
  uncertainty[uncertainty == 0] <- NA # Values = 1 are eliminated
  limit                 <- 0.6
  unc_mask              <- reclassify(uncertainty, c(0, limit, 0, limit, 1, 1)) #reclasificación de raster de incertidumbre, usando como umbral el 0.6
  plot(unc_mask)
  
  clusterraster2        <- cluster_raster
  
  print("Reclassify...")
  
  rclm                  <- matrix(c(
                                  #unsuitable
                                  0.5,1.5,(NumberOfClusters+3),
                                  1.5,2.5,(NumberOfClusters+3),
                                  #suitable:  
                                  #1.5,2.5,1,
                                  2.5,3.5,3,
                                  3.5,4.5,4,
                                  4.5,5.5,5,
                                  5.5,6.5,6,
                                  6.5,7.5,7
                                ),ncol = 3, byrow = T)
  
  clusterraster2      <- reclassify(clusterraster2, rclm) 
  clusterraster3      <- clusterraster2 * unc_mask #Model agreement with mode higher than 50% are equal to the mode
  
  clusterraster3[clusterraster2!=  (NumberOfClusters+5)&unc_mask!=1]  <- NumberOfClusters+1 # Idoneo; Model agreement with mode lower than 50% and mode suitable
  clusterraster3[clusterraster2 == (NumberOfClusters+5)&unc_mask!=1]  <- NumberOfClusters+2 # No Idoneo; Model agreement with mode lower than 50% and mode unsuitable
  
  print("Write Raster...")
  
  writeRaster(clusterraster3,     paste(path, '/_RF/', run, '/_results/_process/RF_', NumberOfClusters, "clust", years[i], ".asc", sep = ""), format = "ascii", overwrite = T) 
  writeRaster(cluster_raster, paste(path, '/_RF/', run, '/_results/_process/RF_', NumberOfClusters, "clustModal", years[i], ".asc", sep = ""), format = "ascii", overwrite = T) 

}

# Probabilistic

for(i in 1:length(years)){
  
  print(years[i])
  
  layers           <- list.files(paste0(path, '/_RF/', run, '/_results/_raw/', years[i]), full.names = TRUE, pattern = ".asc$") %>%
                            grep('Prob', ., value = T) %>%
                            stack()
  layers_prob_mean <- mean(layers)
  
  writeRaster(layers_prob_mean, paste0(path, '/_RF/', run, '/_results/_process/RF_', NumberOfClusters, "prob", years[i], ".asc"), format = "ascii", overwrite = TRUE)
  
}





