
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
load(paste0('_rData/', run, '/threshold_prob.rData'))
load(paste0('_rData/', run, '/threshold_prob05.rData'))
load(file = paste0('_rData/', run, '/clustereddata.rData'))
gcm <- 'current'

pathClust <- paste0('_RF/', run, '/_results/_raw/_current/RF_5Clust_lim_current.asc')
pathUnc   <- paste0('_RF/', run, '/_results/_raw/_current/RF_5Unc_current.asc')
pathProb  <- paste0('_RF/', run, '/_results/_raw/_current/RF_5Prob_current.asc')
path_output <- paste0('_RF/', run, '/_results/_raw/_current')
threshold   <- threshold #Prob
no.absenceclasses <- 2
no.clusters <- 5

clusterpresdata <- read.csv('_points/_csv/_run6/6_occ_swd_rmOtl.csv')

mixedCategory <- function(pathClust, pathUnc, pathProb, path_output, threshold, no.absenceclasses){
  
  occ <- clusterpresdata[,1:2]
  lyrClust <- raster(pathClust)
  lyrUnc   <- raster(pathUnc)
  lyrProb  <- raster(pathProb)
  
  thrUnc   <- raster::extract(lyrUnc, occ[,1:2]) 
  thrUnc   <- thrUnc[!is.na(thrUnc)]#quantile(thrUnc, seq(0, 1, 0.01))
  thrUnc1  <- quantile(thrUnc, 0.1) %>% # 10%
                as.numeric()
  min(thrUnc)
  # values   <- raster::extract(lyrUnc, occ[,1:2])
  # quantile(values, seq(0,1,0.01))
  
  result <- lyrClust
  
  result[which(lyrUnc[] < thrUnc1 & lyrProb[] > threshold)] <- max(unique(lyrClust[]), na.rm = T) + 1 # 
  
  print('To Write Raster')
  
  writeRaster(result, paste0(path_output, '/RF_', no.clusters, 'Classes_unc', gcm, '.asc'), overwrite = T)
  
  return(result)
  
}


mixta <- mixedCategory(pathClust = paste0('_RF/', run, '/_results/_process/RF_5Clust_lim_current.asc'),
                       pathUnc = paste0('_RF/', run, '/_results/_raw/_current/RF_5Unc_current.asc'),
                       pathProb = paste0('_RF/', run, '/_results/_raw/_current/RF_5Prob_current.asc'),
                       path_output = paste0('_RF/', run, '/_results/_process'),
                       threshold = threshold,
                       no.absenceclasses = 2)

mixta <- mixedCategory(pathClust = paste0('_RF/', run, '/_results/_process/_percentil0_5/RF_5Clust_lim_current.asc'),
                       pathUnc = paste0('_RF/', run, '/_results/_raw/_current/RF_5Unc_current.asc'),
                       pathProb = paste0('_RF/', run, '/_results/_raw/_current/RF_5Prob_current.asc'),
                       path_output = paste0('_RF/', run, '/_results/_process/_percentil0_5'),
                       threshold = threshold,
                       no.absenceclasses = 2)



occ <- clusterpresdata[,1:2]
pathUnc <- paste0('_RF/', run, '/_results/_raw/_current/RF_5Unc_current.asc')

thrUnc <- raster::extract(raster(pathUnc), occ[,1:2]) %>%
             quantile(0.1) %>% # Quantile 10%
             as.numeric()

quantile(test, seq(0, 1, 0.01))

save(thrUnc, file = paste0('_rData/', run, '/threshold_unc.rData'))

# write.csv(occ, paste0(path, '/_points/_csv/occ_final.csv'), row.names = F)


## ---

# a <- raster('Z:/_cam/_RF/_run2/_results/_process/_mixed/RF_5Classes_unc_current.asc')
# b <- raster(paste0(path_output, '/RF_', no.clusters, 'Classes_unc_test', gcm, '.asc'))
# 
# hist(a)
# hist(b)
