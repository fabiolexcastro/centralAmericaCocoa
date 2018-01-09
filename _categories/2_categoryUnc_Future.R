
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(rgeos)
require(gtools)
require(stringr)
require(doMC)
require(foreach)
require(doSNOW)

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

# Functions
mkdirs <- function(fp) {
  
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
  
} 

mixtaCategory <- function(pathClust, pathUnc, pathProb, threshold_unc, threshold){
  
  lyrClust <- raster(pathClust)
  lyrUnc   <- raster(pathUnc)
  lyrProb  <- raster(pathProb)# To 2 criterios
 
  result <- lyrClust
  
  result[which(lyrUnc[] < threshold_unc & lyrProb[] > threshold)] <- max(unique(lyrClust[]), na.rm = T) + 1
  
  return(result)
  
}

# Execute functions
run <- '_run6'
load(paste0('_rData/', run, '/threshold_prob.rData'))
load(paste0('_rData/', run, '/threshold_prob05.rData'))
load(paste0('_rData/', run, '/threshold_unc.rData'))
load(file = paste0('_rData/', run, '/clustereddata.rData'))

path_future <- paste0('_RF/', run, '/_results')
years <- c('_2030', '_2050')
models <- list.files(paste0('_RF/_run6/_results/_process/_limitations/', years[1])) %>%
              gsub('RF_5Clust_lim_', '', .) %>%
              gsub('.asc', '', .) 
no.clusters <- 5

# registerDoMC(19)
cl <- makeCluster(length(models)) #Número de nucleos a utilizar
registerDoSNOW(cl)

foreach(i = 1:length(models), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  # print(models[i]) 
  
  foreach(j = 1:length(years)) %do% {
    
    print(years[j])
  
    path_clust <- paste(path_future, '_process/_limitations', years[j], sep = '/') %>%
                    list.files(., full.names = T, pattern = '.asc') %>% 
                    grep('Clust', ., value = T) %>%
                    grep(models[i], ., value = T, fixed = T) %>%
                    .[1]
                  
    path_Unc   <- paste(path_future, '_raw', years[j], sep = '/') %>%
                    list.files(., full.names = T, pattern = '.asc') %>% 
                    grep('Unc', ., value = T) %>%
                    grep(models[i], ., value = T, fixed = T) %>%
                    .[1]
    
    path_prob   <- paste(path_future, '_raw', years[j], sep = '/') %>%
                    list.files(., full.names = T, pattern = '.asc') %>% 
                    grep('Prob', ., value = T) %>%
                    grep(models[i], ., value = T, fixed = T) %>%
                    .[1]
    
    mixta <- mixtaCategory(pathClust = path_clust,
                           pathUnc = path_Unc,
                           pathProb = path_prob,
                           threshold_unc = thrUnc, 
                           threshold = threshold)
    
    writeRaster(mixta, paste0('_RF/', run, '/_results/_process/_percentil0_5/_mixed/', years[j], '/RF_', no.clusters, 'Classes_unc_', models[i], '.asc'), overwrite = T)
  
  }
  
}



