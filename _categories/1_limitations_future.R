
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
  setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam')
} else {
  if(OSys == 'Windows'){
  setwd('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}
cat('\f')

# Function
mkdirs <- function(fp) {
  
    if(!file.exists(fp)) {
      mkdirs(dirname(fp))
      dir.create(fp)
    }
  
} 

limitations <- function(path_lyr_prob, path_lyr_clust, path_output, nameOutput, no.clusters, no.absenceclasses){
  
  require(raster)
  require(rgdal)
  require(tidyverse)
  
  lyr_prob  <- raster(path_lyr_prob)
  lyr_clust <- raster(path_lyr_clust)
  
  mtx_prob  <- matrix(c(0, threshold, 0, threshold, 1, 2), ncol = 3, byrow = T)
  mtx_clust <- matrix(c(0.5, no.absenceclasses + 0.5, 0, no.absenceclasses + 0.5, no.absenceclasses + no.clusters + 0.5, 1), nrow = 2, byrow = T)
  
  lyr_prob_rcl  <- raster::reclassify(lyr_prob, mtx_prob)
  lyr_clust_rcl <- raster::reclassify(lyr_clust, mtx_clust)
  
  diff <- lyr_prob_rcl - lyr_clust_rcl
  result <- lyr_clust
  result[which(diff[] == -1)] <- no.absenceclasses + no.clusters + 1
  result[which(diff[] == 2)]  <- no.absenceclasses + no.clusters + 1
  
  print('To Write')
  
  writeRaster(result, paste(path_output, nameOutput, sep = '/'))
  
}

# Code execute
run <- '_run6'
# load(paste0('_rData/', run, '/threshold_prob.rData'))
load(paste0('_rData/', run, '/threshold_prob05.rData'))
load(file = paste0('_rData/', run, '/clustereddata.rData'))
path_future <- paste0('_RF/', run, '/_results')
years <- c('_2030', '_2050')
lapply(paste0('_RF/', run, '/_results/_process/_limitations/', years), mkdirs)# make output dirs

no.absenceclasses <- 2 
no.clusters <- 5

gcm <- list.files(paste(path_future, '/_raw/', years[1], sep = '/')) %>%
          gsub('RF_5Clust_', '', .) %>%
          gsub('_2030.asc', '', .) %>%
          .[-grep('RF', ., value = F)] 

# registerDoMC(19) 
cl <- makeCluster(length(gcm)) #N?mero de nucleos a utilizar
registerDoSNOW(cl)

foreach(i = 1:length(gcm), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'sp', 'stringr'), .verbose = TRUE) %dopar% {
  
  foreach(j = 1:length(years)) %do% {
    
  print(gcm[i]) 
  
  path_lyr_prob <- paste(path_future, '_raw', years[j], sep = '/') %>%
                      list.files(., full.names = T, pattern = '.asc') %>% 
                      grep('rob', ., value = T) %>%
                      grep(gcm[i], ., value = T, fixed = T) %>%
                      .[1]
  
  path_lyr_clust <- paste(path_future, '_raw', years[j], sep = '/') %>%
                        list.files(., full.names = T, pattern = '.asc') %>% 
                        grep('Clust', ., value = T) %>%
                        grep(gcm[i], ., value = T, fixed = T) %>%
                        .[1]
        
  limitations(path_lyr_prob = path_lyr_prob,
              path_lyr_clust = path_lyr_clust,
              path_output = paste0('_RF/', run, '/_results/_process/_percentil0_5/_limitations/', years[j]),
              nameOutput = paste0('RF_', no.clusters, 'Clust_lim_', gcm[i], '.asc'),
              no.absenceclasses = 2, 
              no.clusters = no.clusters)
  
  }
  
}



limitations(path_lyr_prob = paste0('_RF/', run, '/_results/_raw/_current/RF_5Prob_current.asc'),
            path_lyr_clust = paste0('_RF/', run, '/_results/_raw/_current/RF_5Clust_current.asc'),
            path_output = paste0('_RF/', run, '/_results/_process/_percentil0_5'),
            nameOutput = paste0('RF_', no.clusters, 'Clust_lim_', gcm, '.asc'),
            no.absenceclasses = 2, 
            no.clusters = no.clusters)
