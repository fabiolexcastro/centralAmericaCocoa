
# Load libraries
library(raster)
library(rgdal)
library(dplyr)
library(tidyr)
library(sf)
library(gtools)
library(rgeos)
library(doSNOW)
library(foreach)
library(parallel)
library(doMC)

# Initial setup
rm(list = ls())
options(scipen = 999)
setwd('Z:/_cam')
myproj <- '+proj=longlat +datum=WGS84 +no_defs'
cat('\f')

# Load Data
load('_rData/_run6/clustereddata.rData'); rm(clusteredpresdata, labelRF, no.clusters)

vrs <- c('prec', 'tmax', 'tmean', 'tmin')
gcms <- list.files('_raster/_climate/_future/_rcp60/_asc/_2030', full.names = F)
yrs <- c('_2030', '_2050')

y <- 1

# Extract values
cl <- makeCluster(19)
registerDoSNOW(cl)

# registerDoMC()

foreach(i = 1:length(gcms), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'sp'), .verbose = TRUE) %dopar% {
  
  fls <- list.files(paste0('_raster/_climate/_future/_rcp60/_asc/', yrs[y], '/', gcms[i]), full.names = T, pattern = '.asc$') %>%
            .[-grep('bio', ., value = F)] %>%
            .[-grep('cons_mths', ., value = F)] %>%
            mixedsort() %>%
            stack()
  
  df <- raster::extract(fls, occ[,1:2]) %>% 
            cbind(., occ[,1:2]) %>% 
            tbl_df() %>% 
            dplyr::select(-c(Lon, Lat))
  mns <- colMeans(df, na.rm = T) %>% t() %>% data.frame()
  mns <- mutate(mns, zone = 1:nrow(mns)) %>% select(zone, prec_1:tmin_12)
  
  write.csv(mns, paste0('_workspace/_graphPointsCocoa/', yrs[y], '/mean_', gcms[i], '_', yrs[y], '.csv'), row.names = F)
  
}

# Another way
lapply(1:length(yrs), function(y){
  
  print(yrs[y])
  
  lapply(1:length(gcms), function(i){ 
    
    print(gcms[i])
    
    fls <- list.files(paste0('_raster/_climate/_future/_rcp60/_asc/', yrs[y], '/', gcms[i]), full.names = T, pattern = '.asc$') %>%
              .[-grep('bio', ., value = F)] %>%
              .[-grep('cons_mths', ., value = F)] %>%
              mixedsort() %>%
              stack()
    
    df <- raster::extract(fls, occ[,1:2]) %>% 
              cbind(., occ[,1:2]) %>% 
              tbl_df() %>% 
              dplyr::select(-c(Lon, Lat))
    mns <- colMeans(df, na.rm = T) %>% t() %>% data.frame()
    mns <- mutate(mns, zone = 1:nrow(mns)) %>% select(zone, prec_1:tmin_12)
    
    write.csv(mns, paste0('_workspace/_graphPointsCocoa/', yrs[y], '/mean_', gcms[i], '_', yrs[y], '.csv'), row.names = F)
    
  }) 
  
})

