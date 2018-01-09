
# CIAT, 2017
# Author: Castro 
# Target: Extremes scenarios

# Load  libraries
library(tidyverse)
library(raster)
library(rgdal)
library(magrittr)

# Initial setup
rm(list = ls())
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//dapadfs/workspace_cluster_9/Coffee_Cocoa/_cam')
} else {
  if(OSys == 'Windows'){
    setwd('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}
options(scipen = 999)
cat('\f')

# Load data
run <- '_run6'
load(paste0('_rdata/', run, '/clustereddata.rData'))

# Function
calc_mean_crn <- function(path){
  
  result <- raster(path) %>% raster::extract(., occ[,c('Lon', 'Lat')]) %>% mean()
  
}

calc_mean_ftr <- function(path, var){
  
  # path <- '_raster/_climate/_future/_rcp60/_asc'
  # var <- 'bio_1.asc'
  
  yrs <- list.files(path, full.names = F)
  mdl <- list.dirs(paste(path, yrs[1], sep = '/'), full.names = F, recursive = F)
  fls <- list.files(paste0(path), full.names = T, pattern = '.asc$', recursive = T)
  stk <- grep(var, fls, value = T) %>% grep('2050', ., value = T) %>% stack()
  vls <- raster::extract(stk, occ[,c('Lon', 'Lat')])
  colnames(vls) <- c(paste(mdl, yrs[2], sep = ''))
  
  df <- apply(vls, 2, mean) %>% 
    as.data.frame() %>% 
    rownames_to_column(var = 'Model') %>% 
    mutate(var = unlist(strsplit(var, '.asc')),
           period = 2050) 
  
  colnames(df) <- c('Model', 'Valor', 'var', 'period')
  df <- dplyr::select(df, var, period, Model, Valor)
  
  df$Model <- gsub('_2050', '', df$Model)
  
  return(df)
  
}

# Extraction values current
crn_bio1 <- calc_mean_crn(path = '_raster/_climate/_current/_asc/bio_1.asc')
crn_bio12 <- calc_mean_crn(path = '_raster/_climate/_current/_asc/bio_12.asc')
df_crn <- data.frame(crn_bio1 = crn_bio1, crn_bio12 = crn_bio12) %>% 
            t() %>% 
            as.data.frame() %>% 
            rename(Valor = V1) %>% 
            mutate(var = c('bio_1', 'bio_12'),
                   period = 'current',
                   Model = 'current') %>%
            dplyr::select(var, period, Model, Valor)

# Extraction values Future
ftr_bio1 <- calc_mean_ftr(path = '_raster/_climate/_future/_rcp60/_asc', var = 'bio_1.asc')
ftr_bio12 <- calc_mean_ftr(path = '_raster/_climate/_future/_rcp60/_asc', var = 'bio_12.asc')

# Join data frames
df_all <- rbind(df_crn, ftr_bio1, ftr_bio12); rm(df_crn, ftr_bio1, ftr_bio12, crn_bio1, crn_bio12)

# Calc differences
bio1_crn <- filter(df_all, var == 'bio_1', period == 'current') %>% extract2(4)
bio1_ftr <- filter(df_all, var == 'bio_1', period == '2050') %>% extract2(4)
bio1_dif <- bio1_ftr - bio1_crn
bio1_dif <- data.frame(Model = extract2(filter(df_all, var == 'bio_1', period == '2050'), 3), Valor = bio1_dif)

bio12_crn <- filter(df_all, var == 'bio_12', period == 'current') %>% extract2(4)
bio12_ftr <- filter(df_all, var == 'bio_12', period == '2050') %>% extract2(4)
bio12_dif <- bio12_ftr - bio12_crn
bio12_dif <- data.frame(Model = extract2(filter(df_all, var == 'bio_12', period == '2050'), 3), Valor = bio12_dif)

# Temperature increase & precipitation reduction
summary(bio1_dif$Valor)
summary(bio12_dif$Valor)

bio1_dif[bio1_dif$Valor > 20.23,]
bio12_dif[bio12_dif$Valor < 0,] #mohc_hadgem2_es

# Intermediate
summary(bio1_dif$Valor)
summary(bio12_dif$Valor)

bio1_dif[bio1_dif$Valor > 15.75 & bio1_dif$Valor < 19.75,]
bio12_dif[findInterval(mean(bio12_dif$Valor), sort(bio12_dif$Valor)),] #nimr_hadgem2_ao

# Low increase in the temperature & increase in the precipitation
summary(bio1_dif$Valor)
summary(bio12_dif$Valor)

bio1_dif[bio1_dif$Valor < 14.99,]
bio12_dif[bio12_dif$Valor > 73.41,] #gfdl_esm2m






