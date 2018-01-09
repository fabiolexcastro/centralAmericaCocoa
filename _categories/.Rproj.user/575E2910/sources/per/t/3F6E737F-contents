
# CIAT, 2017
# Author: Mesa & Castro 
# Target: Impact gradient cluster

library(ggalt)
library(hrbrthemes)
library(tidyverse)
library(raster)
library(rgdal)
library(magrittr)

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

# Execute
all_options <- read_csv('_tables/classesImpGraLimMix.csv')
years <- c(2030, 2050)
labelss <- data.frame(value = c(0, 1, 2, 3, 4, 5), category = c('Unsuit', 'cope', 'adjust', 'transform', 'opportunity', 'resilience'))# labelss <- data.frame(value = c(1, 2, 3, 4, 5, 6), category = c('cope', 'adjust', 'transform', 'opportunity', 'limitations', 'limitations')) #NEW

run <- '_run6'
current <- raster(paste0('_RF/', run, '/_results/_process/_percentil0_5/_mixed/RF_5Classes_unccurrent.asc'))
future_30 <- raster(paste0('_RF/', run, '/_results/_process/_percentil0_5/_mixed/RF_5Classes_unc_2030.asc'))
future_50 <- raster(paste0('_RF/', run, '/_results/_process/_percentil0_5/_mixed/RF_5Classes_unc_2050.asc'))

unique(all_options$current)
unique(all_options$future)
unique(all_options$category)
unique(current[])
unique(future_30[])
unique(future_50[])

# Impact gradient
reclassify_options <- function(x, y, labels, options){

  require(tidyverse)
  # x <- current #Proof
  # y <- future_50 #Proof
  
  temp_raster    <- x
  name_raster_x  <- names(x)
  name_raster_y  <- names(y)
  
  coordinates_df <- coordinates(x)
  x              <- as.data.frame(raster::extract(x, coordinates_df, cellnumbers = TRUE))
  number_cell    <- dplyr::select(x, cells)
  x              <- select_(x, name_raster_x)
  colnames(x)    <- 'current'
  
  y              <- as.data.frame(raster::extract(y, coordinates_df[,c('x', 'y')], cellnumbers = TRUE))
  y              <- select_(y, name_raster_y)
  colnames(y)    <- 'future'
  
  z              <- data.frame(x, y, number_cell) %>%
                      as_tibble()
  
  print("Results...")
  sort(unique(z$current))
  sort(unique(z$future))
  results        <- left_join(z, all_options, by = c('current', 'future'))
  labels         <- as_tibble(labelss) 
  unique(results$category)
  
  # results        <- results %>%
  #                     mutate(category = as.factor(category))
  
  results_1      <- left_join(results, labels, by = 'category') %>%#full_join
                      dplyr::select(value) %>%
                        extract2(1)
  
  length(results_1)
  print("Done!")
  
  temp_raster    <- raster::setValues(temp_raster, results_1)
  
  return(temp_raster)
  
}

# raster_fut <- list(future_30, future_50)
# resultsss <- lapply(1:length(raster_fut), function(x){ 
#   
#                 resultss <- reclassify_options(current, raster_fut[[x]], labells, all_options)
#                 return(resultss)
#                 
#               })
# z_2030 <- resultsss[[1]]

z_2030      <- reclassify_options(current, future_30, labelss, all_options)

system.time(expr = {
  
  z_2050 <- reclassify_options(current, future_50, labelss, all_options)  
  
})

writeRaster(z_2030, paste0('_RF/', run, '/_results/_process/_percentil0_5/_impGra/Cluster_change_2030.asc'), overwrite = TRUE)
writeRaster(z_2050, paste0('_RF/', run, '/_results/_process/_percentil0_5/_impGra/Cluster_change_2050.asc'), overwrite = TRUE)


require(raster)
tst <- raster('Z:/_cam/_RF/_run6/_results/_process/RF_5Classes_unccurrent.asc')
unique(tst[])
hist(tst[])
table(tst[])
