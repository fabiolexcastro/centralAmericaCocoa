
# CIAT, 2017 
# Author: Castro F
# Target: Altitude histogram

# Load libraries

library(tidyverse)
library(raster)
library(rgdal)

# Directory workspace

OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){
  path <- "//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam" 
} else {
  if(OSys == "Windows"){
    path <- "//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam"
  }
}

# Function

alt_hist <- function(path_occ, path_srtm, path_graph_output, path_boxplot){
  
  srtm <- raster(path_srtm)
  occ  <- read_csv(path_occ)
  occ_srtm <- raster::extract(srtm, occ[,c('Longitude', 'Latitude')]) %>%
                  cbind(occ, altitude = .)
  
  summ <- summary(occ_srtm$altitude)
  
  gg   <- ggplot(data = occ_srtm, aes(occ_srtm$altitude)) + 
            geom_histogram(breaks = seq(0, (max(occ_srtm$altitude) + 200), 200), alpha = 0.5) +
            xlab('Altitude (m.a.s.l)') +
            ylab('Frequency')
  
  # qplot(altitude, data = occ_srtm, binwidth = 10, width = 5)
  
  boxplot <- ggplot(data = occ_srtm, aes(x = Countries,  y = altitude)) +
                geom_boxplot() +
                xlab('') +
                ylab('Altitude (m.a.s.l)')
  
  # ggsave(gg, file = path_graph_output, width = 8, height = 6)
  # ggsave(boxplot, file = path_boxplot, width = 8, height = 6)
   
  return(list(occ_srtm, summ))

}

result <- alt_hist(path_occ = paste0(path, '/_points/_csv/1_occ_rmDupCell.csv'),
                   path_srtm = 'W:/_data/_srtm/srtm_v41_30s', 
                   path_graph_output = paste0(path, '/_figures/_plots/histAlt.png'),
                   path_boxplot = paste0(path, '/_figures/_plots/boxplotAlt.png'))

result[[2]]

outliers <- filter(result[[1]], altitude > 2000)

nrow(result[[1]])
nrow(filter(result[[1]], Source != 'Random Points GLC 2000'))
nrow(filter(result[[1]], Source == 'Random Points GLC 2000'))

