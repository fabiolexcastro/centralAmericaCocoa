
# CIAT, 2017 
# Author: Castro F
# Target: Altitude histogram

# Load libraries

library(tidyverse)
library(raster)
library(rgdal)
library(rgeos)
library(gtools)
library(foreign)

# Directory workspace

OSys <- Sys.info(); OSys <- OSys[names(OSys)=="sysname"]
if(OSys == "Linux"){
  path <- "//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam" 
} else {
  if(OSys == "Windows"){
    path <- "//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam"
  }
}

occ  <- read.dbf(paste0(path, '/_points/_shp/occ_rmDupCells.dbf'))
# occ <- read_csv(paste0(path, '/_points/_csv/1_occ_rmDupCell.csv'))
files <- list.files(paste0(path, '/_raster/_climate/_current/_asc'), full.names = TRUE, pattern = '.asc$') %>%
            grep('bio_', ., value = T) %>%        
            mixedsort()
lyrs <- stack(files)

occ_swd <- raster::extract(lyrs, occ[,7:8]) %>%
              cbind(occ, .)
occ_swd <- tbl_df(occ_swd)
occ_swd_ok <- tbl_df(occ_swd[complete.cases(occ_swd),])# unique(is.na(occ_swd$bio_21))

nrow(occ_swd) - nrow(occ_swd_ok)

write.csv(occ_swd_ok, paste0(path, '/_points/_csv/2_occ_swd.csv'), row.names = F)



