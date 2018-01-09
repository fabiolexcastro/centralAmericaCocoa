
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(sf)
require(readxl)
require(stringr)

# Initial setups
rm(list = ls())
options(scipen = 999)
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam')
} else {
  if(OSys == 'Windows'){
    setwd('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}; rm(OSys)

# Load data
gtm <- shapefile('_shp/_base/_admCountries/GTM_adm0.shp')
alta <- read_excel('_points/_gtm/AltaVerapaz.xlsx', sheet = 2) %>%
          dplyr::select(Pais, Lon, Lat)
costa <- read_excel('_points/_gtm/CostaSur.xlsx', sheet = 2) %>%
          dplyr::select(Pais, Lon, Lat)
df <- rbind(alta, costa); rm(alta, costa)

write.csv(df, '_points/_gtm/joinGTM.csv')

occ <- read_csv('_points/_csv/6_occ_swd.csv')
df <- rbind(occ, df[,2:3])

write.csv('_points/_csv/6_occ_swd2.csv')



