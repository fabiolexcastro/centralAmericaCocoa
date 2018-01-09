
# CIAT, 2017
# Author: Castro 
# Target: tidy the result of impact gradient (combinational or)

# Load libraries
library(raster)
library(rgdal)
library(tidyverse)
library(rgeos)
library(gtools)
library(foreign)
library(reshape2)

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
options(scipen = 999)
cat('\f')

# Function to use
trImpGra <- function(lyr, tbl, yr){
  
  freq <- table(lyr[]) %>% t() %>% as.data.frame() %>% dplyr::select(Var2, Freq) %>% tbl_df() %>% rename(value = Var2) %>% mutate(value = as.numeric(value))
  colnames(tbl) <- c('value', 'crn', 'ftr')
  tbl  <- dplyr::select(tbl, crn, ftr, value) %>% tbl_df()
  df <- inner_join(freq, tbl, by = c('value' = 'value')) %>% dcast(crn ~ ftr, value.var = 'Freq')
  
  write.csv(df, paste0('_RF/_run6/_results/_process/_percentil0_5/_tbl/impGra_countPix_', yr, '.csv'), row.names = F)
  
  return(df)
  
}

# Load data
fls <- list.files('_RF/_run6/_results/_process/_percentil0_5/_impGra', full.names = TRUE, pattern = '.tif$')
i30 <- grep('2030', fls, value = T) %>% raster()
i50 <- grep('2030', fls, value = T) %>% raster()
t30 <- read.dbf('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_changeAll_2030.tif.vat.dbf')
t50 <- read.dbf('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_changeAll_2050.tif.vat.dbf')

# Execute function
tb_30 <- trImpGra(lyr = i30, tbl = t30, yr = 'i30')
tb_50 <- trImpGra(lyr = i50, tbl = t50, yr = 'i50')



