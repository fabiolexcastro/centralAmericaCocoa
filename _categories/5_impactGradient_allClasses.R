
# CIAT, 2017
# Author: Castro 
# Target: Impact gradient cluster amon all the class

# Load libraries
library(ggalt)
library(hrbrthemes)
library(tidyverse)
library(raster)
library(rgdal)
library(magrittr)
library(stringr)

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

# Function to create the convert a Integer
toInt <- function(path_code, path_inp, path_out, nameRas){
  
  sink(path_code)
  cat('import arcpy', fill = T)
  cat('from arcpy import env', fill = T)
  cat('from arcpy.sa import *', fill = T)
  cat(paste0('env.workspace = ', "'", path_inp, "'"), fill = T) 
  cat('arcpy.CheckOutExtension("Spatial")', fill = T)
  cat(paste0('result = ', '"', path_out, '/int_', unlist(strsplit(nameRas, '.asc')), '.tif', '"'), fill = T)
  cat('print "Process"', fill = T)
  cat('arcpy.CheckOutExtension("Spatial")', fill = T)
  cat(paste0('intRaster = ', '"', nameRas, '"'), fill = T)
  cat('outInt = Int(intRaster)', fill = T)
  cat('outInt.save(result)', fill = T)
  cat('print "Finish"', fill = T)
  sink()
  
  system(paste0('python ', path_code)) 
  
}

combOr <- function(path_code, path_wks, nameRas1, nameRas2, path_out){
  
  sink(path_code)
  cat('import arcpy', fill = T)
  cat('from arcpy import env', fill = T)
  cat('from arcpy.sa import *', fill = T)
  cat(paste0('env.workspace = ', "'", path_wks, "'"), fill = T) 
  cat(paste0('inRaster1 = ', "'", nameRas1, "'"), fill = T) 
  cat(paste0('inRaster2 = ', "'", nameRas2, "'"), fill = T) 
  cat('arcpy.CheckOutExtension("Spatial")', fill = T)
  cat('outCOr = CombinatorialOr(inRaster1, inRaster2)', fill = T)
  cat(paste0('outCOr.save(', "'", path_out, "'", ')'), fill = T)
  cat('print"Finish"', fill = T)
  sink()
  
  system(paste0('python ', path_code))

}

# Load data to convert an integer
fls_mxd <- list.files('_RF/_run6/_results/_process/_percentil0_5/_mixed', full.names = T, pattern = '.asc$')
rst_mxd <- basename(fls_mxd)

# Load data to make the combinational or
fls_mxd_int <- list.files('_RF/_run6/_results/_process/_percentil0_5/_mixed', full.names = T, pattern = '.tif$')
rst_mxd_int <- basename(fls_mxd_int)
nms_imp <- c('Cluster_changeAll_2030.tif', 'Cluster_changeAll_2050.tif')

# Execute function to convert a Integer
Map('toInt', path_code = 'W:/_cam/_codes/_py/int_rst.py', path_inp = 'W:/_cam/_RF/_run6/_results/_process/_percentil0_5/_mixed', path_out = 'W:/_cam/_RF/_run6/_results/_process/_percentil0_5/_mixed', nameRas = rst_mxd)

# Execute function 'combinational or'
combOr(path_code = 'W:/_cam/_codes/_py/comOr.py', path_wks = 'W:/_cam/_RF/_run6/_results/_process/_percentil0_5/_mixed', nameRas1 = rst_mxd_int[3], nameRas2 = rst_mxd_int[1], path_out = paste0('W:/_cam/_RF/_run6/_results/_process/_percentil0_5/_impGra/', nms_imp[[1]]))
combOr(path_code = 'W:/_cam/_codes/_py/comOr.py', path_wks = 'W:/_cam/_RF/_run6/_results/_process/_percentil0_5/_mixed', nameRas1 = rst_mxd_int[3], nameRas2 = rst_mxd_int[2], path_out = paste0('W:/_cam/_RF/_run6/_results/_process/_percentil0_5/_impGra/', nms_imp[[2]]))




