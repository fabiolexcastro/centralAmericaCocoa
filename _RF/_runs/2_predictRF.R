
# CIAT, 2016 Updated: February 2017
# Author: Bunn & Castro
# Target: predict RF - Cluster

# Load libraries

library(tidyverse)
library(raster)
library(rgdal)
library(cclust)
library(outliers)
library(dismo)
library(gtools)
library(multcomp)
library(sp)
library(rgeos)
library(outliers)  
library(FactoMineR)
library(pROC)
library(randomForest)
library(stringr)

# Load files

set.seed(1234)

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

run <- '_run6'
# load(paste0(path, '/_rData/', run, '/clusterpresdata.rData'))
load(paste0(path, '/_rData/', run, '/clusterdata.rData'))
load(paste0(path, '/_RF/', run, '/_models/rflist_5.rData'))

gcmlist        <- 'current'
ar5biofolder   <- paste0(path, '/_raster/_climate/_current/_asc')
resultsfolder  <- paste0(path, '/_RF/', run, '/_results/_raw') 
modelfolder    <- paste0(path, '/_RF/', run, '/_models')
gcm            <- gcmlist
toMatch        <- "bio"
gcmfiles       <- list.files(ar5biofolder, full.names = TRUE, pattern = ".asc$") %>% 
                     mixedsort() %>%
                     grep('bio', ., value = T)
climatelayers  <- stack(gcmfiles) 

# Values

climatevalues  <- data.frame(getValues(climatelayers))
rff <- do.call(randomForest::combine, rflist)
rasterClust <- raster::predict(rff, climatevalues) # To cluster be use of modalraster
rasterRFclust_mask <- climatelayers[[1]]
values(rasterRFclust_mask) <- rasterClust

writeRaster(rasterRFclust_mask, paste0(path, '/_RF/', run, '/_results/_raw/_current/RF_5Clust_Current.asc'), overwrite = T)

# Probabilistic

rasterProbs            <- raster::predict(rff, climatevalues, type = "prob")
max(rasterProbs[,1], na.rm = TRUE)
rasterRF               <- rowSums(rasterProbs[,3:7]) 
uncertainty            <- apply(rasterProbs, 1, max)   #valor m?ximo por fila

rasterRFprob           <- climatelayers[[1]]
values(rasterRFprob)   <- rasterRF 
rasterRFuncertainty    <- climatelayers[[1]] 
values(rasterRFuncertainty) <- uncertainty
no.clusters <- 5

writeRaster(rasterRFprob,        paste(resultsfolder, "/_current/RF_", no.clusters, "Prob_", gcm, ".asc", sep=""),  format="ascii", overwrite = T)
writeRaster(rasterRFuncertainty, paste(resultsfolder, "/_current/RF_", no.clusters, "Unc_",  gcm, ".asc", sep=""),   format="ascii", overwrite = T)

plot(rasterRFprob)
title(main = gcm, sub="Suitability")
plot(rasterRFclust_mask)
title(main=gcm,sub="SuitClass")
plot(rasterRFuncertainty)
title(main=gcm,sub="Uncertainty")


