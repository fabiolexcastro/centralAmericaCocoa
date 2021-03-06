# CIAT, 2016 Updated: February 2017
# Author: Bunn & Castro
# Target: predict RF Future

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
library(foreach)
library(doMC)
library(doSNOW)

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

# load(paste0(path, '/_rData/_run2/clusterpresdata.rData'))
run <- '_run6'
load(paste0('_RF/', run, '/_models/rflist_5.rData'))

NumberOfClusters <- 5
ar5biofolder <- '_raster/_climate/_future/_rcp60/_asc'
yearlist <- list.files(ar5biofolder)
gcmlist <- list.files(paste0(ar5biofolder, '/', yearlist[1]))
resultsfolder <- paste0('_RF/', run, '/_results/_raw') 
modelfolder <- paste0('_RF/', run, '/_models')

rff <- do.call(randomForest::combine, rflist)
myproj <- CRS("+proj=longlat +datum=WGS84")

# To windows
rasterOptions(tmpdir = '_temp')
cl <- makeCluster(32) #N?mero de nucleos a utilizar #.export = 'rasterRFprob'
registerDoSNOW(cl)

# registerDoMC(3)#1:length(gcmlist)

y <- 2

foreach(i = 1:length(gcmlist), .packages = c('raster', 'dplyr', 'gtools', 'foreach', 'randomForest', 'sp', 'stringr')) %dopar% {
  
  print(gcmlist[i]) 
 
  gcmfiles <- paste(ar5biofolder, yearlist[y], gcmlist[i], sep = '/') %>%
                  list.files(., full.names = T, pattern = '.asc$') %>% 
                  grep('bio', ., value = T) %>%  
                  mixedsort()
  
  climatelayers <- stack(gcmfiles)
  climatevalues <- data.frame(getValues(climatelayers))
  
  print('Climate values')
  
  #
  rasterProbs <- predict(rff, climatevalues, type = 'prob') # proximity = T
  rasterRF <- rowSums(rasterProbs[,c(3:(NumberOfClusters+2))])
  uncertainty <- apply(rasterProbs, 1, max)  
  
  rasterRFprob <- climatelayers[[1]]
  values(rasterRFprob) <- rasterRF 
  
  rasterRFuncertainty <- climatelayers[[1]]
  values(rasterRFuncertainty) <- uncertainty 
  
  rasterRF <- max.col(rasterProbs, 'first')
  rasterRFclass <- climatelayers[[1]]
  values(rasterRFclass) <- rasterRF
  
  # Write Raster
  
  print("Write Raster...")

  writeRaster(rasterRFclass, paste(resultsfolder, '/', yearlist[y], '/RF_', NumberOfClusters, 'Clust_', gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = F)
  writeRaster(rasterRFprob, paste(resultsfolder, '/', yearlist[y], '/RF_', NumberOfClusters, 'Prob_',  gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = F)
  writeRaster(rasterRFuncertainty, paste(resultsfolder, '/', yearlist[y], '/RF_', NumberOfClusters, 'Unc_',   gcmlist[i], yearlist[y], '.asc', sep=''),  format = 'ascii', overwrite = F)
  
  print('Done!')
  print(gcmlist[i])
  
  # removeTmpFiles(h = 0)
  
}

 # other way to generate the results
print('Cluster')

rasterClust                <- raster::predict(rff, climatevalues)
rasterRFclust_mask         <- climatelayers[[21]]
values(rasterRFclust_mask) <- rasterClust

# Probabilistic

print('Probabilistic...')

rasterProbs            <- predict(rff, climatevalues, type = "prob")
rasterRF               <- rowSums(rasterProbs[,3:7]) #esto mide la certidumbre del modelo
uncertainty            <- apply(rasterProbs, 1, max)   #valor m?ximo por fila

rasterRFprob                <- climatelayers[[21]]
values(rasterRFprob)        <- rasterRF 
rasterRFuncertainty         <- climatelayers[[21]] # 
values(rasterRFuncertainty) <- uncertainty

#
y <- list.files('Z:/_cam/_RF/_run2/_results/_raw/_2030', full.names = T, pattern = '2050')

lapply(1:length(y), function(x){
  
  file.copy(from = y, to = 'Z:/_cam/_RF/_run2/_results/_raw/_2050')
  
})


# ----------------------------------------------
# Other way that is for RF model

for (i in 1:length(gcmlist)){ 
  
  gcmfiles              <- list.files(paste(ar5biofolder, yearlist[1], gcmlist[i], sep="/"), full.names = T)
  climatelayers         <- stack(gcmfiles)
  names(climatelayers)  <- str_replace(names(climatelayers), "_100", "")
  climatelayers         <- climatelayers[[descriptors]]
  
  print(gcmlist[i])
  
  registerDoMC(19) #cantidad de nucleos a utilizar
  
  this                     <- foreach(repe = 1:25,.packages=c("randomForest","raster","sp", "stringr")) %dopar% {
    
    if (isTRUE(file.exists(paste(resultsfolder,"Clusterfuture/Intermediates/RF_",NumberOfClusters,"clust_","rep_",repe,gcmlist[i],".asc",sep="")))) {
      #nothing
    }else{
      
      load(file = paste(modelfolder,"RF_",NumberOfClusters,"Prob_","rep_",repe,".rdata", sep = "")) #uno a uno
      print("rf")
      
      coff_rf   <- raster::predict(climatelayers, rfmodel, inf.rm=T) #se utiliza para un solo modelo #inf.rm remueve valores que no son finitos, es deicr los infinitos
      coff_rf   <- coff_rf+1 #para que se le suma 1 al raster?
      
      #writeRaster(coff_rf, paste(resultsfolder,"Clusterfuture/Intermediates/RF_",NumberOfClusters,"clust_","rep_",repe, gcmlist[i],".asc",sep=""),format="ascii",overwrite=T) # RAW
      writeRaster(coff_rf, paste(resultsfolder,"Clusterfuture/Intermediates/RF_",NumberOfClusters,"clust_","rep_",repe, gcmlist[i],".asc",sep=""),format="ascii",overwrite=T)
      
    }
    
  }
}    

