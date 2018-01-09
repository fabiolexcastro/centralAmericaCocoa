
# CIAT, 2016 Updated: February 2017
# Author: Bunn & Castro
# Target: cluster analysis

# Load libraries

library(dplyr)
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

# Initial setup
rm(list = ls())
options(scipen = 9999)
set.seed(1234)
setwd('Z:/_cam')
run <- '_run6'
myproj <- CRS('+proj=longlat +datum=WGS84')
source('_codes/_R/_RF/FunctionsRFclustering.R')

# Load data
load(paste0('_rData/', run, '/clustereddata.rData'))
modelfolder <- paste0('_RF/', run, '/_models')
climatefolder <- paste0('_raster/_climate/_current/_asc')

# clusteredpresdata <- cbind(norm[,1:2], env_values, cluster = labelRF) %>% na.omit() %>% tbl_df()
# lyrs <- list.files(climatefolder, full.names = T, pattern = '.asc$') %>% grep('bio', ., value = T) %>% mixedsort() %>% stack() # clusterpresdata <- occ_cluster

# Load climate data
toMatch <- 'bio' #<- c('bio', 'af')
listado <- list.files(climatefolder, full.names = T, pattern = '.asc$') %>% 
             mixedsort() %>%
             grep(paste0(toMatch, collapse = '|'), ., value = TRUE) %>%
             unique()
climatelayers <- stack(listado)
crs(climatelayers)<- myproj
clusteredpresdata <- occ_cluster
occ_cluster <- clusteredpresdata
SPspecies <- SpatialPoints(occ_cluster[,1:2]) # SPspecies <- SpatialPoints(clusteredpresdata[,1:2]) #Data frame to Shp
crs(SPspecies) <- myproj

bias.raster <- raster('_raster/bias_raster.asc')

# Generation of background (pseudoabsences) - We used 1:1 (1 pseudoabsence for each presence)

back_raster       <- climatelayers[[21]] * 0 + 1 
speciescell       <- raster::extract(back_raster, SPspecies, cellnumber=TRUE)
back_raster[speciescell[,1]]  <- NA #remove the cell with presences
samplesize        <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) #CHANGE #samplesize <- sum(summary(as.factor(clusteredpresdata$cluster))) # dissolvee <- gUnaryUnion(districts_mwi) #dissolve polygon
NumberOfClusters  <- max(clusteredpresdata$cluster) 
ratio             <- NumberOfClusters/1
numberofpresences <- nrow(clusteredpresdata) #numberofpresences <- NumberOfClusters * samplesize

# bias raster - process

back_raster       <- bias.raster
speciescell       <- raster::extract(bias.raster, SPspecies, cellnumber=TRUE)
back_raster[speciescell[,1]]  <- NA #remove the cell with presences
samplesize        <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) #CHANGE #samplesize <- sum(summary(as.factor(clusteredpresdata$cluster))) # dissolvee <- gUnaryUnion(districts_mwi) #dissolve polygon
NumberOfClusters  <- max(clusteredpresdata$cluster) 
ratio             <- NumberOfClusters/1
numberofpresences <- nrow(clusteredpresdata) #numberofpresences <- NumberOfClusters * samplesize

#

back_raster <- resample(back_raster, climatelayers[[21]]) %>%
                  raster::crop(., climatelayers[[21]]) %>%
                  raster::mask(., climatelayers[[21]])

crs(back_raster) <- myproj
back <- randomPoints(back_raster, 1*numberofpresences, prob = T) %>%
            as_data_frame()
coordinates(back) <- ~ x + y
back_swd  <- raster::extract(climatelayers, back) %>% 
                cbind(coordinates(back), .)

nrow(back_swd)
nrow(back_swd[complete.cases(back_swd),])

# back_swd  <- raster::extract(climatelayers, bck_coords[,1:2]) %>% # New line
#                       cbind(bck_coords, .)
# 
# write.csv(back_swd, paste0(path, '/_points/_csv/bck_swd.csv'), row.names = F) #new line


# back_swd2 <- back_swd[sample(nrow(back_swd), numberofpresences),]
# row.names(back_swd) <- 1:nrow(back)

# Cluster analysis to pseudoabsences

env_values_bck <- as.matrix(back_swd[,3:ncol(back_swd)])
datRF <- as.data.frame(env_values_bck)
d <- dist(datRF, method = 'euclidean')

# for(i in 2:ncol(env_values_bck)){ #for(i in 2:20){
#   
#   clusterdata        <- cclust(env_values_bck, i, method = 'kmeans')
#   cat(clustIndex(clusterdata, env_values_bck, index = 'calinski'), '\n')
# }

clusterdata          <- hclust(d, method='ward.D2')
plot(clusterdata, hang = -0.01, cex = 0.7)

for(i in 2:10){
  
  rect.hclust(clusterdata, k = i) 
  
}

no.clusters                <- 2
labelRF                    <- cutree(clusterdata, no.clusters)

back_swd <- tbl_df(back_swd[complete.cases(back_swd),])
env_values_cluster_bck <- cbind(back_swd, labelRF)
bck_coords             <- cbind(back_swd[,c(1:2)], labelRF)

save(bck_coords, file = paste0(path, '/_rData/', run, '/bck_coords.rData'))
write.csv(bck_coords, paste0(path, '/_points/_csv/bck_run5.csv'), row.names = FALSE)

bck_coords <- read.csv(paste0(path, '/_points/_csv/bck_run5.csv'))

# RF

datRF <- as.data.frame(back_swd[,3:ncol(back_swd)]) #Be care with the first number [,1:...]

attach(datRF)
no.forests                 <- 50#raw = 25
no.trees                   <- 500
distRF                     <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absenceclasses          <- 2
labelRF                    <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
# classdata                  <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])
classdata                  <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])

presvalue_swd              <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>%
                                cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
                                na.omit() %>%
                                as.data.frame()

presvalue_swd <- raster::extract(climatelayers, clusteredpresdata[,1:2]) %>%
                    cbind(clusteredpresdata[,1:2], ., clusteredpresdata[,3])
colnames(presvalue_swd)[ncol(presvalue_swd)] <- 'cluster'
presvalue_swd$pb <- presvalue_swd$cluster + 2; bckpres <- presvalue_swd
presvalue_swd    <- dplyr::select(presvalue_swd, -cluster, -Lon, - Lat)
presvalue_swd    <- dplyr::select(presvalue_swd, pb, bio_1:bio_33)

classdata_2                <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)])
dim(classdata_2); dim(presvalue_swd)
nrow(presvalue_swd[complete.cases(presvalue_swd),])
presvalue_swd <- presvalue_swd[complete.cases(presvalue_swd),]

presvalue_swd$pb <- as.factor(presvalue_swd$pb)
allclasses_swd  <- rbind(classdata_2, presvalue_swd[,1:34])

# classdata_3 <- classdata_2[,c(1, 3:ncol(classdata_2))]
# allclasses_swd  <- rbind(classdata_3, presvalue_swd[,1:34])

write.csv(allclasses_swd, paste0(path, '/_points/_csv/all_clases_swd_run5.csv'), row.names = FALSE)

# Model RF

model1                     <- as.formula(paste('factor(pb) ~', paste(paste('bio',c(1:33), sep='_'), collapse = '+',sep=' ')))

rflist                     <- vector('list', 50) 

for (repe in 1:50){ # 50 bosques
  
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      rows          <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                              size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      rows          <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                              size=samplesize)
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species           <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit       <- sample(rownames(species)) 
  
  envtrain          <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest           <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel           <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit 
                                    , nodesize = 2) 
  
  save(rfmodel, file = paste(modelfolder, '/', 'RF_' ,NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]]    <- rfmodel
  
  # AUC 
  
  predicted         <- as.numeric(predict(rfmodel, envtest))
  observed          <- as.vector(envtest[,'pb'])
  auc               <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc,'\n')
  
}

rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)
class(importance)

save(rflist, file = paste(modelfolder, '/rflist_', NumberOfClusters, '.rdata', sep=''))
save(importance, file = paste0(path, '/_rData/', run, '/importanceRF.rData'))
save(auc, file = paste0(path, '/_rData/', run, '/aucRF_dist.rData'))
save(rff, file = paste0(path, '/_rData/', run, '/rff_dist.rData'))
