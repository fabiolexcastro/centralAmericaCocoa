
# CIAT, 2016 Updated: February 2017
# Author: Bunn & Castro
# Target: cluster analysis

# Load libraries
library(dplyr)
library(raster)
library(rgdal)
library(cclust)
library(dismo)
library(gtools)
library(sp)
library(rgeos)
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
source('_codes/_R/_RF/_run6/FunctionsRFclustering.R')

# Function
rf.clust <- function(occ, nforest, ntrees, nVars, nclasses){
  
  datRF_presences <- occ[,3:ncol(occ)]
  print(nrow(datRF))
  
  attach(datRF_presences)
  no.forests <- nforest
  no.trees <- ntrees
  distRF_presences <- RFdist(datRF_presences, mtry1 = nVars, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)
  no.presencesclasses <- nclasses
  labelRF <- pamNew(distRF_presences$cl1, no.presencesclasses)
  print(table(labelRF))
  clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')
  
  return(list(labelRF, clusterdata))
  
}

# Load data
load(paste0('_rData/', run, '/clustereddata.rData'))
if(!file.exists('_RF/_run6/_models')){ dir.create('_RF/_run6/_models')}
modelfolder <- paste0('_RF/', run, '/_models')
climatefolder <- '_raster/_climate/_current/_asc'

# clusteredpresdata <- cbind(norm[,1:2], env_values, cluster = labelRF) %>% na.omit() %>% tbl_df()
# lyrs <- list.files(climatefolder, full.names = T, pattern = '.asc$') %>% grep('bio', ., value = T) %>% mixedsort() %>% stack() # clusterpresdata <- occ_cluster

# Load climate data
toMatch <- 'bio' #<- c('bio', 'af')
bias.raster <- raster('_raster/bias_raster.asc')
listado <- list.files(climatefolder, full.names = T, pattern = '.asc$') %>% 
             mixedsort() %>%
             grep(paste0(toMatch, collapse = '|'), ., value = TRUE) %>%
             unique()
climatelayers <- stack(listado)
crs(climatelayers)<- myproj
occ_cluster <- clusteredpresdata
SPspecies <- SpatialPoints(occ_cluster[,1:2]) # SPspecies <- SpatialPoints(clusteredpresdata[,1:2]) #Data frame to Shp
crs(SPspecies) <- myproj

# Generation of background (pseudoabsences) - We used 1:1 (1 pseudoabsence for each presence)
back_raster <- climatelayers[[21]] * 0 + 1 
speciescell <- raster::extract(back_raster, SPspecies, cellnumber=TRUE)
back_raster[speciescell[,1]]  <- NA #remove the cell with presences
samplesize      <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) #CHANGE #samplesize <- sum(summary(as.factor(clusteredpresdata$cluster))) # dissolvee <- gUnaryUnion(districts_mwi) #dissolve polygon
NumberOfClusters  <- max(clusteredpresdata$cluster) 
ratio             <- NumberOfClusters/1
numberofpresences <- nrow(clusteredpresdata) #numberofpresences <- NumberOfClusters * samplesize

# bias raster - process
back_raster <- bias.raster
speciescell <- raster::extract(bias.raster, SPspecies, cellnumber = TRUE)
back_raster[speciescell[,1]]  <- NA #remove the cell with presences
samplesize  <- round(min(summary(as.factor(clusteredpresdata$cluster))) / 2, 0) #CHANGE #samplesize <- sum(summary(as.factor(clusteredpresdata$cluster))) # dissolvee <- gUnaryUnion(districts_mwi) #dissolve polygon
NumberOfClusters <- max(clusteredpresdata$cluster) 
ratio <- NumberOfClusters/1
numberofpresences <- nrow(clusteredpresdata) #numberofpresences <- NumberOfClusters * samplesize

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

if(!file.exists(' _points/_csv/_run6/bck_swd.csv')){
  write.csv(back_swd, paste0('_points/_csv/_run6/bck_swd.csv'), row.names = F) #new line
}

# Cluster analysis to pseudoabsences

bckclust <- rf.clust(occ = back_swd, nforest = 50, ntrees = 500, nVars = 8, nclasses = 2)
datRF <- as.data.frame(back_swd[,3:ncol(back_swd)])
attach(datRF)
no.forests <- 50#raw = 25
no.trees <- 500
distRF <- RFdist(datRF, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp =T, oob.prox1 = T)# mtry1 = 4 raw  # es la cantidad de variables a utilizar en cada no
no.absenceclasses <- 2
labelRF <- pamNew(distRF$cl1, no.absenceclasses)
detach(datRF)
classdata <- cbind(pb = as.factor(labelRF), back_swd[,3:ncol(back_swd)])

# Join presences and background
presvalue_swd  <- clusteredpresdata[,3:ncol(clusteredpresdata)] %>%
                    cbind(pb = (clusteredpresdata$cluster + no.absenceclasses), .) %>%
                    na.omit() %>%
                    as.data.frame() %>%
                    mutate(cluster = cluster + no.absenceclasses)
presvalue_swd <- dplyr::select(presvalue_swd, pb, bio_1:bio_33)
presvalue_swd <- mutate(presvalue_swd, pb = as.factor(pb))

classdata_2 <- cbind(pb = as.data.frame(classdata)$pb, classdata[,2:ncol(classdata)]) # Background
dim(classdata_2); dim(presvalue_swd)
allclasses_swd <- rbind(classdata_2, presvalue_swd)
unique(allclasses_swd$pb)

write.csv(allclasses_swd, paste0(path, '/_points/_csv/all_clases_swd_run5.csv'), row.names = FALSE)

# Model RF
model1 <- as.formula(paste('factor(pb) ~', paste(paste('bio',c(1:33), sep='_'), collapse = '+', sep =' ')))
rflist <- vector('list', 50) 
auc <- vector('list', 50)

for(repe in 1:50){ # 50 bosques
  
  print(repe)
  pressample <- list()
  
  for (i in 1:(NumberOfClusters+no.absenceclasses)){
    
    if(any(i==c(1:no.absenceclasses))) { 
      
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), 
                              size = samplesize*NumberOfClusters/2/no.absenceclasses)
    } else {
      rows <- sample(rownames(allclasses_swd[allclasses_swd$pb==i,]), size=samplesize)
    }
    pressample[[i]] <- allclasses_swd[rows,] 
  }
  
  species <- na.omit(do.call(rbind, pressample)) 
  head(species)
  Samplesplit <- sample(rownames(species)) 
  
  envtrain <- species[Samplesplit[1:(0.8*nrow(species))],] 
  envtest <- species[Samplesplit[(0.8*nrow(species)):nrow(species)],] 
  
  rfmodel <- randomForest(model1, data = envtrain, ntree = 500, na.action = na.omit, nodesize = 2) 
  
  save(rfmodel, file = paste(modelfolder, '/', 'RF_' ,NumberOfClusters, 'Prob_' , 'rep_' ,repe, '.rdata' ,sep=''))
  rflist[[repe]] <- rfmodel
  
  # AUC 
  predicted <- as.numeric(predict(rfmodel, envtest))
  observed <- as.vector(envtest[,'pb'])
  auc[[repe]] <- auc(observed, predicted) 
  rm(rfmodel)
  
  cat(auc[[repe]] ,'\n')
  
}

auc <- unlist(auc)
rff <- do.call(randomForest::combine, rflist)
importance <- as.data.frame(rff$importance)

save(rflist, file = paste(modelfolder, '/rflist_', NumberOfClusters, '.rdata', sep=''))
save(importance, file = paste0('_rData/', run, '/importanceRF.rData'))
save(auc, file = paste0('_rData/', run, '/aucRF_dist.rData'))
save(rff, file = paste0('_rData/', run, '/rff_dist.rData'))
