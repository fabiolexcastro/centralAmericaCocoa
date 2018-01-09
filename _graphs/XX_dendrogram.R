

library(dplyr)
library(readr)
library(rgeos)
library(gtools)
library(raster)

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/Workspace_cluster_9/Coffee_Cocoa2/_eastAfrica' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_eastAfrica'
  }
}

source('Z:/_cam/_codes/_r/_graphs/A2R_dendrogram.R')
setwd('Z:/_cam')


## Rough dendrogram 
occ <- read.csv('_points/_csv/_run6/6_occ_swd_rmOtl.csv')
bios <- paste0('bio_', 1:35, '.asc')
lyrs <- list.files('', full.names = T, pattern = '.asc$') %>%
            mixedsort() %>%
            lapply(FUN = raster)
lyrs <- stack(paste0(path, '/_raster/_extents/_mlw/_current/', bios))
occ_swd <- raster::extract(lyrs, occ[,1:2])
occ_swd <- cbind(occ, occ_swd)
datRF   <- as.data.frame(occ[,3:ncol(occ)])
d <- dist(datRF,method = "euclidean")
clusterdata <- hclust(d,method="ward.D2")
plot(clusterdata)

rect.hclust(clusterdata, k = 2)

for(i in 2:10){
  
  rect.hclust(clusterdata,k=i)
}


# Dendogram RF Clustering

load('./_rData/_run6/rfclustering.rData')
load('./_rData/_run6/rfclustering.rData')
load('./_rData/_run6/clusterdata.rData')

clusterdata
ED.clusters <- 5

## Make a nice Dendrogram

ppar          <- par(no.readonly=TRUE)
clusterlabels <- paste("Cluster",1:ED.clusters)

png(filename = 'Z:/_cam/_figures/dendogram_run6_2.png', width = 9, height = 6, units = "in", res = 300)
opar <- par(no.readonly=TRUE)
A2Rplot.hclust(clusterdata, k = ED.clusters, boxes = FALSE, col.up = "gray50", #clusterdata
               col.down = c("#4bd14b", "#ce53ed", "#dbc24f","#a87000","#57debe"),
               #        main="Dendrogram of agglomerative clustering",
               main=NULL,
               ylab="Height",
               mtext(seq(0, 1000000, 10000), side = 2, at = seq(0, 1000000, 10000), # seq(0,10000,5000), at = seq(0, 5000, 1000),
                     line = 1, las = 1),
               hang=-1,axes=T,show.labels=F,lty.up = 2,lty.down = 1)

A2Rplot.hclust(clusterdata, k = ED.clusters, boxes = FALSE, col.up = "gray50", #clusterdata
               col.down = c("#4bd14b", "#ce53ed", "#dbc24f","#a87000","#57debe"),
               #        main="Dendrogram of agglomerative clustering",
               main=NULL,
               ylab="Height",
               hang=-1,axes=T,show.labels=F,lty.up = 2,lty.down = 1)

axis(side = 2, at = seq(0, 5000, 1000), labels = F, lwd = 1, line = -4.5, tick = T) # at = seq(0, 2, 0.36), at = seq(0, 5000, 1000)
title(ylab = "RF Clustering", line = -2.5)# 'Euclidenn distance'
par(opar)

# legend coordinates depend on Euclidean distance (y) and number of cases (x)
legend("topright", legend = c('Type 1', 'Type 2', 'Type 3', 'Type 4', 'Type 5'), col = c("#4bd14b", "#ce53ed", "#dbc24f", '#a87000', '#57debe'),
       lty = 1, lwd = 2) # legend = clusterlabels, y.intersp = 1.1
## Dendrogram Ende
dev.off()
