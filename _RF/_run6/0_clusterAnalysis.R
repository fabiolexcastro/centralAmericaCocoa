
# CIAT, 2016 Updated: February 2017
# Author: Bunn & Castro
# Target: cluster analysis

# Load libraries
library(dplyr)
library(tibble)
library(tidyr)
library(raster)
library(rgdal)
library(cclust)
library(outliers)
library(dismo)
library(gtools)
library(multcomp)
library(foreign)
library(magrittr)
library(sf)
library(rgeos)
library(readr)
library(ggplot2)

# Initial setup
rm(list = ls())
options(scipen = 9999)
set.seed(1234)
setwd('Z:/_cam')
setwd('W:/_cam')
run <- '_run6'
source('_codes/_r/_RF/_run6/FunctionsRFclustering.R')


# Functions to use
dup_cell <- function(path_mask, path_df){
  
  mask <- raster(path_mask)
  df<- read.csv(path_df)
  
  cellNum <- raster::extract(mask, df[,c('Lon', 'Lat')], cellnumbers = T) 
  cells <- xyFromCell(mask, cellNum[,'cells'])
  dupvec <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell <- tbl_df(df[dupvec,])
  
  return(list(occ_rmDupCell, occ_DupCell))
  
}

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
climatefolder <- '_raster/_climate/_current/_asc'
srtm <- raster('_raster/_srtm/srtm_v41_30s')
mask <- raster('_raster/_mask/mask_cam_2_5min.tif')
shp <- shapefile('_shp/_base/_admCountries/CAM_Adm0.shp') 
lyrs <- list.files(climatefolder, full.names = T, pattern = '.asc$') %>%
            grep('bio', ., value = T) %>%
            mixedsort() %>%
            stack()

# Occurrences [Join]
occ1 <- read_csv('_points/_csv/_run6/6_occ.csv')
occ2 <- read_csv('_points/_gtm/joinGTM.csv') %>% dplyr::select(Lon, Lat)
occ <- rbind(occ1, occ2)

write.csv(occ, '_points/_csv/_run6/6_occ_ok.csv', row.names = F)

# Remove  bias according with the resoluciton 2.5 arc-minutes
dup <- dup_cell(path_mask = '_raster/_mask/mask_cam_2_5min.tif', path_df = '_points/_csv/_run6/6_occ_ok.csv')
occ <- dup[[2]]

write.csv(dup[[1]], '_points/_csv/_run6/occ_dup.csv', row.names = F)
write.csv(dup[[2]], '_points/_csv/_run6/occ_rmDup.csv', row.names = F)

# Values altitude for the presences
load('_rData/_run6/occ.rData')
occ

occ_srtm <- raster::extract(srtm, occ[,c('Lon', 'Lat')]) %>% cbind(occ, .) %>% tbl_df() %>% dplyr::select(Lon, Lat, .)

summary(occ_srtm$.)
hist(occ_srtm$.)

hstg <- qplot(occ_srtm$.) + xlab('') + ylab('m.s.n.m')
ggsave(plot = hstg, filename = '_figures/hist_srtm.png', width = 10, height = 8, dpi = 100, units = 'in')

# Count presences by Country
occ.adm <- raster::extract(shp, occ[,1:2]) %>% dplyr::select(NAME_ENGLI) %>% group_by(NAME_ENGLI) %>% summarize(Count = n())
gg.occ.adm <- ggplot(data = occ.adm, aes(x = NAME_ENGLI, y = Count)) + 
                geom_bar(stat = 'identity') + 
                xlab('') +
                ylab('Cantidad') +
                theme(axis.text = element_text(size = 14),
                      axis.title = element_text(size = 14))
ggsave(plot = gg.occ.adm, filename = '_figures/_plots/_run6/countOccCntry.png', dpi = 300, units = 'in', width = 12, height = 9)
rm(occ.adm, gg.occ.adm)

# Extract values to presence data
occ <- raster::extract(lyrs, occ[,1:2]) %>% cbind(occ, .)

# Outliers
norm <- scores(occ[,3:ncol(occ)], 'z') ; norm_na <- norm
norm_na[abs(norm_na)>3.5]  <- NA 
normpoints <- cbind(occ[,1:2], norm_na) %>%
                na.omit(.) %>%
                tbl_df()
occ.otl <- occ[which(is.na(norm_na), arr.ind = T)[,1],] %>% dplyr::select(Lon, Lat)
occ.otl.alt <- raster::extract(srtm, occ.otl[,1:2]) 

occ <- raster::extract(lyrs, normpoints[,1:2]) %>% cbind(normpoints[,1:2], .) %>% tbl_df() 

write.csv(occ, '_points/_csv/_run6/6_occ_swd_rmOtl.csv', row.names = F)

# Filter by altitude
occ_srtm <- raster::extract(srtm, occ[,1:2]) %>%
              cbind(occ[,1:2], altitude = .) %>%
              tbl_df() # filter(occ_srtm, altitude > 1300) # 0 presencias por encima de 1300


# Count presences by Country
occ_adm0 <- raster::extract(shp, occ[,1:2]) %>% 
              cbind(., occ[,1:2]) %>% 
              dplyr::select(NAME_0, Lon, Lat) %>%
              tbl_df()

cntOcc <- occ_adm0 %>%
            group_by(NAME_0) %>%
            summarise(count = n()) %>%
            ungroup()

write.csv(cntOcc, '_tables/count_occRmDup_run6.csv', row.names = F)

# Background sampling probability
adm0 <- st_read(paste0('_shp/_base/_admCountries/CAM_Adm0.shp')) %>%
          dplyr::select(ISO, NAME_0, NAME_ENGLI) %>%
          mutate(NAME_ENGLI = as.character(NAME_ENGLI))

# Count points by country
prodCocoa <- read_csv('_tables/_FAO/summ_2000_2010.csv')
bias <- inner_join(prodCocoa, cntOcc, by = c('Area' = 'NAME_0')) %>%
            mutate(prob = count/Produccion)
bias.shape <- inner_join(adm0, bias, by = c('NAME_ENGLI' = 'Area'))
bias.shape <- as(bias.shape, 'Spatial')
bias.raster <- rasterize(bias.shape, lyrs[[1]], field = 'prob', fun = 'mean')#bias.raster <- poly_to_raster(bias.shape, lyrs[[1]], copy.data = TRUE)

# Histogram
if(!file.exists('_figures/_hist/_run6')){dir.create('_figures/_hist/_run6')}	

for (i in 1:nlayers(lyrs)){
  
  png((filename = paste0('_figures/_hist/', run, '/', 'bio_', i, '.png')), width = 600, height = 450, res = 100)
  x <- extract2(normpoints[,paste('bio_', i, sep = '')], 1)
  hist(x, main = NULL, xlab = 'Z-Score')
  title(main = paste('Bio_',i, sep = ''))
  dev.off()
  
}

# Clustering - Random Forest
env_values <- as.matrix(occ[,3:ncol(occ)]); nrow(env_values)
datRF <- as.data.frame(occ[,3:ncol(occ)]); nrow(datRF)
d <- dist(datRF, method = "euclidean")  
rfClust <- rf.clust(occ = occ, nforest = 25, ntrees = 100, nVars = 8, nclasses = 5)
labelRF <- rfClust[[1]]
clusterdata <- rfClust[[2]]
classdata <- cbind(pb = as.factor(labelRF), occ[,3:ncol(occ)])
clusteredpresdata <- cbind(occ, cluster = labelRF) %>% na.omit() %>% tbl_df()
no.clusters <- 5

save(datRF, file = paste0('_rData/', run, '/datRF.rData'))
save(clusterdata, file = paste0('_rData/', run, '/clusterdata.rData'))
save(distRF_presences, file = paste0('_rData/' , run, '/distRF_presences.rData'))
save(occ, clusteredpresdata, no.clusters, labelRF, file = paste0('_rData/', run, '/clustereddata.rData'))

# Plot Clusters
windows()
plot(clusterdata)
for(i in 2:15){ rect.hclust(clusterdata,k=i)}

# Cluster OCC
grouped <- as.data.frame(cbind(env_values, cluster = as.factor(labelRF))) %>% tbl_df()
occ_cluster <- cbind(occ[,1:2], grouped[,'cluster'])
save(occ_cluster, file = paste0('_rData/', run, '/occ_cluster.Rdata'))
save(grouped, file = paste0('_rData/', run, '/grouped.rData'))


# ----------------------------------------------

datRF_presences <- as.data.frame(occ[,3:ncol(occ)]); nrow(datRF)
attach(datRF_presences)
no.forests <- 25
no.trees   <- 100
distRF_presences <- RFdist(datRF_presences, mtry1 = 8, no.trees, no.forests, addcl1 = T, addcl2 = F, imp = T, oob.prox1 = T)#raw mtry1 = 4
no.presenceclasses <- 3
labelRF <- pamNew(distRF_presences$cl1, no.presenceclasses)
table(labelRF)
clusterdata <- hclust(as.dist(distRF_presences$cl1), method = 'ward.D2')

windows()
plot(clusterdata)
for(i in 2:15){
  rect.hclust(clusterdata,k=i)
}

rect.hclust(clusterdata,k=10,border="blue")
no.clusters <- 3
labelRF <- cutree(clusterdata,k=no.clusters)
table(labelRF)
detach(datRF_presences)

# sapply(2:5, function(n.cluster)table(cutree(clusterdata,n.cluster)))

classdata <- cbind(pb = as.factor(labelRF), occ[,3:ncol(occ)])
clusteredpresdata <- cbind(occ, cluster = labelRF) %>%
                       na.omit() %>%
                       tbl_df()
table(clusteredpresdata$cluster)

save(clusterdata, file = paste0('_rData/', run, '/clusterdata.rData'))
save(distRF_presences, file = paste0('_rData/' , run, '/distRF_presences.rData'))
save(occ, clusteredpresdata, no.clusters, labelRF, file = paste0('_rData/', run, '/clustereddata.rData'))

#

grouped <- as.data.frame(cbind(env_values, cluster = as.factor(labelRF))) %>% tbl_df()
occ_cluster <- cbind(occ[,1:2], grouped[,'cluster'])
save(occ_cluster, file = paste0('_rData/', run, '/occ_cluster.Rdata'))

# Contrast plot
grouped$cluster <- as.factor(grouped$cluster)# grouped[,"cluster"] <- as.factor(grouped[,'cluster'])
descriptors <- colnames(env_values)

contrastPub <- function(descriptors, grouped, biomains, biolabels, pathGraph, nameGraph, Nrow, Ncol, width, height){   
  
  png((filename = paste(pathGraph, nameGraph, sep = '/')), width = width, height = height, res = 120)
  par(mfrow = c(Nrow, Ncol))
  
  for(i in 1:length(descriptors)){
    
    formula <- as.formula(paste(descriptors[i], "~ cluster"))
    Anov    <- aov(formula = formula, data = grouped)
    cont    <- glht(Anov, mcp(cluster = 'GrandMean'))
    plot(cont, xlab = NA, sub = NULL, main = NA)
    title(main = biomains[i], line = 1.5)
    title(ylab = 'Group', line = 2.5)
    # title(xlab = paste(biolabels[i], '\n', '?', round(mean(grouped[,descriptors[i]]), digits = 1)), line = 3) 
    # title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1), line = 3)) 
    # print(round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))
    title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))) 
    
  }  
  
  dev.off()
}

# Temperature (centigrados)
centigrados <- c(1, 2, 5, 6, 7, 8, 9, 10, 11, 32, 33)
desc_temp_grados <- names(grouped[,centigrados])
grouped_temp_grados <- grouped[centigrados] %>%
                          cbind(., grouped['cluster']) %>%
                          mutate(bio_1 = bio_1/10,
                                 bio_2 = bio_2/10,
                                 bio_5 = bio_5/10,
                                 bio_6 = bio_6/10,
                                 bio_7 = bio_7/10,
                                 bio_8 = bio_8/10,
                                 bio_9 = bio_9/10,
                                 bio_10 = bio_10/10,
                                 bio_11 = bio_11/10) 
biolabels_temp_grados <- rep("°C", length(centigrados))
biomains_temp_grados  <- c("Annual mean temp", "Mean diurnal range", "Max Temp of Warmest Month", "Min temp of coldest month", 
                           "Temp annual range", "Mean temp of wettest quarter", "Mean temp of driest quarter", "Mean temp of warmest quarter", 
                           "Mean temp of coldest quarter", "Mean temp during \n growing season", "Max dry season temp")

contrastPub(descriptors = desc_temp_grados, grouped = grouped_temp_grados, biomains = biomains_temp_grados,
            biolabels = biolabels_temp_grados, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Temp Grados.png', Nrow = 4, Ncol = 3, width = 1200, height = 1000)

# Temperature (no centigrados)
noCentigrados <- c(3, 4)
desc_temp_nogrados <- names(grouped[noCentigrados])  
grouped_temp_nogrados <- grouped[noCentigrados] %>% cbind(., grouped['cluster'])
biolabels_temp_nogrados <- rep("-", length(noCentigrados)) 
biomains_temp_nogrados <- c('Isothermality (bio 2/bio 7) * 100', 'Temperature seasonality (sd * 100)')

contrastPub(descriptors = desc_temp_nogrados, grouped = grouped_temp_nogrados, biomains = biomains_temp_nogrados,
            biolabels = biolabels_temp_nogrados, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Temp No Grados.png', Nrow = 1, Ncol = 2, width = 900, 450) 

# Precipitation
desc_ppt <- descriptors[c(12:20)]
biomains_ppt <- c('Annual prec', 'Prec of wettest month', 'Prec of driest month', 'Prec seasonality', 'Prec of wettest quarter', 'Prec of driest quarter', 'Prec of warmest quarter', 'Prec of coldest quarter', 'Number of consecutive months \n less 100 mm')
grouped_ppt <- grouped[,c(desc_ppt, 'cluster')]
biolabels_ppt <- c(rep('mm', 3), '-', rep('mm',4),'Number months')

contrastPub(descriptors = desc_ppt, grouped = grouped_ppt, biomains = biomains_ppt,
            biolabels = biolabels_ppt, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Precipitation.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)

# ETP 
desc_etp <- descriptors[c(21:29)]
biolabels_etp <- c("mm", "-", rep("mm",7))
biomains_etp <- c("Annual PET", "PET Seasonality", "PET Max", "PET Min", "PET Range", "PET of wettest quarter", 
                    "PET of driest quarter", "PET of warmest quarter", "PET of coldest quarter")
grouped_etp    <- grouped[,c(desc_etp, "cluster")]

contrastPub(descriptors = desc_etp, grouped = grouped_etp, biomains = biomains_etp,
            biolabels = biolabels_etp, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot ETP.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)

# New climatic variables
desc_water <- descriptors[c(30:31)]
biolabels_water <- c('Number Months', 'mm')
biomains_water <- c('Consecutive Months with less \n Prec than PET', 'Sum of water deficit during dry season')
grouped_water <- grouped[c(desc_water, 'cluster')]
namesWater<- paste0('bio_', 30:31)

contrastPub(descriptors = desc_water, grouped = grouped_water, biomains = biomains_water,
            biolabels = biolabels_water, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Water.png', Nrow = 1, Ncol = 2, width = 1200, height = 600) 

# Contrast plot for publication
# clusterdata <- hclust(d, method = "ward.D2")
# plot(clusterdata, hang = -0.01, cex = 0.7)
# 
# for(i in 2:10){ 
#   
#   rect.hclust(clusterdata, k=i) 
#   
# }
# 
# # Cluster number
# 
# n.cluster   <- sapply(2:5, function(n.cluster)table(cutree(clusterdata,n.cluster)))#table(cutree(clusterdata, k = 2)); table(cutree(clusterdata, k = 3)); table(cutree(clusterdata, k = 4)); table(cutree(clusterdata, k = 5))
# no.clusters <- 5
# labelRF     <- cutree(clusterdata, no.clusters)
# grouped     <- as.data.frame(cbind(env_values, cluster = as.factor(labelRF)))
# grouped     <- tbl_df(grouped)
# 
# occ_cluster <- cbind(occ[,1:2], grouped[,'cluster'])
# 
# save(occ_cluster, file = paste0(path, '/_rData/', run, '/occ_cluster.Rdata'))
# 
# # OJO
# 
# env_values2 <- raster::extract(lyrs, occ_cluster[,1:2]) %>%
#                   cbind(occ_cluster, .)
# env_values2 <- tbl_df(env_values2)
# 
# clusteredpresdata <- dplyr::select(env_values2, Lon, Lat, bio_1:bio_33, cluster)
# 
# save(clusteredpresdata, file = paste0(path, '/_rData/', run, 'clusteredpresdata.Rdata'))
# save(no.clusters, labelRF, norm, env_values, file = paste0(path, '/_rData/', run, '/clusterpresdata.Rdata'))
# # load(paste0(path, '/_rdata/_run1/clusterpresdata.Rdata'))
# 
# # --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
# # Contrast plot for publication
# # --=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--=--
# 
# grouped$cluster <- as.factor(grouped$cluster)# grouped[,"cluster"] <- as.factor(grouped[,'cluster'])
# descriptors     <- colnames(env_values)
# 
# # Function of contrast publication
# 
# contrastPub <- function(descriptors, grouped, biomains, biolabels, pathGraph, nameGraph, Nrow, Ncol, width, height){   
#   
#   png((filename = paste(pathGraph, nameGraph, sep = '/')), width = width, height = height, res = 120)
#   par(mfrow = c(Nrow, Ncol))
#   
#   for(i in 1:length(descriptors)){
#     
#     formula <- as.formula(paste(descriptors[i], "~ cluster"))
#     Anov    <- aov(formula = formula, data = grouped)
#     cont    <- glht(Anov, mcp(cluster = 'GrandMean'))
#     plot(cont, xlab = NA, sub = NULL, main = NA)
#     title(main = biomains[i], line = 1.5)
#     title(ylab = 'Group', line = 2.5)
#     # title(xlab = paste(biolabels[i], '\n', '?', round(mean(grouped[,descriptors[i]]), digits = 1)), line = 3) 
#     title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1), line = 3)) 
#     
#   }  
#   
#   dev.off()
# }
# 
# 
# # Temperature (centigrados)
# 
# centigrados         <- c(1, 2, 5, 6, 7, 8, 9, 10, 11, 32, 33)
# desc_temp_grados    <- names(grouped[,centigrados])
# grouped_temp_grados <- grouped[centigrados] %>%
#                           cbind(., grouped['cluster']) %>%
#                           mutate(bio_1 = bio_1/10,
#                                  bio_2 = bio_2/10,
#                                  bio_5 = bio_5/10,
#                                  bio_6 = bio_6/10,
#                                  bio_7 = bio_7/10,
#                                  bio_8 = bio_8/10,
#                                  bio_9 = bio_9/10,
#                                  bio_10 = bio_10/10,
#                                  bio_11 = bio_11/10) 
# biolabels_temp_grados <- rep("°C", length(centigrados))
# biomains_temp_grados  <- c("Annual mean temp", "Mean diurnal range", "Max Temp of Warmest Month", "Min temp of coldest month", 
#                            "Temp annual range", "Mean temp of wettest quarter", "Mean temp of driest quarter", "Mean temp of warmest quarter", "Mean temp of coldest quarter", 
#                            "Mean temp during \n growing season", "Max dry season temp")
# 
# contrastPub(descriptors = desc_temp_grados, grouped = grouped_temp_grados, biomains = biomains_temp_grados,
#             biolabels = biolabels_temp_grados, pathGraph = paste0(path, '/_figures/_contrastPlots/', run), 
#             nameGraph = 'Contrast Plot Temp Grados.png', Nrow = 4, Ncol = 3, width = 1200, height = 1000)
# 
# # Temperature (no centigrados)
# 
# noCentigrados             <- c(3, 4)
# desc_temp_nogrados        <- names(grouped[noCentigrados])  
# grouped_temp_nogrados     <- grouped[noCentigrados] %>%
#                                 cbind(., grouped['cluster'])
# biolabels_temp_nogrados   <- rep("-", length(noCentigrados)) 
# biomains_temp_nogrados    <- c('Isothermality (bio 2/bio 7) * 100', 'Temperature seasonality (sd * 100))')
#                                
# contrastPub(descriptors = desc_temp_nogrados, grouped = grouped_temp_nogrados, biomains = biomains_temp_nogrados,
#             biolabels = biolabels_temp_nogrados, pathGraph = paste0(path, '/_figures/_contrastPlots/', run), 
#             nameGraph = 'Contrast Plot Temp No Grados.png', Nrow = 1, Ncol = 2, width = 1400, 700)
# 
# 
# # Precipitation
# 
# desc_ppt       <- descriptors[c(12:20)]
# biomains_ppt   <- c("Annual prec", "Prec of wettest month", "Prec of driest month", "Prec seasonality", "Prec of wettest quarter", "Prec of driest quarter", "Prec of warmest quarter", "Prec of coldest quarter", "Number of consecutive months \n less 100 mm")
# grouped_ppt    <- grouped[,c(desc_ppt, "cluster")]
# biolabels_ppt  <- c(rep("mm", 3), "-", rep("mm",4),"Number months")
# 
# contrastPub(descriptors = desc_ppt, grouped = grouped_ppt, biomains = biomains_ppt,
#             biolabels = biolabels_ppt, pathGraph = paste0(path, '/_figures/_contrastPlots/', run), 
#             nameGraph = 'Contrast Plot Precipitation.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)
# 
# # ETP 
# 
# desc_etp       <- descriptors[c(21:29)]
# biolabels_etp  <- c("mm", "-", rep("mm",7))
# biomains_etp   <- c("Annual PET", "PET Seasonality", "PET Max", "PET Min", "PET Range", "PET of wettest quarter", 
#                     "PET of driest quarter", "PET of warmest quarter", "PET of coldest quarter")
# grouped_etp    <- grouped[,c(desc_etp, "cluster")]
# 
# contrastPub(descriptors = desc_etp, grouped = grouped_etp, biomains = biomains_etp,
#             biolabels = biolabels_etp, pathGraph = paste0(path, '/_figures/_contrastPlots/', run), 
#             nameGraph = 'Contrast Plot ETP.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)

# New climatic variables
desc_water          <- descriptors[c(30:31)]
biolabels_water     <- c("Number Months", "mm")
biomains_water      <- c("Consecutive Months with less \n Prec than PET", "Sum of water deficit during dry season")
grouped_water       <- grouped[c(desc_water, "cluster")]
namesWater          <- paste0("bio_", 30:31)

contrastPub(descriptors = desc_water, grouped = grouped_water, biomains = biomains_water,
            biolabels = biolabels_water, pathGraph = paste0(path, '/_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Water.png', Nrow = 1, Ncol = 2, width = 1200, height = 600) 

# Soils

# descriptors_soils           <- descriptors[c(32:34)]
# biolabels_soils             <- c("v%", "v%", "pH")
# biomains_soils              <- c("Available water capacity (v%) \n of the fine earth", "Moisture content (v%) of the \n fine earth at saturation", "pH (x 10) of soil-water solution")
# grouped_soils               <- grouped[,c(descriptors_soils, "cluster")]
# nameSoils                   <- paste0("bio_", 32:34)




# # Remove outliers according with the z scores normal

# 1st Way
norm <- as_data_frame(scores(occ[,3:ncol(occ)], 'z'))
head(norm)

norm_na <- norm
norm_na[abs(norm_na)>3.5]  <- NA 
normpoints <- cbind(occ[,1:2], norm_na) %>%
  na.omit(.) %>%
  tbl_df()

# 2nd Way

scores <- apply(occ[,3:ncol(occ)], 2, zscore) %>%
  t() %>%
  cbind(occ[,1:2], .) %>%
  as_data_frame()


nas <- list()

for(i in 1:33){
  
  nas[[i]] <- which(abs(norm[,i]) > 3.5)
  
}

pos <- unique(sort(unlist(nas)))
nrow(norm) - nrow(normpoints) 
occ[pos,]
summary(occ_srtm[pos,'altitude'])

occ <- normpoints
