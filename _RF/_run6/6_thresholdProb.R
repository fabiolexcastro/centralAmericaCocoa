# -------------------------------------------------------------------------------------------------
# Know the threshold for RF Model in East Africa
# Castro - CIAT, 2016
# -------------------------------------------------------------------------------------------------

require(raster)
require(rgdal)
require(data.table)
require(tidyverse)

# Initial setup
rm(list = ls())
options(scipen = 999)
cat('\f')
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam')
} else {
  if(OSys == 'Windows'){
  setwd('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}
run <- '_run6'

# Load data
load(paste0('_rData/', run, '/clustereddata.rData'))
layer <- raster(paste0('_RF/', run, '/_results/_raw/_current/RF_5Prob_current.asc'))
occ <- tbl_df(occ)

# occ <- env_values[,1:2]#occ <- read.csv(paste0(path, '/_points/_csv/1_occ_rmDupCell.csv')) 
value <- raster::extract(layer, occ[,1:2])
value <- value[!is.na(value)]
quantiles_05 <- quantile(value, seq(0,1,0.005))

png(filename = '_figures/th_prob_run6.png',  units = 'in', width = 9, height = 6, res = 100)
plot(quantiles_05, pch = 16)
dev.off()

quantiles_1 <- quantile(value, seq(0,1,0.01))
values_df <- as.data.frame(quantiles_05)

# write.csv(quantiles_1, paste0(path, '/_tables/quantilesPrecAra2.csv'))

threshold <- as.numeric(subset(values_df, rownames(values_df) == '1%'))
threshold <- as.numeric(subset(values_df, rownames(values_df) == '0.5%'))
#threshold<- as.numeric(subset(values_df, rownames(values_df) == '1%'))#threshold <- min(value)
save(threshold, file = paste0('_rData/', run, '/threshold_prob05.rData'))

load(paste0('_rData/', run, '/threshold_prob.rData'))

mtx_rcl <- matrix(c(0, threshold, 0,  threshold, 1,  1), ncol = 3, byrow = T)
layer_rcl <- reclassify(layer, mtx_rcl)

plot(layer_rcl, main = 'Suitability and Not Suitability')

load('_rData/_run6/threshold_prob.rData')
load('_rData/_run6/threshold_unc.rData')

# ------------------------------------------------------------------------------------------------
# quantiles_05    <- quantile(value, seq(0,1,0.005)) #look the percentiles
# quantiles_1     <- quantile(value, seq(0,1,0.01))
# quantiles_05_df <- as.data.frame(setDT(as.data.frame(quantiles_05), keep.rownames = TRUE))
# quantiles_1_df  <- as.data.frame(setDT(as.data.frame(quantiles_1), keep.rownames = TRUE))
# rclm            <- matrix(c(0, quantile(value, 0.01), 0, quantile(value, 0.01), 1, 1), ncol = 3, byrow = T)

threshold_000   <- reclassify(layer, matrix(c(0, quantile(value, 0.0),  0, quantile(value, 0.5),  1, 1), ncol = 3, byrow = T))
threshold_001   <- reclassify(layer, matrix(c(0, quantile(value, 0.01), 0, quantile(value, 0.01), 1, 1), ncol = 3, byrow = T))
threshold_005   <- reclassify(layer, matrix(c(0, quantile(value, 0.05), 0, quantile(value, 0.1),  1, 1), ncol = 3, byrow = T))
threshold_01    <- reclassify(layer, matrix(c(0, quantile(value, 0.1),  0, quantile(value, 0.5),  1, 1), ncol = 3, byrow = T))

th              <- list(threshold_000, threshold_001, threshold_005, threshold_01)
val_qt          <- c(quantile(value, 0), quantile(value, 0.01), quantile(value, 0.05), quantile(value, 0.1))
qt              <- c("Quantile 0", "Quantile 0.01", "Quantile 0.05", "Quantile 0.1")

png((filename = paste0(path, "_plots/_th/", "thresholds_mwi.png")), width = 1000, height = 850, res = 120) 
par(mfrow=c(2,2))

for(i in 1:length(th)){ 
  
  #png((filename = paste0(path, "/_plots/_contrastPlot/_temp/", biomains_temp[i], ".png")), width = 600, height = 450, res = 100) # se utiliza aqui para guardar cada grafico indenpendiente (cada uno representa una variable)
  plot(th[[i]], xlab = "Lon", ylab = "Lat",  sub = NULL, main = NA)
  title(main = paste0(qt[i], " - Prob Value: ",  round(val_qt[i], 2)), line = 1.5) #gosor lwd = 2
  #dev.off() # se utiliza aqui para guardar cada grafico indenpendiente (cada uno representa una variable)
}

dev.off()


