

# Load libraries
require(tidyverse)
require(raster)

# Initial setup
rm(list = ls())
setwd('Z:/_cam')
setwd('W:/_cam')
options(scipen = 999)

# Load data - Summaries
suit_crn <- raster('_RF/_run6/_results/_raw/_current/RF_5Prob_current.asc')
suit_30 <- raster('_RF/_run6/_results/_process/RF_5prob_2030.asc')
suit_50 <- raster('_RF/_run6/_results/_process/RF_5prob_2050.asc')

# Difference
dif_30 <- suit_30 - suit_crn
dif_50 <- suit_50 - suit_crn

writeRaster(dif_30, '_RF/_run6/_results/_process/RF_diff30_prob.asc')
writeRaster(dif_50, '_RF/_run6/_results/_process/RF_diff50_prob.asc')

# Load data - Summaries
models <- c('gfdl_esm2m',  'mohc_hadgem2_es', 'nimr_hadgem2_ao')
nms.30 <- list.files('_RF/_run6/_results/_raw/_2030', full.names = F, pattern = '.asc$') %>% grep(paste0(models, collapse = '|'), ., value = T) %>% grep('Prob', ., value = T)
nms.50 <- list.files('_RF/_run6/_results/_raw/_2050', full.names = F, pattern = '.asc$') %>% grep(paste0(models, collapse = '|'), ., value = T) %>% grep('Prob', ., value = T)
lyr_30 <- list.files('_RF/_run6/_results/_raw/_2030', full.names = T, pattern = '.asc$') %>% grep(paste0(models, collapse = '|'), ., value = T) %>% grep('Prob', ., value = T) %>% lapply(raster)
lyr_50 <- list.files('_RF/_run6/_results/_raw/_2050', full.names = T, pattern = '.asc$') %>% grep(paste0(models, collapse = '|'), ., value = T) %>% grep('Prob', ., value = T) %>% lapply(raster)

dif_mdls_30 <- lapply(1:length(lyr_30), function(x) lyr_30[[x]] - suit_crn)
dif_mdls_50 <- lapply(1:length(lyr_50), function(x) lyr_50[[x]] - suit_crn)


Map('writeRaster', x = dif_mdls_30, filename = paste0('_RF/_run6/_results/_process/diff_', nms.30))
Map('writeRaster', x = dif_mdls_50, filename = paste0('_RF/_run6/_results/_process/diff_', nms.50))


