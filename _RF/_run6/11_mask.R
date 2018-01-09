
# Load libraries
require(tidyverse)
require(raster)

# Initial setup
rm(list = ls())
setwd('Z:/_cam')
setwd('W:/_cam')
options(scipen = 999)

lowerthreshold <- 0.34

# Load data - Summaries
current_prob <-  raster('_RF/_run6/_results/_raw/_current/RF_5Prob_current.asc')
future_prob <- raster('_RF/_run6/_results/_process/RF_5prob_2050.asc')

######## Prob change
binmatrix <- matrix(c(0, lowerthreshold, 0, lowerthreshold, 1, 1), ncol = 3, byrow = T)
currentbin <- reclassify(current_prob, binmatrix)
futurebin <- reclassify(future_prob, binmatrix)
suitmask <- currentbin + futurebin
suitmask[suitmask!=0] <- 1
suitmask[suitmask==0] <- NA

current_masked <- raster::mask(current_prob, suitmask)
future_masked <- raster::mask(future_prob, suitmask)
prob_change <- future_masked - current_masked


prob_change <- future_prob - current_prob
prob_change <- raster::mask(prob_change, suitmask)


writeRaster(prob_change, '_RF/_run6/_results/_process/RF_5change_prob_2050.asc', overwrite = T)
