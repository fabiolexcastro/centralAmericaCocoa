


# Load libraries
require(raster)
require(rgdal)
require(tidyverse)

# Initial setup
rm(list = ls())
options(scipen = 999)
cat('\f')
setwd('Z:/_cam')

# Function to use
calc_area <- function(lyr, shp, yr){
  
  # Tabulate function
  tabFunc <- function(indx, extracted, region, regname) {
    
    dat <- as.data.frame(table(extracted[[indx]]))
    dat$name <- region[[regname]][[indx]]
    
    return(dat)
  }
  
  # Extraction data
  ext <- raster::extract(lyr, adm, method = 'simple')
  tabs <- lapply(seq(ext), tabFunc, ext, adm, 'NAME_ENGLI')
  
  print('Extraction done')
  
  # Tidy data.frame
  df <- do.call(rbind, tabs) %>%
    group_by(name) %>% 
    mutate(Porcentaje = (Freq/sum(Freq)) * 100) %>% 
    ungroup() %>%
    mutate(Porcentaje = round(Porcentaje, 1),
           Var1 = as.numeric(as.character(Var1)))
  
  # labs <- data.frame(number = c(0, 1, 3, 4, 5), categoria = c('Unsuitable', 'Incremental adaptation', 'Transform', 'Expansion', 'Systemic Resilience'))
  # df <- inner_join(df, labs, by = c('Var1' = 'number'))
  
  write.csv(df, paste0('_tables/_cluster/_run6/count_pixelesbyADM0_', yr, '.csv'), row.names = F)
  
  print('Done')
  
}

# Load data
adm <- shapefile('_shp/_base/_admCountries/CAM_Adm0.shp')
crn <- raster('_RF/_run6/_results/_process/_percentil0_5/_mixed/RF_5Classes_unccurrent.asc')
c30 <- raster('_RF/_run6/_results/_process/_percentil0_5/_mixed/RF_5Classes_unc_2030.asc')
c50 <- raster('_RF/_run6/_results/_process/_percentil0_5/_mixed/RF_5Classes_unc_2050.asc')

# Execute functions
calc_area(lyr = crn, shp = adm, yr = 'crn')
calc_area(lyr = c30, shp = adm, yr = 30)
calc_area(lyr = c50, shp = adm, yr = 50)

# Create graph
read.csv('_tables/_cluster/_run6/count_pixelesbyADM0_30.csv')


