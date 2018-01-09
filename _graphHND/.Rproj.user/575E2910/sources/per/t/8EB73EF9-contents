
# Load libraries
require(raster)
require(rgdal)
require(dplyr)
require(rgeos)
require(gtools)

# Workspace
rm(list = ls())
setwd('Z:/_cam')

# Load Data
hnd <- shapefile('./_shp/_base/_admCountries/HND_adm1.shp')
years <- c('_2030', '_2050')
models <- list.files('./_raster/_climate/_future/_rcp60/_asc/_2030', full.names = F)
hnd <- shapefile('./_shp/_base/_admCountries/HND_adm0.shp')
vars <- c('bio_1.asc', 'bio_12.asc')
y <- 1

lapply(1:length(years), function(y){ 

  print(years[y])
  
  lapply(1:length(models), function(x){
    
    print(models[x])
    
    files <- paste0('./_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[x]) %>%
                list.files(full.names = T, pattern = '.asc$') %>%
                grep(paste0(vars, collapse = '|'), ., value = T) %>%
                unique()
    
    print('To Stack')
    
    st <- lapply(files, FUN = raster) %>%
            stack()
    
    print('To Cut')
    st_cut <- raster::crop(st, hnd) %>%
                raster::mask(hnd) %>%
                unstack()
    
    Map('writeRaster', x = st_cut, filename = paste0('./_workspace/_graphHND/_raster/_future/', years[y], '/', models[x], '_', vars))
  
  })

})
 
