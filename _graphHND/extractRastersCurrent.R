

require(raster)
require(rgdal)
require(dplyr)
require(rgeos)
require(gtools)

setwd('Z:/_cam')

files <- list.files('./_raster/_climate/_current/_asc', full.names = T, pattern = '.asc$') %>%
            mixedsort()
bio_1  <- grep('bio_1.asc', files, value = T) %>%
            raster()
bio_12 <- grep('bio_12.asc', files, value = T) %>%
            raster()
st <- stack(bio_1, bio_12)

hnd <- shapefile('./_shp/_base/_admCountries/HND_adm1.shp')

extractValues <- function(st, mask){
  
  var_cut <- raster::crop(st, mask) %>%
                raster::mask(mask)
  
}

result <- extractValues(st = st, mask = hnd)
result <- unstack(result)

Map('writeRaster', x = result, file = paste0('./_workspace/_graphHND/_raster/current_bio_', c(1,12), '.asc'))

