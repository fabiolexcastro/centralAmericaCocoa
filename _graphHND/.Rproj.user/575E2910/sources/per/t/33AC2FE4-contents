
# Load libraries
require(raster)
require(rgdal)
require(dplyr)
require(rgeos)
require(gtools)
require(sf)

rm(list = ls())

# Function to use
zonalSt <- function(lyr_cur, st_fut, shp, by, path_raster, path_output, nameLayer, nameRaster){
  
  diff <- st_fut - lyr_cur
  diff_avg <- mean(diff)
  
  adm_ras <- raster::rasterize(shp, diff_avg, field = 'ID_Reg')
  zonal <- raster::zonal(diff_avg, adm_ras, fun = 'mean', na.rm = T) %>%
              tbl_df()
  
  adm_sf <- st_as_sf(as(shp, "SpatialPolygonsDataFrame"))
  adm_join <- inner_join(adm_sf, zonal, by = c('ID_Reg' = 'zone'))
  
  write_sf(adm_join, dsn = path_output, layer = nameLayer, driver = 'ESRI Shapefile', update = T)
  # writeRaster(diff_avg, paste(path_raster, nameRaster, sep = '/'), overwrite = T)
  
  return(list(adm_join, diff_avg))
}


# Workspace
setwd('W:/_cam')

# Load data
shp <- shapefile('./_shp/_base/_admCountries/HND_adm1.shp')
shp.rgs <- aggregate(shp, by = 'region')


files_cur <- list.files('./_workspace/_graphHND/_raster/_current', full.names = T, pattern = '.asc$')
files_50 <- list.files('./_workspace/_graphHND/_raster/_future/_2050', full.names = T, pattern = '.asc$')
current_bio1 <- raster(files_cur[[1]])
current_bio12 <- raster(files_cur[[2]])
st_bio1_50 <- grep('bio_1.asc', files_50, value = T) %>%
                stack()
st_bio12_50 <- grep('bio_12.asc', files_50, value = T) %>%
                stack()
# shp@data$ID_1  <- as.numeric(shp@data$ID_1)
shp.rgs@data$ID_Reg <- 1:nrow(shp.rgs@data)

# Execute functions!
adm_bio1 <- zonalSt(lyr_cur = current_bio1, st_fut = st_bio1_50, shp = shp.rgs, by = 'ID_Reg', 
                    nameLayer = 'hnd_adm1_bio1_region', path_output = './_workspace/_graphHND/_shp',
                    nameRaster = 'bio_1_diff.asc', path_raster = './_workspace/_graphHND/_raster/_diff') 
adm_bio12 <- zonalSt(lyr_cur = current_bio12, st_fut = st_bio12_50, shp = shp, by = 'ID_Reg', 
                     nameLayer = 'hnd_adm1_bio12_region', path_output = './_workspace/_graphHND/_shp',
                     nameRaster = 'bio_12_diff.asc', path_raster = './_workspace/_graphHND/_raster/_diff')


#
porc <- (adm_bio12[[2]] / mean(st_bio12_50)) * 100
adm_ras <- raster::rasterize(shp, porc, field = 'ID_Reg')
porc_df <- raster::zonal(porc, adm_ras, fun = 'mean', na.rm = T) %>% tbl_df() %>% mutate(mean = round(mean, 2))
adm_sf <- st_as_sf(as(shp, "SpatialPolygonsDataFrame"))
adm_join <- inner_join(adm_sf, porc_df, by = c('ID_Reg' = 'zone'))

write_sf(adm_join, dsn = './_workspace/_graphHND/_shp', layer = 'hnd_adm1_bio12_reg_porc', driver = 'ESRI Shapefile', update = T)
writeRaster(porc, './_workspace/_graphHND/_raster/_diff/bio_12_diff_porc.asc', overwrite = T)

