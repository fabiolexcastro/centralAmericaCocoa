

require(raster)
require(rgdal)
require(tidyverse)
require(spdplyr)
require(rgeos)
require(gtools)

options(scipen = 999)

path <- 'W:/_cam'
shp1 <- shapefile(paste0(path, '/_points/_new/_second/_shp/coordsWGS.shp'))
shp2 <- shapefile(paste0(path, '/_points/_new/_second/_shp/coordsGTM_WGS_clip.shp'))

df  <- as_data_frame(rbind(coordinates(shp1), coordinates(shp2))) %>%
          rename(Lon = coords.x1,
                 Lat = coords.x2)


write.csv(df, paste0(path, '/_points/_new/_second/coordsAllWGS.csv'), row.names = F)

# Add to global presences (Central America)

lyrs <- paste0(path, '/_raster/_climate/_current/_asc') %>%
          list.files(full.names = T, pattern = '.asc$') %>%
          grep('bio', ., value = T) %>%
          mixedsort() %>%
          stack()
occ  <- read_csv(paste0(path, '/_points/_csv/2_occ_swd.csv')) %>%
          dplyr::select(Longitude, Latitude) %>%
          rename(Lon = Longitude,
                 Lat = Latitude)

occ_all <- rbind(occ, df)
occ_all <- raster::extract(lyrs, occ_all[,1:2]) %>%
              cbind(., occ_all[,1:2])
occ_all <- dplyr::select(occ_all, Lon, Lat, bio_1:bio_33)

write.csv(occ_all, paste0(path, '/_points/_csv/3_occ_swd.csv'), row.names = FALSE)


