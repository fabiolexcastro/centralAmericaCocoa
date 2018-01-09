
# Author: Fabio Castro
# CIAT, June 2017
# Target: preparte occurrences to RF model (study area = central america, crop = cocoa)

# Load libraries - if the library is not in your pc, please use install.packages to download and install de libraries in the PC

require(raster)
require(rgdal)
require(tidyverse)
require(maptools)
require(rgeos)
require(gtools)
require(spdplyr)
require(magrittr)
require(sf)

# Function

dup_cell <- function(path_mask, path_df){
  
  mask  <- raster(path_mask)
  df    <- read.csv(path_df)
  
  cellNum <- raster::extract(mask, df[,c('Longitude', 'Latitude')], cellnumbers = T) 
  cells   <- xyFromCell(mask, cellNum[,'cells'])
  dupvec  <- duplicated(cells[,c('x', 'y')])
  occ_rmDupCell <- tbl_df(df[!dupvec,])
  occ_DupCell   <- tbl_df(df[dupvec,])
  
  return(list(occ_rmDupCell, occ_DupCell))
  
}

# Load data

path <- 'W:/_cam'

myproj <- CRS("+proj=longlat +datum=WGS84")
all.countries <- readShapeSpatial(paste(path,'/_shp/_base/all_countries.shp',sep = ''), proj4string = myproj)
# mask <- raster(paste0(path, '/_raster/_mask/mask_cam.tif'))
mask <- raster('D:/_world/_worldclim2/_grid_v_1_4/bio_1')

camerica <- subset(all.countries, all.countries$UNREG1 == 'Central America')
occ_gral <- read_csv('D:/CC/_points/_cacao/_global/occ_cacao_jun.csv')# occ_gral <- read_csv('data/points_gral.csv')

# Choose only presences to central america from general database

points_gral <- occ_gral
coordinates(points_gral) <- ~ Longitude + Latitude
crs(points_gral) <- myproj

points_cam <- crop(points_gral, aggregate(camerica)) %>%
                dplyr::select(ID, Species, Year, Source)
countries  <- raster::extract(camerica, points_cam) %>%
                  dplyr::select(ENGLISH) 
points_cam2 <- mutate(points_cam, Countries = extract2(countries, 1))

points_cam3 <- raster::crop(points_cam2, aggregate(camerica))

write.csv(points_cam2@data, paste0(path, '/_points/_csv/0_occ.csv'))

# Duplicate cells

mask_cam <- raster::crop(mask, camerica) %>%
              raster::mask(., camerica)
mask_cam <- mask_cam * 0 + 1

# writeRaster(mask_cam, paste0(path, '/_raster/_mask/mask_cam_ok.tif'))

df_cells <- dup_cell(path_mask = paste0(path, '/_raster/_mask/mask_cam_1.tif'),
                     path_df = paste0(path, '/_points/_csv/0_occ.csv'))

occ_rmDupCell <- df_cells[[1]]

unique(df_cells[[1]]$Countries)

write.csv(occ_rmDupCell, paste0(path, '/_points/_csv/1_occ_rmDupCell.csv'), row.names = F)

shp_occ_rmDupCell <- occ_rmDupCell
coordinates(shp_occ_rmDupCell) <- ~ Longitude + Latitude
crs(shp_occ_rmDupCell) <- myproj

#detach('package:spdplyr', unload = TRUE)
#writeOGR(obj = shp_occ_rmDupCell, dsn = paste0(path, '/_points/_shp'), layer = 'occ_rmDupCell3', driver = 'ESRI Shapefile')

shp_points <- st_as_sf(as(shp_occ_rmDupCell, "SpatialPointsDataFrame"))

write_sf(obj = shp_points, dsn = paste0(path, '/_points'), layer = 'occ_rmDupCells', driver = 'ESRI Shapefile')

shp_points

# Remove presences generates by random points GLC 2000 process

occ_final <- filter(occ_rmDupCell, Source != 'Random Points GLC 2000')
write.csv(occ_final, paste0(path, '/_points/_csv/1_occ_rmDupCell.csv'), row.names = F)


# Presences count by country

summ_country <- occ_final %>%
                  group_by(Countries) %>%
                  summarise(count = n()) %>%
                  as.data.frame() %>%
                  dplyr::select(Countries, count)

dir.create(paste0(path, '/_tables'))
write.csv(summ_country, paste0(path, '/_tables/count_presencesRmDuplicated.csv'), row.names = F)





# Statistics by country

occ <- read.csv(paste0(path, '/_points/_csv/1_occ_rmDupCell.csv'))

as.character(unique(occ$Source))
unique(occ$Countries)

randomPoints <- filter(occ, Source == 'Random Points GLC 2000')






# myproj_sf <- st_crs("+proj=longlat +datum=WGS84")
# shp_points_2 <- st_transform(shp_points, myproj_sf)
# 
# st_intersects(shp_points_2, camerica)
