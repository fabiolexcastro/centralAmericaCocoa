
require(raster)
require(rgdal)
require(tidyverse)
require(maptools)
require(rgeos)

# Projection

path <- 'W:/_cam' 
myproj <- CRS("+proj=longlat +datum=WGS84")
ext <- c(-120,-77,5,30)
CAMextent <- extent(ext)

### Global Shape 
all.countries <- readShapeSpatial(paste(path,'/_shp/_base/all_countries.shp',sep=""), proj4string = myproj)
america <- subset(all.countries, all.countries$UNREG1 == 'Central America')
camerica <- crop(america, CAMextent)

#### NA and Zero Raster of studyregion
resolution <- 0.008333333
naraster <- raster(CAMextent,ncols=(diff(ext[1:2])/resolution),nrows=(diff(ext[3:4])/resolution),crs=myproj)
naraster[] <- NA
zeroraster <- naraster
zeroraster[] <- 0

country.raster <- rasterize(camerica, zeroraster, field = 'OBJECTID')
country.raster2 <- rasterize(america, zeroraster, field = 'OBJECTID')

# dir.create(paste0(path, '/_raster/_mask'))
writeRaster(country.raster, paste0(path, '/_raster/_mask/mask_cam.tif'))
writeRaster(country.raster2, paste0(path, '/_raster/_mask/mask_cam_2.tif'), overwrite = T)




