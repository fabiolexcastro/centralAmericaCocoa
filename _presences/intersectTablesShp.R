
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(compare)

# Path
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

# Files
filesOcc <- list.files(paste0(path, '/_points/_new/_second/_shp'), full.names = T, pattern = '.shp$')#
filesAdm <- list.files(paste0(path, '/_shp/_base/_admCountries'), full.names = T, pattern = '.shp$')

# Presences
occGTM <- shapefile(grep('GTM', filesOcc, value = T))
occUTM <- shapefile(grep('UTM', filesOcc, value = T))
occWGS <- shapefile(grep('WGS', filesOcc, value = T))

# Shp Adm
admGTM <- shapefile(grep('GTM.shp', filesAdm, value = T))
admUTM <- shapefile(grep('UTM.shp', filesAdm, value = T))
admWGS <- shapefile(grep('GTM_adm1.shp', filesAdm, value = T))

# Intersections
# GTM
occGTM_int <- raster::intersect(occGTM, admGTM) %>% tbl_df()
colnames(occGTM_int)[2:3] <- c('Ubicacion', 'Dpto')
occGTM_int <- dplyr::select(occGTM_int, Ubicacion, Dpto, Specie, X, Y, Coords, NAME_1) %>%
                mutate(NAME_1 = iconv(NAME_1, 'UTF-8', 'latin1'),
                       Dpto = iconv(Dpto, 'UTF-8', 'latin1'))
occGTM_int$NAME_1 <- gsub('é', 'e', occGTM_int$NAME_1)
occGTM_int$Dpto   <- gsub('é', 'e', occGTM_int$Dpto)
occGTM_int <- mutate(occGTM_int, Match = as.character(Dpto == NAME_1))
missGTM <- filter(occGTM_int, Match == 'FALSE') 

write.csv(missGTM, paste0(path, '/_points/_new/_second/missGTM.csv'), row.names = F)

# Rows that are not in the intersect

nrow(occGTM@data) - nrow(occGTM_int)
df1 <- tbl_df(occGTM@data[,c('X', 'Y')])
df2 <- tbl_df(occGTM_int[,c('X', 'Y')])

noMatchGTM <- sqldf('SELECT * FROM df1 EXCEPT SELECT * FROM df2')#a1Ina2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2') #sqldf('SELECT *FROM df WHERE name IN ("Tom", "Lynn")')#https://stackoverflow.com/questions/3171426/compare-two-data-frames-to-find-the-rows-in-data-frame-1-that-are-not-present-in
noMatchGTM_ok <- filter(tbl_df(occGTM@data), X %in% noMatchGTM$X | Y %in% noMatchGTM$Y)
colnames(noMatchGTM_ok)[2] <- 'Ubicacion'

write.csv(noMatchGTM_ok, paste0(path, '/_points/_new/_second/missGTM_noIntersect.csv'), row.names = F)
  
# WGS
occWGS_int <- raster::intersect(occWGS, admWGS) %>% tbl_df() 
colnames(occWGS_int)[2] <- 'Ubicacion'            
occWGS_int <- dplyr::select(occWGS_int, Ubicacion, Dpto, Specie, X, Y, Coords, NAME_1) %>%
                mutate(NAME_1 = iconv(NAME_1, 'UTF-8', 'latin1'),
                       Ubicacion = iconv(Ubicacion, 'UTF-8', 'latin1'))
occWGS_int$NAME_1 <- gsub('é', 'e', occWGS_int$NAME_1)
occWGS_int <- mutate(occWGS_int, Match = as.character(Dpto == NAME_1))


