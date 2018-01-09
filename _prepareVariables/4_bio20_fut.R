# ----------------------------------------------------------------------
# Calculate Variable Bio 20 to GCM's
# Adaptado of Bunn by Castro F
# CIAT, 2017
# ----------------------------------------------------------------------

library(gtools)
library(stringr)
library(rgdal)
library(raster)
library(maptools)
library(dismo)
library(SDMTools)
library(maps)
library(rgeos)
library(dplyr)
library(foreach)
library(doSNOW)
library(doMC) #For Linux

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

bio20 <- function(files, path_output, threshold, mask){
  
  
  layers     <- list.files(files, full.names = T, pattern = '.asc$') %>%
                    mixedsort() %>%
                    grep('prec', ., value = T)
  
  preclayers <- stack(layers)
  precbin    <- reclassify(preclayers, c(-Inf, threshold, 1, threshold, Inf, NA))
  names(precbin) <- names(preclayers)
  twoyears   <- addLayer(precbin, precbin)
  allperiods <- stack()
  
  # print('Stack ...')
  
  for(i in 1:12){
    
    oneyear <- twoyears[[i:(i+11)]]
    drymonths <- cumsum(oneyear)
    maxnumber <- max(drymonths, na.rm = T)
    allperiods <- addLayer(allperiods, maxnumber)
    
    rm(maxnumber)
    rm(drymonths)
    
  }
  
  bio_20 <- max(allperiods, na.rm = T)
  bio_20[is.na(bio_20)] <- 0
  bio_20 <- raster::mask(bio_20, mask)
  
  # print('Write Raster')
  
  writeRaster(bio_20, paste(path_output, 'bio_20.asc', sep = '/'))
  
  rm(allperiods)
  
}

library(foreach)
library(doSNOW) ## For Windows

rasterOptions(tmpdir = paste0(path, '/_temp3'))

cl <- makeCluster(18) ## For Windows
registerDoSNOW(cl)  ## For Windows

# registerDoMC(19)
years   <- c('_2030', '_2050')
models  <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[1])) 
y <- 2

foreach(j = 2:length(models), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach')) %dopar% { 
  
  # print(models[j])
  
  bio20(files = paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j]),
        path_output = paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j]),
        threshold = 100,
        mask = shapefile(paste0(path, '/_shp/mask_cam1.shp')))
  
}

j = y = 1

foreach(y = 1:length(years)) %do% {
  
  # print(years[y])
  
  foreach(j = 1:length(models), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach')) %dopar% { 
    
    # print(models[j])
    
    bio20(files = paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j]),
          path_output = paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j]),
          threshold = 100,
          mask = shapefile(paste0(path, '/_shp/mask_cam1.shp')))
    
  }
  
}

foreach(y = c()) %do% {
  
  print(years[y])
  
  foreach(j = 1:length(models), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach'), .export = 'bio20') %dopar% { 
    
    print(models[j])
    
    bio20(files = paste0(path, '/_raster/_extents/_gral/_future/_rcp26/', years[y], '/_asc/', models[j]),
          path_output = paste0(path, '/_raster/_extents/_gral/_future/_rcp26/', years[y], '/_asc/', models[j]),
          threshold = 50,
          mask = shapefile(paste0(path, '/_shp/countriesEastAfrica.shp')))
    
  }
  
}




foreach(y = 1:length(years)) %do% {
  
  foreach(j = 1:length(models), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach'), .export = 'bio20') %dopar% { 
    
    print(models[j])
    
    gcmfiles         <- list.files(paste0(path, "/_raster/_climate/_future/_rcp60/", years[y], "/", models[[j]]), full.names = TRUE, pattern = ".asc") %>%
                          grep(pattern = "prec", value =T) %>%
                          mixedsort()
    
    preclayers       <- stack(gcmfiles)
    precbin          <- reclassify(preclayers,c(-Inf,50,1,50,Inf,NA))
    names(precbin)   <- names(preclayers)
    twoyears         <- addLayer(precbin, precbin)
    allperiods       <- stack()
    
    print("Stack")
    
    for (i in 1:12){
      
      oneyear        <- twoyears[[i:(i+11)]]
      drymonths      <- cumsum(oneyear)
      maxnumber      <- max(drymonths,na.rm=T)
      allperiods     <- addLayer(allperiods,maxnumber)
      
      rm(maxnumber)
      rm(drymonths)
      
    }
    
    bio20               <- max(allperiods,na.rm=T)
    bio20[is.na(bio20)] <- 0
    bio20               <- mask(bio20, raster(paste(path, "/_raster/_base/studyarea_30s",sep="")))
    
    print("write Raster")
    writeRaster(bio20, paste0(path, "/_raster/_climate/_future/_rcp60/", years[y], "/", models[j], "/bio_20.asc"), format="ascii") #toca iterar por carpeta con una letra variable
    
    rm(allperiods)
    
    cat(models[j], " - done","\n")
    
  }
  
}

