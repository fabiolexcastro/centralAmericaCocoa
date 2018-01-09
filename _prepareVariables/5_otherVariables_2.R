
# Calculate bioclimatic variables about ETP
# Bunn & Castro
# CIAT, 2017

# Load Libraries

library(stringr)
library(rgdal)
library(raster)
library(maptools)
library(dismo)
library(SDMTools)
library(maps)
library(rgeos)
library(doParallel)
# library(doMC)
library(gtools)
library(doSNOW)
library(dplyr)
library(snowfall)

# Files

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

source(paste0(path, '/_codes/_r/_prepareVariables/functionsBios.R'))

# WorldClim SRAD

rasterOptions(tmpdir = '/home/fcastro/temporal')
# rasterOptions(tmpdir = paste0(path, '/_temp'))

if(file.exists(paste0(path, '/_rData/sradstack.rData'))){
  
  load(paste0(path, '/_rData/sradstack.rData'))
  
} else{
  
  sradfiles <- list.files(path = paste0(path, '/_raster/_climate/_current/_rad/_tif'), pattern = '.tif$', recursive = T, full.names = T)
  sradstack <- stack(sradfiles)
  names(sradstack) <- paste('srad_', 01:12, sep='') # Change the names to files
  sradstack <- sradstack*0.408/1000
  save(sradstack, file = paste0(path, '/_rData/sradstack.rData'))
  
}

x <- unstack(sradstack)
nameSrad <- paste0('srad_', 1:12)

Map('writeRaster', x = x, filename = paste0(path, '/_raster/_climate/_current/_sradOk/', nameSrad, '.asc'))

# ------- Current variables -------

files_current <- list.files(paste0(path, '/_raster/_climate/_current/_asc'), full.names = T, pattern = '.asc$')
toMatch <- c('pr', 'tm')
namesVars <- gsub('.asc', '', basename(files_current)) %>%
                gsub('\\d', '', .) %>%
                gsub('_', '', .) %>%
                unique() %>%
                grep(paste0(toMatch, collapse = '|'), ., value = T)

# namesVars <- c('tmean', 'tmax', 'tmin')

lapply(files_current, nameVar = namesVars, FUN = dividTemp)
lapply(files_current, FUN = dividTemp, nameVar = namesVars)
#lapply(files_current, nameVar = 'tmin', FUN = dividTemp)
 
# files <- lapply(namesDirs, list.files, full.names = T, pattern = '.asc$')

tmin_stack  <- dividTemp(files = files_current, nameVar = 'tmin')
tmax_stack  <- dividTemp(files = files_current, nameVar = 'tmax')
tmean_stack <- dividTemp(files = files_current, nameVar = 'tmean')
prec_stack  <- grep('prec', files_current, value = T) %>%
                  mixedsort() %>%
                  stack()
zeroraster  <- tmin_stack[[1]] * 0 + 1 

ETstack <- 0.0023*sradstack*sqrt(reclassify(tmax_stack - tmin_stack, c(-Inf, 0, 0)))*(tmean_stack + 17.8)
names(ETstack) <-str_replace(names(ETstack),'srad','PET')
ETstack <- ETstack * c(31,29,31,30,31,30,31,31,30,31,30,31)

deficitstack <- prec_stack - ETstack
DefAndTemp <- cbind(as.matrix(deficitstack), as.matrix(tmean_stack), as.matrix(tmax_stack))
biovalues  <- t(apply(DefAndTemp, 1, cumTemp))

namesVars <- paste0('bio_', 30:33)

for(i in 1:length(namesVars)){
  
  layer <- zeroraster
  values(layer) <- biovalues[,i]
  writeRaster(layer, paste0(path, '/_raster/_climate/_current/_asc/', namesVars[i]), format = 'ascii', overwrite = T, NAflag = -9999)
  
}

ETPAndPrec <- cbind(as.matrix(ETstack),as.matrix(prec_stack),as.matrix(tmean_stack))
etpbios    <- t(apply(ETPAndPrec, 1, etpvars))
namesETPs  <- paste0('bio_', 21:29)

for(i in 1:ncol(etpbios)){
  
  layer <- zeroraster
  values(layer) <- etpbios[,i]
  writeRaster(layer, paste0(path, '/_raster/_climate/_current/_asc/', namesETPs[i]), format = 'ascii', overwrite = T, NAflag = -9999)
  
}



# ------- Future variables -------

years <- c('_2030', '_2050')
models <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[1]))

registerDoMC(11)
# cl <- makeCluster(length(models)) #N?mero de nucleos a utilizar
# cl <- makeCluster(18)
# registerDoSNOW(cl)

# y = 1 # tropico
y = 2 # caribe

# foreach(y = 1:length(years)) %do% {

#foreach(j = 8:length(models), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'stringr', 'maptools', 'rgeos')) %dopar% {

for(j in 2:length(models)){
  
    print(models[j])
    
    sradstack <- list.files(paste0(path, '/_raster/_climate/_current/_sradOk'), full.names = T, pattern = '.asc$') %>%
                    mixedsort() %>%
                    stack()
  
    print('sradStack')
    
    files <- paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j]) %>%
              list.files(., full.names = T, pattern = '.asc$') %>%
              mixedsort()
    
    print(paste0(models[j], ' files'))
    
    tminstack  <- dividVar(files, 'tmin')
    tmeanstack <- dividVar(files, 'tmean')
    tmaxstack  <- dividVar(files, 'tmax')
    precstack  <- dividVar(files, 'prec')
    zeroraster <- tminstack[[1]] * 0 + 1 
    
    print(paste0(models[j], ' to ETStack'))
    
    ETstack <- 0.0023*sradstack*sqrt(reclassify(tmaxstack - tminstack, c(-Inf, 0, 0)))*(tmeanstack + 17.8)
    names(ETstack) <-str_replace(names(ETstack),'srad','PET')
    ETstack <- ETstack * c(31,29,31,30,31,30,31,31,30,31,30,31)
    
    print(paste0(models[j], ' to deficit stack'))
    
    # rm(tminstack, tmaxstack)
    
    # ------ Def and Temp ------
    
    deficitstack <- precstack-ETstack
    DefAndTemp   <- cbind(as.matrix(deficitstack), as.matrix(tmeanstack), as.matrix(tmaxstack))
    
    # snowfall
     
    sfInit(parallel = TRUE, cpus = 5)
    sfExport('DefAndTemp')
    sfExport('cumTemp')
    sfLibrary(rgeos)
    sfLibrary(raster)
    sfLibrary(rgdal)
    sfLibrary(snowfall)
     
    biovalues <- sfApply(DefAndTemp, 1, cumTemp)
    biovalues_2 <- t(biovalues) 
    biovalues_vars <- paste0('bio_', 30:33)
    
    # biovalues <-  t(apply(DefAndTemp, 1, cumTemp))
    # values(layer) <- biovalues[,i]
    # writeRaster(layer, paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', biovalues_vars))

    print(paste0(models[j], ' to Write other Var'))
    
    for(i in 1:ncol(biovalues_2)){

      layer <- zeroraster
      values(layer) <- biovalues_2[,i]
      writeRaster(layer, paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/',  biovalues_vars[i], '.asc'), overwrite = T, NAflag = -9999)

    }
    
    # print('ETP Bioclimatics')
    
    # ETP Bioclimatics
    
    ETPAndPrec <- cbind(as.matrix(ETstack),as.matrix(precstack),as.matrix(tmeanstack))
    
    rm(ETstack, precstack)
    
    sfInit(parallel = TRUE, cpus = 4)
    sfExport('ETPAndPrec')
    sfExport('etpvars')
    sfLibrary(rgeos)
    sfLibrary(raster)
    sfLibrary(rgdal)
    
    etpbios <- sfApply(ETPAndPrec, 1, etpvars)
    etpbios_2 <- t(etpbios) 
    
    print(paste0(models[j], ' to Write ETP Vars'))
    
    namesETPs  <- paste0('bio_', 21:29)
    etp_bioList <- list()
    
    for(i in 1:ncol(etpbios_2)){
      
      etp_bioList[[i]] <- zeroraster
      values(etp_bioList[[i]]) <- etpbios_2[,i]
      
    }
    
    for(i in 1:length(etp_bioList)){
      
      writeRaster(etp_bioList[[i]], paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/',  namesETPs[i], '2.asc'), overwrite = T, NAflag = -9999)
      
    }
    
    toRemove <- list.files('/home/fcastro/temporal', full.names = T)
    lapply(toRemove, file.remove)
    
    print(paste0(models[j], ' Finish...'))

}
  


