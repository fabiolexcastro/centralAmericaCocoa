
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
library(doMC)
library(gtools)
library(doSNOW)
library(dplyr)

# Files

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

# Function to others bioclimatics variables

cumTemp <- function(x) {
  
  p <- matrix(nrow = 1, ncol = 4)
  colnames(p) <- paste('bio', 21:24, sep = '')
  
  w <- x[25:36] ### tmax
  y <- x[13:24] ### tmean
  x <- x[1:12]  ### Prec-PET
  z <- x
  
  ### if the values are NA the bios are NA
  if(all(is.na(x))) {
    p[,'bio21'] <- NA
    p[,'bio22'] <- NA
    p[,'bio23'] <- NA
    p[,'bio24'] <- NA
  } else {
    
    ## cumulative deficit to determine dry season (=Bio22)
    
    # print('Bio 22...')
    
    x <- z
    lng <- length(x)
    x <- c(x, x[1:12])
    x[x>0] <- NA
    cumdef <- matrix(ncol = 12, nrow = lng)
    for (i in 1:12) {
      cumdef[, i] <- x[i:(lng + i - 1)]
    }
    p[,'bio22'] <- min(c(0,apply(cumdef, MARGIN = 1, FUN = cumsum)),na.rm=T)
    
    ## cumulative surplus to determine growing season
    x <- z
    lng <- length(x)
    x <- c(z, z[1:12])
    x[x<0] <- NA
    cumplus <- matrix(ncol = 12, nrow = lng)
    
    for (i in 1:12) {
      
      cumplus[, i] <- x[i:(lng + i - 1)]
      
    }
    
    ### If there is no dry season
    ### the length becomes 0
    ### the growing season temp is the mean of monthly mean temp
    ### the dry season max temp is the max temp of the driest month 
    
    if(p[,'bio22']==0){
      
      p[,'bio21'] <- 0
      p[,'bio23'] <- mean(y)
      p[,'bio24'] <- w[which.min(z)]
      
    } else {
      
      ### the mean temperatures for all possible seasons
      y <- c(y, y[1:12])
      n <- matrix(ncol = 12, nrow = lng)
      for (i in 1:12) {
        
        n[, i] <- y[i:(lng + i - 1)]
        
      }
      
      meantemp <- apply(n, MARGIN = 1, FUN = cumsum)
      
      ### the max temperatures for all possible seasons
      w <- c(w, w[1:12])
      n <- matrix(ncol = 12, nrow = lng)
      
      for (i in 1:12) {
        
        n[, i] <- w[i:(lng + i - 1)]
        
      }
      maxtemp <- apply(n, MARGIN = 1, FUN = cumsum)
      
      ### Consecutive months with Prec<PET (=bio21)
      x <- z
      x <- c(x, x[1:12])
      x[x>0] <- NA
      x[x<0] <- 1
      o <- matrix(ncol = 12, nrow = lng)
      
      for (i in 1:12) {
        
        o[, i] <- x[i:(lng + i - 1)]
        
      }
      
      con_months <- max(apply(o,1,cumsum),na.rm=T)
      p[,'bio21'] <- con_months
      
      ### if the dry season is 12 months the growing season mean is the mean of the wettest month
      
      if(con_months==12){
        
        p[,'bio23'] <- y[which.max(z)]
        
      } else { 
        
        ### The meantemp of the wettest season
        p[,'bio23'] <- meantemp[which.max(apply(cumplus, MARGIN = 1, FUN = cumsum))]/(12-con_months)
        
      }
      ### The mean maxtemp of the driest season
      
      p[,'bio24'] <- maxtemp[which.min(apply(cumdef, MARGIN = 1, FUN = cumsum))]/con_months    
      
    }
  
  }
  
  return(p)
  
}

# Function to calculate ETPbios

etpvars <- function(x){
  
  p <- matrix(nrow = 1, ncol = 9)
  colnames(p) = paste('bio', 25:33, sep = '')
  
  tavg <- x[25:36] ### Temp
  prec <- x[13:24] ### PREC
  pet <- x[1:12]  ### PET
  
  ### if the values are NA the bios are NA
  if(all(is.na(x))) { 
    return(p)
  } else {
    
    window <- function(x)  { 
      lng <- length(x)
      x <- c(x,  x[1:3])
      m <- matrix(ncol = 3, nrow = lng)
      for (i in 1:3) { m[,i] <- x[i:(lng+i-1)] }
      apply(m, MARGIN = 1, FUN = sum)
    }
    
    ### BIO_23: Annual PET
    p[,1] <- sum(pet)
    ### BIO_24: PET seasonality (Coefficient of Variation)
    p[,2] <- cv(pet)
    ### BIO_25: MAX PET
    p[,3] <- max(pet)
    ### BIO_26: Min PET
    p[,4] <- min(pet)
    ### BIO_27: Range of PET (PETmax-PETmin)
    p[,5] <- p[,3]-p[,4]
    
    wet <- window(prec)
    hot <- window(tavg)/3
    pet2 <- c(pet,pet[1:2])
    
    ### BIO_28: PET of wettest quarter
    p[,6] <- sum(pet2[c(which.max(wet):(which.max(wet)+2))])
    ### BIO_29:	PET of driest quarter
    p[,7] <- sum(pet2[c(which.min(wet):(which.min(wet)+2))])
    ### BIO_30:	PET of warmest quarter
    p[,8] <- sum(pet2[c(which.max(hot):(which.max(hot)+2))])
    ### BIO_31:	PET of coldest quarter
    p[,9] <- sum(pet2[c(which.min(hot):(which.min(hot)+2))])
    
  }
  
  round(p, digits = 2)
  return(p)
  
} 

# Divid temperature variables by 10

dividTemp <- function(files, nameVar){
  
  layers <- grep(nameVar, files, value = T) %>%
              mixedsort() %>% 
              stack()
  
  layers <- layers/10
  
  return(layers)
  
}

# WorldClim SRAD

rasterOptions(tmpdir = paste0(path, '/_temp2'))

sradfiles <- list.files(path = paste0(path, '/_raster/_climate/_current/_rad/_tif'), pattern = '.tif$', recursive = T, full.names = T)
sradstack <- stack(sradfiles)
names(sradstack) <- paste('srad_', 01:12, sep='') # Change the names to files
sradstack <- sradstack*0.408/1000

# -----------------------
# Current variables
# -----------------------

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



# -----------------------
# Future variables
# -----------------------

years <- c('_2030', '_2050')
models <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[1]))

registerDoMC(length(models))

# cl <- makeCluster(length(models)) #N?mero de nucleos a utilizar
# cl <- makeCluster(3)

registerDoSNOW(cl)

y = 1 #cormacarena
y = 2 #climate

foreach(y = 1:length(years)) %do% {
  
  print(years[y])
  
  foreach(j = 1:length(models), .packages = c('raster', 'rgdal', 'dplyr', 'gtools', 'foreach', 'stringr', 'maptools', 'rgeos')) %dopar% {
    
    print(models[j])
    
    files <- paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j]) %>%
              list.files(., full.names = T, pattern = '.asc$') %>%
              mixedsort()
    
    print(paste0(models[j], ' files'))
    
    tminstack  <- grep('tmin', files, value = T) %>%
                    mixedsort() %>%
                    stack() 
    tminstack  <- tminstack/10
    
    print(paste0(models[j], ' tminStack'))
    
    tmeanstack <- grep('tmean', files, value = T) %>%
                    mixedsort() %>%
                    stack() 
    tmeanstack <- tmeanstack/10
    
    print(paste0(models[j], 'tmeanStack'))
    
    tmaxstack <- grep('tmax', files, value = T) %>%
                    mixedsort() %>%
                    stack() 
    tmaxstack <- tmaxstack/10
    
    print(paste0(models[j], ' tmaxStack'))
    
    precstack <- grep('prec', files, value = T) %>%
                    mixedsort() %>%
                    stack() 
    print(paste0(models[j], ' precStack'))
    
    zeroraster <- tminstack[[1]] * 0 + 1 
    
    print('ETStack')
    
    ETstack <- 0.0023*sradstack*sqrt(reclassify(tmaxstack - tminstack, c(-Inf, 0, 0)))*(tmeanstack + 17.8)
    names(ETstack) <-str_replace(names(ETstack),'srad','PET')
    ETstack <- ETstack * c(31,29,31,30,31,30,31,31,30,31,30,31)
    
    print(paste0(models[j], ' to deficit stack'))
    
    deficitstack <- precstack-ETstack
    DefAndTemp   <- cbind(as.matrix(deficitstack), as.matrix(tmeanstack), as.matrix(tmaxstack))
    
    biovalues <-  t(apply(DefAndTemp, 1, cumTemp))
    
    print(paste0(models[j], ' to Write other Var'))
    
    ### BIO_30: Consecutive Months with less Prec than PET
    bio30 <- zeroraster
    values(bio30) <- biovalues[,1]
    writeRaster(bio30, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_30', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_31: Sum of water deficit during dry season
    bio31 <- zeroraster
    values(bio31) <- biovalues[,2]
    writeRaster(bio31, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_31', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_32: Mean temperature during growing season
    bio32 <- zeroraster
    values(bio32) <- biovalues[,3]
    writeRaster(bio32, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_32', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_33: Max dry season temperature
    bio33 <- zeroraster
    values(bio33) <- biovalues[,4]
    writeRaster(bio33, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_33', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    print('ETP Bioclimatics')
    
    # ETP Bioclimatics
    
    ETPAndPrec <- cbind(as.matrix(ETstack),as.matrix(precstack),as.matrix(tmeanstack))
    etpbios    <- t(apply(ETPAndPrec, 1, etpvars))
    
    print(paste0(models[j], ' to Write ETP Vars'))
    
    ### BIO_21: Annual PET
    bio21 <- zeroraster
    values(bio21) <- etpbios[,1]
    writeRaster(bio21, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_21', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_22: PET seasonality (Coefficient of Variation)
    bio22 <- zeroraster
    values(bio22) <- etpbios_2[,2]
    writeRaster(bio22, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_22', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_23: MAX PET
    bio23 <- zeroraster
    values(bio23) <- etpbios_2[,3]
    writeRaster(bio23, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_23', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_24: Min PET
    bio24 <- zeroraster
    values(bio24) <- etpbios_2[,4]
    writeRaster(bio24, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_24', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_25: Range of PET (PETmax-PETmin)
    bio25 <- zeroraster
    values(bio25) <- etpbios_2[,5]
    writeRaster(bio25, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_25', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_26: PET of wettest quarter
    bio26 <- zeroraster
    values(bio26) <- etpbios_2[,6]
    writeRaster(bio26, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_26', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_27:	PET of driest quarter
    bio27 <- zeroraster
    values(bio27) <- etpbios_2[,7]
    writeRaster(bio27, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_27', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_28:	PET of warmest quarter
    bio28 <- zeroraster
    values(bio28) <- etpbios_2[,8]
    writeRaster(bio28, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_28', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    ### BIO_29:	PET of coldest quarter
    bio29 <- zeroraster
    values(bio29) <- etpbios_2[,9]
    writeRaster(bio29, paste(path, '/_raster/_climate/_future/_rcp60/_asc/', years[y], '/', models[j], '/', 'bio_29', sep = ''), format = 'ascii',overwrite = T, NAflag = -9999)
    
    
 }
  
}

