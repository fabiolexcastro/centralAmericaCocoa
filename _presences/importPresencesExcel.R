
require(raster)
require(rgdal)
require(tidyverse)
require(sf)
require(readxl)

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

files <- list.files(paste0(path, '/_points/_new/_second/_xls'), full.names = T)#, pattern = '.xlsx$'
path_output <- paste0(path, '/_points/_new/_second/_csv')

# Proof
file   <- files[1]
sheets <- excel_sheets(file)
colnames1 <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y', 'Coord')
path_output <- paste0(path, '/_points/_new/_second/_csv')
sheet  <- sheets[2]

writeDF <- function(file, path_output, sheet){
  
  df <- read_excel(file, sheet, skip = 2) %>%
            mutate(Coord = colnames(.)[5]) %>%
            .[-1,]
  colnames(df) <- colnames1# class(df$X) == 'character'
  
  if(is.integer(grep('°', df$X, value = F)) == TRUE){
    
    print('Character')
    posDg <- grep('°', df$X, value = F)
    dfDg  <- df[posDg,]
    df    <- df[-posDg,] %>%
              mutate(X = as.numeric(X), Y = as.numeric(Y))
    
    write.csv(df, paste0(path_output, '/', sheet[1], '_', unlist(strsplit(basename(file), '.xls'))[1], '.csv'), row.names = F)
    
  } else{
    
    print('No Character - Only plans coordinates')
    
    
    
    
    
  }
  
  
  
  df <- read_excel(file, sheet = sheet, skip = 2) %>%
          rename(X = X_COORDINATE,
                 Y = Y_COORDINATE)
  
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '.csv')
  write.csv(df, paste(path_output, nameDF, sep = '/'))
  
  #
  
  file <- files[1]
  sheet <- 2
  
  df <- read_excel(file, sheet = sheet, skip = 1) %>%
          .[1:5,]
  colnames(df) <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y', 'Altitud')
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '2.csv')
  
  write.csv(df, paste('W:/_cam/_points/_new/', nameDF))
  
  # FILE 2
  
  file <- files[2]
  sheet <- 1
  
  df <- read_excel(file, sheet = sheet, skip = 2) 
  colnames(df) <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y')
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '1.csv')
  df <- df[-1,]
  
  write.csv(df, paste('W:/_cam/_points/_new/', nameDF))
  
  # 
  
  file <- files[2]
  sheet <- 2
  
  df <- read_excel(file, sheet = sheet, skip = 2) 
  colnames(df) <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y')
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '2.csv')
  df <- df[-1,]
  
  write.csv(df, paste('W:/_cam/_points/_new/', nameDF))
  
  # 
  
  file <- files[2]
  sheet <- 3
  
  df <- read_excel(file, sheet = sheet, skip = 2) 
  colnames(df) <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y')
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '3.csv')
  df <- df[-1,]
  
  write.csv(df, paste('W:/_cam/_points/_new/', nameDF))
  
  # 
  
  file <- files[2]
  sheet <- excel_sheets(file)[4]
  
  df <- read_excel(file, sheet = sheet, skip = 2)
  colnames(df) <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y')
  df <- df[complete.cases(df),]
  
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '4.csv')
  write.csv(df, paste(path_output, nameDF, sep = '/'))
  
  #
  
  file <- files[2]
  sheet <- excel_sheets(file)[5]
  
  df <- read_excel(file, sheet = sheet, skip = 2)
  colnames(df) <- c('Fecha', 'Ubicacion', 'Dpto', 'Specie', 'X', 'Y')
  df <- df[complete.cases(df),]
  
  nameDF <- paste0(unlist(strsplit(basename(file), '.xlsx')), '5.csv')
  write.csv(df, paste(path_output, nameDF, sep = '/'))
  
  
  
}


