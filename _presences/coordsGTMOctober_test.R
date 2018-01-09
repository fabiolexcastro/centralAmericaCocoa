
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)
require(sf)
require(readxl)
require(stringr)

# Initial setups
rm(list = ls())
options(scipen = 999)
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam')
} else {
  if(OSys == 'Windows'){
    setwd('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}

# Functions
treatDF <- function(file){
  
  # file <- fls[3]
  # shts <- excel_sheets(fls[1])
  df <- read_excel(file, sheet = 2)# df_sub <- dplyr::select(df, Colong, Colat)
  
  df1 <- df %>%
          separate(col = Colong, into = c('lon1', 'lon2'), sep = '\\ ') %>%
          separate(col = Colat, into = c('lat1', 'lat2'), sep = '\\ ')
  
  df2 <- df1 %>%
            separate(col = lon1, into = c('lonRm', 'lonGrade'), sep = '[aA-zZ]') %>%
            separate(col = lat1, into = c('latRm', 'latGrade'), sep = '[aA-zZ]') #%>% #dplyr::select(lonGrade, lon2, latGrade, lat2)
  df2$lon2 <- str_replace(df2$lon2, ',', '')
  df2$lat2 <- str_replace(df2$lat2, ',', '')
  df2 <- mutate(df2, lonGrade = as.numeric(lonGrade),
                lon2 = as.numeric(lon2) / 1000,
                latGrade = as.numeric(latGrade),
                lat2 = as.numeric(lat2) / 1000)
  df2 <- df2 %>%
          mutate(lonMin = as.numeric(substr(df2$lon2, start = 1, stop = 2)),
                 latMin = as.numeric(substr(df2$lat2, start = 1, stop = 2)),
                 lonSeg = lon2 - lonMin,
                 latSeg = lat2 - latMin) 
  
  df3 <- df2 %>%
            mutate(lon = lonGrade + (lonMin / 60) + (lonSeg / 3600),
                   lat = latGrade + (latMin / 60) + (latSeg / 3600)) %>%
            dplyr::select(-latRm, - lonRm)
  
  dfOk <- dplyr::select(df3, lon, lat)
  
  return(dfOk)
  
}

# Load files
fls <- list.files('_points/_gtm/', pattern = '.xlsx$', full.names = T) %>% grep('BD', ., value = T)

treatDF(file = fls[1])
