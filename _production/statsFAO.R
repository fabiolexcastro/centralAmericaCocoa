
# Load libraries
require(raster)
require(rgdal)
require(dplyr)
require(readr)
require(tibble)
require(tidyr)
require(ggplot2)
require(sf)

# Initial Setup
OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam')
} else {
  if(OSys == 'Windows'){
    setwd('//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}; 
options(scipen = 999)
rm(list = ls())

# Function clean NA's in the dataframe
rmNA <- function(df){
  
  # df <- df.prod.cocoa.cam
  indx <- as.data.frame(apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))) %>% rownames_to_column(., 'Column')
  colnames(indx)[2] <- 'Logica'
  columns <- filter(indx, Logica != TRUE)$Column
  y <- df[,columns]
  
  return(y)
  
}

# Load Data
shp.cnts <- shapefile('_shp/_base/all_countries.shp')
df.prod <- read_csv('_tables/_FAO/Production_Crops_E_All_Data.csv')
df.prod.cocoa <- dplyr::filter(df.prod, Item == 'Cocoa, beans')

# Select all crops to Central America
shp.cam <- shp.cnts[shp.cnts@data$UNREG1 == 'Central America',]
df.prod.cam <- filter(df.prod, Area %in% c('Belize', 'Mexico', 'Costa Rica', 'Panama', 'Nicaragua', 'Guatemala', 'Honduras', 'El Salvador'))
df.prod.cocoa.cam <- filter(df.prod.cocoa, Area %in% c('Belize', 'Mexico', 'Costa Rica', 'Panama', 'Nicaragua', 'Guatemala', 'Honduras', 'El Salvador'))

# Clean dataframe (remove columns with NA's)
df.prod.cam <- rmNA(df = df.prod.cam)
df.prod.cocoa.cam <- rmNA(df = df.prod.cocoa.cam)

#
unique(df.prod.cam$Item)
df_cocoa <- dplyr::filter(df.prod.cam, Item == 'Cocoa, beans')
shp_cam <- shp.cnts[shp.cnts@data$UNREG1 == 'Central America',]# shp_cam <- filter(all_countries, UNREG1 == 'Central America')
df_cocoa_cam <- filter(df_cocoa, Area %in% unique(shp_cam@data$ENGLISH))

indx <- as.data.frame(apply(df_cocoa_cam, 2, function(x) any(is.na(x) | is.infinite(x))))
indx <- rownames_to_column(indx, 'Column')
colnames(indx)[2] <- 'Logica'
      
columns <- filter(indx, Logica != 'TRUE')$Column
df_cocoa_cam <- df_cocoa_cam[,columns]
ncoldf <- ncol(df_cocoa_cam)
df_tidy <- tidyr::gather(df_cocoa_cam, 'year', 'value', 8:61) %>%#melt(df_cocoa_cam, measure = paste0('Y', 1961:2014))
              separate(year, c('N', 'year'), sep = 'Y') %>%
              dplyr::select(-N) %>%
              mutate(year = as.numeric(year))

View(df_tidy)
prod2014 <- filter(df_tidy, year == 2014, Element == 'Production') 
sum(prod2014$value)            

# Promedio in all the period
mean_prod <- filter(df_tidy, Element == 'Production') %>%
                group_by(Area) %>%
                summarise(Produccion = mean(value),
                          FirstYear  = min(year),
                          LastYear   = max(year))
  
mean_yield<- filter(df_tidy, Element == 'Yield') %>%
                group_by(Area) %>%
                summarise(Rendimto   = mean(value),
                          FirstYear  = min(year),
                          LastYear   = max(year))

mutate(mean_yield, Rendimto = Rendimto / 10)

apply(mean_prod[,2], 2, mean)
apply(mean_prod[,2], 2, sum)
sum(mean_prod$Produccion)
mean(mean_prod$Produccion)

# test <-  filter(df_tidy, Element == 'Production', Area == 'Costa Rica')
# 
# ggplot(dat = test, aes(x = year, y = value)) + 
#           geom_line()

df_tidy$Area <- gsub('Belize', 'Belice', df_tidy$Area)

gg_prod <- ggplot(dat = filter(df_tidy, Element == 'Production'), aes(x = year, y = value, colour = Area)) + 
                geom_line(size = 1.0) +
                scale_y_log10() +
                theme(legend.position = 'top') +
                ylab('Producción (Ton) Log10') + 
                xlab('Año') +
                labs(color = '') +
                scale_x_continuous(breaks = seq(1960, 2015, 5))

gg_area <- ggplot(dat = filter(df_tidy, Element == 'Area harvested'), aes(x = year, y = value, colour = Area)) + 
                geom_line(size = 1.0) +
                #scale_y_log10() +
                theme(legend.position = 'top') +
                ylab('Area (Has)') + 
                xlab('A?o') +
                labs(color = '') +
                scale_x_continuous(breaks = seq(1960, 2015, 5))

ggsave(gg_prod, file = paste0(path, '/_figures/_FAO/prod.png'), width = 12, height = 6)
ggsave(gg_area, file = paste0(path, '/_figures/_FAO/area.png'), width = 12, height = 6)
       
df_gtm <- filter(df_tidy, Element == 'Production', Area == 'Guatemala')
df_blc <- filter(df_tidy, Element == 'Production', Area == 'Belice')
df_mxc <- filter(df_tidy, Element == 'Production', Area == 'Mexico')
summary(df_mxc$value)

which.min(df_mxc$value)
df_mxc[which.max(df_mxc$value),]

write.csv(df_tidy, '_tables/_PRODUCTION/df_tidy_statsFAO.csv', row.names = FALSE)
# Average for years after 2000

df_2000 <- filter(df_tidy, year > 2000, year < 2011) %>%
              group_by(Area) %>% 
              summarise(Produccion = mean(value),
                        FirstYear = min(year),
                        LastYear = max(year))

# Join in a shapefile
sf_cam <- st_as_sf(as(shp_cam, 'SpatialPolygonsDataFrame'))
cam_join <- inner_join(sf_cam, df_2000, by = c('ENGLISH' = 'Area'))

write.csv(df_2000, paste0(path, '/_tables/_FAO/summ_2000_2010.csv'), row.names = FALSE)
write_sf(cam_join, dsn = paste0(path, '/_shp/_prod'), layer = 'cam_prod2000_2010', driver = 'ESRI Shapefile', update = T) 
       
# Summarize all the crops


