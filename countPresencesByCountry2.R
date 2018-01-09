

library(raster)
library(rgdal)
library(tidyverse)
library(spdplyr)

occ <- read.csv('Z:/_cam/_points/_csv/4_occ_swd.csv')
shp <- shapefile('Z:/_cam/_shp/_base/_admCountries/CAM_Adm0.shp') %>%
          dplyr::select(., NAME_0, NAME_ENGLI)

occ_adm0 <- raster::extract(shp, occ[,1:2]) %>% 
              cbind(., occ[,1:2]) %>% 
              dplyr::select(NAME_0:Lat) %>%
              tbl_df()

summ <- occ_adm0 %>%
            group_by(NAME_0) %>%
            summarise(count = n())
            
write.csv(summ, 'Z:/_cam/_points/_csv/countOcc.csv', row.names = F)
