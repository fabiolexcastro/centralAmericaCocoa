
require(raster)
require(rgdal)
require(spdplyr)
require(tidyverse)

# OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
# if(OSys == 'Linux'){
#   path <- '//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam' 
# } else {
#   if(OSys == 'Windows'){
#     path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
#   }
# }
# 
# 
# load(paste0(path, '/_rData/_run1/clusterpresdata.rData'))
# ls()

load('/occ.rData')

# occ <- norm[,1:2]
cnt <- shapefile(paste0(path, '/_shp/_base/all_countries.shp'))
occ_cnt <- raster::extract(cnt, occ[,1:2]) %>%
              cbind(occ, .) %>%
              tbl_df() %>%
              dplyr::select(ENGLISH, Longitude, Latitude)

summ <- occ_cnt %>%
          group_by(ENGLISH) %>%
          summarise(n = n())

write.csv(occ, paste0(path, '/occOk.csv'))

