
require(raster)
require(rgdal)
require(tidyverse)

rec.list <- function(len){
  if(length(len) == 1){
    vector("list", len)
  } else {
    lapply(1:len[1], function(...) rec.list(len[-1]))
  }
}


OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/workspace_cluster_9/Coffee_Cocoa2/_eastAfrica' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_eastAfrica'
  }
}
  
adm1 <- shapefile(paste0(path, '/_shp/_base/MWI_adm1.shp'))
occ  <- read_csv(paste0(path, '/_points/_csv/1_occ_rmDup.csv')) 
occ  <- raster::extract(adm1, occ[,1:2]) %>%
          cbind(occ, .) %>%
          dplyr::select(Lon, Lat, NAME_1)
    
vars <- c('bio_1.asc', 'bio_12.asc')

# Current

lyrs_cur <- list.files(paste0(path, '/_raster/_extents/_gral/_current'), full.names = TRUE, pattern = '.asc$') %>%
                grep(paste0(vars, collapse = '|'), ., value = T) %>%
                unique() %>%
                stack()

values_cur <- raster::extract(lyrs_cur, occ[,1:2]) %>%
                cbind(occ, .) %>%
                rename(bio_1_cur = bio_1,
                       bio_12_cur = bio_12)


# Future 2030 & 2050

models <- list.dirs(paste0(path, '/_raster/_extents/_gral/_future/_rcp60/_2030'), recursive = F, full.names = F)

files_list <- rec.list(length(models))
stack_list <- rec.list(length(models))
df_list    <- rec.list(length(models))

for(i in 1:length(models)){
  
  print(models[[i]])
  
  files_list[[i]] <- list.files(paste0(path, '/_raster/_extents/_gral/_future/_rcp60/_2050/', models[[i]]), full.names = T, pattern = '.asc$') %>%
                          grep(paste0(vars, collapse = '|'), ., value = T) %>%
                          unique()
  
  stack_list[[i]] <- stack(files_list[[i]])
  
  df_list[[i]]    <- raster::extract(stack_list[[i]], occ[,1:2]) %>%
                          cbind(occ, .) %>%
                          mutate(model = models[[i]])
  
  print(paste0(models[[i]], '_ To Write CSV'))
  
  write.csv(df_list[[i]], paste0(path, '/_tables/_presencesValuesBiosByDistrict/_rcp60/_2050/df_', models[[i]], '.csv'), row.names = T)
  
}

# Load dataframes

df_list <- list.files(paste0(path, '/_tables/_presencesValuesBiosByDistrict/_rcp60/_2050'), full.names = T, pattern = '.csv$') %>%
              lapply(., FUN = read.csv)

df_all  <- do.call('rbind', df_list) %>%
              tbl_df() %>%
              mutate(year = 2050)

write.csv(df_all, paste0(path, '/_tables/_presencesValuesBiosByDistrict/_rcp60/df_rcp60_2050_districts.csv'), row.names = T)


# Load dataframes 2030 & 2050

df_2030 <- read_csv(paste0(path, '/_tables/_presencesValuesBiosByDistrict/_rcp60/', 'df_rcp60_2030_districts.csv')) %>%
              mutate(RCP = 'rcp65')

df_2050 <- read_csv(paste0(path, '/_tables/_presencesValuesBiosByDistrict/_rcp60/', 'df_rcp60_2050_districts.csv')) %>%
              mutate(RCP = 'rcp65')

df_all_rcp <- rbind(df_2030, df_2050) %>%
                  dplyr::select(3:ncol(.))

# merge with current

df_cur <- tbl_df(values_cur) %>%
                mutate(model = 'current',
                       year = 'current', 
                       RCP = 'current') %>%
                rename(bio_1 = bio_1_cur,
                       bio_12 = bio_12_cur)

df_all  <- rbind(df_cur, df_all_rcp)

write.csv(df_all, paste0(path, '/_tables/_presencesValuesBiosByDistrict/_rcp60/', 'df_currentRCP60_districts.csv'))






