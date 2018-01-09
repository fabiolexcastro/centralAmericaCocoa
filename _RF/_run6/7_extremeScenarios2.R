
# CIAT, 2017
# Author: Castro 
# Target: Extremes scenarios

library(tidyverse)
library(raster)
library(rgdal)
library(magrittr)

rm(list = ls())

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  setwd('//dapadfs/workspace_cluster_9/Coffee_Cocoa/_cam')
} else {
  if(OSys == 'Windows'){
    setwd('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cam')
  }
}


# Load first data
run <- '_run6'
load(paste0('_rData/', run, '/clustereddata.rData'))

# Extract values current
crn_bio1  <- raster('_raster/_climate/_current/_asc/bio_1.asc')
crn_bio12 <- raster('_raster/_climate/_current/_asc/bio_12.asc')
crn_bio1_vl  <- raster::extract(crn_bio1, occ[,c('Lon', 'Lat')]) %>% mean()
crn_bio12_vl <- raster::extract(crn_bio12, occ[,c('Lon', 'Lat')]) %>% mean()

df_crn <- data.frame(crn_bio1 = crn_bio1_vl, crn_bio12 = crn_bio12_vl) %>%
            t() %>%
            as.data.frame() %>%
            rename(Valor = V1) %>%
            mutate(var = c('bio_1', 'bio_12'),
                   period = 'current',
                   Model = 'current') %>%
            dplyr::select(var, period, Model, Valor)

# Extract values future
dirs <- list.files(paste0('_raster/_climate/_future/_rcp60/_asc'), full.names = TRUE)
years <- list.files(paste0('_raster/_climate/_future/_rcp60/_asc'), full.names = FALSE)
models <- list.dirs(paste0('_raster/_climate/_future/_rcp60/_asc/', years[1]), full.names = FALSE, recursive = FALSE)
ar5files <- list.files(paste0('_raster/_climate/_future/_rcp60/_asc/'), full.names = TRUE, pattern = ".asc$", recursive = TRUE)

bio1_ftr  <- grep('bio_1.asc', ar5files, value = TRUE) %>% grep('2050', ., value = TRUE) %>% stack()
bio12_ftr <- grep('bio_12.asc', ar5files, value = TRUE) %>% grep('2050', ., value = TRUE) %>% stack()

bio1_ftr_vls  <- raster::extract(bio1_ftr, occ[,c('Lon', 'Lat')])
bio12_ftr_vls <- raster::extract(bio12_ftr, occ[,c('Lon', 'Lat')])
                
colnames(bio1_ftr_vls) <- c(paste(models, years[2], sep = ''))
colnames(bio12_ftr_vls) <- c(paste(models, years[2], sep = ''))

bio1_ftr_avg  <- apply(bio1_ftr_vls, 2, mean)
bio12_ftr_avg <- apply(bio12_ftr_vls, 2, mean)

bio1_ftr_df <- as.data.frame(bio1_ftr_avg) %>% rownames_to_column(var = 'Model') 
bio12_ftr_df <- as.data.frame(bio12_ftr_avg) %>% rownames_to_column(var = 'Model')

bio_ftr_df <- cbind.data.frame(bio1_ftr_df, bio12_ftr_df)[,c(1, 2, 4)]
bio_ftr_df <- bio_ftr_df %>%
                gather(Tst, Variable, -Model) %>%
                mutate(var = c(rep('bio_1', 19), rep('bio_12', 19)),
                       period = 2050) %>%
                dplyr::select(var, period, Model, Variable) %>%
                rename(Valor = Variable)

# Join Dataframes (current and future)
df_all <- rbind(df_crn, bio_ftr_df)# ;write.csv(df_all, '_tables/values_presences_bios_cr_ftr.csv', row.names = F)

# Maximo Futuro
pos_max_bio1 <- filter(df_all, var == 'bio_1', period == '2050') %>%
                    extract2(4) %>%
                    which.max()
max_bio1 <- filter(df_all, var == 'bio_1', period == '2050') %>%
                    .[pos_max_bio1,] 
  
pos_max_bio12 <- filter(df_all, var == 'bio_12', period == '2050') %>%
                    extract2(4) %>%
                    which.max()
max_bio12 <- filter(df_all, var == 'bio_12', period == '2050') %>%
                    .[pos_max_bio12,]

bio1_gral <- filter(df_all, var == 'bio_1', period == '2050') %>%
                mutate(Valor = round(Valor, 1))
bio12_gral <- filter(df_all, var == 'bio_12', period == '2050') %>%
                mutate(Valor = round(Valor, 1))

# Máximo temperatura y máximo precipitación es: miroc_esm

# Minimo Futuro
pos_min_bio1 <- filter(df_all, var == 'bio_1', period == '2050') %>%
                    extract2(4) %>%
                    which.min()
min_bio1 <- filter(df_all, var == 'bio_1', period == '2050') %>%
                    .[pos_min_bio1,]

pos_min_bio12 <- filter(df_all, var == 'bio_12', period == '2050') %>%
                    extract2(4) %>%
                    which.min()
min_bio12 <- filter(df_all, var == 'bio_12', period == '2050') %>%
                    .[pos_min_bio12,]

mean(bio12_gral$Valor)
mean(bio1_gral$Valor)

# Minimo temperatura y medianamenta máximo en la precipitación es mri_cgcm3

# Promedio
mean_bio1 <- filter(df_all, var == 'bio_1', period == '2050') %>% 
                extract2(4) %>%
                mean()
mean_bio12 <- filter(df_all, var == 'bio_12', period == '2050') %>% 
                extract2(4) %>%
                mean()

bio1 <- filter(df_all, var == 'bio_1')
bio1_qn <- quantile(bio1$Valor, seq(0, 1, 0.05))

bio12 <- filter(df_all, var == 'bio_12')
bio12_qn <- quantile(bio12$Valor, seq(0, 1, 0.05))

filter(df_all, var == 'bio_1', period == '2050') %>% 
  filter(Valor > 264, Valor < 266)

filter(df_all, var == 'bio_12', period == '2050') %>% 
  filter(Valor > 2290, Valor < 2496)


df_sub <- filter(df_all, Model %in% c('mri_cgcm3_2050', 'cesm1_cam5_2050', 'miroc_esm_2050', 'current'))

diff_bio1_cesm1_cam <- df_sub[3,4] - df_sub[1,4]
diff_bio12_cesm1_cam <- df_sub[6,4] - df_sub[2,4]
diff_bio1_cesm1_cam / df_sub[3,4] * 100
diff_bio12_cesm1_cam / df_sub[6,4] * 100

diff_bio1_miroc_esm <- df_sub[4,4] - df_sub[1,4]
diff_bio12_miroc_esm <- df_sub[7,4] - df_sub[2,4]
diff_bio1_miroc_esm  / df_sub[4,4] * 100
diff_bio12_miroc_esm  / df_sub[7,4] * 100

diff_bio1_mri_cgcm3 <- df_sub[5,4] - df_sub[1,4]
diff_bio12_mri_cgcm3 <- df_sub[8,4] - df_sub[2,4]
diff_bio1_mri_cgcm3 /df_sub[5,4] * 100
diff_bio12_mri_cgcm3 / df_sub[8,4] * 100

# Extract values to current climate
dirs <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc'), full.names = TRUE)
years <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc'), full.names = FALSE)
models <- list.dirs(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[1]), full.names = FALSE, recursive = FALSE)

ar5files <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/'), full.names = TRUE, pattern = ".asc$", recursive = TRUE)
bio1_files <- grep("bio_1.asc", ar5files, value = TRUE) %>% grep('2050', ., value = TRUE)
bio12_files <- grep("bio_12.asc", ar5files, value = TRUE) %>% grep('2050', ., value = TRUE)

bio1 <- stack(bio1_files)
bio12 <- stack(bio12_files)

bio1_vector <- raster::extract(bio1,  occ[,1:2])
bio12_vector <- raster::extract(bio12, occ[,1:2])

# Only 2050

colnames(bio1_vector)  <- c(paste(models, years[2], sep =""))# paste(models, years[2], sep =""))
colnames(bio12_vector) <- c(paste(models, years[2], sep =""))# paste(models, years[2], sep =""))

currentMeans  <- colMeans(current)

# -------------------
# Bio 1
# -------------------

current_bio_1 <- currentMeans['bio_1']
current_bio_1_vector <- as.vector(rep(current_bio_1, ncol(bio1_vector)))

bio_1_means <- colMeans(bio1_vector) %>% 
                  as.data.frame() %>% 
                  rownames_to_column("Model")

resta_bio1  <- bio1_vector - current_bio_1_vector
colnames(bio_1_means)[2] <- 'bio_1' 

current_future_df_1 <- data.frame(bio_1_means, current_bio_1_vector) %>%
                          mutate(diff_1 = bio_1 - current_bio_1_vector)

outlier_bio1 <- boxplot.stats(current_future_df_1$bio_1)
boxplot(current_future_df_1$bio_1, main="Pressure Height", boxwex=0.1)
outliers_bio1 <- data.frame(outliers = outlier_bio1$stats) %>%
                    inner_join(., current_future_df_1, by = c('outliers' = 'bio_1'))

summary(current_future_df_1$bio_1) 

# -------------------
# Bio 12
# -------------------

current_bio_12 <- currentMeans['bio_12'] 
current_bio_12_vector <- as.vector(rep(current_bio_12, ncol(bio12_vector)))

bio_12_means <- colMeans(bio12_vector) %>% 
                  as.data.frame() %>% 
                  rownames_to_column("Model")

resta_bio12  <- bio12_vector - current_bio_12_vector
colnames(bio_12_means)[2] <- 'bio_12' 

current_future_df_12 <- data.frame(bio_12_means, current_bio_12_vector) %>%
                          mutate(diff_12 = bio_12 - current_bio_12_vector)

# variable bio 12

outlier_bio12 <- boxplot.stats(current_future_df_12$bio_12)
boxplot(current_future_df_12$bio_12, main="Pressure Height", boxwex=0.1)
outliers_bio12 <- data.frame(outliers = outlier_bio12$stats) %>%
                    inner_join(., current_future_df_12, by = c('outliers' = 'bio_12'))

# diferencia

outlier_bio12 <- boxplot.stats(current_future_df_12$diff_12)
boxplot(current_future_df_12$diff_12, main="Pressure Height", boxwex=0.1)
outliers_bio12_dif <- data.frame(outliers = outlier_bio12$stats) %>%
                          inner_join(., current_future_df_12, by = c('outliers' = 'diff_12'))


# Promedio
summary(current_future_df_12$bio_12)
summary(current_future_df_1$bio_1)
mean(current_future_df_1$bio_1)

# Intermediate

current_future_df_1[which(current_future_df_1$bio_1 > mean(current_future_df_1$bio_1 + 1)),]

filter(current_future_df_1, bio_1 > (mean(bio_1) + 1)) %>%
  filter(bio_1 < (mean(bio_1) - 1))

filter(current_future_df_12, bio_12 > (mean(bio_12) + 1)) %>%
  filter(bio_12 < (mean(bio_12) - 1))





# ------

# All

df_all <- cbind(current_future_df_1, current_future_df_12[,2:ncol(current_future_df_12)])
df_all <- df_all %>% 
            mutate(Anio = c(rep(2030, length(models)), rep(2050, length(models)))) %>%
            mutate(Models = rep(models, 2))

# Clean the dataframe

df_all <- df_all[,c("Models", "Anio", "current_bio_1_vector", "current_bio_12_vector", "bio_1", "bio_12", "diff_1", "diff_12")]
colnames(df_all) <- c("Model", "Anio", "bio_1_c", "bio_12_c", "bio_1", "bio_12", "diff_1", "diff_12")

df_all %>%
  mutate(bio_1_c  = round(bio_1_c, 2)) %>%
  mutate(bio_12_c = round(bio_12_c, 2)) %>%
  mutate(bio_1    = round(bio_1, 2)) %>%
  mutate(bio_12   = round(bio_1, 2))

df_all <- df_all %>% 
            mutate_each(funs(round(.,1)), starts_with("bio")) %>% 
            mutate_each(funs(round(.,1)), starts_with("diff"))

# 2050 

df_2050 <- filter(df_all, Anio == 2050)

arrange(df_2050, desc(diff_1))[1,]
arrange(df_2050, desc(diff_12))[1,]

df_2050[with(df_2050, order(-diff_1, -diff_12)), ]

summary(df_2050$diff_1)
summary(df_2050$diff_12)

# df_2050[with(df_2050, order(df_2050$diff_1, df_2050$diff_12)),]
# apply(df_all, MARGIN = 2, function(x) max(x, na.rm=TRUE))
# summarise_each(df_all, funs(max(., na.rm=TRUE)))

