
# CIAT, 2017
# Author: Mesa & Castro 
# Target: Extremes scenarios

library(tidyverse)
library(raster)
library(rgdal)


OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa/_cam'
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

run <- '_run4'

load(paste0(path, '/_rData/', run, '/clusteredpresdata.rData'))

occ          <- clusteredpresdata
current      <- occ

# Extract values to current climate

# current      <- raster::extract(stack(list.files(paste0(path, '/_raster/_climate/_current/_asc'), full.names = TRUE, pattern = '.asc$')), occ[,1:2])

dirs         <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc'), full.names = TRUE)
years        <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc'), full.names = FALSE)
models       <- list.dirs(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/', years[1]), full.names = FALSE, recursive = FALSE)

ar5files     <- list.files(paste0(path, '/_raster/_climate/_future/_rcp60/_asc/'), full.names = TRUE, pattern = ".asc$", recursive = TRUE)

bio1_files   <- grep("bio_1.asc", ar5files, value = TRUE) %>% grep('2050', ., value = TRUE)
bio12_files  <- grep("bio_12.asc", ar5files, value = TRUE) %>% grep('2050', ., value = TRUE)

bio1         <- stack(bio1_files)
bio12        <- stack(bio12_files)

bio1_vector  <- raster::extract(bio1,  occ[,1:2])
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

