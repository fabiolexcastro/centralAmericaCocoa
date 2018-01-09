

require(tidyverse)

OSys <- Sys.info(); OSys <- OSys[names(OSys)=='sysname']
if(OSys == 'Linux'){
  path <- '//mnt/Workspace_cluster_9/Coffee_Cocoa2/_cam' 
} else {
  if(OSys == 'Windows'){
    path <- '//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam'
  }
}

files <- list.files(paste0(path, '/_points/_new'), full.names = T, pattern = '.csv')
shp   <- shapefile(paste0(path, '/_shp/_base/_admCountries/GTM_adm2.shp'))

df1 <- read_csv(files[1]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 1 - 2')# Dg

df2 <- read_csv(files[2]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 2 - 1')#Planas - Dg

df3 <- read_csv(files[3]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 2 - 2')#Planas #is.numeric(df3$X)

df4 <- read_csv(files[4]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 2 - 3')#Planas

df5 <- read_csv(files[5]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 1 - 1')#Planas

df6 <- read_csv(files[6]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 2 - 4')#Dg

df7 <- read_csv(files[7]) %>%
        select(Ubicacion:Y) %>%
        mutate(Source = 'File 2 - 5')#Planas

# 

posDg2 <- grep('"', df$X, value = F)
df2Dg  <- df[posDg,]

df2Dg$X <- (as.numeric(substr(df2Dg$X, 1, 2)) + (as.numeric(substr(df2Dg$X, 4, 5)) / 60) + (as.numeric(substr(df2Dg$X, 8, 10)) / 3600)) * 1
df2Dg$Y <- (as.numeric(substr(df2Dg$Y, 1, 2)) + (as.numeric(substr(df2Dg$Y, 4, 5)) / 60) + (as.numeric(substr(df2Dg$Y, 8, 10)) / 3600)) * -1
df2Dg   <- mutate(df2Dg, Source = 'File 2 - 1') %>%
              .[,2:7]

df2 <- df2[-posDg2,] %>%
          mutate(X = as.numeric(X),
                 Y = as.numeric(Y))

df1 <- rbind(df1, df2Dg)

dfDg <- df1

df5 <- df5 %>%
          mutate(Ubicacion = LUGAR,
                 Dpto = rep(NA, nrow(.)),
                 Specie = 'Theobroma cacao', 
                 Source = 'File 1 - 1') %>%
          select(Ubicacion, Dpto, Specie, X, Y, Source)
  
  
dfPl <- rbind(df2, df3, df4, df5, df7)

# Degrees Df1 - Df6

dfDg <- rbind(df1, df6)

# Write tables

write.csv(dfPl, paste0(path, '/_points/_new/df_planas.csv'), row.names = F)
write.csv(dfDg, paste0(path, '/_points/_new/df_degrees.csv'), row.names = F)

