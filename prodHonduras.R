
# Load libraries

library(readxl)
library(tidyverse)
library(sf)
library(maptools)

# Files
path  <- 'W:/_cam'
tabla <- paste0(path, '/_tables/_PRODUCTION') %>%
            list.files(full.names = T)

sheets <- excel_sheets(tabla)

hnd_adm1 <- st_read(dsn = paste0(path, '/_shp/_base/_admCountries/HND_adm1.shp'))
hnd_adm2 <- st_read(dsn = paste0(path, '/_shp/_base/_admCountries/HND_adm2.shp'))

# First Sheet

df_dptos <- read_excel(tabla, sheet = sheets[1])
abr <- as.vector(as.matrix(df_dptos[1,]))
df_dptos <- df_dptos[-1,] %>%
              .[complete.cases(.),] %>%
              mutate(Código = as.numeric(Código),
                     Producción = as.numeric(Producción),
                     Productividad = as.numeric(Productividad))

colnames(df_dptos)[c(4,5,6,7)] <- c('Productores', 'AreaCultivada', 'Produccion_QQOro', 'Productividad_QQOroMz')

df_dptos <- df_dptos %>%
                mutate(Prod_Ton = (Produccion_QQOro * 45.36)/1000, #1 quintal son 45.36
                       Rdos_Ton_Ha = ((Productividad_QQOroMz * 45.36) / 0.7) / 1000,
                       Productores = as.numeric(Productores),
                       AreaCultivada = as.numeric(AreaCultivada) * 0.7) %>%
                select(-c(Produccion_QQOro, Productividad_QQOroMz))

hnd_adm1 <- hnd_adm1 %>%
                mutate(NAME_1 = iconv(NAME_1, 'UTF-8', 'latin1'),
                       NAME_1 = iconv(NAME_1, to = 'ASCII//TRANSLIT')) 

hnd_adm1_join <- inner_join(hnd_adm1, df_dptos, by = c('NAME_1' = 'Departamento'))
plot(hnd_adm1_join[,'Prod_Ton'])

write_sf(obj = hnd_adm1_join, dsn = paste0(path, '/_shp/_prod/cafe_prod.shp'), driver = 'ESRI Shapefile')

# Second Sheet

df_mpios <- read_excel(tabla, sheet = sheets[2]) %>%
              mutate(AreaCafe = AreaCafe * 0.7,
                     ProdTonHa = ((ProdQQOroMz * 45.36) / 0.7) / 1000,
                     ProdTon = (ProdQQOro * 45.36) / 1000) %>%
              select(-c(ProdQQOro, ProdQQOroMz))

min(df_mpios$ProdQQOro, na.rm = T) * 45.36 / 1000

hnd_adm2 <- hnd_adm2 %>%
              mutate(NAME_1 = iconv(NAME_1, 'UTF-8', 'latin1'),
                     NAME_1 = iconv(NAME_1, to = 'ASCII//TRANSLIT'),
                     NAME_1 = toupper(NAME_1),
                     NAME_2 = iconv(NAME_2, 'UTF-8', 'latin1'),
                     NAME_2 = iconv(NAME_2, to = 'ASCII//TRANSLIT'),
                     NAME_2 = toupper(NAME_2))

hnd_adm2_join <- inner_join(hnd_adm2, df_mpios, by = c('NAME_2' = 'DptoMpio'))
plot(hnd_adm2_join[,'ProdTon'])

write_sf(obj = hnd_adm2_join, dsn = paste0(path, '/_shp/_prod/cafe_prod_adm2.shp'), driver = 'ESRI Shapefile')
