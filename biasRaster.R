

path <- 'W:/_cam'

lyr <- raster(paste0(path, '/_raster/_climate/_current/_asc/bio_1.asc'))
lyr <- lyr * 0 + 1
dprodCocoa <- read_csv(paste0(path, '/_tables/_FAO/summ_2000_2010.csv'))
adm0      <- shapefile(paste0(path, '/_shp/_base/_admCountries/CAM_Adm0.shp')) %>%
  dplyr::select(ISO, NAME_0, NAME_ENGLI) %>%
  mutate(NAME_ENGLI = as.character(NAME_ENGLI))


cntOcc <- read.csv(paste0(path, '/_points/_csv/countOcc.csv')) %>%
  mutate(NAME_0 = as.character(NAME_0))
bias <- inner_join(prodCocoa, cntOcc, by = c('Area' = 'NAME_0')) %>%
  mutate(prob = count/Produccion)
bias.shape <- inner_join(adm0, bias, by = c('NAME_ENGLI' = 'Area'))

bias.raster <- rasterize(bias.shape, lyr, field = 'prob', fun = 'mean')
plot(bias.raster)


writeRaster(bias.raster, paste0(path, '/_raster/bias_raster.asc'))
