


# Load libraries
require(raster)
require(rgdal)
require(tidyverse)

# Initial setup
rm(list = ls())
options(scipen = 999)
cat('\f')
setwd('Z:/_cam')

# Function to use
calc_area <- function(lyr, shp, yr){
  
  # Tabulate function
  tabFunc <- function(indx, extracted, region, regname) {
    
    dat <- as.data.frame(table(extracted[[indx]]))
    dat$name <- region[[regname]][[indx]]
    
    return(dat)
  }
  
  # Extraction data
  ext <- raster::extract(lyr, adm, method = 'simple')
  
  print('To Make the tab')
  
  tabs <- lapply(seq(ext), tabFunc, ext, adm, 'NAME_ENGLI')
  
  print('Extraction done')
  
  # Tidy data.frame
  df <- do.call(rbind, tabs) %>%
    group_by(name) %>% 
    mutate(Porcentaje = (Freq/sum(Freq)) * 100) %>% 
    ungroup() %>%
    mutate(Porcentaje = round(Porcentaje, 1),
           Var1 = as.numeric(as.character(Var1)))
  
  # labs <- data.frame(number = c(0, 1, 3, 4, 5), categoria = c('Unsuitable', 'Incremental adaptation', 'Transform', 'Expansion', 'Systemic Resilience'))
  # df <- inner_join(df, labs, by = c('Var1' = 'number'))
  
  write.csv(df, paste0('_tables/_cluster/_run6/count_pixelesImpGrabyADM0_', yr, '.csv'), row.names = F)
  
  print('Done')
  
}

# Load data
adm <- shapefile('_shp/_base/_admCountries/CAM_Adm0.shp')
c30 <- raster('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_change_2030.asc')
c50 <- raster('_RF/_run6/_results/_process/_percentil0_5/_impGra/Cluster_change_2050.asc')

# Execute functions
calc_area(lyr = c30, shp = adm, yr = 30)
calc_area(lyr = c50, shp = adm, yr = 50)

# Create graph
imp30 <- read.csv('_tables/_cluster/_run6/count_pixelesbyADM0_30.csv')


gg <- ggplot(data = filter(df_grph, categoria != 'Unsuitable'), aes(x = categoria, y = prc, group = period, fill = period)) +  
  geom_bar(stat = 'identity', position = 'dodge') +
  ylab('Percentage') + 
  xlab('') + 
  theme(legend.position = 'top',
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 14)) +
  labs(fill = '') +
  scale_fill_manual(values = c('#86B404', '#31B404', '#0B6138'))


