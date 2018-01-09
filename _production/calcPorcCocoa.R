
# Load libraries
require(raster)
require(rgdal)
require(tidyverse)

# Initial setup
rm(list = ls())
cat('\f')
setwd('W:/_cam')
options(scipen = 999)

# Load data
df <- read.csv('_tables/_FAO/FAOSTAT_data_10-25-2017.csv') %>% 
        tbl_df() %>%
        mutate(Area = iconv(Area, 'UTF-8', 'latin1'),
               Producto = iconv(Producto, 'UTF-8', 'latin1'),
               Elemento = iconv(Elemento, 'UTF-8', 'latin1'))

dfSum <- df %>%
            group_by(Area, CodigoELemento, Elemento, Unidad, Producto) %>%
            summarize(ValorAvg = mean(Valor)) %>%
            ungroup() %>%
            na.omit()

df.cnt <- filter(dfSum, Area != 'América del Centro')

df.cnt.sumcrops <- df.cnt %>%
                      group_by(Area, Elemento, Unidad, Producto) %>%
                      summarize(ProdTotal = round(sum(ValorAvg), 1)) %>%
                      ungroup()


# Mexico




sum(df.cnt.sumcrops$ProdTotal)

mutate(df.cnt.sumcrops, Porc = (ProdTotal / sum(ProdTotal) * 100)) %>%
  filter(Producto == 'Cacao, en grano')


df.cnt.sumcrops %>%
  group_by(Elemento) %>%
  summarize(x = sum(ProdTotal, na.rm = T))

df.cnt.porc <- df.cnt %>%
                filter(Elemento %in% c('Producción', 'Área Cosechada')) %>%
                group_by(Area, Elemento, Unidad, Producto) %>%
                mutate(Porcentaje = (ValorAvg / sum(ValorAvg)) * 100)

x <- filter(df.cnt, Producto == 'Cacao, en grano', Elemento == 'Producción')
y <- df.cnt %>%
      filter(Elemento == 'Producción') %>%
      group_by(Area, Elemento, Producto) %>%
      summarize(Value = sum(ValorAvg)) %>%
      ungroup() %>%
      mutate(Value = round(Value, 0))

View(y)

filter(df.cnt.sumcrops, Elemento == 'Rendimiento')
filter(df.cnt.sumcrops, Producto == 'Cacao, en grano')
