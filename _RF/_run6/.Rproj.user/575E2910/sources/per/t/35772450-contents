
# Load libraries
require(tidyverse)
require(heatmaply)


# Initial setup
rm(list = ls())
setwd('Z:/_cam')
setwd('W:/_cam')
options(scipen = 999)

# Function
contrastPub <- function(descriptors, grouped, biomains, biolabels, pathGraph, nameGraph, Nrow, Ncol, width, height){   
  
  png((filename = paste(pathGraph, nameGraph, sep = '/')), width = width, height = height, res = 120)
  par(mfrow = c(Nrow, Ncol))
  
  for(i in 1:length(descriptors)){
    
    formula <- as.formula(paste(descriptors[i], "~ cluster"))
    Anov    <- aov(formula = formula, data = grouped)
    cont    <- glht(Anov, mcp(cluster = 'GrandMean'))
    plot(cont, xlab = NA, sub = NULL, main = NA)
    title(main = biomains[i], line = 1.5)
    title(ylab = 'Group', line = 2.5)
    # title(xlab = paste(biolabels[i], '\n', '?', round(mean(grouped[,descriptors[i]]), digits = 1)), line = 3) 
    # title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1), line = 3)) 
    # print(round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))
    title(xlab = paste(biolabels[i], '\n', 'ø', round(mean(extract2((grouped[,descriptors[i]]), 1)), 1))) 
    
  }  
  
  dev.off()
}

# Load data
load('_rData/_run6/grouped.rData')
run <- '_run6'
grouped$cluster <- as.factor(grouped$cluster)# grouped[,"cluster"] <- as.factor(grouped[,'cluster'])
descriptors <- colnames(grouped)

# Temperature (centigrados)
centigrados <- c(1, 2, 5, 6, 7, 8, 9, 10, 11, 32, 33)
desc_temp_grados <- names(grouped[,centigrados])
grouped_temp_grados <- grouped[centigrados] %>%
                          cbind(., grouped['cluster']) %>%
                          mutate(bio_1 = bio_1/10,
                                 bio_2 = bio_2/10,
                                 bio_5 = bio_5/10,
                                 bio_6 = bio_6/10,
                                 bio_7 = bio_7/10,
                                 bio_8 = bio_8/10,
                                 bio_9 = bio_9/10,
                                 bio_10 = bio_10/10,
                                 bio_11 = bio_11/10) 
biolabels_temp_grados <- rep("°C", length(centigrados))
biomains_temp_grados  <- c('Temp. promedio anual', 'Rango diurno Temp.', 'Temp. máx. mes más cálido', 'Temp. minima mes más frio', 
                           'Rango temp. anual', 'Temp. promedio del mes más húmedo', 'Temp. promedio del mes más seco', 'Temp. promedio mes más cálido', 
                           'Temp. promedio trimestre más frio', 'Temp. promedio durante\n temporada de desarrollo', 'Temp. máxima de estación seca')
# biomains_temp_grados  <- c("Annual mean temp", "Mean diurnal range", "Max Temp of Warmest Month", "Min temp of coldest month", 
#                            "Temp annual range", "Mean temp of wettest quarter", "Mean temp of driest quarter", "Mean temp of warmest quarter", 
#                            "Mean temp of coldest quarter", "Mean temp during \n growing season", "Max dry season temp")

contrastPub(descriptors = desc_temp_grados, grouped = grouped_temp_grados, biomains = biomains_temp_grados,
            biolabels = biolabels_temp_grados, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Temp Grados.png', Nrow = 4, Ncol = 3, width = 1200, height = 1000)

# Temperature (no centigrados)
noCentigrados <- c(3, 4)
desc_temp_nogrados <- names(grouped[noCentigrados])  
grouped_temp_nogrados <- grouped[noCentigrados] %>% cbind(., grouped['cluster'])
biolabels_temp_nogrados <- rep("-", length(noCentigrados)) 
biomains_temp_nogrados <- c('Isotermalidad (bio 2/bio 7) * 100', 'Estacionalidad temperatura (sd * 100)')# biomains_temp_nogrados <- c('Isothermality (bio 2/bio 7) * 100', 'Temperature seasonality (sd * 100)')

contrastPub(descriptors = desc_temp_nogrados, grouped = grouped_temp_nogrados, biomains = biomains_temp_nogrados,
            biolabels = biolabels_temp_nogrados, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Temp No Grados.png', Nrow = 1, Ncol = 2, width = 1000, height = 400) 

# Precipitation
desc_ppt <- descriptors[c(12:20)]
biomains_ppt <- c('Prec. anual', 'Prec. mes más húmedo', 'Prec. mes más seco', 'Estacionalidad de la prec.', 
                  'Prec. trimestre más humedo', 'Prec. trimestre más seco', 'Prec trimestre más calido', 
                  'Prec. trimestre mas frio', 'Meses consecutivos secos < 100 mm')# biomains_ppt <- c('Annual prec', 'Prec of wettest month', 'Prec of driest month', 'Prec seasonality', 'Prec of wettest quarter', 'Prec of driest quarter', 'Prec of warmest quarter', 'Prec of coldest quarter', 'Number of consecutive months \n less 100 mm')
grouped_ppt <- grouped[,c(desc_ppt, 'cluster')]
biolabels_ppt <- c(rep('mm', 3), '-', rep('mm',4),'Numero meses')

contrastPub(descriptors = desc_ppt, grouped = grouped_ppt, biomains = biomains_ppt,
            biolabels = biolabels_ppt, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Precipitation.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)

# ETP 
desc_etp <- descriptors[c(21:29)]
biolabels_etp <- c("mm", "-", rep("mm",7))
biomains_etp <- c("ETP Anual", "Estacinoalidad ETP", "ETP máxima", "ETP mínima", "Rango ETP", "ETP trimestre más humedo", 
                  "ETP trimestre más seco", "ETP trimestre más calido", "ETP trimestre más frio")
# biomains_etp <- c("Annual PET", "PET Seasonality", "PET Max", "PET Min", "PET Range", "PET of wettest quarter", 
#                   "PET of driest quarter", "PET of warmest quarter", "PET of coldest quarter")
grouped_etp    <- grouped[,c(desc_etp, "cluster")]

contrastPub(descriptors = desc_etp, grouped = grouped_etp, biomains = biomains_etp,
            biolabels = biolabels_etp, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot ETP.png', Nrow = 3, Ncol = 3, width = 1000, height = 800)

# New climatic variables
desc_water <- descriptors[c(30:31)]
biolabels_water <- c('Number Months', 'mm')
biomains_water <- c('Meses consecutivos con ETP\n menor a la prec.', 'Suma de déficit de agua durante\n temporada seca')
# biomains_water <- c('Consecutive Months with less \n Prec than PET', 'Sum of water deficit during dry season')
grouped_water <- grouped[c(desc_water, 'cluster')]
namesWater<- paste0('bio_', 30:31)

contrastPub(descriptors = desc_water, grouped = grouped_water, biomains = biomains_water,
            biolabels = biolabels_water, pathGraph = paste0('_figures/_contrastPlots/', run), 
            nameGraph = 'Contrast Plot Water.png', Nrow = 1, Ncol = 2, width = 1200, height = 600) 
