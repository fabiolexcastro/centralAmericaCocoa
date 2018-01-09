# Barplot with double Y-axis
# H. Achicanoy
# CIAT, 2016

# R options
options(warn = -1)
options(scipen = 999)

# load packages
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))
suppressMessages(library(grid))
suppressMessages(library(gtable))

#setwd('C:/Users/haachicanoy/Documents/CIAT/Asesorias/Fabio Castro/_future')
#setwd("D:/CC/_bd/_costaMarfil/_tables/_tablesToGraphs/_future")
#setwd("D:/CC/_bd/_malawi/_tables/_climate/_toGraphs")
#setwd("Z:/_bd/_eastAfrica/_tables/_zonalStatistic/_districts_countries")
setwd("W:/_eastAfrica/_tables/_districts_countries")

#Current data
#metrics <- c('min', 'mean', 'max')
metrics <- 'mean'
currentInfo <- lapply(metrics, function(x){
  file <- list.files(path = './_current', pattern = x, full.names = T)
  data <- read.csv(file)
  data <- data[, c("zone", "prec_1", "prec_10", "prec_11", "prec_12", "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8", "prec_9",
                      "etp_1", "etp_2", "etp_3", "etp_4", "etp_5", "etp_6", "etp_7", "etp_8", "etp_9", "etp_10", "etp_11", "etp_12")]
  data$varID  <- x
  data$gcm    <- 'Present'
  data$Period <- 'current'
  return(data)
})
currentInfo <- do.call(rbind, currentInfo)

# Future 2030
gcmList     <- list.files(path = './_2030/', pattern = metrics[1], full.names = F)
gcmList     <- gsub(pattern = 'mean_', replacement = '', x = gcmList, fixed = T)
gcmList     <- gsub(pattern = '_2030.csv', replacement = '', x = gcmList, fixed = T)
fut2030Info <- lapply(metrics, function(x){
  process   <- lapply(gcmList, function(y){
    file    <- list.files(path = './_2030/', pattern = x, full.names = T)
    file    <- file[grep(pattern = paste('_', y, '_2030', sep = ''), x = file)]
    data    <- read.csv(file)
    data <- data[, c("zone", "prec_1", "prec_10", "prec_11", "prec_12", "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8", "prec_9",
                     "etp_1", "etp_2", "etp_3", "etp_4", "etp_5", "etp_6", "etp_7", "etp_8", "etp_9", "etp_10", "etp_11", "etp_12")]
    data$varID <- x
    data$gcm <- y
    data$Period <- '2030'
    return(data)
  })
  process <- do.call(rbind, process)
  return(process)
})
fut2030Info <- do.call(rbind, fut2030Info) #Join in only one table
head(fut2030Info)

# Future 2050
fut2050Info <- lapply(metrics, function(x){
  process <- lapply(gcmList, function(y){
    file <- list.files(path = './_2050/', pattern = x, full.names = T)
    file <- file[grep(pattern = paste('_', y, '_2050', sep = ''), x = file)]
    data <- read.csv(file)
    data <- data[, c("zone", "prec_1", "prec_10", "prec_11", "prec_12", "prec_2", "prec_3", "prec_4", "prec_5", "prec_6", "prec_7", "prec_8", "prec_9",
                     "etp_1", "etp_2", "etp_3", "etp_4", "etp_5", "etp_6", "etp_7", "etp_8", "etp_9", "etp_10", "etp_11", "etp_12")]
    data$varID <- x
    data$gcm <- y
    data$Period <- '2050'
    return(data)
  })
  process <- do.call(rbind, process)
  return(process)
})
fut2050Info <- do.call(rbind, fut2050Info)

# all info in just one dataset
all_data <- rbind(currentInfo, fut2030Info, fut2050Info); rm(currentInfo, fut2030Info, fut2050Info, gcmList, metrics)

precData <- all_data[,c('zone', 'varID', 'gcm', 'Period', paste('prec_', 1:12, sep=''))] #data organization
etpData  <- all_data[,c('zone', 'varID', 'gcm', 'Period', paste('etp_', 1:12, sep=''))]

precData        <- precData %>% gather(month, prec, prec_1:prec_12) #uso of gather
precData$month  <- as.numeric(as.character(gsub(pattern = 'prec_', replacement = '', x = precData$month)))

etpData        <- etpData %>% gather(month, etp, etp_1:etp_12)
etpData$month  <- as.numeric(as.character(gsub(pattern = 'etp_', replacement = '', x = etpData$month)))

all_data2       <- Reduce(function(...) merge(..., by=c('zone', 'varID', 'gcm', 'Period', 'month'), all.x=TRUE), list(precData, etpData)) 
rm(precData, etpData)
all_data2$month <- factor(x = all_data2$month, levels = 1:12, ordered = TRUE) #conversion to factor to month

# summarize future information by period
precData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(mean(prec))
names(precData)[length(precData)] <- 'prec' #change header
etpData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(mean(etp))
names(etpData)[length(etpData)] <- 'etp'

# summarize future information by period sd
sdprecData <- all_data2 %>% group_by(zone, varID, Period, month) %>% summarise(sd(prec))
names(sdprecData)[length(sdprecData)] <- 'sd_prec'
sdprecData$sd_prec[sdprecData$Period=='current'] <- 0

#all_data3       <- Reduce(function(...) merge(..., by=c('zone', 'varID', 'Period', 'month'), all.x=TRUE), list(precData, tminData, tmeaData, tmaxData))
all_data3       <- Reduce(function(...) merge(..., by=c('zone', 'varID', 'Period', 'month'), all.x=TRUE), list(precData, etpData, sdprecData))
#rm(precData, tminData, tmeaData, tmaxData)
rm(precData, tminData, tmeaData, tmaxData, sdprecData)
all_data3$month <- gsub(pattern = 10, replacement = 'Oct', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 11, replacement = 'Nov', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 12, replacement = 'Dec', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 1, replacement = 'Jan', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 2, replacement = 'Feb', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 3, replacement = 'Mar', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 4, replacement = 'Apr', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 5, replacement = 'May', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 6, replacement = 'Jun', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 7, replacement = 'Jul', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 8, replacement = 'Aug', x = all_data3$month, fixed = T)
all_data3$month <- gsub(pattern = 9, replacement = 'Sep', x = all_data3$month, fixed = T)
all_data3$month  <- factor(all_data3$month, levels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), ordered = TRUE)
all_data3$Period <- gsub(pattern = 'current', replacement = 'Current', x = all_data3$Period)
all_data3$Period <- factor(x = all_data3$Period, levels = c('Current', '2030', '2050'), ordered = T)

# load script to do dual axis
#source('Z:/_bd/_eastAfrica/_codes/_R/_graphs/dual_axis_function.Rdata')
source('W:/_eastAfrica/_codes/_R/_graphs/dual_axis_function.R')

max_zone_1 <- all_data3[all_data3$zone & all_data3$varID=='mean',] %>%
				  mutate(val_up = prec + sd_prec) %>%
				  summarise(max(val_up))

max_zone__temp <- all_data3[all_data3$zone & all_data3$varID=='mean',] %>%
				            mutate(val_up = etp) %>%
				            summarise(max(val_up))


# Zone 1
#gg <- ggplot(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = prec)) + geom_bar(aes(fill = Period), stat = "identity", position = "dodge")
gg <- ggplot(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = prec)) + geom_bar(aes(fill = Period), stat = "identity", position = "dodge") + geom_errorbar(aes(ymin = prec - sd_prec, ymax = prec + sd_prec, group = Period), width=.2, position=position_dodge(.9))
gg <- gg + theme_bw() + xlab('Months') + ylab('Precipitation (mm)')
gg <- gg + theme(legend.position = "top")
gg <- gg + scale_y_continuous(limits = c(0, 250), breaks = c(50,100,150,200,250)) #Añadido por Fabio
#gg <- gg + theme(panel.grid.major.x = element_blank(),
                 #panel.grid.minor.x = element_blank(),
                 #panel.grid.major.y = element_blank(),
                 #panel.grid.minor.y = elem???ent_blank())
gg <- gg + ggtitle("Districts that produce Tea in Kenia - Tanzania & Uganda") + theme(panel.grid.major = element_line(colour = "black"))

gg2 <- ggplot(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = etp)) + geom_line(aes(colour = Period, group = Period))
#gg2 <- gg2 + geom_line(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = tmin, colour = Period, group = Period), linetype = 2)
#gg2 <- gg2 + geom_line(data = all_data3[all_data3$zone==1 & all_data3$varID=='mean',], aes(x = month, y = tmax, colour = Period, group = Period), linetype = 2)
gg2 <- gg2 + theme_bw() + xlab('Months') + ylab(expression("ETP (mm)"))
gg2 <- gg2 + scale_y_continuous(limits = c(0, 250), breaks = c(50,100,150,200,250))
gg2 <- gg2 + theme(panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.grid.major.y = element_blank(),
                   panel.grid.minor.y = element_blank()) 
             
png(filename = "../Districts_Countries_etpPrec.png", width = 9, height = 8, units = 'in', res = 300)
ggplot_dual_axis(gg, gg2, "y")
dev.off()




