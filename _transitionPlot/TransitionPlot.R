library("Gmisc")
library("grid")
library("raster")
library("maptools")


folder <- getwd()
datafolder <- paste(folder ,"/data/",sep="")
resultsfolder <- paste(folder ,"/results/",sep="")
modelfolder <- paste(folder ,"/models/",sep="")
gisdatafolder <- "C:/Users/CBunn/Documents/GISData/"

myproj = CRS("+proj=longlat +datum=WGS84")

# clustercurrent <- raster(paste(resultsfolder,"AEZ_2030s.asc",sep=""),crs=myproj)
# clusterfuture <- raster(paste(resultsfolder,"AEZ_2050s.asc",sep=""),crs=myproj)

 clustercurrent <- raster(paste(resultsfolder,"Clusterfuture/RF_3clust_","current",".asc",sep=""))
 clusterfuture <- raster(paste(resultsfolder,"Clusterfuture/RF_3clust_","2050s",".asc",sep=""))

peru_adm0 <- readShapeSpatial(paste(gisdatafolder,"CountryData/PER_adm/PER_adm0.shp",sep=""),proj4string=myproj)

clustercurrent <- mask(clustercurrent,peru_adm0)
clusterfuture <- mask(clusterfuture,peru_adm0)

# rclm <- matrix(c(
#   0.5,1.5,1,
#   1.5,2.5,2,
#   2.5,3.5,2,
#   3.5,4.5,3,
#   4.5,5.5,4,
#   5.5,6.5,5,
#   6.5,7.5,6),ncol=3,byrow=T)
# 
# clustercurrent <- reclassify(clustercurrent,rclm)
# clusterfuture <- reclassify(clusterfuture,rclm)

######## to which cluster belong the grid cells of current zones in the future? 
No_of_classes <- 6

changesum <- data.frame(Domain=1:No_of_classes)
for (i in 1:No_of_classes){
  zone <- clusterfuture
  zone[zone!=i] <- NA
  zone[zone==i] <- 1
  change <- zone*clustercurrent
  freqchange <- freq(change)
  dimnames(freqchange)[[2]][2] <- paste("Current",i)
  changesum <- merge(changesum,freqchange,by.x="Domain",by.y="value",all=T)
}

clusterlabels2 <- c("Tipo 1","Tipo 2","Tipo 3","Mixed","Marginal","No apto"
)
clustercols2<-c("#d95f02","mediumslateblue","#1b9e77","khaki1",grey(0.6),grey(0.95)               
                #             ,"#7570b3",grey(0.6),grey(0.95)
                # ,grey(0.8),"#e7298a","#e6ab02"
)


trans <- changesum[1:No_of_classes,2:(No_of_classes+1)]
trans[is.na(trans)]<- 0
trans[No_of_classes,No_of_classes]<-0

transition_matrix <- trans

# no_boxes <- 7
# transition_matrix <- matrix(NA, nrow=no_boxes, ncol=no_boxes)
# transition_matrix[1,] <- c(0,2214,5588, 975,4176,   16685)
# transition_matrix[2,] <- c(1372,3143,  0,   5, 489,  0)
# transition_matrix[3,] <- c( 12349,6722,9869,3901  ,   15715, 250)
# transition_matrix[4,] <- c(  12867,8193, 217  ,   35935,1329, 125)
# transition_matrix[5,] <- c(720, 619,  0,  0,2065,  0)
# transition_matrix[6,] <- c(   0,  0,  0,  0,  0,  0)
#transition_matrix <- t(transition_matrix)

grid.newpage()
transitionPlot(transition_matrix,
               box_txt=clusterlabels2,
               type_of_arrow="gradient",
               box_width = 1/4,
               cex=1,
               min_lwd = unit(0.15,"mm"),
               max_lwd = unit(20,"mm"),
               tot_spacing = 0.1,
               overlap_add_width = unit(0.1,"mm"),
               fill_start_box=clustercols2,
               box_label = c("Actual","2050s"),
               box_label_cex = 2,
               lwd_prop_total = T,
               arrow_clr = clustercols2,
               mar = unit(c(18,3,18,3), "mm"),
               txt_start_clr=c(rep("white",No_of_classes-1),"black")
               )


#grid.newpage()
