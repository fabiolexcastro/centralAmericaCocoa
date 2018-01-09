
path <- "//dapadfs/Workspace_cluster_9/Coffee_Cocoa2/_cam"

occGTM <- read_csv('Z:/_cam/_points/_new/_second/coordsAllWGS.csv')

load(paste0(path, '/_rData/', '_run4', '/clusteredpresdata.Rdata'))

occ_run4 <- clusteredpresdata[,1:2]

load(paste0(path, '/_rData/', '_run5', '/occ_cluster.rData'))

occ_run5 <- tbl_df(occ_cluster)

filter(occ_run4, Longitude %in% occGTM$Lon & Latitude %in% occGTM$Lat)
filter(occ_run5, Lon %in% occGTM$Lon & Lat %in% occGTM$Lat)

mask <- raster(paste0(path, '/_raster/_mask/mask_cam_2_5min.tif'))

# Remove Duplicate

cellNum <- raster::extract(mask, occ_run4[,c('Longitude', 'Latitude')], cellnumbers = T) 
cells   <- xyFromCell(mask, cellNum[,'cells'])
dupvec  <- duplicated(cells[,c('x', 'y')])
occ_rmDupCell <- tbl_df(occ_run4[!dupvec,])
occ_DupCell   <- tbl_df(occ_run4[dupvec,])

write.csv(occ_rmDupCell, paste0(path, '/_points/_csv/occ_run4_rm.csv'), row.names = F)


