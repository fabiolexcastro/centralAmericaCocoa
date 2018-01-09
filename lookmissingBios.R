


path <- 'W:/_cam/_raster/_climate/_future/_rcp60/_asc'
namesDirs <- list.dirs(paste0(path, '/_2030'), recursive = F)
files <- lapply(namesDirs, list.files, full.names = T, pattern = '.asc$')

bios <- list()

for(i in 1:length(files)){
  
  bios[[i]] <- grep('bio_20', files[[i]], value = T)
  
}

print(bios)
unlist(bios)

