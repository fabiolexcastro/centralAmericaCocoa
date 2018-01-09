
# Load libraries
require(tidyverse)
require(heatmaply)
require(gplots)
require(ggplot2)
require(dendextend)
require(magrittr)
require(rgeos)
require(gtools)

#webshot::install_phantomjs()

# Initial setup 
rm(list = ls())
options(scipen = 999)
cat('\f')
setwd('W:/_cam')

# Load data
load('_rData/_run6/occ.rData')

# Importance variables
load('_rData/_run6/importanceRF.rData')# load('_rData/_run6/aucRF_dist.rData')

df <- data.frame(variable = rownames(importance), value = importance) %>% tbl_df()
df <- df %>% mutate(porcentaje = (MeanDecreaseGini / sum(MeanDecreaseGini)) * 100)
df_sub <- filter(df, porcentaje > 4) %>% arrange(desc(porcentaje))
vrs <- extract2(df_sub, 1) %>% as.character() %>% mixedsort()

df <- as.data.frame(df)
save(df, file = '_rData/varImportance_porc.rData')

# Sub Data
df <- dplyr::select(occ, one_of(vrs))
mt <- data.matrix(df)

# First graph # Tutorial http://earlglynn.github.io/RNotes/package/gplots/heatmap2.html 
png('_figures/_heatmap/heatMap_version1.png', units = 'in', width = 20, height = 20, res = 300)
heatmap.2(mt, dendogram="row")
dev.off()

# Second graph # Tutorial https://cran.r-project.org/web/packages/heatmaply/vignettes/heatmaply.html 
row_dend <- mt %>% dist %>% hclust('ward.D2') %>% as.dendrogram %>%
            set("branches_k_color", k = 5) %>% set("branches_lwd", c(1,5)) %>%
            ladderize

col_dend  <- mt %>% t %>% dist %>% hclust('ward.D2') %>% as.dendrogram %>%
              set("branches_k_color", k = 5) %>% set("branches_lwd", c(1,5)) %>%
              ladderize

# browseURL('_figures/_heatmap/heatMapOkOk.png')
heatmaply(mt, Rowv = row_dend, Colv = col_dend, file = '_figures/_heatmap/heatMap_version2.png')








