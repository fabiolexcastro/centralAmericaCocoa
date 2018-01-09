
require(tidyverse)
setwd('Z:/_cam')
load('_rData/_run6/importanceRF.rData')
load('_rData/_run6/aucRF_dist.rData')


df <- data.frame(variable = rownames(importance), value = importance) %>% tbl_df()
df <- df %>% mutate(porcentaje = (MeanDecreaseGini / sum(MeanDecreaseGini)) * 100)

df_sub <- filter(df, porcentaje > 4) %>% arrange(desc(porcentaje))
sd(df$porcentaje)

mean(auc)
sd(auc)
