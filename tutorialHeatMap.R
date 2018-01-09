# Tutorial 
# URL: http://sebastianraschka.com/Articles/heatmaps_in_r.html#b-reading-in-data-and-transform-it-into-matrix-format

header.true <- function(df) {
  names(df) <- as.character(unlist(df[1,]))
  df[-1,]
}


nms <- c('measure', 'var1','var2','var3','var4')
ln1 <- c('measurement1',0.094,0.668,0.4153,0.4613)
ln2 <- c('measurement2',0.1138,-0.3847,0.2671,0.1529)
ln3 <- c('measurement3',0.1893,0.3303,0.5821,0.2632)
ln4 <- c('measurement4',-0.0102,-0.4259,-0.5967,0.18)
ln5 <- c('measurement5',0.1587,0.2948,0.153,-0.2208)
ln6 <- c('measurement6',-0.4558,0.2244,0.6619,0.0457)
ln7 <- c('measurement7',-0.6241,-0.3119,0.3642,0.2003)
ln8 <- c('measurement8',-0.227,0.499,0.3067,0.3289)
ln9 <- c('measurement9',0.7365,-0.0872,-0.069,-0.4252)
ln10 <- c('measurement10',0.9761,0.4355,0.8663,0.8107)

df <- as.data.frame(rbind(nms, ln1, ln2, ln3, ln4, ln5, ln6, ln7, ln8, ln9, ln10))
df <- header.true(df)
df <- df %>% mutate(var1 = as.numeric(as.character(var1)), var2 = as.numeric(as.character(var2)), var3 = as.numeric(as.character(var3)), var4 = as.numeric(as.character(var4)))
rownames(df) <- as.character(df[,1])
rnames <- as.character(df[,1])
df <- df[,2:5]
mat_data <-  data.matrix(df)

my_palette <- colorRampPalette(c("red", "yellow", "green"))(n = 299)
col_breaks <- c(seq(-1, 0, length = 100), # for red
                seq(0, 0.8, length = 100),  # for yellow
                seq(0.81, 1, length = 100)) # for green

heatmap.2(mat_data,
          cellnote = mat_data,  # same data set for cell labels
          main = 'Correlation', # heat map title
          notecol = 'black',    # change font color of cell labels to black
          density.info="none",  # turns off density plot inside color legend
          trace = 'none',         # turns off trace lines inside the heat map
          margins = c(12,9),     # widens margins around plot
          col = my_palette,       # use on color palette defined earlier
          # breaks=col_breaks,    # enable color transition at specified limits
          dendrogram = "row",     # only draw a row dendrogram
          Colv = "NA")            # turn off column clustering

# Using cluster data
distance <- dist(mat_data, method = "manhattan")
cluster <- hclust(distance, method = "ward.D")

heatmap.2(mat_data,
          # cellnote = mat_data,
          main="Correlation",
          notecol="black",
          density.info="none",
          trace="none",
          margins=c(12, 9),
          col=my_palette,
          # breaks = col_breaks,
          dendogram="row",
          Rowv=as.dendrogram(cluster), # apply default clustering method
          Colv=as.dendrogram(cluster)) # apply default clustering method


heatmap.2(mat_data,
          # cellnote = mat_data,
          main="Correlation",
          notecol="black",
          density.info="none",
          trace="none",
          margins=c(12, 9),
          col=my_palette,
          # breaks = col_breaks,
          dendogram="row",
          Rowv=as.dendrogram(cluster), # apply default clustering method
          # Colv=as.dendrogram(cluster),
          RowSideColors = c(    # grouping row-variables into different
            rep("gray", 3),   # categories, Measurement 1-3: green
            rep("blue", 3),    # Measurement 4-6: blue
            rep("black", 4)))


heatmaply(mat_data, file = "my_heatmap.html")
heatmaply(mat_data)
