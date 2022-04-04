library("rPref")
library("dplyr")
library("igraph")
library("ggplot2")
library("readxl")
library(openxlsx)
library(tidyr)


indexes <- read_excel("ResultadosClusters2014-index.xlsx")
techniques <- indexes$technique
distances <- indexes$distance
indexes$distance = NULL
indexes$technique = NULL
indexes$year = NULL
indexes$SvD = NULL
indexes$SvB = NULL
indexes$DvB = NULL

indexesNorm <- apply(indexes, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

indexesDF <- as.data.frame.table(indexesNorm)
indexesDF <- spread(data = indexesDF, key = Var2, value = Freq)
indexesDF$clusters_amount <- indexes$clusters_amount
indexesDF$technique <- techniques
indexesDF$distance <- distances
indexesDF$distancetozero = NULL

for (n in 1:nrow(indexesDF)){
  indexesDF$ballhall_index[n] = 1 - indexesDF$ballhall_index[n]
  print(indexesDF$ballhall_index[n]*indexesDF$ballhall_index[n])
  print(indexesDF$silhouette_index[n]*indexesDF$silhouette_index[n])
  print(indexesDF$dunn_index[n]*indexesDF$dunn_index[n])
  indexesDF$distancetozero[n] = sqrt((indexesDF$ballhall_index[n] * indexesDF$ballhall_index[n]) + (indexesDF$silhouette_index[n] * indexesDF$silhouette_index[n]) + (indexesDF$dunn_index[n] * indexesDF$dunn_index[n]))
}

# Calculate Skyline
sky1 <- psel(indexesDF, high(indexesDF$silhouette_index) * high(indexesDF$ballhall_index) * high(indexesDF$dunn_index))

print(sky1)

write.xlsx(sky1, 'MejoresResultados2014.xlsx')