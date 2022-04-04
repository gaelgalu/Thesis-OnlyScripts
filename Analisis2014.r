#Imports

library("readxl")
library("caret")
library("cluster")
library(openxlsx)
library("fpc")
library("dbscan")
library(mclust)
library("mstknnclust")
library("amap")
library(clusterCrit)

data2014 <- read.csv(
  file = 'mat_datos_2014.csv',
  stringsAsFactors = TRUE, 
  strip.white = TRUE,
  sep = ';',
  header = TRUE
)

for (n in 1:ncol(data2014)){
  data2014[,n] <- as.numeric(unlist(data2014[,n]))
}

names <- read_excel("complejidadHospitalesOriginal.xlsx")

data2014$nombreEstablecimiento <- ifelse(data2014$IdEstablecimiento == names$IdEstablecimiento, names$Nombre)

data2014$complejidad <- ifelse(data2014$IdEstablecimiento == names$IdEstablecimiento, names$Complejidad)

data2014$considerar <- ifelse(data2014$IdEstablecimiento == names$IdEstablecimiento, names$Considerar)

data2014 = subset(data2014, data2014$complejidad != "NULL")
data2014 = subset(data2014, data2014$considerar != "NULL")

dataAfterRemoves2014 <- data2014[, -nearZeroVar(data2014, uniqueCut = 1, saveMetrics = FALSE,
                            names = FALSE, foreach = FALSE, allowParallel = TRUE)]

#write.xlsx(dataAfterRemoves2014, 'VariablesFiltradas2014.xlsx')

dataAfterRemoves2014$complejidad <- NULL
dataAfterRemoves2014$nombreEstablecimiento <- NULL

#dataAfterRemoves2014 = read_excel("254VariablesFiltradas2014.xlsx")
#dataAfterRemoves2014$IdEstablecimiento <- NULL

#average_quality=NULL

#df <- data.frame()
#names(df) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")

normalizedData = as.matrix(apply(dataAfterRemoves2014, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X))))
d1 = as.matrix(Dist(normalizedData, method="correlation"))
d2 = as.matrix(Dist(normalizedData, method="euclidean"))

# # 1) Normalizar la matriz
# # 2) Aplicación de clustering
# # 	- Sobre los datos (matriz) completa
# # 	  - Kmeans
# # 	  - Clara
# # 	  - PAM
# # 	  - Fanny
# # 	  - Mclust


# # 	- Distancia - Hamming y correlación.
# # 	  - Kmeans
# # 	  - Clara
# # 	  - PAM
# # 	  - Fanny
# # 	  - Mclust
# # 	  - Mstknn

# #Kmeans
# for (a in 2:10)
# {
# 	cluster_kmeans = kmeans(normalizedData, a)
# 	quality_kmeans =  silhouette(cluster_kmeans$cluster, dist(normalizedData))
# 	quality_kmeans_index=summary(quality_kmeans)
# 	average_quality = c(average_quality,quality_kmeans_index$si.summary[[4]])
	
# 	indexes = intCriteria(normalizedData, cluster_kmeans$cluster, c("Dunn", "Ball_Hall"))
	
# 	#de <- data.frame("kmeans", a, quality_kmeans_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "raw normalized matrix/euclidean", "2014")
# 	de <- data.frame("kmeans", a, quality_kmeans_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "raw normalized matrix/euclidean", "2014")
# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# #Kmeans
# for (a in 2:10)
# {
# 	cluster_kmeans = kmeans(d1, a)
# 	quality_kmeans =  silhouette(cluster_kmeans$cluster, d1)
# 	quality_kmeans_index=summary(quality_kmeans)
# 	average_quality = c(average_quality,quality_kmeans_index$si.summary[[4]])

# 	indexes = intCriteria(d1, cluster_kmeans$cluster, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("kmeans", a, quality_kmeans_index$si.summary[[4]], "correlation", "2014")
# 	de <- data.frame("kmeans", a, quality_kmeans_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "correlation", "2014")
# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# #Kmeans
# for (a in 2:10)
# {
# 	cluster_kmeans = kmeans(d2, a)
# 	quality_kmeans =  silhouette(cluster_kmeans$cluster, d2)
# 	quality_kmeans_index=summary(quality_kmeans)
# 	average_quality = c(average_quality,quality_kmeans_index$si.summary[[4]])
# 	indexes = intCriteria(d2, cluster_kmeans$cluster, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("kmeans", a, quality_kmeans_index$si.summary[[4]], "euclidean", "2014")
# 	de <- data.frame("kmeans", a, quality_kmeans_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "euclidean", "2014")
# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_clara = clara(normalizedData, a)
# 	quality_clara =  silhouette(cluster_clara)
# 	quality_clara_index=summary(quality_clara)
# 	average_quality = c(average_quality,quality_clara_index$si.summary[[4]])

# 	indexes = intCriteria(normalizedData, cluster_clara$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("clara", a, quality_clara_index$si.summary[[4]], "raw normalized data","2014")
# 	de <- data.frame("clara", a, quality_clara_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "raw normalized matrix", "2014")
# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_clara = clara(d1, a)
# 	quality_clara =  silhouette(cluster_clara$clustering, d1)
# 	quality_clara_index=summary(quality_clara)
# 	average_quality = c(average_quality,quality_clara_index$si.summary[[4]])

# 	indexes = intCriteria(d1, cluster_clara$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("clara", a, quality_clara_index$si.summary[[4]], "correlation", "2014")

# 	de <- data.frame("clara", a, quality_clara_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "correlation", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_clara = clara(d2, a)
# 	quality_clara =  silhouette(cluster_clara$clustering, d2)
# 	quality_clara_index=summary(quality_clara)
# 	average_quality = c(average_quality,quality_clara_index$si.summary[[4]])

# 	indexes = intCriteria(d2, cluster_clara$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("clara", a, quality_clara_index$si.summary[[4]], "euclidean", "2014")

# 	de <- data.frame("clara", a, quality_clara_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "euclidean", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_pam = pam(normalizedData, a)
# 	quality_pam =  silhouette(cluster_pam)
# 	quality_pam_index=summary(quality_pam)
# 	average_quality = c(average_quality,quality_pam_index$si.summary[[4]])

# 	indexes = intCriteria(normalizedData, cluster_pam$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("pam", a, quality_pam_index$si.summary[[4]], "raw normalized data", "2014")

# 	de <- data.frame("pam", a, quality_pam_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "raw normalized matrix", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_pam = pam(d1, a)
# 	quality_pam =  silhouette(cluster_pam$clustering, d1)
# 	quality_pam_index=summary(quality_pam)
# 	average_quality = c(average_quality,quality_pam_index$si.summary[[4]])

# 	indexes = intCriteria(d1, cluster_pam$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("pam", a, quality_pam_index$si.summary[[4]], "correlation", "2014")

# 	de <- data.frame("pam", a, quality_pam_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "correlation", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_pam = pam(d2, a)
# 	quality_pam =  silhouette(cluster_pam$clustering, d2)
# 	quality_pam_index=summary(quality_pam)
# 	average_quality = c(average_quality,quality_pam_index$si.summary[[4]])

# 	indexes = intCriteria(d2, cluster_pam$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("pam", a, quality_pam_index$si.summary[[4]], "euclidean", "2014")

# 	de <- data.frame("pam", a, quality_pam_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "euclidean", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_fanny = fanny(normalizedData, a, memb.exp = 1.05)
# 	quality_fanny =  silhouette(cluster_fanny)
# 	quality_fanny_index=summary(quality_fanny)
# 	average_quality = c(average_quality,quality_fanny_index$si.summary[[4]])

# 	indexes = intCriteria(normalizedData, cluster_fanny$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("fanny", a, quality_fanny_index$si.summary[[4]], "raw normalized data", "2014")

# 	de <- data.frame("fanny", a, quality_fanny_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "raw normalized matrix", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_fanny = fanny(d1, a)
# 	quality_fanny =  silhouette(cluster_fanny$clustering, d1)
# 	quality_fanny_index=summary(quality_fanny)
# 	average_quality = c(average_quality,quality_fanny_index$si.summary[[4]])

# 	indexes = intCriteria(d1, cluster_fanny$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("fanny", a, quality_fanny_index$si.summary[[4]], "correlation", "2014")

# 	de <- data.frame("fanny", a, quality_fanny_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "correlation", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# for (a in 2:10)
# {
# 	cluster_fanny = fanny(d2, a, memb.exp = 1.05)
# 	quality_fanny =  silhouette(cluster_fanny$clustering, d2)
# 	quality_fanny_index=summary(quality_fanny)
# 	average_quality = c(average_quality,quality_fanny_index$si.summary[[4]])

# 	indexes = intCriteria(d2, cluster_fanny$clustering, c("Silhouette","Dunn", "Ball_Hall"))

# 	#de <- data.frame("fanny", a, quality_fanny_index$si.summary[[4]], "euclidean", "2014")

# 	de <- data.frame("fanny", a, quality_fanny_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "euclidean", "2014")

# 	names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# 	df <- rbind(df, de)
# }

# average_quality=NULL

# de <- NULL

# cluster_mclust = Mclust(normalizedData)
# quality_mclust = silhouette(cluster_mclust$classification, dist(normalizedData))
# quality_mclust_index = summary(quality_mclust)

# indexes = intCriteria(normalizedData, as.integer(cluster_mclust$classification), c("Silhouette","Dunn", "Ball_Hall"))

# #de <- data.frame("Mclust", length(quality_mclust_index$clus.sizes), quality_mclust_index$si.summary[[4]], "raw normalized data/euclidean", "2014")

# de <- data.frame("mclust", length(quality_mclust_index$clus.sizes), quality_mclust_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "raw normalized data/euclidean", "2014")

# names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# df <- rbind(df, de)

# de <- NULL

# cluster_mclust=Mclust(d1)
# quality_mclust =  silhouette(cluster_mclust$classification,d1)
# quality_mclust_index=summary(quality_mclust)

# indexes = intCriteria(d1, as.integer(cluster_mclust$classification), c("Silhouette","Dunn", "Ball_Hall"))

# #de <- data.frame("Mclust", length(quality_mclust_index$clus.sizes), quality_mclust_index$si.summary[[4]], "correlation", "2014")

# de <- data.frame("mclust", length(quality_mclust_index$clus.sizes), quality_mclust_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "correlation", "2014")

# names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# df <- rbind(df, de)

# de <- NULL

# cluster_mclust=Mclust(d2)
# quality_mclust =  silhouette(cluster_mclust$classification,d2)
# quality_mclust_index=summary(quality_mclust)

# indexes = intCriteria(d2, as.integer(cluster_mclust$classification), c("Silhouette","Dunn", "Ball_Hall"))

# #de <- data.frame("Mclust", length(quality_mclust_index$clus.sizes), quality_mclust_index$si.summary[[4]], "euclidean", "2014")

# de <- data.frame("mclust", length(quality_mclust_index$clus.sizes), quality_mclust_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "euclidean", "2014")

# names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# df <- rbind(df, de)

# de <- NULL

# cluster_mstknn <- mst.knn(d1)
# quality_mstknn =  silhouette(cluster_mstknn$cluster, d1)
# quality_mstknn_index=summary(quality_mstknn)

# indexes = intCriteria(d1, as.integer(cluster_mstknn$cluster), c("Silhouette","Dunn", "Ball_Hall"))

# #de <- data.frame("Mstknn", length(quality_mstknn_index$clus.sizes), quality_mstknn_index$si.summary[[4]], "correlation", "2014")

# de <- data.frame("mstknn", a, quality_mstknn_index$si.summary[[4]], indexes$dunn, indexes$ball_hall, "correlation", "2014")

# names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# df <- rbind(df, de)

# #de <- NULL
# #cluster_mstknn <- NULL

# #cluster_mstknn <- mst.knn(d2)
# #quality_mstknn =  silhouette(cluster_mstknn$cluster)
# #quality_mstknn_index=summary(quality_mstknn)
# #print(silhouette(cluster_mstknn$cluster,Dist(d2, method = "euclidean")))


# #de <- data.frame("Mstknn", cluster_mstknn, quality_mstknn_index$si.summary[[4]], "euclidean", "2014")
# #names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "distance", "year")
# #df <- rbind(df, de)

# #de <- NULL


# # cluster_dbscan=dbscan(d, eps=1500000) #Why? I don't know. We don't know. It's a mystery.
# # quality_dbscan = silhouette(cluster_dbscan$cluster,d)
# # quality_dbscan_index=summary(quality_dbscan)

# # print(length(quality_dbscan_index$clus.sizes)-1)

# # de <- data.frame("DBscan", length(quality_dbscan_index$clus.sizes)-1, quality_dbscan_index$si.summary[[4]], "2014")
# # names(de) <- c("technique", "clusters_amount", "silhouette_index", "dunn_index", "ballhall_index", "year")
# # df <- rbind(df, de)

# # de <- NULL

# write.xlsx(df, 'ResultadosClusters2014-index.xlsx')






# #The code below it's to create the matrix with the incomes and outcomes. Now isn't necessary, because the file it's already created, and it can be loaded directly from the excel.

#spendings <- read_excel("PLANILLA_GASTO_HOSPITALES_2014_2019.xlsx", sheet = 5)

#spendings = subset(spendings, spendings$considerar != "NULL")

#dataAfterRemoves2014$sub21 <- ifelse(dataAfterRemoves2014$IdEstablecimiento == spendings$IdEstablecimiento, spendings$`Gasto Sub 21`)
#dataAfterRemoves2014$sub22 <- ifelse(dataAfterRemoves2014$IdEstablecimiento == spendings$IdEstablecimiento, spendings$`Gasto Sub 22`)

#df = NULL
#df = data.frame()
#names(df) = c("Listado Hospitales", "Gasto sub 21", "Gasto sub 22", "E02", "E08", "total consultas médicas y especialidades", "Año")

#for (n in 1:nrow(dataAfterRemoves2014)){
#  totalE08 = sum(dataAfterRemoves2014[n,]$X330_E08, dataAfterRemoves2014[n,]$X401_E08, dataAfterRemoves2014[n,]$X403_E08, dataAfterRemoves2014[n,]$X405_E08, dataAfterRemoves2014[n,]$X406_E08, dataAfterRemoves2014[n,]$X407_E08, dataAfterRemoves2014[n,]$X411_E08, dataAfterRemoves2014[n,]$X413_E08, dataAfterRemoves2014[n,]$X414_E08, dataAfterRemoves2014[n,]$X415_E08, dataAfterRemoves2014[n,]$X416_E08, dataAfterRemoves2014[n,]$X418_E08, dataAfterRemoves2014[n,]$X402_E08, dataAfterRemoves2014[n,]$X404_E08, dataAfterRemoves2014[n,]$X408_E08, dataAfterRemoves2014[n,]$X409_E08, dataAfterRemoves2014[n,]$X410_E08, dataAfterRemoves2014[n,]$X412_E08, dataAfterRemoves2014[n,]$X421_E08, dataAfterRemoves2014[n,]$X420_E08, dataAfterRemoves2014[n,]$X419_E08, dataAfterRemoves2014[n,]$X423_E08, dataAfterRemoves2014[n,]$X424_E08, dataAfterRemoves2014[n,]$X422_E08, dataAfterRemoves2014[n,]$X426_E08, na.rm = TRUE)
#  totalE02 = sum(dataAfterRemoves2014[n,]$X330_E02, dataAfterRemoves2014[n,]$X401_E02, dataAfterRemoves2014[n,]$X403_E02, dataAfterRemoves2014[n,]$X405_E02, dataAfterRemoves2014[n,]$X406_E02, dataAfterRemoves2014[n,]$X407_E02, dataAfterRemoves2014[n,]$X411_E02, dataAfterRemoves2014[n,]$X413_E02, dataAfterRemoves2014[n,]$X414_E02, dataAfterRemoves2014[n,]$X415_E02, dataAfterRemoves2014[n,]$X416_E02, dataAfterRemoves2014[n,]$X418_E02, dataAfterRemoves2014[n,]$X402_E02, dataAfterRemoves2014[n,]$X404_E02, dataAfterRemoves2014[n,]$X402_E02, dataAfterRemoves2014[n,]$X409_E02, dataAfterRemoves2014[n,]$X410_E02, dataAfterRemoves2014[n,]$X412_E02, dataAfterRemoves2014[n,]$X421_E02, dataAfterRemoves2014[n,]$X420_E02, dataAfterRemoves2014[n,]$X419_E02, dataAfterRemoves2014[n,]$X423_E02, dataAfterRemoves2014[n,]$X424_E02, dataAfterRemoves2014[n,]$X422_E02, na.rm = TRUE)
#  totalConsultas = sum(dataAfterRemoves2014[n,]$X07020130, dataAfterRemoves2014[n,]$X07020230, dataAfterRemoves2014[n,]$X07020330, dataAfterRemoves2014[n,]$X07020400, dataAfterRemoves2014[n,]$X07020500, dataAfterRemoves2014[n,]$X07020600, dataAfterRemoves2014[n,]$X07020700, dataAfterRemoves2014[n,]$X07020800, dataAfterRemoves2014[n,]$X07020900, dataAfterRemoves2014[n,]$X07021000, dataAfterRemoves2014[n,]$X07021100, dataAfterRemoves2014[n,]$X07021230, dataAfterRemoves2014[n,]$X07021300, dataAfterRemoves2014[n,]$X07021400, dataAfterRemoves2014[n,]$X07021531, dataAfterRemoves2014[n,]$X07021600, dataAfterRemoves2014[n,]$X07021700, dataAfterRemoves2014[n,]$X07021800, dataAfterRemoves2014[n,]$X07021900, dataAfterRemoves2014[n,]$X07022000, dataAfterRemoves2014[n,]$X07022130, dataAfterRemoves2014[n,]$X07022131, dataAfterRemoves2014[n,]$X07022200, dataAfterRemoves2014[n,]$X07022330, dataAfterRemoves2014[n,]$X07022400, dataAfterRemoves2014[n,]$X07022500, dataAfterRemoves2014[n,]$X07022631, dataAfterRemoves2014[n,]$X07022700, dataAfterRemoves2014[n,]$X07022800, dataAfterRemoves2014[n,]$X07022900, dataAfterRemoves2014[n,]$X07023000, dataAfterRemoves2014[n,]$X07023100, dataAfterRemoves2014[n,]$X07023200, dataAfterRemoves2014[n,]$X07023400, dataAfterRemoves2014[n,]$X07023600, dataAfterRemoves2014[n,]$X07023700, dataAfterRemoves2014[n,]$X07023800, dataAfterRemoves2014[n,]$X07023900, dataAfterRemoves2014[n,]$X07024000, dataAfterRemoves2014[n,]$X07024200, na.rm = TRUE )
#  print(n)
#  print(totalE08)
#  print(totalE02)
#  print(totalConsultas)
#  de = NULL
#  de = data.frame(dataAfterRemoves2014[n,]$IdEstablecimiento, dataAfterRemoves2014[n,]$sub21, dataAfterRemoves2014[n,]$sub22, totalE02, totalE08, totalConsultas, "2014")
#  names(de) = c("Listado Hospitales", "Gasto sub 21", "Gasto sub 22", "E02", "E08", "total consultas médicas y especialidades", "Año")
#  df <- rbind(df,de)
#}

#print(dataAfterRemoves2014[n,]$X07020230)

#write.xlsx(df, 'premalmquist2014.xlsx')





# #incomesAndOutcomes <- read_excel("premalmquist2014.xlsx")

# #dataAfterRemoves2014norm = apply(dataAfterRemoves2014, MARGIN = 2, FUN = function(X) (X - min(X))/diff(range(X)))

# # dataMinsal = read_excel("complejidadHospitales.xlsx")
# # dataMinsal = subset(dataMinsal, dataMinsal$Considerar != "NULL")

# # print(summary(silhouette(dataMinsal$`Minsal mejorado`, dist(normalizedData))))

cluster_2014_1 = clara(normalizedData, 2)
cluster_2014_2 = clara(normalizedData, 6)
cluster_2014_3 = Mclust(d1)
cluster_2014_4 = clara(normalizedData, 6)
df = data.frame(cluster_2014_1$clustering, cluster_2014_2$clustering, cluster_2014_3$classification, cluster_2014_4$clustering)


# #cluter_2014 = clara(dataAfterRemoves2014norm, 2)
# #print(summary(silhouette(cluster_2014$clustering, Dist(dataAfterRemoves2014, method="correlation"))))
write.xlsx(df, "datos.xlsx")

