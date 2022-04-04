library("readxl")
library("caret")
library(openxlsx)
library(dplyr)


data2014 <- read.csv(
  file = 'mat_datos_2014.csv',
  stringsAsFactors = TRUE, 
  strip.white = TRUE,
  sep = ';',
  header = TRUE
)

names <- read_excel("complejidadHospitalesOriginal.xlsx")

data2014$complejidad <- ifelse(data2014$IdEstablecimiento == names$IdEstablecimiento, names$Complejidad)
data2014$considerar <- ifelse(data2014$IdEstablecimiento == names$IdEstablecimiento, names$Considerar)

data2014 = subset(data2014, data2014$complejidad != "NULL")
data2014 = subset(data2014, data2014$considerar != "NULL")

var257 <- read_excel("RESUMEN_BD_HOSPITALES.xlsx")

#data2014[, -grep(var257$2014, colnames(data2014))]

columnsToKeep = var257$Year2014

data2014$X08222630

filtered = subset(data2014, select = c(columnsToKeep[1:3]) )
filtered00 = subset(data2014, select = c(columnsToKeep[5:9]) )
filtered01 = subset(data2014, select = c(columnsToKeep[11:14]) )
filtered02 = subset(data2014, select = c(columnsToKeep[17]) )
filtered03 = subset(data2014, select = c(columnsToKeep[19:23]) )
filtered04 = subset(data2014, select = c(columnsToKeep[31:33]) )
filtered05 = subset(data2014, select = c(columnsToKeep[35:36]) )
filtered06 = subset(data2014, select = c(columnsToKeep[42]) )
filtered07 = subset(data2014, select = c(columnsToKeep[45:59]) )
filtered08 = subset(data2014, select = c(columnsToKeep[64:87]) )
filtered09 = subset(data2014, select = c(columnsToKeep[90:121]) )
filtered010 = subset(data2014, select = c(columnsToKeep[125:167]) )
filtered2 = subset(data2014, select = c(columnsToKeep[169:193]) )
filtered3 = subset(data2014, select = c(columnsToKeep[195:219]) )
filtered4 = subset(data2014, select = c(columnsToKeep[221:245]) )
filtered5 = data2014[, grep("_A1$", colnames(data2014))]
filtered6 = data2014[, grep("_A2$", colnames(data2014))]
filtered7 = data2014[, grep("_A3$", colnames(data2014))]
filtered8 = data2014[, grep("_B1$", colnames(data2014))]
filtered9 = data2014[, grep("_B2$", colnames(data2014))]
filtered10 = data2014[, grep("_B3$", colnames(data2014))]
filtered11 = data2014[, grep("_C1$", colnames(data2014))]
filtered12 = data2014[, grep("_C2$", colnames(data2014))]
filtered13 = data2014[, grep("_C3$", colnames(data2014))]
filtered14 = data2014[, grep("_D1$", colnames(data2014))]
filtered15 = data2014[, grep("_D2$", colnames(data2014))]
filtered16 = data2014[, grep("_D3$", colnames(data2014))]

sumaE08 = NULL
for (n in 1:nrow(filtered2)){
  sumaE08 <- c(sumaE08, sum(filtered2[n,1:ncol(filtered2)]))
}
filtered2$SumaE08 = sumaE08

sumaE02 = NULL
for (n in 1:nrow(filtered3)){
  sumaE02 <- c(sumaE02, sum(filtered3[n,1:ncol(filtered3)]))
}
filtered3$SumaE02 = sumaE02

sumaE03 = NULL
for (n in 1:nrow(filtered4)){
  sumaE03 <- c(sumaE03, sum(filtered4[n,1:ncol(filtered4)]))
}
filtered4$SumaE03 = sumaE03

#Suma A1

sumaA1 = NULL
for (n in 1:nrow(filtered5)){
  sumaA1 <- c(sumaA1, sum(filtered5[n,1:ncol(filtered5)]))
}
filtered5$SumaA1 = sumaA1

sumaA2 = NULL
for (n in 1:nrow(filtered6)){
  sumaA2 <- c(sumaA2, sum(filtered6[n,1:ncol(filtered6)]))
}
filtered6$SumaA2 = sumaA2

sumaA3 = NULL
for (n in 1:nrow(filtered7)){
  sumaA3 <- c(sumaA3, sum(filtered7[n,1:ncol(filtered7)]))
}
filtered7$SumaA3 = sumaA3

#Suma B1

sumaB1 = NULL
for (n in 1:nrow(filtered8)){
  sumaB1 <- c(sumaB1, sum(filtered8[n,1:ncol(filtered8)]))
}
filtered8$SumaB1 = sumaB1

sumaB2 = NULL
for (n in 1:nrow(filtered9)){
  sumaB2 <- c(sumaB2, sum(filtered9[n,1:ncol(filtered9)]))
}
filtered9$SumaB2 = sumaB2

sumaB3 = NULL
for (n in 1:nrow(filtered10)){
  sumaB3 <- c(sumaB3, sum(filtered10[n,1:ncol(filtered10)]))
}
filtered10$SumaB3 = sumaB3

#Suma C1

sumaC1 = NULL
for (n in 1:nrow(filtered11)){
  sumaC1 <- c(sumaC1, sum(filtered11[n,1:ncol(filtered11)]))
}
filtered11$SumaC1 = sumaC1

sumaC2 = NULL
for (n in 1:nrow(filtered12)){
  sumaC2 <- c(sumaC2, sum(filtered12[n,1:ncol(filtered12)]))
}
filtered12$SumaC2 = sumaC2

sumaC3 = NULL
for (n in 1:nrow(filtered13)){
  sumaC3 <- c(sumaC3, sum(filtered13[n,1:ncol(filtered13)]))
}
filtered13$SumaC3 = sumaC3

#Suma D1

sumaD1 = NULL
for (n in 1:nrow(filtered14)){
  sumaD1 <- c(sumaD1, sum(filtered14[n,1:ncol(filtered14)]))
}
filtered14$SumaD1 = sumaD1

sumaD2 = NULL
for (n in 1:nrow(filtered15)){
  sumaD2 <- c(sumaD2, sum(filtered15[n,1:ncol(filtered15)]))
}
filtered15$SumaD2 = sumaD2

sumaD3 = NULL
for (n in 1:nrow(filtered16)){
  sumaD3 <- c(sumaD3, sum(filtered16[n,1:ncol(filtered16)]))
}
filtered16$SumaD3 = sumaD3

resultado = cbind(filtered, filtered00, filtered01, filtered02, filtered03, filtered04, filtered05,
                  filtered06, filtered07, filtered08, filtered09, filtered010, filtered2, filtered3,
                  filtered4, sumaA1, sumaA2, sumaA3, sumaB1, sumaB2, sumaB3, sumaC1, sumaC2, sumaC3,
                  sumaD1, sumaD2, sumaD3)

write.xlsx(resultado, "257VariablesFiltradas2014.xlsx")
