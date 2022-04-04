#Imports

library("readxl")
library(openxlsx)
library("amap")

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

df = NULL
df = data.frame(1, 2, 3, 4, 5)
names(df) = c("Listado Hospitales", "E02", "E08", "total consultas médicas y especialidades", "Año")

for (n in 1:nrow(data2014)){
	totalE08 = sum(data2014[n,]$X330_E08, data2014[n,]$X401_E08, data2014[n,]$X403_E08, data2014[n,]$X405_E08, data2014[n,]$X406_E08, data2014[n,]$X407_E08, data2014[n,]$X411_E08, data2014[n,]$X413_E08, data2014[n,]$X414_E08, data2014[n,]$X415_E08, data2014[n,]$X416_E08, data2014[n,]$X418_E08, data2014[n,]$X402_E08, data2014[n,]$X404_E08, data2014[n,]$X408_E08, data2014[n,]$X409_E08, data2014[n,]$X410_E08, data2014[n,]$X412_E08, data2014[n,]$X421_E08, data2014[n,]$X420_E08, data2014[n,]$X419_E08, data2014[n,]$X423_E08, data2014[n,]$X424_E08, data2014[n,]$X422_E08, data2014[n,]$X426_E08, na.rm = TRUE)
	totalE02 = sum(data2014[n,]$X330_E02, data2014[n,]$X401_E02, data2014[n,]$X403_E02, data2014[n,]$X405_E02, data2014[n,]$X406_E02, data2014[n,]$X407_E02, data2014[n,]$X411_E02, data2014[n,]$X413_E02, data2014[n,]$X414_E02, data2014[n,]$X415_E02, data2014[n,]$X416_E02, data2014[n,]$X418_E02, data2014[n,]$X402_E02, data2014[n,]$X404_E02, data2014[n,]$X402_E02, data2014[n,]$X409_E02, data2014[n,]$X410_E02, data2014[n,]$X412_E02, data2014[n,]$X421_E02, data2014[n,]$X420_E02, data2014[n,]$X419_E02, data2014[n,]$X423_E02, data2014[n,]$X424_E02, data2014[n,]$X422_E02, data2014[n,]$X426_E02, na.rm = TRUE)
	totalConsultas = sum(data2014[n,]$X7020130, data2014[n,]$X7020230, data2014[n,]$X7020330, data2014[n,]$X7020400, data2014[n,]$X7020500, data2014[n,]$X7020600, data2014[n,]$X7020700, data2014[n,]$X7020800, data2014[n,]$X7020900, data2014[n,]$X7021000, data2014[n,]$X7021100, data2014[n,]$X7021230, data2014[n,]$X7021300, data2014[n,]$X7021400, data2014[n,]$X7021531, data2014[n,]$X7021600, data2014[n,]$X7021700, data2014[n,]$X7021800, data2014[n,]$X7021900, data2014[n,]$X7022000, data2014[n,]$X7022130, data2014[n,]$X7022131, data2014[n,]$X7022200, data2014[n,]$X7022330, data2014[n,]$X7022400, data2014[n,]$X7022500, data2014[n,]$X7022631, data2014[n,]$X7022700, data2014[n,]$X7022800, data2014[n,]$X7022900, data2014[n,]$X7023000, data2014[n,]$X7023100, data2014[n,]$X7023200, data2014[n,]$X7023400, data2014[n,]$X7023600, data2014[n,]$X7023700, data2014[n,]$X7023800, data2014[n,]$X7023900, data2014[n,]$X7024000, data2014[n,]$X7024200, na.rm = TRUE )
	print(n)
	print(totalE08)
	print(totalE02)
	print(totalConsultas)
	de = NULL
	de = data.frame(data2014[n,]$IdEstablecimiento, totalE02, totalE08, totalConsultas, "2014")
	names(de) = c("Listado Hospitales", "E02", "E08", "total consultas médicas y especialidades", "Año")
	df <- rbind(df,de)
}

print(data2014[n,]$X7020230)

write.xlsx(df, 'premalmquist2014.xlsx')





