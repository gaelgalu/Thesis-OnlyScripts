library("readxl")
library(openxlsx)

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
df = data.frame(1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 1, 2, 3, 4, 5, 6, 7, 8)
names(df) = c("Listado Hospitales", "totalE02", "totalE03", "totalE04", "totalE08", "totalA1", "totalA2", "totalA3", "totalB1", "totalB2", "totalB3", "totalC1", "totalC2", "totalC3", "totalD1", "totalD2", "totalD3", "Año")
print("hola")
for (n in 1:nrow(data2014)){
	totalE02 = sum(data2014[n,]$X330_E02, data2014[n,]$X401_E02, data2014[n,]$X402_E02, data2014[n,]$X405_E02, data2014[n,]$X406_E02, data2014[n,]$X407_E02, data2014[n,]$X411_E02, data2014[n,]$X412_E02, data2014[n,]$X414_E02, data2014[n,]$X415_E02, data2014[n,]$X416_E02, data2014[n,]$X418_E02, data2014[n,]$X402_E02, data2014[n,]$X404_E02, data2014[n,]$X408_E02, data2014[n,]$X409_E02, data2014[n,]$X410_E02, data2014[n,]$X412_E02, data2014[n,]$X421_E02, data2014[n,]$X420_E02, data2014[n,]$X419_E02, data2014[n,]$X422_E02, data2014[n,]$X424_E03, data2014[n,]$X422_E03, data2014[n,]$X426_E03, na.rm = TRUE)
	totalE03 = sum(data2014[n,]$X330_E03, data2014[n,]$X401_E03, data2014[n,]$X403_E03, data2014[n,]$X405_E03, data2014[n,]$X406_E03, data2014[n,]$X407_E03, data2014[n,]$X411_E03, data2014[n,]$X413_E03, data2014[n,]$X414_E03, data2014[n,]$X415_E03, data2014[n,]$X416_E03, data2014[n,]$X418_E03, data2014[n,]$X402_E03, data2014[n,]$X404_E03, data2014[n,]$X408_E03, data2014[n,]$X409_E03, data2014[n,]$X410_E03, data2014[n,]$X412_E03, data2014[n,]$X421_E03, data2014[n,]$X420_E03, data2014[n,]$X419_E03, data2014[n,]$X423_E03, data2014[n,]$X424_E03, data2014[n,]$X422_E03, data2014[n,]$X426_E03, na.rm = TRUE)
	totalE04 = sum(data2014[n,]$X330_E04, data2014[n,]$X401_E04, data2014[n,]$X404_E04, data2014[n,]$X405_E04, data2014[n,]$X406_E04, data2014[n,]$X407_E04, data2014[n,]$X411_E04, data2014[n,]$X414_E04, data2014[n,]$X414_E04, data2014[n,]$X415_E04, data2014[n,]$X416_E04, data2014[n,]$X418_E04, data2014[n,]$X402_E04, data2014[n,]$X404_E04, data2014[n,]$X408_E04, data2014[n,]$X409_E04, data2014[n,]$X410_E04, data2014[n,]$X412_E04, data2014[n,]$X421_E04, data2014[n,]$X420_E04, data2014[n,]$X419_E04, data2014[n,]$X424_E04, data2014[n,]$X424_E04, data2014[n,]$X422_E04, data2014[n,]$X426_E04, na.rm = TRUE)
	totalE08 = sum(data2014[n,]$X330_E08, data2014[n,]$X401_E08, data2014[n,]$X403_E08, data2014[n,]$X405_E08, data2014[n,]$X406_E08, data2014[n,]$X407_E08, data2014[n,]$X411_E08, data2014[n,]$X413_E08, data2014[n,]$X414_E08, data2014[n,]$X415_E08, data2014[n,]$X416_E08, data2014[n,]$X418_E08, data2014[n,]$X402_E08, data2014[n,]$X404_E08, data2014[n,]$X408_E08, data2014[n,]$X409_E08, data2014[n,]$X410_E08, data2014[n,]$X412_E08, data2014[n,]$X421_E08, data2014[n,]$X420_E08, data2014[n,]$X419_E08, data2014[n,]$X423_E08, data2014[n,]$X424_E08, data2014[n,]$X422_E08, data2014[n,]$X426_E08, na.rm = TRUE)
	totalA1 = sum(data2014[n,]$X330_A1, data2014[n,]$X401_A1, data2014[n,]$X403_A1, data2014[n,]$X405_A1, data2014[n,]$X406_A1, data2014[n,]$X407_A1, data2014[n,]$X411_A1, data2014[n,]$X413_A1, data2014[n,]$X414_A1, data2014[n,]$X415_A1, data2014[n,]$X416_A1, data2014[n,]$X418_A1, data2014[n,]$X402_A1, data2014[n,]$X404_A1, data2014[n,]$X408_A1, data2014[n,]$X409_A1, data2014[n,]$X410_A1, data2014[n,]$X412_A1, data2014[n,]$X421_A1, data2014[n,]$X420_A1, data2014[n,]$X419_A1, data2014[n,]$X423_A1, data2014[n,]$X424_A1, data2014[n,]$X422_A1, data2014[n,]$X426_A1, na.rm = TRUE)
	totalA2 = sum(data2014[n,]$X330_A2, data2014[n,]$X401_A2, data2014[n,]$X403_A2, data2014[n,]$X405_A2, data2014[n,]$X406_A2, data2014[n,]$X407_A2, data2014[n,]$X411_A2, data2014[n,]$X413_A2, data2014[n,]$X414_A2, data2014[n,]$X415_A2, data2014[n,]$X416_A2, data2014[n,]$X418_A2, data2014[n,]$X402_A2, data2014[n,]$X404_A2, data2014[n,]$X408_A2, data2014[n,]$X409_A2, data2014[n,]$X410_A2, data2014[n,]$X412_A2, data2014[n,]$X421_A2, data2014[n,]$X420_A2, data2014[n,]$X419_A2, data2014[n,]$X423_A2, data2014[n,]$X424_A2, data2014[n,]$X422_A2, data2014[n,]$X426_A2, na.rm = TRUE)
	totalA3 = sum(data2014[n,]$X330_A3, data2014[n,]$X401_A3, data2014[n,]$X403_A3, data2014[n,]$X405_A3, data2014[n,]$X406_A3, data2014[n,]$X407_A3, data2014[n,]$X411_A3, data2014[n,]$X413_A3, data2014[n,]$X414_A3, data2014[n,]$X415_A3, data2014[n,]$X416_A3, data2014[n,]$X418_A3, data2014[n,]$X402_A3, data2014[n,]$X404_A3, data2014[n,]$X408_A3, data2014[n,]$X409_A3, data2014[n,]$X410_A3, data2014[n,]$X412_A3, data2014[n,]$X421_A3, data2014[n,]$X420_A3, data2014[n,]$X419_A3, data2014[n,]$X423_A3, data2014[n,]$X424_A3, data2014[n,]$X422_A3, data2014[n,]$X426_A3, na.rm = TRUE)
	totalB1 = sum(data2014[n,]$X330_B1, data2014[n,]$X401_B1, data2014[n,]$X403_B1, data2014[n,]$X405_B1, data2014[n,]$X406_B1, data2014[n,]$X407_B1, data2014[n,]$X411_B1, data2014[n,]$X413_B1, data2014[n,]$X414_B1, data2014[n,]$X415_B1, data2014[n,]$X416_B1, data2014[n,]$X418_B1, data2014[n,]$X402_B1, data2014[n,]$X404_B1, data2014[n,]$X408_B1, data2014[n,]$X409_B1, data2014[n,]$X410_B1, data2014[n,]$X412_B1, data2014[n,]$X421_B1, data2014[n,]$X420_B1, data2014[n,]$X419_B1, data2014[n,]$X423_B1, data2014[n,]$X424_B1, data2014[n,]$X422_B1, data2014[n,]$X426_B1, na.rm = TRUE)
	totalB2 = sum(data2014[n,]$X330_B2, data2014[n,]$X401_B2, data2014[n,]$X403_B2, data2014[n,]$X405_B2, data2014[n,]$X406_B2, data2014[n,]$X407_B2, data2014[n,]$X411_B2, data2014[n,]$X413_B2, data2014[n,]$X414_B2, data2014[n,]$X415_B2, data2014[n,]$X416_B2, data2014[n,]$X418_B2, data2014[n,]$X402_B2, data2014[n,]$X404_B2, data2014[n,]$X408_B2, data2014[n,]$X409_B2, data2014[n,]$X410_B2, data2014[n,]$X412_B2, data2014[n,]$X421_B2, data2014[n,]$X420_B2, data2014[n,]$X419_B2, data2014[n,]$X423_B2, data2014[n,]$X424_B2, data2014[n,]$X422_B2, data2014[n,]$X426_B2, na.rm = TRUE)
	totalB3 = sum(data2014[n,]$X330_B3, data2014[n,]$X401_B3, data2014[n,]$X403_B3, data2014[n,]$X405_B3, data2014[n,]$X406_B3, data2014[n,]$X407_B3, data2014[n,]$X411_B3, data2014[n,]$X413_B3, data2014[n,]$X414_B3, data2014[n,]$X415_B3, data2014[n,]$X416_B3, data2014[n,]$X418_B3, data2014[n,]$X402_B3, data2014[n,]$X404_B3, data2014[n,]$X408_B3, data2014[n,]$X409_B3, data2014[n,]$X410_B3, data2014[n,]$X412_B3, data2014[n,]$X421_B3, data2014[n,]$X420_B3, data2014[n,]$X419_B3, data2014[n,]$X423_B3, data2014[n,]$X424_B3, data2014[n,]$X422_B3, data2014[n,]$X426_B3, na.rm = TRUE)
	totalC1 = sum(data2014[n,]$X330_C1, data2014[n,]$X401_C1, data2014[n,]$X403_C1, data2014[n,]$X405_C1, data2014[n,]$X406_C1, data2014[n,]$X407_C1, data2014[n,]$X411_C1, data2014[n,]$X413_C1, data2014[n,]$X414_C1, data2014[n,]$X415_C1, data2014[n,]$X416_C1, data2014[n,]$X418_C1, data2014[n,]$X402_C1, data2014[n,]$X404_C1, data2014[n,]$X408_C1, data2014[n,]$X409_C1, data2014[n,]$X410_C1, data2014[n,]$X412_C1, data2014[n,]$X421_C1, data2014[n,]$X420_C1, data2014[n,]$X419_C1, data2014[n,]$X423_C1, data2014[n,]$X424_C1, data2014[n,]$X422_C1, data2014[n,]$X426_C1, na.rm = TRUE)
	totalC2 = sum(data2014[n,]$X330_C2, data2014[n,]$X401_C2, data2014[n,]$X403_C2, data2014[n,]$X405_C2, data2014[n,]$X406_C2, data2014[n,]$X407_C2, data2014[n,]$X411_C2, data2014[n,]$X413_C2, data2014[n,]$X414_C2, data2014[n,]$X415_C2, data2014[n,]$X416_C2, data2014[n,]$X418_C2, data2014[n,]$X402_C2, data2014[n,]$X404_C2, data2014[n,]$X408_C2, data2014[n,]$X409_C2, data2014[n,]$X410_C2, data2014[n,]$X412_C2, data2014[n,]$X421_C2, data2014[n,]$X420_C2, data2014[n,]$X419_C2, data2014[n,]$X423_C2, data2014[n,]$X424_C2, data2014[n,]$X422_C2, data2014[n,]$X426_C2, na.rm = TRUE)
	totalC3 = sum(data2014[n,]$X330_C3, data2014[n,]$X401_C3, data2014[n,]$X403_C3, data2014[n,]$X405_C3, data2014[n,]$X406_C3, data2014[n,]$X407_C3, data2014[n,]$X411_C3, data2014[n,]$X413_C3, data2014[n,]$X414_C3, data2014[n,]$X415_C3, data2014[n,]$X416_C3, data2014[n,]$X418_C3, data2014[n,]$X402_C3, data2014[n,]$X404_C3, data2014[n,]$X408_C3, data2014[n,]$X409_C3, data2014[n,]$X410_C3, data2014[n,]$X412_C3, data2014[n,]$X421_C3, data2014[n,]$X420_C3, data2014[n,]$X419_C3, data2014[n,]$X423_C3, data2014[n,]$X424_C3, data2014[n,]$X422_C3, data2014[n,]$X426_C3, na.rm = TRUE)
	totalD1 = sum(data2014[n,]$X330_D1, data2014[n,]$X401_D1, data2014[n,]$X403_D1, data2014[n,]$X405_D1, data2014[n,]$X406_D1, data2014[n,]$X407_D1, data2014[n,]$X411_D1, data2014[n,]$X413_D1, data2014[n,]$X414_D1, data2014[n,]$X415_D1, data2014[n,]$X416_D1, data2014[n,]$X418_D1, data2014[n,]$X402_D1, data2014[n,]$X404_D1, data2014[n,]$X408_D1, data2014[n,]$X409_D1, data2014[n,]$X410_D1, data2014[n,]$X412_D1, data2014[n,]$X421_D1, data2014[n,]$X420_D1, data2014[n,]$X419_D1, data2014[n,]$X423_D1, data2014[n,]$X424_D1, data2014[n,]$X422_D1, data2014[n,]$X426_D1, na.rm = TRUE)
	totalD2 = sum(data2014[n,]$X330_D2, data2014[n,]$X401_D2, data2014[n,]$X403_D2, data2014[n,]$X405_D2, data2014[n,]$X406_D2, data2014[n,]$X407_D2, data2014[n,]$X411_D2, data2014[n,]$X413_D2, data2014[n,]$X414_D2, data2014[n,]$X415_D2, data2014[n,]$X416_D2, data2014[n,]$X418_D2, data2014[n,]$X402_D2, data2014[n,]$X404_D2, data2014[n,]$X408_D2, data2014[n,]$X409_D2, data2014[n,]$X410_D2, data2014[n,]$X412_D2, data2014[n,]$X421_D2, data2014[n,]$X420_D2, data2014[n,]$X419_D2, data2014[n,]$X423_D2, data2014[n,]$X424_D2, data2014[n,]$X422_D2, data2014[n,]$X426_D2, na.rm = TRUE)
	totalD3 = sum(data2014[n,]$X330_D3, data2014[n,]$X401_D3, data2014[n,]$X403_D3, data2014[n,]$X405_D3, data2014[n,]$X406_D3, data2014[n,]$X407_D3, data2014[n,]$X411_D3, data2014[n,]$X413_D3, data2014[n,]$X414_D3, data2014[n,]$X415_D3, data2014[n,]$X416_D3, data2014[n,]$X418_D3, data2014[n,]$X402_D3, data2014[n,]$X404_D3, data2014[n,]$X408_D3, data2014[n,]$X409_D3, data2014[n,]$X410_D3, data2014[n,]$X412_D3, data2014[n,]$X421_D3, data2014[n,]$X420_D3, data2014[n,]$X419_D3, data2014[n,]$X423_D3, data2014[n,]$X424_D3, data2014[n,]$X422_D3, data2014[n,]$X426_D3, na.rm = TRUE)

	print("#############################################################")
	print(n)
	print(nrow(data2014))
	print("2014")
	print("#############################################################\n")
	print(totalE02)
	print(totalE03)
	print(totalE04)
	print(totalE08)
	print(totalA1)
	print(totalA2)
	print(totalA3)
	print(totalB1)
	print(totalB2)
	print(totalB3)
	print(totalC1)
	print(totalC2)
	print(totalC3)
	print(totalD1)
	print(totalD2)
	print(totalD3)


	de = NULL
	de = data.frame(data2014[n,]$IdEstablecimiento, totalE02, totalE03, totalE04, totalE08, totalA1, totalA2, totalA3, totalB1, totalB2, totalB3, totalC1, totalC2, totalC3, totalD1, totalD2, totalD3, 2014)
	names(de) = c("Listado Hospitales", "totalE02", "totalE03", "totalE04", "totalE08", "totalA1", "totalA2", "totalA3", "totalB1", "totalB2", "totalB3", "totalC1", "totalC2", "totalC3", "totalD1", "totalD2", "totalD3", "Año")
	df <- rbind(df,de)
}

write.xlsx(df, 'VariablesCriterioCalidad2014.xlsx')