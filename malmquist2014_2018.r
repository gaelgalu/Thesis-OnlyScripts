library("productivity")
library("readxl")
library(openxlsx)

datos=read.csv2("megadata.csv",sep=";",header = T,check.names = F)
clusters=read_excel("Clusters.xlsx")

datos$sub21=as.numeric(datos$sub21)
datos$sub22=as.numeric(datos$sub22)
datos$minsal = clusters$minsal

cant_clusters = max(clusters$minsal)
print(cant_clusters)
datos$mic = c(1:910)
results = read_excel("malmquistresults.xlsx")

#Aplicar MALMQUIST

for (n in 1:cant_clusters){
  hospitales = subset(datos, datos$minsal == n)
  
  Malmquist <- malm(data = hospitales, id.var = "codigo", time.var = "ano", 
                    x.vars = c("sub21", "sub22"), 
                    y.vars = c("e02", "e08","consultas"), rts = "nirs", orientation = "out")
  for (i in 1:182){
    for(j in 1:length(Malmquist$Changes$codigo)){
      if(results$codigo[i] == Malmquist$Changes$codigo[j]){
        salto = length((hospitales$codigo))/5
        # print("---------------------------")
        # print(results$codigo[i])
        # print(Malmquist$Changes$codigo[j])
        # print(Malmquist$Changes$codigo[j+salto])
        # print(Malmquist$Changes$codigo[j+salto*2])
        # print(Malmquist$Changes$codigo[j+salto*3])
        # print("---------------------------")

        
        results$prod2014_2015[i] = Malmquist$Changes$malmquist[j]
        results$eff2014_2015[i] = Malmquist$Changes$effch[j]
        results$prod2015_2016[i] = Malmquist$Changes$malmquist[j+salto]
        results$eff2015_2016[i] = Malmquist$Changes$effch[j+salto]
        results$prod2016_2017[i] = Malmquist$Changes$malmquist[j+salto*2]
        results$eff2016_2017[i] = Malmquist$Changes$effch[j+salto*2]
        results$prod2017_2018[i] = Malmquist$Changes$malmquist[j+salto*3]
        results$eff2017_2018[i] = Malmquist$Changes$effch[j+salto*3]
        break
      }
    }
  }
}


write.xlsx(results, "malmquistresults.xlsx")

