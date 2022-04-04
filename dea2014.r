library(rDEA)
library(dplyr)
library("readxl")
library("caret")
library(openxlsx)

clusters <- read_excel("Clusters2014.xlsx")
cant_clusters = max(clusters$minsal)
clusters$mic = c(1:182)
clusters$miv = c(1:182)
clusters$min = c(1:182)
clusters$moc = c(1:182)
clusters$mov = c(1:182)
clusters$mon = c(1:182)
datosPreDea <- read_excel("predea2014.xlsx")

#Aplicar DEA
for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$minsal == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="constant")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$mic[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$minsal == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="variable")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$miv[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$minsal == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="non-increasing")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$min[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$minsal == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="constant")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$moc[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$minsal == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="variable")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$mov[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$minsal == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="non-increasing")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$mon[i] = di_naive$thetaOpt[j]
      }
    }
  }
}



#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

cant_clusters = max(clusters$distanciamayor)
clusters$bic = c(1:182)
clusters$biv = c(1:182)
clusters$bin = c(1:182)
clusters$boc = c(1:182)
clusters$bov = c(1:182)
clusters$bon = c(1:182)


for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$distanciamayor == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="constant")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$bic[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$distanciamayor == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="variable")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$biv[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$distanciamayor == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="input", RTS="non-increasing")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$bin[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$distanciamayor == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="constant")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$boc[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$distanciamayor == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="variable")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$bov[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

for (n in 1:cant_clusters){
  hospitales = subset(clusters, clusters$distanciamayor == n)
  preDEAfiltered = filter(datosPreDea, datosPreDea$`Listado Hospitales` %in% hospitales$codigo)
  Y = preDEAfiltered[c('E02', 'E08', 'total consultas médicas y especialidades')]
  X = preDEAfiltered[c('Gasto sub 21', 'Gasto sub 22')]
  #W = NULL
  di_naive = dea(XREF=X, YREF=Y, X=X, Y=Y, model="output", RTS="non-increasing")
  print(di_naive$thetaOpt)
  for (i in 1:182){
    for(j in 1:nrow(preDEAfiltered)){
      if(clusters$codigo[i] == preDEAfiltered$`Listado Hospitales`[j]){
        clusters$bon[i] = di_naive$thetaOpt[j]
      }
    }
  }
}

write.xlsx(clusters, "valoresDEA2014.xlsx")

