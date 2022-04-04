library("readxl")
library(openxlsx)
require(GGally)
require(VGAM)

datos <- read_excel("ResumenCriterioCalidad.xlsx", sheet = 1)

summary(datos)

df = NULL
df = data.frame(1, 2, 3, 4, 5, 6, 7)
names(df) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")

#---------------

s = summary(m <- vglm(mic ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("mic", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(miv ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("miv", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(min ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("min", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(moc ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("moc", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(mov ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("mov", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(mon ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("mon", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------
#---------------
#---------------
#---------------
#---------------
#---------------
#---------------

s = summary(m <- vglm(dic ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("dic", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(div ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("div", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(din ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("din", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(doc ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("doc", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(dov ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("dov", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- vglm(don ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, tobit, data = datos))
s@coef3[3:7, 1]
s

de = NULL
de = data.frame("don", s@coef3[3,1], s@coef3[4,1], s@coef3[5,1], s@coef3[6,1], s@coef3[7,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)


write.xlsx(df, 'AnalisisTobit2014.xlsx')


