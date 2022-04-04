library("readxl")
library(openxlsx)
require(GGally)
require(VGAM)

datos <- read_excel("ResumenCriterioCalidad.xlsx", sheet = 1)

df = NULL
df = data.frame(1, 2, 3, 4, 5, 6, 7)
names(df) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")

#---------------

s = summary(m <- lm(mic ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
s$coefficients[2:6,1]
s
#s@coef3[3:7, 1]

de = NULL
de = data.frame("mic", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(miv ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("miv", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(min ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("min", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(moc ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("moc", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(mov ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("mov", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(mon ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("mon", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------
#---------------
#---------------
#---------------
#---------------
#---------------
#---------------

s = summary(m <- lm(dic ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("dic", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(div ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("div", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(din ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("din", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(doc ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("doc", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(dov ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("dov", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)

#---------------

s = summary(m <- lm(don ~ PromDiasEstada + DiasCama + Egresos + IndiceLetalidad + pacientesD2_D3, data = datos))
#s@coef3[3:7, 1]
s$coefficients[2:6,1]
s

de = NULL
de = data.frame("don", s$coefficients[2,1], s$coefficients[3,1], s$coefficients[4,1], s$coefficients[5,1], s$coefficients[6,1], 2014)
names(de) = c("Clusters", "PromDiasEstada", "DiasCama", "Egresos", "IndiceLetalidad", "PacientesD2_D3", "Año")
df <- rbind(df,de)


write.xlsx(df, 'AnalisisRegresionLineal2014.xlsx')


