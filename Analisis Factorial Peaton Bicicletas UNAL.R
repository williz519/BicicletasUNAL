
library(psych)
library(tidyverse)
library(readxl)
library(lubridate)
library(sf)
library(leaflet)
library(RColorBrewer)
library(sqldf)
require(reshape)
require(dplyr)
require(psych)
require(GGally)
library(corrplot)
library(corrr)

#Leer la dataset 

#Leer dataset Ciclistas

DBCilista <- read_excel("C:/Users/sin definir/Desktop/Bicicleta UNAL/TabulacionEncuestaPeatonCiclista.xlsx", 
                                                            sheet = "CICLISTA")

IndCiclista <- select(DBCilista, -(No:FQ4))

names(IndCiclista)


sink('AFactorialCiclistas.txt')

# Analisis Factorial Ciclistas

#Correlacion
cor(na.omit(IndCiclista), use = "pairwise.complete.obs")

RcorCi <- cor(na.omit(IndCiclista))

# Gráfico de las Correlaciones
corrplot(RcorCi, method = "shade", type="upper", order = "hclust", tl.col = "black", tl.cex = 1)

corrplot.mixed(RcorCi,lower.col = "black",number.cex=.6)

# Determinante de la Matriz de correlaciones
det(RcorCi)


# El test de esfericidad de Bartlett 
# Contrasta la hipótesis nula de que la matriz de correlaciones es igual a una matriz de 
# identidad. Lo que nos interesa para efectos de buscar multicolinealidad, por lo tanto, 
# es rechazar la hipótesis nula, y aceptar la hipótesis alternativa de que la matriz es 
# distinta a una matriz de identidad, y por ende hay un nivel suficiente de multicolinealidad 
# entre las variables. Este procedimiento es particularmente útil cuando el tamaño
# muestral es pequeño. 

#Test de esfericidad de Bartlett
print(cortest.bartlett(RcorCi, n=nrow(IndCiclista)))

#El índice KMO compara la magnitud de los coeficientes de correlación observados con la
# magnitud de los coeficientes de correlación parcial. Este estadístico varía entre 0 y 1, y se pueden
# calificar de la siguiente forma:
#  0,90 > KMO Muy bueno
#  0,90 > KMO > 0,80 Bueno
#  0,80 > KMO > 0,70 Aceptable
#  0,70 > KMO > 0,60 Mediocre o regular
#  0,60 > KMO > 0,50 Malo
#  0,50 > KMO Inaceptable o muy malo

#KMO
print(KMO(RcorCi))

#Analisis de Componentes Principales
pcaCi <- princomp(na.omit(IndCiclista), scores = TRUE, cor = TRUE)
summary(pcaCi)

#Cargar los componentes principales
loadings(pcaCi)

#Screen los componentes principales
plot(pcaCi)
screeplot(pcaCi, type = "line", main = "Scree Plot")


#Analisis Factorial

faCi <-factanal(na.omit(IndCiclista), factor=3, rotation = "varimax", na.rm = TRUE)
print(faCi,cutoff=0.35, sort=FALSE)

factoresCi <- factanal(na.omit(IndCiclista), factor=3, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores
print(factoresCi,cutoff=0.35, sort=FALSE)

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

DBFAC <- cbind(na.omit(IndCiclista), factoresCi)

DBFAC$Factor1 <- round(((DBFAC$Factor1 - min(DBFAC$Factor1))/(max(DBFAC$Factor1)-min(DBFAC$Factor1))),3)
DBFAC$Factor2 <- round(((DBFAC$Factor2 - min(DBFAC$Factor2))/(max(DBFAC$Factor2)-min(DBFAC$Factor2))),3)
DBFAC$Factor3 <- round(((DBFAC$Factor3 - min(DBFAC$Factor3))/(max(DBFAC$Factor3)-min(DBFAC$Factor3))),3)

DBFAC

sink()




# Analisis Factorial Peatones

#Leer dataset Peaton
DBPeaton <- read_excel("C:/Users/sin definir/Desktop/Bicicleta UNAL/TabulacionEncuestaPeatonCiclista.xlsx", 
                       sheet = "PEATON")

IndPeaton <- select(DBPeaton, -(No:FQ4))

names(IndPeaton)

sink('AFactorialPeaton.txt')

#Correlacion
cor(na.omit(IndPeaton), use = "pairwise.complete.obs")

RcorPea <- cor(na.omit(IndPeaton))

# Gráfico de las Correlaciones
corrplot(RcorPea, method = "shade", type="upper", order = "hclust", tl.col = "black", tl.cex = 1)

corrplot.mixed(RcorPea,lower.col = "black",number.cex=.6)

# Determinante de la Matriz de correlaciones
det(RcorPea)


# El test de esfericidad de Bartlett 
# Contrasta la hipótesis nula de que la matriz de correlaciones es igual a una matriz de 
# identidad. Lo que nos interesa para efectos de buscar multicolinealidad, por lo tanto, 
# es rechazar la hipótesis nula, y aceptar la hipótesis alternativa de que la matriz es 
# distinta a una matriz de identidad, y por ende hay un nivel suficiente de multicolinealidad 
# entre las variables. Este procedimiento es particularmente útil cuando el tamaño
# muestral es pequeño. 

#Test de esfericidad de Bartlett
print(cortest.bartlett(RcorPea, n=nrow(IndPeaton)))

#El índice KMO compara la magnitud de los coeficientes de correlación observados con la
# magnitud de los coeficientes de correlación parcial. Este estadístico varía entre 0 y 1, y se pueden
# calificar de la siguiente forma:
#  0,90 > KMO Muy bueno
#  0,90 > KMO > 0,80 Bueno
#  0,80 > KMO > 0,70 Aceptable
#  0,70 > KMO > 0,60 Mediocre o regular
#  0,60 > KMO > 0,50 Malo
#  0,50 > KMO Inaceptable o muy malo

#KMO
print(KMO(RcorPea))

#Analisis de Componentes Principales
pcaPea <- princomp(na.omit(IndPeaton), scores = TRUE, cor = TRUE)
summary(pcaPea)

#Cargar los componentes principales
loadings(pcaPea)

#Screen los componentes principales
plot(pcaPea)
screeplot(pcaPea, type = "line", main = "Scree Plot")


#Analisis Factorial

faPea <-factanal(na.omit(IndPeaton), factor=3, rotation = "varimax", na.rm = TRUE)
print(faPea,cutoff=0.35, sort=FALSE)

factoresPea <- factanal(na.omit(IndPeaton), factor=3, rotation = "varimax", na.rm = TRUE, scores = "regression")$scores
print(factoresPea,cutoff=0.35, sort=FALSE)

#Normalizamos los valores que podran tomarse como el peso para el cálculo de un indice que
# explica por cada i-esima fila la variabilidad de todo el conjunto de datos

DBFACPEA <- cbind(na.omit(IndPeaton), factoresPea)

DBFACPEA$Factor1 <- round(((DBFACPEA$Factor1 - min(DBFACPEA$Factor1))/(max(DBFACPEA$Factor1)-min(DBFACPEA$Factor1))),3)
DBFACPEA$Factor2 <- round(((DBFACPEA$Factor2 - min(DBFACPEA$Factor2))/(max(DBFACPEA$Factor2)-min(DBFACPEA$Factor2))),3)
DBFACPEA$Factor3 <- round(((DBFACPEA$Factor3 - min(DBFACPEA$Factor3))/(max(DBFACPEA$Factor3)-min(DBFACPEA$Factor3))),3)

DBFACPEA

sink()
