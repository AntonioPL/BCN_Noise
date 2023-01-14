## -------------------------------------------------------------------------
## SCRIPT: Contraste te Hipotesis.R
## Descripción: Script para el análisis de los resultados de las redes neuronales
## FECHA: 10/12/2022
## Paquetes Necesarios: dplyr, stringr
## -------------------------------------------------------------------------


## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
library("stringr")
library("ggplot2")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

CarpetaInput = "Resultados Agregados Redes Neuronales"
CarpetaOutput = "Contraste de Hipotesis"

Fecha=Sys.Date()

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

crearCarpeta(CarpetaOutput)

## -------------------------------------------------------------------------

##### 4. Bloque de selección de ficheros #####

Estadisticas = read.csv2(paste0(CarpetaInput,"/",Fecha,"_Resultados.csv"))


## -------------------------------------------------------------------------

##### 5. Valor máximo #####

min(Estadisticas$Accuracy)
max(Estadisticas$Accuracy)

Estadisticas[which(Estadisticas$Accuracy == max(Estadisticas$Accuracy)),]

## -------------------------------------------------------------------------

##### 6. Mayores y Menores #####

TamClusters=c(27,23,7,11)

baseline = max(TamClusters)/sum(TamClusters)

sum(Estadisticas$Accuracy>=baseline)
sum(Estadisticas$Accuracy<=baseline)

sum(Estadisticas$Accuracy<=0.25)
160/192
## -------------------------------------------------------------------------

##### 7. Contraste de Hipótesis #####


t.test(Estadisticas$Accuracy, mu=baseline,  alternative ="greater")


## -------------------------------------------------------------------------