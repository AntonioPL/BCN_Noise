## -------------------------------------------------------------------------
## SCRIPT: Completitud.R
## Descripción: Script para crear tabla de resumen de completitud
## FECHA: 11/04/2021
## Paquetes Necesarios: stringr, dplyr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library(stringr)
library("dplyr")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

Fecha=Sys.Date()

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

crearCarpeta("Completitud")

## -------------------------------------------------------------------------

##### 4. Bloque de selección de ficheros #####

creados = list.files("./Estadisticos Diarios/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]

## -------------------------------------------------------------------------

##### 5. Bloque de calculo de estadísticos diarios #####

NodeID=c()
FirstDay=c()
NumberDays=c()
TotalRecords=c()
ValidRecords=c()
NullRecords=c()
NullPercentage=c()

for (i in 1:length(creados)){
  print(paste0("procesando fichero ",creados[i]))
  
  df <- try(read.csv(paste0("./Estadisticos Diarios/",creados[i]),stringsAsFactors = FALSE))
  df = df[df$Total!=df$Nulos,]
  NodeID=c(NodeID,df$Codigo[1])
  FirstDay=c(FirstDay,df$Fecha[1])
  NumberDays=c(NumberDays,dim(df)[1])
  Total=sum(df$Total)
  Nulos=sum(df$Nulos)
  TotalRecords=c(TotalRecords,Total)
  ValidRecords=c(ValidRecords,Total-Nulos)
  NullRecords=c(NullRecords,Nulos)
  NullPercentage =c(NullPercentage,paste(round(100*Nulos/Total,2),"%",sep=""))
}
  
Completitud =data.frame(NodeID,FirstDay,NumberDays,TotalRecords,ValidRecords,NullRecords,NullPercentage)

write.csv(Completitud, paste0("./Completitud/",Fecha," Completitud.csv"),row.names = FALSE)

## -------------------------------------------------------------------------