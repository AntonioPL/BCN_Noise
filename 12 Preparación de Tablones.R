## -------------------------------------------------------------------------
## SCRIPT: Preparacion de Tablones.R
## Descripción: Script para lectura de datos de nodo y calculo de estadísticos
## FECHA: 27/02/2021
## Paquetes Necesarios: stringr, dplyr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

#library(stringr)
#library("dplyr")
#library("tidyr")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

Fecha=Sys.Date()

CarpetaInput = "Datos Agrupados por Franjas"
CarpetaOutput = "Tablones de Modelado por Franjas"
Nombre_Analisis = "Analisis No Supervisado LdLeLnS"
NumClusters = 4

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

crearCarpeta(CarpetaOutput)

## -------------------------------------------------------------------------

##### 4. Bloque de carga de Maestro #####

Maestro = read.csv("./Maestro/Maestro2.csv",stringsAsFactors = FALSE, fileEncoding = "Latin1")
Maestro2 = read.csv("./Maestro/Maestro2.csv",stringsAsFactors = FALSE)
Clusters = read.csv2(paste0("./",Nombre_Analisis,"/5 asignaciones/Asignaciones ",NumClusters," clusters.csv")) 

Datos = merge(Maestro[,c("ID","codigo")],Clusters, by.x ="codigo",by.y="Codigo",all.x =TRUE)
Datos2 = merge(Maestro2[,c("ID","codigo")],Clusters, by.x ="codigo",by.y="Codigo",all.x =TRUE)
## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files(paste0("./",CarpetaInput,"/"))

elementos = grep(".csv",x = creados)

creados=creados[elementos]

## -------------------------------------------------------------------------

##### 6. Bloque de calculo de estadísticos diarios #####

for(fichero in creados){
  print(paste0("procesando fichero ",fichero))
  if (exists("Resultado")){rm("Resultado")}
  df <- try(read.csv(paste0("./",CarpetaInput,"/",fichero)))
  if (inherits(df, 'try-error')){ 
  } else{
    df <- read.csv(paste0("./",CarpetaInput,"/",fichero),stringsAsFactors = FALSE)
  }  
  df2 = merge(df,Datos, by.x ="ID",by.y="ID",all.x =TRUE)
  df2Solved =df2[!is.na(df2$codigo),]
  df2NotSolved =df2[is.na(df2$codigo),1:64]
  df3 = merge(df2NotSolved,Datos2, by.x ="ID",by.y="ID",all.x =TRUE)
  df3NotSolved =df3[is.na(df3$codigo),1:64]
  print(paste0("No resueltos: ",dim(df3NotSolved)[1]))
  
  Resultado =rbind(df2Solved,df3)
  write.csv(Resultado, paste0("./",CarpetaOutput,"/",fichero),row.names = FALSE)  
}



## -------------------------------------------------------------------------