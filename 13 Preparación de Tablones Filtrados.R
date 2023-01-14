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

CarpetaInput = "Tablones de Modelado por Franjas"
CarpetaOutput = "Tablones de Modelado por Franjas Filtrados"
Results = "Resultados Franjas"

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

crearCarpeta(CarpetaOutput)
crearCarpeta(Results)

## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files(paste0("./",CarpetaInput,"/"))

elementos = grep(".csv",x = creados)

creados=creados[elementos]

## -------------------------------------------------------------------------

##### 6. Bloque de calculo de estadísticos diarios #####

if (exists("Resultados")){rm("Resultados")}
for(fichero in creados){
  print(paste0("procesando fichero ",fichero))
  if (exists("Resultado")){rm("Resultado")}
  df <- try(read.csv(paste0("./",CarpetaInput,"/",fichero)))
  if (inherits(df, 'try-error')){ 
  } else{
    df <- read.csv(paste0("./",CarpetaInput,"/",fichero),stringsAsFactors = FALSE)
  }  
  
  Franja = df$Franja[1]
  Total = dim(df)[1]
  Resultado = df[complete.cases(df),]
  Complete = dim(Resultado)[1]
  Na = Total - Complete
  Porcentaje = paste0(round(100*Na/Total,2)," %")
  NumNodes = length(Resultado$codigo[!duplicated(Resultado$codigo)])
  RatioNodes = paste0(round(Complete/NumNodes,2))
    
  write.csv(Resultado, paste0("./",CarpetaOutput,"/",fichero),row.names = FALSE)  
  if (exists("Resultados")){
    Resultados = rbind(Resultados,c(Franja,Total,Complete,Na,Porcentaje, NumNodes, RatioNodes))
  } else{
    Resultados = c(Franja,Total,Complete,Na,Porcentaje, NumNodes, RatioNodes)
  }
}
Result=as.data.frame(Resultados)
colnames(Result)=c("Franja","Total","RegistrosCompletos","RegistrosConNA","Porcentaje","NumNodos","RatioNodos")
Result$Franja=as.numeric(Result$Franja)
Result$Total=as.numeric(Result$Total)
Result$RegistrosCompletos=as.numeric(Result$RegistrosCompletos)
Result$RegistrosConNA=as.numeric(Result$RegistrosConNA)
Result$NumNodos=as.numeric(Result$NumNodos)
Result$RatioNodos=as.numeric(Result$RatioNodos)
Result=Result[order(Result$Franja),]
write.csv2(Result, paste0("./",Results,"/Estadisticos Ficheros Filtrados.csv"),row.names = FALSE)

## -------------------------------------------------------------------------