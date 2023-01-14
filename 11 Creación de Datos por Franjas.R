## -------------------------------------------------------------------------
## SCRIPT: Estadisticos.R
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

CarpetaInput = "Datos por Horas"
CarpetaOutput = "Datos Agrupados por Franjas"


Franjas = 0:23

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

#Maestro = read.csv("./Maestro/Maestro2.csv",stringsAsFactors = FALSE)
#Clusters = read.csv("./...") 

## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files(paste0("./",CarpetaInput,"/"))

elementos = grep(".csv",x = creados)

creados=creados[elementos]
#creadosAux = gsub("Seleccionados","",creados)

## -------------------------------------------------------------------------

##### 5. Bloque Opcional de selección de Nodos en el Maestro #####

#Maestro = Maestro[c(17),]

## -------------------------------------------------------------------------

##### 6. Bloque de calculo de estadísticos diarios #####

for(franja in Franjas){
  #franja = Franjas[1]
  print(paste0("procesando franja ",franja))
  if (exists("Resultado")){rm("Resultado")}
  for (i in 1:length(creados)){
    #i=1 #ELIMINAR EN PRODUCCION
    print(paste0("procesando fichero ",creados[i]))
    df <- try(read.csv(paste0("./",CarpetaInput,"/",creados[i])))
    if (inherits(df, 'try-error')){ 
    } else{
      df <- read.csv(paste0("./",CarpetaInput,"/",creados[i]))
    }  
    dfFiltrado = df[df$Franja == franja,]
    if (exists("Resultado")){
      Resultado = rbind(Resultado,dfFiltrado)
    } else{
      Resultado = dfFiltrado
    }
  }
  write.csv(Resultado, paste0("./",CarpetaOutput,"/Franja ",franja,".csv"),row.names = FALSE)  
}

## -------------------------------------------------------------------------