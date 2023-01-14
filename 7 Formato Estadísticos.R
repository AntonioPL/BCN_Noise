## -------------------------------------------------------------------------
## SCRIPT: Formato Estadísticos.R
## Descripción: Script para formatear los estadisticos
## FECHA: 11/04/2021
## Paquetes Necesarios: 
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####


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

crearCarpeta("Estadisticos Formateados")

## -------------------------------------------------------------------------

##### 4. Bloque de selección de ficheros #####

creados = list.files("./Estadisticos Diarios/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]

## -------------------------------------------------------------------------

##### 5. Bloque de calculo de estadísticos diarios #####

for (i in 1:length(creados)){
  print(paste0("procesando fichero ",creados[i]))
  
  df <- try(read.csv(paste0("./Estadisticos Diarios/",creados[i]),stringsAsFactors = FALSE))
  tipos <- sapply(df, class)
  
  # redondear a dos decimales las variables tipo numeric
  for (j in 1:length(tipos)){
    if (tipos[j]=="numeric"){
      df[,j]=round(df[,j],2)
    }
  }
  write.csv(df, paste0("./Estadisticos Formateados/",creados[i]),row.names = FALSE)
}
## -------------------------------------------------------------------------