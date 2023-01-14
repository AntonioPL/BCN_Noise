## -------------------------------------------------------------------------
## SCRIPT: Crear Maestro.R
## Descripción: Creación de tablon Maestro
## FECHA: 27/02/2021
## Paquetes Necesarios: stringr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library(stringr)

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

crearCarpeta("Maestro")

## -------------------------------------------------------------------------

##### 4. Bloque de selección de ficheros #####

creados = list.files("./Nodos/")
elementos = grep(".csv",x = creados)

creados=creados[elementos]

## -------------------------------------------------------------------------

##### 5. Bloque de lectura de ficheros #####

Maestro = data.frame(ID = character(),
                     Direccion = character(),
                     fichero = character(),
                     codigo = character(),
                     stringsAsFactors=FALSE)

nodos = str_split(creados,"_",simplify = TRUE)[,2]
creados = creados[!duplicated(nodos)]

for (h in 1:length(creados)){
  nodo=str_split(creados[h],"_")[2]
  if (!(nodo %in% Maestro$ID)){
    df <- try(read.csv(paste0("./Nodos/",creados[h])))
    if (inherits(df, 'try-error')){ 
    } else{
      datos = read.csv(paste0("./Nodos/",creados[h]), header = TRUE,stringsAsFactors = FALSE,encoding = "UTF-8")
      Maestro[h,c(1,2)] = datos[1,c(1,2)]
      Maestro[h,3] = str_split(creados[h],"_",simplify = TRUE)[2]
      Maestro[h,4] = paste0("BCN_",h)
    } 
  }
}

write.csv(Maestro, "./Maestro/Maestro.csv",row.names = FALSE)

## -------------------------------------------------------------------------