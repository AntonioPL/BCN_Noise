## -------------------------------------------------------------------------
## SCRIPT: Extracción Datos Nodos BCN.R
## Descripción: Script para la desagregacion por nodo y mes de los datos de Barcelona
## FECHA: 20/02/2021
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

crearCarpeta("Nodos")


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

## -------------------------------------------------------------------------

##### 4. Bloque de selección de ficheros #####

ficheros = list.files()
elementos = grep(".csv",x = ficheros)

ficheros = ficheros[elementos]

creados = list.files("./Nodos/")

## -------------------------------------------------------------------------

##### 5. Bloque de lectura de ficheros #####

dfempty = data.frame(ID = character(),
                    Direccion = character(),
                    Fecha = character(),
                    Hora = character(),
                    Metrica= integer(),
                    Valor= numeric(),
                    stringsAsFactors=FALSE)

for (h in 1:length(ficheros)){
  StartTime=Sys.time()
  print(ficheros[h])
  df <- try(read.csv2(paste0(ficheros[h])))
  if (inherits(df, 'try-error')){ 
  } else{
    tablonAux = dfempty
    datos = read.csv2(paste0(ficheros[h]), header = FALSE,stringsAsFactors = FALSE,encoding = "UTF-8")
    fila = 1
    for (i in 4:dim(datos)[1]){
      if (datos$V1[i]!=""){
        if (dim(tablonAux)[1]>1){
          tablonAux$ID = ID
          tablonAux$Direccion = Direccion
          write.csv(tablonAux,paste("./Nodos/",nombreFichero,sep=""), row.names = FALSE)
          tablonAux = dfempty
          fila = 1
        }
        ID = datos$V1[i]
        ID = gsub("/","-",ID)
        Direccion = paste(datos$V2[i],datos$V3[i],datos$V4[i],sep="_")
        nombreFichero = paste("long",ID,ficheros[h],sep="_")
        print(paste0("creando ",nombreFichero))
        if (nombreFichero %in% creados){crear =FALSE} else {crear = TRUE}
      }
      if (crear){
        if (!is.na(datos$V5[i])){
          Hora = paste0("0",datos$V5[i])
          Hora = substrRight(Hora,2)
        }
        for (j in 7:dim(datos)[2]){
          tablonAux[fila,3] = datos[1,j]
          tablonAux[fila,4] = paste(Hora,substrRight(paste0("0",datos[i,6]),2),sep=":")
          tablonAux[fila,5] = datos[3,j]
          tablonAux[fila,6] = as.numeric(gsub(",",".",datos[i,j]))
          fila = fila +1
          if (fila %% 2000==0){
            print(paste("procesando fila",fila))
          }
        }
      }
      
    }
    if (crear){
      tablonAux$ID = ID
      tablonAux$Direccion = Direccion
      write.csv(tablonAux,paste("./Nodos/",nombreFichero,sep=""), row.names = FALSE)
    }
  } 
  print("Este fichero ha tardado:")
  print(Sys.time()-StartTime)
}
    
## -------------------------------------------------------------------------