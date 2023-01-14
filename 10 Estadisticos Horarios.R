## -------------------------------------------------------------------------
## SCRIPT: Estadisticos.R
## Descripción: Script para lectura de datos de nodo y calculo de estadísticos
## FECHA: 27/02/2021
## Paquetes Necesarios: stringr, dplyr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library(stringr)
library("dplyr")
library("tidyr")

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

crearCarpeta("Datos por Horas")

## -------------------------------------------------------------------------

##### 4. Bloque de carga de Maestro #####

Maestro = read.csv("./Maestro/Maestro2.csv",stringsAsFactors = FALSE)

## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files("./Nodos2/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]
creadosAux = gsub("\\(2\\)","",creados)

## -------------------------------------------------------------------------

##### 5. Bloque Opcional de selección de Nodos en el Maestro #####

#Maestro = Maestro[c(17),]

## -------------------------------------------------------------------------

##### 6. Bloque de calculo de estadísticos diarios #####

for (i in 1:dim(Maestro)[1]){
  #i=1 #ELIMINAR EN PRODUCCION
  if (exists("Resultado")){rm("Resultado")}
  Nodo = Maestro[i,]
  print(paste0("procesando nodo ",Nodo$fichero))
  
  elementos = grep(paste0("_",gsub("\\(2\\)","",Nodo$fichero),"_"),x = creadosAux)
  
  seleccionados = creados[elementos]
  
  for (h in 1:length(seleccionados)){
    #h=1 #ELIMINAR EN PRODUCCION
    #print(seleccionados[h]) #ELIMINAR EN PRODUCCION
    df <- try(read.csv(paste0("./Nodos2/",seleccionados[h])))
    if (inherits(df, 'try-error')){ 
    } else{
      datos = read.csv(paste0("./Nodos2/",seleccionados[h]), header = TRUE,stringsAsFactors = FALSE,encoding = "UTF-8")
      if (is.na(datos[1,1])) { print(paste0("Error en ",seleccionados[h]))}
      datos$Hora = as.numeric(gsub(":","",datos$Hora))
      datos2=datos
      datos2$Franja = floor(datos2$Hora/100)
      datos2$Minutos = datos2$Hora-datos2$Franja*100
      datos3= pivot_wider(datos2, id_cols = c("ID","Direccion","Fecha","Franja"), names_from = Minutos, values_from = c("Valor"))
      
      if (exists("Resultado")){
        Resultado=rbind(Resultado,datos3)
      } else{
        Resultado= datos3
      }
    }
  }
  
  write.csv(Resultado, paste0("./Datos por Horas/Seleccionados",Nodo$fichero,".csv"),row.names = FALSE)
}

## -------------------------------------------------------------------------