## -------------------------------------------------------------------------
## SCRIPT: Preparacion de Tablones Filtrados Cruzados.R
## Descripción: Script para lectura de datos de nodo y calculo de estadísticos
## FECHA: 28/09/2024
## Paquetes Necesarios: stringr, dplyr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

#library(stringr)
#library("dplyr")
#library("tidyr")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Datos BCN")

Fecha=Sys.Date()

CarpetaInput = "Tablones de Modelado por Franjas Filtrados"
CarpetaOutput = "Tablones de Modelado por Franjas Filtrados Cruzados"

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

crearCarpeta(CarpetaOutput)

## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files(paste0("./",CarpetaInput,"/"))

elementos = grep(".csv",x = creados)

creados=creados[elementos]

## -------------------------------------------------------------------------

##### 6. Bloque de calculo de estadísticos diarios #####

if (exists("Resultados")){rm("Resultados")}
length(creados)

for(i in c(1:length(creados))){
  fichero1 = creados[i]
  print(paste0("procesando fichero ",fichero1))
  df1 <- try(read.csv(paste0("./",CarpetaInput,"/",fichero1)))
  df1=df1[!duplicated(df1),]
  colnames(df1) = gsub("Franja","Franja1",colnames(df1))
  nomFichero1= strsplit(fichero1,".",fixed=T)[[1]][1]
  for(j in c((i+1):length(creados))){
    fichero2 = creados[j]
    print(paste0("....con el fichero ",fichero2))
    df2 <- try(read.csv(paste0("./",CarpetaInput,"/",fichero2)))
    df2=df2[!duplicated(df2),]
    colnames(df2) = gsub("Franja","Franja2",colnames(df2))
    colnames(df2) = gsub("X","Y",colnames(df2))
    df2[,c("Direccion","Variable1","Variable2","Variable3","Variable4","CLUSTER")] = NULL
    dfmerge = merge(df1,df2, by=c("ID","Fecha","codigo"))
    dfmerge2 = dfmerge[,c("ID","Direccion","Fecha","Franja1","Franja2","X0","X1","X2","X3","X4","X5","X6","X7","X8","X9","X10","X11","X12","X13","X14","X15","X16","X17","X18","X19","X20","X21","X22","X23","X24","X25","X26","X27","X28","X29","X30","X31","X32","X33","X34","X35","X36","X37","X38","X39","X40","X41","X42","X43","X44","X45","X46","X47","X48","X49","X50","X51","X52","X53","X54","X55","X56","X57","X58","X59","Y0","Y1","Y2","Y3","Y4","Y5","Y6","Y7","Y8","Y9","Y10","Y11","Y12","Y13","Y14","Y15","Y16","Y17","Y18","Y19","Y20","Y21","Y22","Y23","Y24","Y25","Y26","Y27","Y28","Y29","Y30","Y31","Y32","Y33","Y34","Y35","Y36","Y37","Y38","Y39","Y40","Y41","Y42","Y43","Y44","Y45","Y46","Y47","Y48","Y49","Y50","Y51","Y52","Y53","Y54","Y55","Y56","Y57","Y58","Y59","codigo","Variable1","Variable2","Variable3","Variable4","CLUSTER")]
    write.csv(dfmerge2, paste0("./",CarpetaOutput,"/",nomFichero1,"_",fichero2),row.names = FALSE)  
  }
}

## -------------------------------------------------------------------------