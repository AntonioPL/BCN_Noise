## -------------------------------------------------------------------------
## SCRIPT: 5 Analisis Series Temporales.R
## Descripción: Script para realizar análisis de Series Temporales
## FECHA: 13/03/2021
## PAQUETES NECESARIOS: ggplot2, hrbrthemes, imputeTS,zoo, tidyr
## INTRUCCIONES: Modificar los Bloques 3 (Variables) y 6 (Filtrado Temporal)
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("ggplot2")
library("hrbrthemes")
library("imputeTS")
library("zoo")
library("tidyr")
library("dplyr")

## -------------------------------------------------------------------------

##### 2. Bloque de definición de funciones auxiliares #####

crearDirectorio=function(subDir){
  if (file.exists(subDir)){
  } else {
    dir.create(file.path(getwd(), subDir))
  }
}

## -------------------------------------------------------------------------

##### 3. Bloque de parametros iniciales #####

Nombre_Analisis = "Analisis Serie Temporal"

VariablesAnalisis = c("LAeq24","Ld","Ln","Le","LAS01","LAS10","LAS50","LAS90","LAS99","LDEN")

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

crearDirectorio(Nombre_Analisis)

crearDirectorio(paste0(Nombre_Analisis,"/1 grafica serie"))
crearDirectorio(paste0(Nombre_Analisis,"/2 descomposicion"))
crearDirectorio(paste0(Nombre_Analisis,"/3 grafica medias moviles"))

dev.off()

## -------------------------------------------------------------------------

##### 4. Bloque de lectura de datos #####

creados = list.files("./Estadisticos Diarios/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]

if(exists("Data")) {rm(Data)}

for (fichero in creados){
print(fichero)
Data = read.csv(paste0("./Estadisticos Diarios/",fichero),stringsAsFactors = FALSE)

## -------------------------------------------------------------------------

##### 5. Bloque de formateo de fecha y orden #####

Data$Fecha=as.Date(Data$Fecha,format="%d/%m/%Y")
Data=Data[order(Data$Fecha),]

## -------------------------------------------------------------------------

##### 6. Bloque de filtrado temporal #####

FechaInicial = as.Date("2018-01-01",format ="%Y-%m-%d")
FechaFinal = as.Date("2021-12-31",format ="%Y-%m-%d")

Data = Data[Data$Fecha<FechaFinal & Data$Fecha>=FechaInicial,]

## -------------------------------------------------------------------------

##### 7. Bloque de analisis serie variable #####

for (variable in VariablesAnalisis){
  Data$VariableAnalisis=Data[[variable]]
  
  Grafico <- ggplot(Data, aes(x=Fecha, y=VariableAnalisis)) +
    geom_line() + 
    xlab("Date") +
    ylab("Lden,1d [dBA]") +
    scale_x_date(date_labels = "%m-%Y",date_breaks  = "30 day") +
    theme_ipsum() +
    theme(axis.title.y=element_text(size=24,face="bold"),axis.title.x=element_text(size=24,face="bold"),axis.text=element_text(size=14,face="bold"), axis.text.x=element_text(angle=60, hjust=1,size=14)) +
    geom_vline(xintercept = as.Date("15/03/2020",format="%d/%m/%Y"),color="red")+
    geom_vline(xintercept = as.Date("21/06/2020",format="%d/%m/%Y"),color="red")
  
  png(paste0(Nombre_Analisis,"/1 grafica serie/Variable ",variable," del punto ",Data$Codigo[1],".png"),width = 1024, height = 440)
  print(Grafico)
  dev.off()
  
## -------------------------------------------------------------------------
  
##### 8. Bloque de descomposición de la serie #####
  
  Serie <- ts(Data$VariableAnalisis,frequency = 7)
  
  # Imputamos nulos si los hubiese
  Serie <- na_seadec(Serie)
  
  # Descomposición de la serie
  Mul_Desc=decompose(Serie,type="multiplicative")
  Add_Desc=decompose(Serie,type="additive")
  
  png(paste0(Nombre_Analisis,"/2 descomposicion/Multiplicativa Variable ",variable," del punto ",Data$Codigo[1],".png"),width = 1024, height = 880)
  plot(Mul_Desc)
  dev.off()
  
  png(paste0(Nombre_Analisis,"/2 descomposicion/Aditiva Variable ",variable," del punto ",Data$Codigo[1],".png"),width = 1024, height = 880)
  plot(Add_Desc)
  dev.off()
  
  
## -------------------------------------------------------------------------
  
##### 9. Bloque de representación medias moviles #####
  
  DataMediasMoviles <- Data %>%
    select(Fecha, Variable = VariableAnalisis) %>%
    mutate(VarMa03 = rollmean(Variable, k = 7, fill = NA),
           VarMa15 = rollmean(Variable, k = 15, fill = NA),
           VarMa31 = rollmean(Variable, k = 31, fill = NA),
           VarMa65 = rollmean(Variable, k = 65, fill = NA),
           VarMa135 = rollmean(Variable, k = 135, fill = NA))
  
  
  Grafico <- DataMediasMoviles %>%
    #pivot_longer(Variable:VarMa31, names_to = "metric", values_to = "value") %>%
    gather(metric, value, Variable:VarMa135) %>%
    ggplot(aes(Fecha, value, color = metric)) +
    geom_line() +
    geom_vline(xintercept = as.Date("15/03/2020",format="%d/%m/%Y"),color="black")+
    geom_vline(xintercept = as.Date("21/06/2020",format="%d/%m/%Y"),color="black")
  
  png(paste0(Nombre_Analisis,"/3 grafica medias moviles/Variable ",variable," del punto ",Data$Codigo[1],".png"),width = 1024, height = 880)
  print(Grafico)
  dev.off()
}
}

## -------------------------------------------------------------------------