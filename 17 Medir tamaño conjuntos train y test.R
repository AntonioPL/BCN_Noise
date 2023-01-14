## -------------------------------------------------------------------------
## SCRIPT: Tamaños Train y Test.R
## Entrenamiento de Modelos Supervisados
## FECHA: 06/10/2022
## Paquetes Necesarios: 
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y entorno de trabajo#####




## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

Fecha=Sys.Date()
fechaChar = gsub("-","",as.character(Fecha))

CarpetaInput = "Tablones de Modelado por Franjas Filtrados"
#CarpetaOutput = "Modelos Supervisados V10"
Results = "Distribución Train-Test"

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

#crearCarpeta(CarpetaOutput)
crearCarpeta(Results)

## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files(paste0("./",CarpetaInput,"/"))

elementos = grep(".csv",x = creados)

creados=creados[elementos]

if (exists("Resultados")){rm("Resultados")}
if (exists("times")){rm("times")}
times =c()



if(exists("Resultados")){remove(Resultados)}



for (i in 1:length(creados)){
  start.time = Sys.time()
  times =c(times,start.time )
## -------------------------------------------------------------------------

##### 6. Bloque de carga de ficheros #####
  
  fichero = creados[i]
  
  print(paste("Procesando Fichero:",fichero))

  df_total <- try(read.csv(paste0("./",CarpetaInput,"/",fichero)))
  if (inherits(df, 'try-error')){ 
  } else{
    df_total <- read.csv(paste0("./",CarpetaInput,"/",fichero),stringsAsFactors = FALSE)
  }  

  Franja = df_total$Franja[1]

## -------------------------------------------------------------------------

##### 7. Bloque de Preparación de Datos y Datasets #####

  df = df_total[,c(5:64,70)]

  set.seed(1234)
  indexes = sample(1:nrow(df), size = .80 * nrow(df))

  train <- df[indexes, ]
  test <- df[-indexes, ]

  Fila = c( sum(table(train$CLUSTER)),table(train$CLUSTER),sum(table(test$CLUSTER)),table(test$CLUSTER))
  
  if(exists("Resultados")){
    Resultados=rbind(Resultados,Fila)
    
  } else{
    Resultados=as.data.frame(t(Fila))
  }
  print(paste("El tiempo de este fichero son: ",round(Sys.time()-start.time,4),"segundos"))
}
## -------------------------------------------------------------------------

##### 8. Bloque de Exportación de Resultados #####

colnames(Resultados)=c("1Total", "1Pattern 1" ,"1Pattern 2", "1Pattern 3", "1Pattern 4", "Total", "Pattern 1" ,"Pattern 2","Pattern 3", "Pattern 4")

write.csv2(Resultados, paste0("./",Results,"/DistribucionTrainTest.csv"),row.names = FALSE)

## -------------------------------------------------------------------------