## -------------------------------------------------------------------------
## SCRIPT: Modelado Supervisado.R
## Entrenamiento de Modelos Supervisados
## FECHA: 06/10/2022
## Paquetes Necesarios: caret
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y entorno de trabajo#####

library("caret")
library("xgboost")
library("tidyverse")
library("lightgbm")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

Fecha=Sys.Date()
fechaChar = gsub("-","",as.character(Fecha))

CarpetaInput = "Tablones de Modelado por Franjas Filtrados"
CarpetaOutput = "Modelos Supervisados no ANN"
Results = "Resultados Modelado Supervisado no ANN"

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

if (exists("Resultados")){rm("Resultados")}
if (exists("times")){rm("times")}
times =c()


if(file.exists(paste0("./",Results,"/Evaluacion.csv"))){
  Resultados = read.csv2(paste0("./",Results,"/Evaluacion.csv"))
  
  Resultados$Franja = as.numeric(Resultados$Franja )
  Resultados$SizeTrain = as.numeric(Resultados$SizeTrain )
  Resultados$Fecha = as.Date(Resultados$Fecha )
  Resultados$SizeTest = as.numeric(Resultados$SizeTest )
  Resultados$Accuracy = as.numeric(Resultados$Accuracy )
  Resultados$Loss = as.numeric(Resultados$Loss )
} else{
  if(exists("Resultados")){remove(Resultados)}
}


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

  numberOfClasses <- length(unique(df[,61]))
  set.seed(1234)
  indexes = sample(1:nrow(df), size = .80 * nrow(df))

  train <- df[indexes, ]
  test <- df[-indexes, ]

  y_train <- as.numeric(train[,61])-1
  train.x <- as.matrix(train[,1:60], ncol(train),nrow(train))
  train.y <- to_categorical(matrix(y_train))
  train_mat <- xgb.DMatrix(train.x,label = y_train)
  
  
  colSums(train.y)

  y_test <- as.numeric(test[,61])-1
  test.x <- as.matrix(test[,1:60], ncol(test),nrow(test))
  test.y <- to_categorical(matrix(y_test))
  test_mat <- xgb.DMatrix(test.x,label = y_test)

  colSums(test.y)

## -------------------------------------------------------------------------

##### 8. Bloque de Creación de Modelos #####

  for (m in 1:2){
    
    if (m == 1){
      NombreModelo ="XGBoost" 
      print(paste("Procesando Modelo:",NombreModelo))
      if(exists("Resultados")){
        if(NombreModelo %in% Resultados$NombreModelo[Resultados$Franja == as.character(Franja)]){
          next
        }
      }
      nround    <- 50
      cv.nfold  <- 5
      
      xgb_params <- list("objective" = "multi:softprob",
                         "eval_metric" = "mlogloss",
                         "num_class" = numberOfClasses)
      
      set.seed(1234)
      model = xgb.train(params = xgb_params,
                     data = train_mat, 
                     nrounds = nround,
                     verbose = TRUE)
      
      save(model, file=paste0("./",CarpetaOutput,"/",fechaChar,"_Franja_",Franja,"_",NombreModelo,"_Modelo.Rdata"))
      
      
      pred <- model %>% predict(test.x,verbose = 0) #si quitamos verbose se visualizan las evoluciones
      
      pred <- matrix(round(as.numeric(pred), 2),ncol=numberOfClasses, byrow = TRUE)
      Result <- data.frame("Cluster 1"=pred[,1], "Cluster 2"=pred[,2], "Cluster 3"=pred[,3], "Cluster 4"=pred[,4], 
                           "predicted" = ifelse(max.col(pred[ ,1:numberOfClasses])==1, "Cluster 1",
                                                ifelse(max.col(pred[ ,1:numberOfClasses])==2, "Cluster 2",
                                                       ifelse(max.col(pred[ ,1:numberOfClasses])==3, "Cluster 3", "Cluster 4"))),
                           original = test[ ,61])
      
      write.csv2(Result, paste0(CarpetaOutput,"/",fechaChar,"_Franja_",Franja,"_",NombreModelo,"_Result.csv"),row.names = FALSE)
      
    }
    
    if(m == 2){
      NombreModelo ="lightGBM"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[Resultados$Franja == as.character(Franja)]){
        break
      }

      dtrain <- lgb.Dataset(train.x, label = y_train)
      dtest <- lgb.Dataset.create.valid(dtrain, data = test.x, label = y_test)
      valids <- list(test = dtest)
      
      params <- list(
        objective = "multiclass"
        , metric = "multi_error"
        , num_class = numberOfClasses
        , min_data = 1L
        , learning_rate = 1.0
      )
      
      model <- lgb.train(
        params
        , dtrain
        , 100L
        , valids
        , early_stopping_rounds = 10L
      )
      save(model, file=paste0("./",CarpetaOutput,"/",fechaChar,"_Franja_",Franja,"_",NombreModelo,"_Modelo.Rdata"))
      
      pred <- predict(model, test.x)
      
      pred <- matrix(round(as.numeric(pred), 2),ncol=numberOfClasses, byrow = TRUE)
      Result <- data.frame("Cluster 1"=pred[,1], "Cluster 2"=pred[,2], "Cluster 3"=pred[,3], "Cluster 4"=pred[,4], 
                           "predicted" = ifelse(max.col(pred[ ,1:numberOfClasses])==1, "Cluster 1",
                                                ifelse(max.col(pred[ ,1:numberOfClasses])==2, "Cluster 2",
                                                       ifelse(max.col(pred[ ,1:numberOfClasses])==3, "Cluster 3", "Cluster 4"))),
                           original = test[ ,61])
      
      write.csv2(Result, paste0(CarpetaOutput,"/",fechaChar,"_Franja_",Franja,"_",NombreModelo,"_Result.csv"),row.names = FALSE)
      
    }
    



## -------------------------------------------------------------------------

##### 10. Bloque de Evaluación de Modelos #####

    cfm=caret::confusionMatrix(as.factor(Result$predicted), as.factor(paste("Cluster",Result$original)))
    print(cfm)
    #str(cfm)
    cfm$overall
    cfm$overall[1]
  
    save(cfm, file = paste0(CarpetaOutput,"/",fechaChar,"_Franja_",Franja,"_",NombreModelo,"_ConfusionMatrix.RData"))
  
      Auxiliar = data.frame(Franja = Franja,NombreModelo = NombreModelo,Fecha=Fecha,SizeTrain =dim(train)[1] ,SizeTest=dim(test)[1], Accuracy = cfm$overall[1])
    
    if (exists("Resultados")){
      
      Resultados = rbind(Resultados,Auxiliar)
    } else{
      Resultados = Auxiliar
    }
    write.csv2(Resultados, paste0("./",Results,"/Evaluacion.csv"),row.names = FALSE)
  }
  print(paste("El tiempo de este fichero son: ",round(Sys.time()-start.time,4)))
}
## -------------------------------------------------------------------------

##### 11. Bloque de Exportación de Resultados #####


write.csv2(Resultados, paste0("./",Results,"/Evaluacion.csv"),row.names = FALSE)

## -------------------------------------------------------------------------