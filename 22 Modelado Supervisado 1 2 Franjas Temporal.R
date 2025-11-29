## -------------------------------------------------------------------------
## SCRIPT: Modelado Supervisado 2 Franjas Temporal.R
## Entrenamiento de Modelos Supervisados
## FECHA: 20/09/2025
## Paquetes Necesarios: tensorflow, keras, caret
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias y entorno de trabajo#####

library("reticulate")
#repl_python()
#py_config()
#virtualenv_list()

library("tensorflow")
#gpu <- tf$config$experimental$get_visible_devices('GPU')[[1]]
#tf$config$experimental$set_memory_growth(device = gpu, enable = TRUE)
use_virtualenv("R-DeepLearning") 
#use_virtualenv("r-reticulate") 
library("keras")
library("caret")

# Comprobación del entorno
tf$constant("Hello Tensorflow!")
tf$constant("Hello Tensorflow!")
tf$constant("Hello Tensorflow!")


## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Datos BCN")

Fecha=Sys.Date()
fechaChar = gsub("-","",as.character(Fecha))

CarpetaInput = "Tablones de Modelado por Franjas Filtrados Cruzados"
CarpetaOutput = "Modelos Supervisados Cruzados Temporales"
Results = "Resultados Modelado Supervisado Cruzados Temporales"

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
  
  Resultados$Franja1 = as.numeric(Resultados$Franja1 )
  Resultados$Franja2 = as.numeric(Resultados$Franja2 )
  Resultados$SizeTrain = as.numeric(Resultados$SizeTrain )
  Resultados$Fecha = as.Date(Resultados$Fecha )
  Resultados$SizeTest = as.numeric(Resultados$SizeTest )
  Resultados$Accuracy = as.numeric(Resultados$Accuracy )
  Resultados$Loss = as.numeric(Resultados$Loss )
} else{
  if(exists("Resultados")){remove(Resultados)}
}

#i=1
for (i in 1:length(creados)){
  start.time = Sys.time()
  times =c(times,start.time )
## -------------------------------------------------------------------------

##### 6. Bloque de carga de ficheros #####
  
  fichero = creados[i]
  
  print(paste("Procesando Fichero:",fichero, " indice: ",i))

  df_total <- try(read.csv(paste0("./",CarpetaInput,"/",fichero)))
  if (inherits(df_total, 'try-error')){ 
  } else{
    df_total <- read.csv(paste0("./",CarpetaInput,"/",fichero),stringsAsFactors = FALSE)
  }  

  Franja1 = df_total$Franja1[1]
  Franja2 = df_total$Franja2[1]
  
## -------------------------------------------------------------------------

##### 7. Bloque de Preparación de Datos y Datasets #####

  colnames(df_total)
  
  # CAMBIO A FORMATO TEMPORAL TRAIN PREVIO A TEST
  
  df_total$Fecha=as.Date(df_total$Fecha,format = "%d/%m/%Y")
  df_total$anio <- format(df_total$Fecha, "%Y")
  
  train <- subset(df_total, anio == "2018")
  test  <- subset(df_total, anio == "2019")
  
  train = train[,c(6:125,131)]
  test = test[,c(6:125,131)]
  
  #df = df_total[,c(6:125,131)] #### MODIFICAR PARA ADAPTAR AL PROBLEMA
  
  #colnames(df)
  N_vars = 120
  
  #set.seed(1234)
  #indexes = sample(1:nrow(df), size = .80 * nrow(df))

  #train <- df[indexes, ]
  #test <- df[-indexes, ]

  matrix(as.numeric(train[,N_vars+1])-1)

  train.x <- as.matrix(train[,1:N_vars], ncol(train),nrow(train))
  train.y <- to_categorical(matrix(as.numeric(train[,N_vars+1])-1))
  
  #vector=matrix(as.numeric(train[,N_vars+1])-1)
  #to_categorical(c(0,0,1,1,0,1),num_classes = NULL)
  
  #vector=matrix(as.numeric(train[,N_vars+1])-1)
  #train.y <- to_categorical(y=train[,N_vars+1]-1)

  colSums(train.y)

  test.x <- as.matrix(test[,1:N_vars], ncol(test),nrow(test))
  test.y <- to_categorical(matrix(as.numeric(test[,N_vars+1])-1))

  colSums(test.y)

## -------------------------------------------------------------------------

##### 8. Bloque de Creación de Modelos #####

  #Modelos =c(1:8)
  for (m in 1:11){
    if(exists("model")){rm(model)}
    model <- keras_model_sequential() 
    
    if (m == 1){
      NombreModelo ="Red_32_4" 
      print(paste("Procesando Modelo:",NombreModelo))
      if(exists("Resultados")){
        if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
          next
        }
      }
      model %>% 
        layer_dense(units = 32, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 2){
      NombreModelo ="Red_64_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 64, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 3){
      NombreModelo ="Red_32_32_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 32, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 4){
      NombreModelo ="Red_64_32_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 64, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if (m == 5){
      NombreModelo ="Red_16_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 16, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 6){
      NombreModelo ="Red_16_16_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 16, activation = "relu", input_shape = c(N_vars)) %>%
        layer_dense(units = 16, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 7){
      NombreModelo ="Red_16_16_16_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 16, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 16, activation = "relu") %>% 
        layer_dense(units = 16, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 8){
      NombreModelo ="Red_64_32_16_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 64, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = 16, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 9){
      NombreModelo ="Red_32_32_32_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 32, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 10){
      NombreModelo ="Red_32_32_16_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        next
      }
      model %>% 
        layer_dense(units = 32, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 32, activation = "relu") %>% 
        layer_dense(units = 16, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    if(m == 11){
      NombreModelo ="Red_32_16_8_4"
      print(paste("Procesando Modelo:",NombreModelo))
      if(NombreModelo %in% Resultados$NombreModelo[(Resultados$Franja1 == as.character(Franja1))&(Resultados$Franja2 == as.character(Franja2))]){
        break
      }
      model %>% 
        layer_dense(units = 32, activation = "relu", input_shape = c(N_vars)) %>% 
        layer_dense(units = 16, activation = "relu") %>% 
        layer_dense(units = 8, activation = "relu") %>% 
        layer_dense(units = 4, activation = "softmax")  
    }
    
    model %>% compile(optimizer = "rmsprop", 
                  loss = "categorical_crossentropy",  
                  metric=c("accuracy"))

    #print(model)
  
## -------------------------------------------------------------------------

##### 9. Bloque de Entrenamiento de Modelos #####
  
    set.seed(1234)
    model %>% fit(train.x, train.y,epochs = 25, batch_size = 50,verbose =0) #si quitamos verbose se visualizan las evoluciones

    save_model_tf(model, paste0(CarpetaOutput,"/",fechaChar,"_Franja1_",Franja1,"_Franja2_",Franja2,"_",NombreModelo,"_Modelo"))
## -------------------------------------------------------------------------

##### 10. Bloque de Evaluación de Modelos #####

    pred <- model %>% predict(test.x,verbose = 0) #si quitamos verbose se visualizan las evoluciones

    pred <- format(round(pred, 2), nsamll = 4)
    Result <- data.frame("Cluster 1"=pred[,1], "Cluster 2"=pred[,2], "Cluster 3"=pred[,3], "Cluster 4"=pred[,4], 
                     "predicted" = ifelse(max.col(pred[ ,1:4])==1, "Cluster 1",
                                          ifelse(max.col(pred[ ,1:4])==2, "Cluster 2",
                                                 ifelse(max.col(pred[ ,1:4])==3, "Cluster 3", "Cluster 4"))),
                     original = test[ ,N_vars+1])
  
    write.csv2(Result, paste0(CarpetaOutput,"/",fechaChar,"_Franja1_",Franja1,"_","_Franja2_",Franja2,"_",NombreModelo,"_Result.csv"),row.names = FALSE)

    #head(Result,20)

    scores <- model %>% evaluate(test.x, test.y)

    print(scores)

    #str(scores)
    #write.csv2(scores, paste0(CarpetaOutput,"/",fechaChar,"_Franja_",Franja,"_",NombreModelo,"_Scores.csv"),row.names = FALSE)
    

    cfm=caret::confusionMatrix(as.factor(Result$predicted), as.factor(paste("Cluster",Result$original)))
    print(cfm)
    #str(cfm)
    cfm$overall
  
    save(cfm, file = paste0(CarpetaOutput,"/",fechaChar,"_Franja1_",Franja1,"_","_Franja2_",Franja2,"_",NombreModelo,"_ConfusionMatrix.RData"))
  
      Auxiliar = data.frame(Franja1 = Franja1,Franja2 = Franja2,NombreModelo = NombreModelo,Fecha=Fecha,SizeTrain =dim(train)[1] ,SizeTest=dim(test)[1], Accuracy = scores[["accuracy"]], Loss = scores[["loss"]])
    
    if (exists("Resultados")){
      
      Resultados = rbind(Resultados,Auxiliar)
    } else{
      Resultados = Auxiliar
      #Resultados$Franja = as.numeric(Resultados$Franja )
      #Resultados$SizeTrain = as.numeric(Resultados$SizeTrain )
      #Resultados$SizeTest = as.numeric(Resultados$SizeTest )
      #Resultados$Accuracy = as.numeric(Resultados$Accuracy )
      #Resultados$Loss = as.numeric(Resultados$Loss )
    }
    write.csv2(Resultados, paste0("./",Results,"/Evaluacion.csv"),row.names = FALSE)
  }
  print(paste("El tiempo de este fichero son: ",round(Sys.time()-start.time,4),"minutos"))
}
## -------------------------------------------------------------------------

##### 11. Bloque de Exportación de Resultados #####


write.csv2(Resultados, paste0("./",Results,"/Evaluacion.csv"),row.names = FALSE)

## -------------------------------------------------------------------------