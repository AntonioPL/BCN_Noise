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

crearCarpeta("Estadisticos Diarios")

## -------------------------------------------------------------------------

##### 4. Bloque de carga de Maestro #####

Maestro = read.csv("./Maestro/Maestro.csv",stringsAsFactors = FALSE)

## -------------------------------------------------------------------------

##### 5. Bloque de selección de ficheros #####

creados = list.files("./Nodos/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]
creadosAux = gsub("\\(2\\)","",creados)

## -------------------------------------------------------------------------

##### 5. Bloque Opcional de selección de Nodos en el Maestro #####

# Maestro = Maestro[c(68),]

## -------------------------------------------------------------------------

##### 6. Bloque de calculo de estadísticos diarios #####

for (i in 1:dim(Maestro)[1]){
  Nodo = Maestro[i,]
  print(paste0("procesando nodo ",Nodo$fichero))
  
  elementos = grep(paste0("_",gsub("\\(2\\)","",Nodo$fichero),"_"),x = creadosAux)
  
  seleccionados = creados[elementos]
  
  Estadisticas = data.frame(Codigo = character(),
                            Direccion = character(),
                            Fecha = character(),
                            Ld = numeric(),
                            Le = numeric(),
                            Ln = numeric(),
                            LAMedia = numeric(),
                            LAS01 = numeric(),
                            LAS10 = numeric(),
                            LAS50 = numeric(),
                            LAS90 = numeric(),
                            LAS99 = numeric(),
                            Total = integer(),
                            Nulos = numeric(),
                            LAeq24 = numeric(),
                            SD = numeric(),
                            TNI = numeric(),
                            LDEN = numeric(),
                            LNP = numeric(),
                            stringsAsFactors=FALSE)
  
  for (h in 1:length(seleccionados)){
    #print(seleccionados[h])
    df <- try(read.csv(paste0("./Nodos/",seleccionados[h])))
    if (inherits(df, 'try-error')){ 
    } else{
      datos = read.csv(paste0("./Nodos/",seleccionados[h]), header = TRUE,stringsAsFactors = FALSE,encoding = "UTF-8")
      if (is.na(datos[1,1])) { print(paste0("Error en ",seleccionados[h]))}
      datos$Hora = as.numeric(gsub(":","",datos$Hora))
      datos$ValorA= 10^(datos$Valor/10)
      datos$Valord = datos$ValorA
      datos$Valore = datos$ValorA
      datos$Valorn = datos$ValorA
      datos$Valord[datos$Hora<700 | datos$Hora>=1900] = NA
      datos$Valore[datos$Hora<1900 | datos$Hora>=2300] = NA
      datos$Valorn[datos$Hora>=700 & datos$Hora<2300] = NA
      
      Agregado=summarise(group_by(datos,Fecha),
                         Total = n(),
                         Nulos = sum(is.na(Valor)),
                         Totald = sum(!is.na(Valord)),
                         Totale = sum(!is.na(Valore)),                        
                         Totaln = sum(!is.na(Valorn)),                         
                         Ld = 10*log10((1/(Totald))*sum(Valord,na.rm = TRUE)),
                         Le = 10*log10((1/(Totale))*sum(Valore,na.rm = TRUE)), 
                         Ln = 10*log10((1/(Totaln))*sum(Valorn,na.rm = TRUE)),
                         Leq24 = mean(Valor,na.rm = TRUE),
                         LAS01 = quantile(Valor,probs = 0.99,na.rm = TRUE),
                         LAS10 = quantile(Valor,probs = 0.90,na.rm = TRUE),
                         LAS50 = quantile(Valor,probs = 0.50,na.rm = TRUE),
                         LAS90 = quantile(Valor,probs = 0.10,na.rm = TRUE),
                         LAS99 = quantile(Valor,probs = 0.01,na.rm = TRUE),
                         LAeq24 = 10*log10((1/(Total-Nulos))*sum(ValorA,na.rm = TRUE)),
                         SD = sd(Valor,na.rm = TRUE))
      Agregado$TNI= 4 * (Agregado$LAS10-Agregado$LAS90) + Agregado$LAS90-30
      Agregado$LDEN = 10 * log10((12*10^(Agregado$Ld/10)+4*10^((Agregado$Le+5)/10)+8*10^((Agregado$Ln+10)/10))/24)
      Agregado$LNP = Agregado$LAeq24 + 2.56*Agregado$SD
      Estadisticas = rbind(Estadisticas,Agregado)
    }
  }
  
  Estadisticas$Codigo = Nodo$codigo
  Estadisticas$Direccion = Nodo$Direccion
  
  Estadisticas=Estadisticas[,c("Codigo","Direccion","Fecha","Ld","Le","Ln","Leq24","LAeq24","LAS01","LAS10","LAS50","LAS90","LAS99","Total","Nulos","SD","TNI","LDEN","LNP")]
  
  Estadisticas=Estadisticas[order(as.Date(Estadisticas$Fecha,"%d/%m/%Y"),decreasing = FALSE),]
  
  write.csv(Estadisticas, paste0("./Estadisticos Diarios/",Nodo$fichero,".csv"),row.names = FALSE)
 
  Estadisticas=Estadisticas[,c("Codigo","Fecha","Ld","Le","Ln","LDEN","LAS01","LAS10","LAS50","LAS90","LAS99")]
  
  write.csv(Estadisticas, paste0("./Estadisticos Diarios/Seleccionados",Nodo$fichero,".csv"),row.names = FALSE)
}

## -------------------------------------------------------------------------