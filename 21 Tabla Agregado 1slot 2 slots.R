## -------------------------------------------------------------------------
## SCRIPT: Tabla Agregada 1slot 2slot.R
## Descripción: Script para el análisis agregado de los resultados de las redes neuronales
## FECHA: 07/02/2025
## Paquetes Necesarios: dplyr, stringr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
#library("tidyr")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Datos BCN")

CarpetaInput1 = "Resultados Agregados Redes Neuronales V4"
CarpetaInput2 = "Resultados Agregados Redes Neuronales 2 Franjas"
CarpetaOutput = "Resultados Agregados 1slot versus 2slot"

Fecha=Sys.Date()

## -------------------------------------------------------------------------

##### 3. Bloque de definicion de funciones auxiliares #####

crearCarpeta=function(Carpeta){
  if (!file.exists(Carpeta)){
    dir.create(file.path(getwd(), Carpeta))
  }
}

crearCarpeta(CarpetaOutput)

## -------------------------------------------------------------------------

##### 4. Bloque de análisis 2 slot #####

datos2 = read.csv2(paste0("./",CarpetaInput2,"/2024-11-23_Resultados.csv"))
datos2$N1 = datos2$P1R1+datos2$P2R1+datos2$P3R1+datos2$P4R1
datos2$N2 = datos2$P1R2+datos2$P2R2+datos2$P3R2+datos2$P4R2
datos2$N3 = datos2$P1R3+datos2$P2R3+datos2$P3R3+datos2$P4R3
datos2$N4 = datos2$P1R4+datos2$P2R4+datos2$P3R4+datos2$P4R4
datos2$BalAccuracyMA=(datos2$N1*datos2$BalAccuracyC1+datos2$N2*datos2$BalAccuracyC2+datos2$N3*datos2$BalAccuracyC3+datos2$N4*datos2$BalAccuracyC4)/(datos2$N1+datos2$N2+datos2$N3+datos2$N4)
summary(datos2)

datos2 = datos2[!duplicated(datos2[,c(2,3,4)]),]

datos2Aux = datos2[,c(1,2,4,3,5:49)]
colnames(datos2Aux)=colnames(datos2)

datos2_total= rbind(datos2,datos2Aux)
summary(datos2_total)

Agregado2 = datos2_total %>%
  group_by(Modelo) %>%
  summarise(
    Min_Accuracy = min(Accuracy, na.rm = TRUE),
    Max_Accuracy = max(Accuracy, na.rm = TRUE),
    Median_Accuracy = median(Accuracy, na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy = sd(Accuracy, na.rm = TRUE)
  )


Agregado2

Agregado2 = datos2_total %>%
  group_by(Modelo) %>%
  summarise(
    #Max_Kappa = max(Kappa, na.rm = TRUE),
    Max_Accuracy = max(Accuracy, na.rm = TRUE),
    Max_BalAccuracyC1 = max(BalAccuracyC1, na.rm = TRUE),
    Max_BalAccuracyC2 = max(BalAccuracyC2, na.rm = TRUE),
    Max_BalAccuracyC3 = max(BalAccuracyC3, na.rm = TRUE),
    Max_BalAccuracyC4 = max(BalAccuracyC4, na.rm = TRUE),
    Max_BalAccuracyMA = max(BalAccuracyMA, na.rm = TRUE)
  )


Agregado2

write.table(Agregado2,paste0("./",CarpetaOutput,"/Agregado 2 slot.csv"), dec=".",sep=";",row.names = FALSE)

## -------------------------------------------------------------------------

##### 5. Bloque de análisis 1 slot #####


datos1 = read.csv2(paste0("./",CarpetaInput1,"/2022-12-22_Resultados.csv"))
datos1$N1 = datos1$P1R1+datos1$P2R1+datos1$P3R1+datos1$P4R1
datos1$N2 = datos1$P1R2+datos1$P2R2+datos1$P3R2+datos1$P4R2
datos1$N3 = datos1$P1R3+datos1$P2R3+datos1$P3R3+datos1$P4R3
datos1$N4 = datos1$P1R4+datos1$P2R4+datos1$P3R4+datos1$P4R4
datos1$BalAccuracyMA=(datos1$N1*datos1$BalAccuracyC1+datos1$N2*datos1$BalAccuracyC2+datos1$N3*datos1$BalAccuracyC3+datos1$N4*datos1$BalAccuracyC4)/(datos1$N1+datos1$N2+datos1$N3+datos1$N4)
summary(datos1)


datos1_total= datos1

Agregado1 = datos1_total %>%
  group_by(Modelo) %>%
  summarise(
    Min_Accuracy = min(Accuracy, na.rm = TRUE),
    Max_Accuracy = max(Accuracy, na.rm = TRUE),
    Median_Accuracy = median(Accuracy, na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy = sd(Accuracy, na.rm = TRUE)
  )


Agregado1


Agregado1 = datos1_total %>%
  group_by(Modelo) %>%
  summarise(
    #Max_Kappa = max(Kappa, na.rm = TRUE),
    Max_Accuracy = max(Accuracy, na.rm = TRUE),
    Max_BalAccuracyC1 = max(BalAccuracyC1, na.rm = TRUE),
    Max_BalAccuracyC2 = max(BalAccuracyC2, na.rm = TRUE),
    Max_BalAccuracyC3 = max(BalAccuracyC3, na.rm = TRUE),
    Max_BalAccuracyC4 = max(BalAccuracyC4, na.rm = TRUE),
    Max_BalAccuracyMA = max(BalAccuracyMA, na.rm = TRUE)
  )


Agregado1
write.table(Agregado1,paste0("./",CarpetaOutput,"/Agregado 1 slot.csv"), dec=".",sep=";",row.names = FALSE)

## -------------------------------------------------------------------------

##### 6. Bloque de Analisis Conjunto #####


Agregado = merge(Agregado1, Agregado2, by="Modelo",all.x= TRUE, all.y= TRUE)

write.table(Agregado,paste0("./",CarpetaOutput,"/Agregado Total.csv"), dec=".",sep=";",row.names = FALSE)
