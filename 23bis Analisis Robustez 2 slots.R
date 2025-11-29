## -------------------------------------------------------------------------
## SCRIPT: Preparar y Analizar Resultados Modelos.R
## Descripción: Script para el análisis de los resultados de las redes neuronales
## FECHA: 09/10/2024
## Paquetes Necesarios: dplyr, stringr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
library("stringr")
library("ggplot2")
library("tidyr")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Datos BCN")

CarpetaInput = "Resultados Agregados Redes Neuronales 2 Franjas Temporales"
CarpetaOutput = "Robustez 2 Franjas Temporales"

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

##### 4. Bloque de selección de ficheros #####

Estadisticas = read.csv2(paste0(CarpetaInput,"/2025-10-08_Resultados_Global.csv"))


## -------------------------------------------------------------------------

##### 5. Bloque de rankeado #####


Estadisticas <- Estadisticas %>%
  group_by(Modelo) %>%
  arrange(desc(BalAccuracyMicro), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()


Estadisticas$slot2 = paste(Estadisticas$Franja1,Estadisticas$Franja2,sep="_")


Estadisticas_summary <- Estadisticas %>%
  group_by(Franja1, Franja2) %>%
  summarise(
    BA_min   = min(BalAccuracyMicro, na.rm = TRUE),
    BA_Q1    = quantile(BalAccuracyMicro, 0.25, na.rm = TRUE),
    BA_mean  = mean(BalAccuracyMicro, na.rm = TRUE),
    BA_median= median(BalAccuracyMicro, na.rm = TRUE),
    BA_Q3    = quantile(BalAccuracyMicro, 0.75, na.rm = TRUE),
    BA_max   = max(BalAccuracyMicro, na.rm = TRUE),
    Rank_min   = min(rank, na.rm = TRUE),
    Rank_Q1    = quantile(rank, 0.25, na.rm = TRUE),
    Rank_mean  = mean(rank, na.rm = TRUE),
    Rank_median= median(rank, na.rm = TRUE),
    Rank_Q3    = quantile(rank, 0.75, na.rm = TRUE),
    Rank_max   = max(rank, na.rm = TRUE),
    .groups = "drop"
  )
