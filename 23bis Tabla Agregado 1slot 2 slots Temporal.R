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

CarpetaInput1 = "Resultados Agregados Redes Neuronales 1 Franja Temporal"
CarpetaInput2 = "Resultados Agregados Redes Neuronales 2 Franjas Temporales"
CarpetaOutput = "Resultados Agregados 1slot versus 2slot Temporal"

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

datos2 = read.csv2(paste0("./",CarpetaInput2,"/2025-10-08_Resultados.csv"))
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

Agregado2Mean = datos2_total %>%
  group_by(Modelo) %>%
  summarise(
    #Max_Kappa = mean(Kappa, na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    Mean_BalAccuracyC1 = mean(BalAccuracyC1, na.rm = TRUE),
    Mean_BalAccuracyC2 = mean(BalAccuracyC2, na.rm = TRUE),
    Mean_BalAccuracyC3 = mean(BalAccuracyC3, na.rm = TRUE),
    Mean_BalAccuracyC4 = mean(BalAccuracyC4, na.rm = TRUE),
    Mean_BalAccuracyMA = mean(BalAccuracyMA, na.rm = TRUE)
  )


Agregado2Mean


Agregado2Article = datos2_total %>%
  group_by(Modelo) %>%
  summarise(
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy = sd(Accuracy, na.rm = TRUE),
    Mean_BalAccuracyMA = mean(BalAccuracyMA, na.rm = TRUE),
    SD_BalAccuracyMA = sd(BalAccuracyMA, na.rm = TRUE)
  )


Agregado2Article


write.table(Agregado2,paste0("./",CarpetaOutput,"/Agregado 2 slot.csv"), dec=".",sep=";",row.names = FALSE)

## -------------------------------------------------------------------------

##### 5. Bloque de análisis 1 slot #####


datos1 = read.csv2(paste0("./",CarpetaInput1,"/2025-11-06_Resultados.csv"))
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

Agregado1Mean = datos1_total %>%
  group_by(Modelo) %>%
  summarise(
    #Max_Kappa = max(Kappa, na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    Mean_BalAccuracyC1 = mean(BalAccuracyC1, na.rm = TRUE),
    Mean_BalAccuracyC2 = mean(BalAccuracyC2, na.rm = TRUE),
    Mean_BalAccuracyC3 = mean(BalAccuracyC3, na.rm = TRUE),
    Mean_BalAccuracyC4 = mean(BalAccuracyC4, na.rm = TRUE),
    Mean_BalAccuracyMA = mean(BalAccuracyMA, na.rm = TRUE)
  )

Agregado1Mean


Agregado1Article = datos1_total %>%
  group_by(Modelo) %>%
  summarise(
    #Max_Kappa = max(Kappa, na.rm = TRUE),
    Mean_Accuracy = mean(Accuracy, na.rm = TRUE),
    SD_Accuracy = sd(Accuracy, na.rm = TRUE),
    Mean_BalAccuracyMA = mean(BalAccuracyMA, na.rm = TRUE),
    SD_BalAccuracyMA = sd(BalAccuracyMA, na.rm = TRUE)
  )

Agregado1Article


modelosSelec=c("Red_16_16_4", "Red_16_4", "Red_32_32_4", "Red_32_4", 
               "Red_64_32_4", "Red_64_4", "Red_32_32_32_4","Red_64_32_16_4","Red_16_16_16_4")

i=1
datos1_total$Accuracy[datos1_total$Modelo==modelosSelec[i]]
datos2_total$Accuracy[datos2_total$Modelo==modelosSelec[i]]
for (i in 1:length(modelosSelec)){
  print(t.test(datos1_total$Accuracy[datos1_total$Modelo==modelosSelec[i]],datos2_total$Accuracy[datos2_total$Modelo==modelosSelec[i]]))  
}



write.table(Agregado1,paste0("./",CarpetaOutput,"/Agregado 1 slot.csv"), dec=".",sep=";",row.names = FALSE)

## -------------------------------------------------------------------------

##### 7. Bloque de Analisis Conjunto #####


Agregado = merge(Agregado1, Agregado2, by="Modelo",all.x= TRUE, all.y= TRUE)

write.table(Agregado,paste0("./",CarpetaOutput,"/Agregado Total.csv"), dec=".",sep=";",row.names = FALSE)


AgregadoMean = merge(Agregado1Mean , Agregado2Mean , by="Modelo",all.x= TRUE, all.y= TRUE)

write.table(AgregadoMean ,paste0("./",CarpetaOutput,"/Agregado Total Mean .csv"), dec=".",sep=";",row.names = FALSE)


AgregadoArticle = merge(Agregado1Article , Agregado2Article , by="Modelo",all.x= TRUE, all.y= TRUE)

write.table(AgregadoArticle ,paste0("./",CarpetaOutput,"/Agregado Total Article.csv"), dec=".",sep=";",row.names = FALSE)



## -------------------------------------------------------------------------

##### 8. Bloque de Test de Wilcoxon #####


AgregadoMeanFiltrado = AgregadoMean[-c(4,5),]

wilcox.test(AgregadoMeanFiltrado$Mean_Accuracy.x, AgregadoMeanFiltrado$Mean_Accuracy.y, paired = TRUE, alternative = "less")
wilcox.test(AgregadoMeanFiltrado$Mean_BalAccuracyMA.x, AgregadoMeanFiltrado$Mean_BalAccuracyMA.y, paired = TRUE, alternative = "less")



wilcox.test(AgregadoMeanFiltrado$Mean_Accuracy.x, AgregadoMeanFiltrado$Mean_Accuracy.y, paired = TRUE, alternative = "two.sided")
wilcox.test(AgregadoMeanFiltrado$Mean_BalAccuracyMA.x, AgregadoMeanFiltrado$Mean_BalAccuracyMA.y, paired = TRUE, alternative = "two.sided")


## -------------------------------------------------------------------------

##### 9. Bloque de Ranking Horas #####


datos2 %>%
  arrange(desc(BalAccuracyMA)) %>%        
  select(Modelo, Franja1, Franja2, Accuracy, BalAccuracyMA) %>% 
  head(15)                         

ranking <- datos2 %>%
  arrange(desc(BalAccuracyMA)) %>%        
  select(Modelo, Franja1, Franja2, Accuracy, BalAccuracyMA) 

## -------------------------------------------------------------------------

##### 10. Bloque de Comparación Categorias #####


summary(datos1_total)
summary(datos2_total)
