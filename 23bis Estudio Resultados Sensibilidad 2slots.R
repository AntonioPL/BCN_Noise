## -------------------------------------------------------------------------
## SCRIPT: Estudio Resultados Sensibilidad 2slots.R
## Sensibilidad
## FECHA: 17/11/2025
## Paquetes Necesarios: tensorflow, keras, caret
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
library("ggplot2")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Datos BCN")

Num_Franjas = 2

Fecha=Sys.Date()
fechaChar = gsub("-","",as.character(Fecha))

CarpetaInput = paste0("Resultados Sensibilidad 2slots ",Num_Franjas)
CarpetaOutput = paste0("Resultados Sensibilidad 2slots ",Num_Franjas)
Results = paste0("Resultados Sensibilidad 2slots ",Num_Franjas)

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

##### 5. Bloque de carga de fichero #####

Resultados = read.csv2(paste0("./",CarpetaInput,"/Evaluacion prueba.csv"))


## -------------------------------------------------------------------------

##### 6. Bloque de preparación de datos #####


Resultados$slot2=paste(Resultados$Franja1,Resultados$Franja2,sep="_")


Resultados$Group="Rest"

GroupNightyEarly =c("1_20","1_21","1_22","1_23","2_20","2_21","2_22","2_23","20_3","21_3","22_3","23_3",
                "20_4","21_4","22_4","23_4","20_5","21_5","22_5","23_5")


Resultados$Group[Resultados$slot2 %in% GroupNightyEarly]="NightyEarly"

#GroupMediaTarde =c("11_12","11_13","11_14","11_15","11_16","11_17",
#                   "12_13","12_14","12_15","12_16","12_17",
#                   "13_14","13_15","13_16","13_17",
#                   "14_15","14_16","14_17",
#                   "15_16","15_17",
#                   "16_17")

#Resultados$Group[Resultados$slot2 %in% GroupMediaTarde]="Med_Tar"


#Filtro de Selección de Número de Experimentos
#Resultados = Resultados[Resultados$Experimento<=10,]


## -------------------------------------------------------------------------

##### 7. Bloque de agregado #####

Resultados_resumen <- Resultados %>%
  group_by(slot2) %>%
  summarise(
    hourly_time_slot1 = mean(Franja1),
    hourly_time_slot2 = mean(Franja2),
    count = n(),
    mean_accuracy = mean(Accuracy, na.rm = TRUE),
    sd_accuracy = sd(Accuracy, na.rm = TRUE),
    Group = max(Group),
  ) %>%
  mutate(
    error = qt(0.975, df = count - 1) * sd_accuracy / sqrt(count),
    ic_inf = mean_accuracy - error,
    ic_sup = mean_accuracy + error
  ) %>%
  mutate(
    decile = ntile(mean_accuracy, 10)
  )



ggplot(Resultados_resumen, aes(
  x = mean_accuracy,
  y = sd_accuracy,
  color = as.factor(decile)
)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = slot2), vjust = -0.8, size = 3) +
  scale_color_brewer(palette = "Spectral", name = "Decil") +
  theme_minimal() +
  labs(
    x = "Media Accuracy",
    y = "Desviación Típica",
    title = "Scatterplot de Media vs. Desviación con etiquetas (slot2)"
  )


## -------------------------------------------------------------------------

##### 8. Bloque de categoria Madrugada/Noche #####


                


colores_contraste <- c(
  "#E41A1C", # rojo
  "#377EB8", # azul
  "#4DAF4A", # verde
  "#984EA3", # morado
  "#FF7F00", # naranja
  "#FFFF33", # amarillo
  "#A65628", # marrón
  "#F781BF", # rosa fuerte
  "#999999", # gris
  "#1B9E77"  # verde azulado
)


ggplot(Resultados_resumen, aes(
  x = mean_accuracy,
  y = sd_accuracy,
  color = as.factor(Group)
)) +
  geom_point(size = 3, alpha = 0.8) +
  geom_text(aes(label = slot2), vjust = -0.8, size = 3) +
  scale_color_manual(values = colores_contraste, name = "Group") +
  theme_minimal() +
  labs(
    x = "Mean Accuracy",
    y = "SD Accuracy",
    title = "Scatterplot  (2-slot)"
  )


## -------------------------------------------------------------------------

##### 8. Bloque de Mapa de Calor #####

Resultados_resumen_select=Resultados_resumen[,c("hourly_time_slot1","hourly_time_slot2","mean_accuracy","decile")]
Resultados_resumen_sim = Resultados_resumen_select[,c("hourly_time_slot2","hourly_time_slot1","mean_accuracy","decile")]
colnames(Resultados_resumen_sim)=colnames(Resultados_resumen_select)

Resultados_resumen_total= rbind(Resultados_resumen_select,Resultados_resumen_sim)





colores_deciles <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#FFFF33", "#A65628", "#F781BF", "#999999", "#1B9E77"
)

colores_deciles <- c(
  "#999999", "#999999", "#999999", "#999999", "#999999",
  "#999999", "#999999", "#999999", "#999999", "#1B9E77"
)


ggplot(Resultados_resumen_total, aes(
  x = hourly_time_slot1,
  y = hourly_time_slot2,
  fill = as.factor(decile)
)) +
  geom_tile(color = "white") +
  scale_fill_manual(
    values = colores_deciles,
    name = "Decile"
  ) +
  geom_text(aes(label = round(mean_accuracy, 2)), color = "white") +
  labs(
    title = "Heatmap by accuracy decile",
    x = "Hourly Time Slot",
    y = "Hourly Time Slot"
  ) +
  theme_minimal()



## -------------------------------------------------------------------------

##### 9. Bloque de boxplot #####

boxplot(Accuracy~Group, data=Resultados, col=c("#E41A1C","#377EB8"))

