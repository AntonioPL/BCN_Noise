## -------------------------------------------------------------------------
## SCRIPT: Preparar y Analizar Resultados Modelos.R
## Descripción: Script para el análisis de los resultados de las redes neuronales
## FECHA: 06/12/2022
## Paquetes Necesarios: dplyr, stringr
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
library("stringr")
library("ggplot2")

## -------------------------------------------------------------------------

##### 2. Bloque de parametros iniciales #####

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

CarpetaInput = "Modelos Supervisados"
CarpetaOutput = "Resultados Agregados Redes Neuronales"

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

ficheros = list.files(CarpetaInput)
elementos = grep(".RData",x = ficheros)

ficheros = ficheros[elementos]

## -------------------------------------------------------------------------

##### 5. Bloque de selección de data frame #####


Estadisticas = data.frame(Fecha = character(),
                          Modelo = character(),
                          Franja = numeric(),
                          Accuracy = numeric(),
                          Kappa = numeric(),
                          AccuracyLower = numeric(),
                          AccuracyUpper = numeric(),
                          P1R1 = numeric(),
                          P1R2 = numeric(),
                          P1R3 = numeric(),
                          P1R4 = numeric(),
                          P2R1 = numeric(),
                          P2R2 = numeric(),
                          P2R3 = numeric(),
                          P2R4 = numeric(),
                          P3R1 = numeric(),
                          P3R2 = numeric(),
                          P3R3 = numeric(),
                          P3R4 = numeric(),
                          P4R1 = numeric(),
                          P4R2 = numeric(),
                          P4R3 = numeric(),
                          P4R4 = numeric(),
                          BalAccuracyC1 = numeric(),
                          BalAccuracyC2 = numeric(),
                          BalAccuracyC3 = numeric(),
                          BalAccuracyC4 = numeric(),
                          PrecisionC1 = numeric(),
                          PrecisionC2 = numeric(),
                          PrecisionC3 = numeric(),
                          PrecisionC4 = numeric(),
                          RecallC1 = numeric(),
                          RecallC2 = numeric(),
                          RecallC3 = numeric(),
                          RecallC4 = numeric(),
                          F1C1 = numeric(),
                          F1C2 = numeric(),
                          F1C3 = numeric(),
                          F1C4 = numeric(),
                          SpecificityC1 = numeric(),
                          SpecificityC2 = numeric(),
                          SpecificityC3 = numeric(),
                          SpecificityC4 = numeric(),
                          stringsAsFactors=FALSE)


## -------------------------------------------------------------------------

##### 6. Bloque de extracción de estadisticos #####

for (i in 1:length(ficheros)){
  fichero=ficheros[i]
  terminos = strsplit(fichero,"_")[[1]]
  
  Estadisticas[i,1] = terminos[1]
  Estadisticas[i,2] = paste(terminos[4:(length(terminos)-1)],collapse="_")
  Estadisticas[i,3] = as.numeric(terminos[3])
  
  load(paste0(CarpetaInput,"/",fichero))
  
  #cfm
  
  #cfm$overall
  Estadisticas[i,4] = as.numeric(cfm$overall[1]) #Accuracy
  Estadisticas[i,5] = as.numeric(cfm$overall[2]) #Kappa
  Estadisticas[i,6] = as.numeric(cfm$overall[3]) #AccuracyLower
  Estadisticas[i,7] = as.numeric(cfm$overall[4]) #AccuracyUpper
  
  #cfm$table
  #PiRj Prediction I Real j
  Estadisticas[i,8] = cfm$table[1,1]
  Estadisticas[i,9] = cfm$table[1,2]
  Estadisticas[i,10] = cfm$table[1,3]
  Estadisticas[i,11] = cfm$table[1,4]
  Estadisticas[i,12] = cfm$table[2,1]
  Estadisticas[i,13] = cfm$table[2,2]
  Estadisticas[i,14] = cfm$table[2,3]
  Estadisticas[i,15] = cfm$table[2,4]
  Estadisticas[i,16] = cfm$table[3,1]
  Estadisticas[i,17] = cfm$table[3,2]
  Estadisticas[i,18] = cfm$table[3,3]
  Estadisticas[i,19] = cfm$table[3,4]
  Estadisticas[i,20] = cfm$table[4,1]
  Estadisticas[i,21] = cfm$table[4,2]
  Estadisticas[i,22] = cfm$table[4,3]
  Estadisticas[i,23] = cfm$table[4,4]
  
  #cfm$byClass
  #BalAccuracy
  Estadisticas[i,24] = cfm$byClass[1,11]
  Estadisticas[i,25] = cfm$byClass[2,11]
  Estadisticas[i,26] = cfm$byClass[3,11]
  Estadisticas[i,27] = cfm$byClass[4,11]
  #Precision
  Estadisticas[i,28] = cfm$byClass[1,5]
  Estadisticas[i,29] = cfm$byClass[2,5]
  Estadisticas[i,30] = cfm$byClass[3,5]
  Estadisticas[i,31] = cfm$byClass[4,5]
  #Recall
  Estadisticas[i,32] = cfm$byClass[1,6]
  Estadisticas[i,33] = cfm$byClass[2,6]
  Estadisticas[i,34] = cfm$byClass[3,6]
  Estadisticas[i,35] = cfm$byClass[4,6]
  #F1
  Estadisticas[i,36] = cfm$byClass[1,7]
  Estadisticas[i,37] = cfm$byClass[2,7]
  Estadisticas[i,38] = cfm$byClass[3,7]
  Estadisticas[i,39] = cfm$byClass[4,7]
  #Specificity 
  Estadisticas[i,40] = cfm$byClass[1,2]
  Estadisticas[i,41] = cfm$byClass[2,2]
  Estadisticas[i,42] = cfm$byClass[3,2]
  Estadisticas[i,43] = cfm$byClass[4,2]
}

## -------------------------------------------------------------------------

##### 7. Bloque de Guardado de Resultados  #####

write.csv2(Estadisticas,paste0(CarpetaOutput,"/",Fecha,"_Resultados.csv"),row.names = FALSE)

## -------------------------------------------------------------------------

##### 8. Bloque de Analisis de Resultados: General  #####

summary(Estadisticas)


## -------------------------------------------------------------------------

##### 8. Bloque de color de Franjas #####

#Estadisticas$ColorDEN= ifelse(Estadisticas$Franja<7,"Blue",ifelse(Estadisticas$Franja<19,"Red",ifelse(Estadisticas$Franja<23,"Green","Blue")))

#table(Estadisticas$ColorDEN, Estadisticas$Franja)

ColoresDEN=c(rep("grey",7),rep("yellow",12),rep("orange",4),"grey")
ColoresArticulo = c("black")

## -------------------------------------------------------------------------

##### 8. Bloque de Análisis de Resultados:Agregados por Horas #####

NombresVariables=c("Fecha","Modelo","Franja","Accuracy","Kappa","Accuracy Lower","Accuracy Upper","P1R1","P1R2","P1R3","P1R4","P2R1","P2R2","P2R3","P2R4","P3R1","P3R2","P3R3","P3R4","P4R1","P4R2","P4R3","P4R4","Balanced Accuracy Cluster 1","Balanced Accuracy Cluster 2","Balanced Accuracy Cluster 3","Balanced Accuracy Cluster 4","Precision Cluster 1","Precision Cluster 2","Precision Cluster 3","Precision Cluster 4","Sensitivity Cluster 1","Sensitivity Cluster 2","Sensitivity Cluster 3","Sensitivity Cluster 4","F1 Cluster 1","F1 Cluster 2","F1 Cluster 3","F1 Cluster 4","Specificity  Cluster 1","Specificity  Cluster 2","Specificity  Cluster 3","Specificity  Cluster 4")
NombreColores = "DEN"
Color = ColoresDEN

#dev.off()

for (j in (4:length(NombresVariables))){
  Variable = Estadisticas[,j]
  NombreVariable = NombresVariables[j]
  new_order_Median <- with(Estadisticas, reorder(Franja , Variable, median , na.rm=T,decreasing = TRUE))
  new_order_Mean <- with(Estadisticas, reorder(Franja , Variable, mean , na.rm=T,decreasing = TRUE))
  
  if (NombresVariables[j] %in% c("Balanced Accuracy Cluster 1","Balanced Accuracy Cluster 2","Balanced Accuracy Cluster 3","Balanced Accuracy Cluster 4")){
    limitesY = c(0.5,1)
  } else if(NombresVariables[j] %in% c("Accuracy")){
    limitesY = c(0.25,0.75)
  } else{
    limitesY = c(0,1)
  }
  
  if(NombreVariable %in% c("F1 Cluster 1","F1 Cluster 2","F1 Cluster 3","F1 Cluster 4")){
    NombreVariableY="F1"
  }else{
    NombreVariableY=NombreVariable
    }
  
  
  #as.numeric(levels(new_order_Mean))
  #Color[as.numeric(levels(new_order_Mean))+1]
  ColorMedio = Color[as.numeric(levels(new_order_Mean))+1]
  ColorMediano = Color[as.numeric(levels(new_order_Median))+1]
  
  #main=paste0(NombreVariable," by Time Slot")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," ",NombreColores,".png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~Franja,data= Estadisticas,xlab = "Hourly Time Slot",ylab=NombreVariableY,col=Color, ylim = limitesY,cex.lab=2,cex.axis=1.3)
  dev.off()
  
  
  #main=paste0(NombreVariable," by Time Slot ordered by ",NombreVariable,"'s Mean")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Media ",NombreColores,".png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Mean,data= Estadisticas,xlab = "Hourly Time Slot",ylab=NombreVariableY,col=ColorMedio, ylim = limitesY,cex.lab=2,cex.axis=1.3)
  dev.off()
  
  
  #paste0(NombreVariable," by Time Slot ordered by ",NombreVariable,"'s Median")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Mediana ",NombreColores,".png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Median,data= Estadisticas,xlab = "Hourly Time Slot",ylab=NombreVariableY,col=ColorMediano, ylim = limitesY,cex.lab=2,cex.axis=1.3)
  dev.off()
}

## -------------------------------------------------------------------------

##### 8. Bloque de Análisis de Resultados: Agregados por Cluster #####

NombreVariable = "Balanced Accuracy"
OrdenVariablesClusters=c(24,25,26,27)

if(exists("DataFrame")){rm(DataFrame) }

for (k in 1:4){
  datos= data.frame(Cluster = k, Variable=Estadisticas[,OrdenVariablesClusters[k]])
  if(exists("DataFrame")){
    DataFrame = rbind(DataFrame,datos)
  } else{
    DataFrame = datos
  }
  
}
#main=paste0(NombreVariable," by Behavior Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Behavior Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0.5,1),cex.lab=2,cex.axis=1.3)
dev.off()





NombreVariable = "F1"
OrdenVariablesClusters=c(36,37,38,39)

if(exists("DataFrame")){rm(DataFrame) }

for (k in 1:4){
  datos= data.frame(Cluster = k, Variable=Estadisticas[,OrdenVariablesClusters[k]])
  if(exists("DataFrame")){
    DataFrame = rbind(DataFrame,datos)
  } else{
    DataFrame = datos
  }
  
}


#main=paste0(NombreVariable," by Behavior Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Behavior Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
dev.off()






NombreVariable = "Precision"
OrdenVariablesClusters=c(28,29,30,31)

if(exists("DataFrame")){rm(DataFrame) }

for (k in 1:4){
  datos= data.frame(Cluster = k, Variable=Estadisticas[,OrdenVariablesClusters[k]])
  if(exists("DataFrame")){
    DataFrame = rbind(DataFrame,datos)
  } else{
    DataFrame = datos
  }
  
}

#main=paste0(NombreVariable," by Behavior Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Behavior Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
dev.off()





NombreVariable = "Sensitivity "
OrdenVariablesClusters=c(32,33,34,35)

if(exists("DataFrame")){rm(DataFrame) }

for (k in 1:4){
  datos= data.frame(Cluster = k, Variable=Estadisticas[,OrdenVariablesClusters[k]])
  if(exists("DataFrame")){
    DataFrame = rbind(DataFrame,datos)
  } else{
    DataFrame = datos
  }
  
}

#main=paste0(NombreVariable," by Behavior Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Behavior Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
dev.off()





NombreVariable = "Specificity"
OrdenVariablesClusters=c(40,41,42,43)

if(exists("DataFrame")){rm(DataFrame) }

for (k in 1:4){
  datos= data.frame(Cluster = k, Variable=Estadisticas[,OrdenVariablesClusters[k]])
  if(exists("DataFrame")){
    DataFrame = rbind(DataFrame,datos)
  } else{
    DataFrame = datos
  }
  
}

#main=paste0(NombreVariable," by Behavior Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Behavior Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
dev.off()

## -------------------------------------------------------------------------

##### 9. Bloque de Análisis de Resultados: Agregados por Modelo #####

Estadisticas$Modelo = as.factor(Estadisticas$Modelo)

levels(Estadisticas$Modelo)
Colores=c("darkblue","blue","lightblue","blue","lightblue","darkblue","blue","lightblue")
names(Colores)=levels(Estadisticas$Modelo)

layers=c(3,2,1,2,1,3,2,1)
names(layers)=levels(Estadisticas$Modelo)
Estadisticas$Layers =as.factor(layers[Estadisticas$Modelo])

paletaAzules=c("lightblue","blue","darkblue")
ColorPuntoMedio="red"

for (j in (4:length(NombresVariables))){
  Variable = Estadisticas[,j]
  NombreVariable = NombresVariables[j]
  new_order_Median <- with(Estadisticas, reorder(Modelo , Variable, median , na.rm=T,decreasing = TRUE))
  new_order_Mean <- with(Estadisticas, reorder(Modelo , Variable, mean , na.rm=T,decreasing = TRUE))
  
  ColorMedio = Colores[levels(new_order_Mean)]
  olorMediano = Colores[levels(new_order_Median)]
  
  if (NombresVariables[j] %in% c("Balanced Accuracy Cluster 1","Balanced Accuracy Cluster 2","Balanced Accuracy Cluster 3","Balanced Accuracy Cluster 4")){
    limitesY = c(0.5,1)
  } else if(NombresVariables[j] %in% c("Accuracy")){
    limitesY = c(0.25,0.75)
  } else{
    limitesY = c(0,1)
  }
  
  
  #main=paste0(NombreVariable," by ANN's Architevcture")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," ",NombreColores," Modelos.png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~Modelo,data= Estadisticas,xlab = "ANN's Architecture",ylab=NombreVariable,ylim=limitesY, col=Colores,cex.lab=2,cex.axis=1.3,las =1)
  dev.off()
  
  #main=paste0(NombreVariable," by ANN's Architevcture ordered by ",NombreVariable,"'s Mean")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Media ",NombreColores," Modelos.png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Mean,data= Estadisticas,xlab = "ANN's Architecture",ylab=NombreVariable,ylim=limitesY, col=ColorMedio,cex.lab=2,cex.axis=1.3,las =1)
  dev.off()
  
  #main=paste0(NombreVariable," by ANN's Architevcture ordered by ",NombreVariable,"'s Median")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Mediana ",NombreColores," Modelos.png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Median,data= Estadisticas,xlab = "ANN's Architecture",ylab=NombreVariable,ylim=limitesY,col=ColorMediano,cex.lab=2,cex.axis=1.3,las =1)
  dev.off()
  
  qplot(Variable,Modelo, data=Estadisticas ,geom = "boxplot",fill=Layers) + coord_flip() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1,size =16,face="bold"),axis.text.y = element_text(vjust = 0.5, hjust=1,size =16,face="bold"),axis.title=element_text(size=20,face="bold"),legend.title=element_text(size=16),legend.text=element_text(size=16),legend.key.size = unit(2, "cm")) + scale_x_continuous(name=NombreVariable, limits=limitesY) + scale_y_discrete(name="")+ scale_fill_manual(values=paletaAzules)+stat_summary(fun=mean, geom="point", shape=19, size=5, color=ColorPuntoMedio)+ guides(fill=guide_legend(title="Hidden Layers"))
  ggsave(paste0(CarpetaOutput,"/Boxplot2 ",NombreVariable," ",NombreColores," Modelos.png"),width = 16, height = 9, dpi = 100)
  
  qplot(Variable,new_order_Mean,data=Estadisticas, geom = "boxplot",fill=Layers) + coord_flip() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1,size =16,face="bold"),axis.text.y = element_text(vjust = 0.5, hjust=1,size =16,face="bold"),axis.title=element_text(size=20,face="bold"),legend.title=element_text(size=16),legend.text=element_text(size=16),legend.key.size = unit(2, "cm")) + scale_x_continuous(name=NombreVariable, limits=limitesY) + scale_y_discrete(name="")+ scale_fill_manual(values=paletaAzules)+stat_summary(fun=mean, geom="point", shape=19, size=5, color=ColorPuntoMedio)+ guides(fill=guide_legend(title="Hidden Layers"))
  ggsave(paste0(CarpetaOutput,"/Boxplot2 ",NombreVariable," Media ",NombreColores," Modelos.png"),width = 16, height = 9, dpi = 100)
  
  qplot(Variable,new_order_Median,data=Estadisticas, geom = "boxplot",fill=Layers) + coord_flip() + theme(axis.text.x = element_text(angle = 15, vjust = 0.5, hjust=1,size =16,face="bold"),axis.text.y = element_text(vjust = 0.5, hjust=1,size =16,face="bold"),axis.title=element_text(size=20,face="bold"),legend.title=element_text(size=16),legend.text=element_text(size=16),legend.key.size = unit(2, "cm")) + scale_x_continuous(name=NombreVariable, limits=limitesY) + scale_y_discrete(name="")+ scale_fill_manual(values=paletaAzules)+stat_summary(fun=mean, geom="point", shape=19, size=5, color=ColorPuntoMedio)+ guides(fill=guide_legend(title="Hidden Layers"))
  ggsave(paste0(CarpetaOutput,"/Boxplot2 ",NombreVariable," Mediana ",NombreColores," Modelos.png"),width = 16, height = 9, dpi = 100)
}


## -------------------------------------------------------------------------

##### 9. Bloque de Análisis de Resultados: Nubes de Puntos #####

png(paste0(CarpetaOutput,"/ScatterPlor Accuracy Kappa.png"),width = 1024, height = 512)
plot(Estadisticas$Accuracy, Estadisticas$Kappa,main="Dos dimensiones",xlab = "Accuracy",ylab="Kappa",col=as.factor(Estadisticas$Modelo),pch=20,cex.lab=2,cex.axis=1.3)
dev.off()

## -------------------------------------------------------------------------