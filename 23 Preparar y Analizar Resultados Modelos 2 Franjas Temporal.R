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

CarpetaInput = "Modelos Supervisados Cruzados Temporales"
CarpetaOutput = "Resultados Agregados Redes Neuronales 2 Franjas Temporales Green"

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
                          Franja1 = numeric(),
                          Franja2 = numeric(),
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
  Estadisticas[i,2] = paste(terminos[7:(length(terminos)-1)],collapse="_")
  Estadisticas[i,3] = as.numeric(terminos[3])
  Estadisticas[i,4] = as.numeric(terminos[6])
  
  load(paste0(CarpetaInput,"/",fichero))
  
  #cfm
  
  #cfm$overall
  Estadisticas[i,5] = as.numeric(cfm$overall[1]) #Accuracy
  Estadisticas[i,6] = as.numeric(cfm$overall[2]) #Kappa
  Estadisticas[i,7] = as.numeric(cfm$overall[3]) #AccuracyLower
  Estadisticas[i,8] = as.numeric(cfm$overall[4]) #AccuracyUpper
  
  #cfm$table
  #PiRj Prediction I Real j
  Estadisticas[i,9] = cfm$table[1,1]
  Estadisticas[i,10] = cfm$table[1,2]
  Estadisticas[i,11] = cfm$table[1,3]
  Estadisticas[i,12] = cfm$table[1,4]
  Estadisticas[i,13] = cfm$table[2,1]
  Estadisticas[i,14] = cfm$table[2,2]
  Estadisticas[i,15] = cfm$table[2,3]
  Estadisticas[i,16] = cfm$table[2,4]
  Estadisticas[i,17] = cfm$table[3,1]
  Estadisticas[i,18] = cfm$table[3,2]
  Estadisticas[i,19] = cfm$table[3,3]
  Estadisticas[i,20] = cfm$table[3,4]
  Estadisticas[i,21] = cfm$table[4,1]
  Estadisticas[i,22] = cfm$table[4,2]
  Estadisticas[i,23] = cfm$table[4,3]
  Estadisticas[i,24] = cfm$table[4,4]
  
  #cfm$byClass
  #BalAccuracy
  Estadisticas[i,25] = cfm$byClass[1,11]
  Estadisticas[i,26] = cfm$byClass[2,11]
  Estadisticas[i,27] = cfm$byClass[3,11]
  Estadisticas[i,28] = cfm$byClass[4,11]
  #Precision
  Estadisticas[i,29] = cfm$byClass[1,5]
  Estadisticas[i,30] = cfm$byClass[2,5]
  Estadisticas[i,31] = cfm$byClass[3,5]
  Estadisticas[i,32] = cfm$byClass[4,5]
  #Recall
  Estadisticas[i,33] = cfm$byClass[1,6]
  Estadisticas[i,34] = cfm$byClass[2,6]
  Estadisticas[i,35] = cfm$byClass[3,6]
  Estadisticas[i,36] = cfm$byClass[4,6]
  #F1
  Estadisticas[i,37] = cfm$byClass[1,7]
  Estadisticas[i,38] = cfm$byClass[2,7]
  Estadisticas[i,39] = cfm$byClass[3,7]
  Estadisticas[i,40] = cfm$byClass[4,7]
  #Specificity 
  Estadisticas[i,41] = cfm$byClass[1,2]
  Estadisticas[i,42] = cfm$byClass[2,2]
  Estadisticas[i,43] = cfm$byClass[3,2]
  Estadisticas[i,44] = cfm$byClass[4,2]
}



## -------------------------------------------------------------------------

##### 7. Bloque de Filtrado de Modelos Duplicados  #####


Estadisticas = Estadisticas[!duplicated(Estadisticas[,c(2,3,4)]),]

## -------------------------------------------------------------------------

##### 7. Bloque de Guardado de Resultados  #####

write.csv2(Estadisticas,paste0(CarpetaOutput,"/",Fecha,"_Resultados.csv"),row.names = FALSE)

Estadisticas$N1=Estadisticas$P1R1+Estadisticas$P2R1+Estadisticas$P3R1+Estadisticas$P4R1
Estadisticas$N2=Estadisticas$P1R2+Estadisticas$P2R2+Estadisticas$P3R2+Estadisticas$P4R2
Estadisticas$N3=Estadisticas$P1R3+Estadisticas$P2R3+Estadisticas$P3R3+Estadisticas$P4R3
Estadisticas$N4=Estadisticas$P1R4+Estadisticas$P2R4+Estadisticas$P3R4+Estadisticas$P4R4
Estadisticas$N =Estadisticas$N1+Estadisticas$N2+Estadisticas$N3+Estadisticas$N4
  
Estadisticas$PrecisionMacro = (Estadisticas$PrecisionC1+Estadisticas$PrecisionC2+Estadisticas$PrecisionC3+Estadisticas$PrecisionC4)/4
Estadisticas$PrecisionMicro = (Estadisticas$N1*Estadisticas$PrecisionC1+Estadisticas$N2*Estadisticas$PrecisionC2+Estadisticas$N3*Estadisticas$PrecisionC3+Estadisticas$N4*Estadisticas$PrecisionC4)/(Estadisticas$N)
Estadisticas$RecallMacro = (Estadisticas$RecallC1+Estadisticas$RecallC2+Estadisticas$RecallC3+Estadisticas$RecallC4)/4
Estadisticas$RecallMicro = (Estadisticas$N1*Estadisticas$RecallC1+Estadisticas$N2*Estadisticas$RecallC2+Estadisticas$N3*Estadisticas$RecallC3+Estadisticas$N4*Estadisticas$RecallC4)/(Estadisticas$N)
Estadisticas$F1Macro = (Estadisticas$F1C1+Estadisticas$F1C2+Estadisticas$F1C3+Estadisticas$F1C4)/4
Estadisticas$F1Micro = (Estadisticas$N1*Estadisticas$F1C1+Estadisticas$N2*Estadisticas$F1C2+Estadisticas$N3*Estadisticas$F1C3+Estadisticas$N4*Estadisticas$F1C4)/(Estadisticas$N)
Estadisticas$SpecificityMacro = (Estadisticas$SpecificityC1+Estadisticas$SpecificityC2+Estadisticas$SpecificityC3+Estadisticas$SpecificityC4)/4
Estadisticas$SpecificityMicro = (Estadisticas$N1*Estadisticas$SpecificityC1+Estadisticas$N2*Estadisticas$SpecificityC2+Estadisticas$N3*Estadisticas$SpecificityC3+Estadisticas$N4*Estadisticas$SpecificityC4)/(Estadisticas$N)
Estadisticas$BalAccuracyMacro = (Estadisticas$BalAccuracyC1+Estadisticas$BalAccuracyC2+Estadisticas$BalAccuracyC3+Estadisticas$BalAccuracyC4)/4
Estadisticas$BalAccuracyMicro = (Estadisticas$N1*Estadisticas$BalAccuracyC1+Estadisticas$N2*Estadisticas$BalAccuracyC2+Estadisticas$N3*Estadisticas$BalAccuracyC3+Estadisticas$N4*Estadisticas$BalAccuracyC4)/(Estadisticas$N)



write.csv2(Estadisticas,paste0(CarpetaOutput,"/",Fecha,"_Resultados_Global.csv"),row.names = FALSE)

## -------------------------------------------------------------------------

##### 8. Bloque de Analisis de Resultados: General  #####

summary(Estadisticas)
Estadisticas$Modelo = gsub("Red","Net",Estadisticas$Modelo)

## -------------------------------------------------------------------------

##### 8. Bloque de color de Franjas #####

#Estadisticas$ColorDEN= ifelse(Estadisticas$Franja<7,"Blue",ifelse(Estadisticas$Franja<19,"Red",ifelse(Estadisticas$Franja<23,"Green","Blue")))

#table(Estadisticas$ColorDEN, Estadisticas$Franja)

ColoresDEN=c(rep("grey",7),rep("yellow",12),rep("orange",4),"grey")
ColoresArticulo = c("black")

Estadisticas$Franja=paste0(Estadisticas$Franja1,"_",Estadisticas$Franja2)

## -------------------------------------------------------------------------

##### 8. Bloque de Análisis de Resultados:Agregados por Horas #####

NombresVariables=c("Fecha","Modelo","Franja","Accuracy","Kappa","Accuracy Lower","Accuracy Upper","P1R1","P1R2","P1R3","P1R4","P2R1","P2R2","P2R3","P2R4","P3R1","P3R2","P3R3","P3R4","P4R1","P4R2","P4R3","P4R4","Balanced Accuracy Cluster 1","Balanced Accuracy Cluster 2","Balanced Accuracy Cluster 3","Balanced Accuracy Cluster 4","Precision Cluster 1","Precision Cluster 2","Precision Cluster 3","Precision Cluster 4","Sensitivity Cluster 1","Sensitivity Cluster 2","Sensitivity Cluster 3","Sensitivity Cluster 4","F1 Cluster 1","F1 Cluster 2","F1 Cluster 3","F1 Cluster 4","Specificity  Cluster 1","Specificity  Cluster 2","Specificity  Cluster 3","Specificity  Cluster 4")
NombreColores = "DEN"
Color = ColoresDEN

dev.off()

for (j in (4:length(NombresVariables))){
  Variable = Estadisticas[,j]
  NombreVariable = NombresVariables[j]
  new_order_Median <- with(Estadisticas, reorder(Franja , Variable, median , na.rm=T,decreasing = TRUE))
  new_order_Mean <- with(Estadisticas, reorder(Franja , Variable, mean , na.rm=T,decreasing = TRUE))
  
  if (NombresVariables[j] %in% c("Balanced Accuracy Cluster 1","Balanced Accuracy Cluster 2","Balanced Accuracy Cluster 3","Balanced Accuracy Cluster 4")){
    limitesY = c(0,1)
  } else if(NombresVariables[j] %in% c("Accuracy")){
    limitesY = c(0,1)
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
  legend("bottomleft", inset=.02, title="Periods", c("daytime","evening","night"), fill=c("yellow","orange","grey"), horiz=TRUE, cex=0.8)
  dev.off()
  
  
  #main=paste0(NombreVariable," by Time Slot ordered by ",NombreVariable,"'s Mean")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Media ",NombreColores,".png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Mean,data= Estadisticas,xlab = "Hourly Time Slot",ylab=NombreVariableY,col=ColorMedio, ylim = limitesY,cex.lab=2,cex.axis=1.3)
  legend("bottomleft", inset=.02, title="Periods", c("daytime","evening","night"), fill=c("yellow","orange","grey"), horiz=TRUE, cex=0.8)
  dev.off()
  
  
  #paste0(NombreVariable," by Time Slot ordered by ",NombreVariable,"'s Median")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Mediana ",NombreColores,".png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Median,data= Estadisticas,xlab = "Hourly Time Slot",ylab=NombreVariableY,col=ColorMediano, ylim = limitesY,cex.lab=2,cex.axis=1.3)
  legend("bottomleft", inset=.02, title="Periods", c("daytime","evening","night"), fill=c("yellow","orange","grey"), horiz=TRUE, cex=0.8)
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
#main=paste0(NombreVariable," by Pattern Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Pattern Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
dev.off()

aggregate(.~Cluster,data=DataFrame, mean)
aggregate(.~Cluster,data=DataFrame, median)



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

aggregate(.~Cluster,data=DataFrame, mean)
aggregate(.~Cluster,data=DataFrame, median)


#main=paste0(NombreVariable," by Pattern Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Pattern Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
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

#main=paste0(NombreVariable," by Pattern Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Pattern Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
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

#main=paste0(NombreVariable," by Pattern Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Pattern Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
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

#main=paste0(NombreVariable," by Pattern Category")
png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Clusters.png"),width = 512, height = 256)
par(mar=c(5.1,6.1,4.1,2.1))
boxplot(Variable~Cluster,data= DataFrame,xlab = "Pattern Category",ylab=NombreVariable,col=c("black","magenta","cyan","brown"),ylim=c(0,1),cex.lab=2,cex.axis=1.3)
dev.off()

## -------------------------------------------------------------------------

##### 9. Bloque de Análisis de Resultados: Agregados por Modelo #####

Estadisticas$Modelo = as.factor(Estadisticas$Modelo)

levels(Estadisticas$Modelo)
Colores=c("darkblue","blue","lightblue","darkblue","darkblue","darkblue","blue","lightblue","darkblue","blue","lightblue")
names(Colores)=levels(Estadisticas$Modelo)

layers=c(3,2,1,3,3,3,2,1,3,2,1)
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
    limitesY = c(0,1)
  } else if(NombresVariables[j] %in% c("Accuracy")){
    limitesY = c(0,1)
  } else{
    limitesY = c(0,1)
  }
  
  
  #main=paste0(NombreVariable," by ANN's Architevcture")
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," ",NombreColores," Modelos.png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~Modelo,data= Estadisticas,xlab = "ANN's Architecture",ylab=NombreVariable,ylim=limitesY, col=Colores,cex.lab=2,cex.axis=1.3,las =1)
  dev.off()
  
  #main=paste0(NombreVariable," by ANN's Architevcture ordered by ",NombreVariable,"'s Mean")
  means <- tapply(Variable, new_order_Mean, mean)
  png(paste0(CarpetaOutput,"/Boxplot ",NombreVariable," Media ",NombreColores," Modelos.png"),width = 1024, height = 512)
  par(mar=c(5.1,6.1,4.1,2.1))
  boxplot(Variable~new_order_Mean,data= Estadisticas ,xlab = "",ylab=NombreVariable,ylim=limitesY, col=ColorMedio,cex.lab=2,cex.axis=1.3,las =1, xaxt = "n" )
  points(means, pch=19, cex=2, col=ColorPuntoMedio)
  axis(side = 1, labels = FALSE, at=1:length(levels(new_order_Mean)))
  
  text(x = 1:length(levels(new_order_Mean)),
       ## Move labels to just below bottom of chart.
       #y = par("usr")[3] - 0.45,
       y = par("usr")[3]-0.025,
       ## Use names from the data list.
       labels = levels(new_order_Mean),
       ## Change the clipping region.
       xpd = NA,
       ## Rotate the labels by 35 degrees.
       srt = 35,
       ## Adjust the labels to almost 100% right-justified.
       adj = 0.965,
       ## Increase label size.
       cex = 1.2)
  legend("bottomleft", legend=c(1,2,3),title="Hidden Layers",
         fill=paletaAzules, lty=0, cex=0.8,title.cex = 1.5)
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

##### 10. Bloque de Análisis de Resultados: Nubes de Puntos #####

png(paste0(CarpetaOutput,"/ScatterPlor Accuracy Kappa.png"),width = 1024, height = 512)
plot(Estadisticas$Accuracy, Estadisticas$Kappa,main="Dos dimensiones",xlab = "Accuracy",ylab="Kappa",col=as.factor(Estadisticas$Modelo),pch=20,cex.lab=2,cex.axis=1.3)
dev.off()














## -------------------------------------------------------------------------

##### 11. Bloque de Mapas de Calor por Modelos #####
table(Estadisticas$Modelo)
Modelos = unique(Estadisticas$Modelo)


Metricas =c(mean,median,max,min,sd)

NombresMetricas = c("Average","Median","Maximum","Minimum","Std Deviation")
#NombresMetricas =c("Maximum")
for (k in 1:length(Modelos)){
  Datos=Estadisticas[Estadisticas$Modelo == Modelos[k],]
  for (i in 1:length(Metricas)){
    Metrica = Metricas[i][[1]]
    NomMetrica = NombresMetricas[i]
    for (j in c(5:44,50:59)){
      Datos$Variable = Datos[,j]
      NomVariable = colnames(Datos)[j]
      
      # Calcular la media de Z para cada combinación de X e Y
      df_agg <- aggregate(Variable ~ Franja1 + Franja2, data = Datos, FUN = Metrica)
      
      df_agg_sim = df_agg[,c(2,1,3)]
      colnames(df_agg_sim)=colnames(df_agg)
      
      df_agg_total= rbind(df_agg,df_agg_sim)
      
      # Crear la visualización con ggplot2
      #png(paste0(CarpetaOutput,"/Heatmap ",NomVariable," ",NomMetrica ,".png"),width = 1024, height = 512)
      ggplot(df_agg_total, aes(x = Franja1, y = Franja2, fill = Variable)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "blue") +
        geom_text(aes(label = round(Variable, 2)), color = "black") + 
        labs(title = paste0(NomMetrica," of ",NomVariable," for Time Hourly Time Slot"),
             x = "Hourly Time Slot",
             y = "Hourly Time Slot",
             fill = paste0(NomMetrica," de ",NomVariable)) +
        theme_minimal()
      #dev.off()
      ggsave(paste0(CarpetaOutput,"/Heatmap ",Modelos[k]," ",NomVariable," ",NomMetrica ,".png"),width = 16, height = 9, dpi = 100)
      
    }
    
  } 
}




## -------------------------------------------------------------------------

##### 12. Bloque de Mapas de Calor #####


#dev.off()

#Metricas =c(mean,median,max,min,sd)
#NombresMetricas = c("Average","Median","Maximum","Minimum","Std Deviation")
  
Metricas =c(max)
NombresMetricas = c("Maximum")

for (i in 1:length(Metricas)){
  Metrica = Metricas[i][[1]]
  print(Metrica)
  NomMetrica = NombresMetricas[i]
  #for (j in c(5:44,50:59)){
  for (j in c(59)){
    Estadisticas$Variable = Estadisticas[,j]
    NomVariable = colnames(Estadisticas)[j]
    print(NomVariable)
    # Calcular la media de Z para cada combinación de X e Y
    df_agg <- aggregate(Variable ~ Franja1 + Franja2, data = Estadisticas, FUN = Metrica)
    
    df_agg_sim = df_agg[,c(2,1,3)]
    colnames(df_agg_sim)=colnames(df_agg)
    
    df_agg_total= rbind(df_agg,df_agg_sim)
    
    # Crear la visualización con ggplot2
    #png(paste0(CarpetaOutput,"/Heatmap ",NomVariable," ",NomMetrica ,".png"),width = 1024, height = 512)
    p<-ggplot(df_agg_total, aes(x = Franja1, y = Franja2, fill = Variable)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "brown") +
      geom_text(aes(label = round(Variable, 2)), color = "black", size = 2.5) + 
      labs(title = "",
           x = "Hourly Time Slot",
           y = "Hourly Time Slot",
           fill = "Performance") +
           #fill = paste0(NomMetrica," de ",NomVariable)) +
      #labs(title = paste0(NomMetrica," of ",NomVariable," for Time Hourly Time Slot"),
      #     x = "Hourly Time Slot",
      #     y = "Hourly Time Slot",
      #     fill = paste0(NomMetrica," de ",NomVariable)) +
      theme_minimal(base_size = 16) +   # tamaño base de fuente más grande
      theme(
        axis.title = element_text(size = 18, face = "bold"),   # títulos de ejes
        axis.text = element_text(size = 14),                   # etiquetas de ejes
        legend.title = element_text(size = 16, face = "bold"), # título de leyenda
        legend.text = element_text(size = 14),                 # texto de leyenda
        plot.title = element_text(size = 18, hjust = 0.5)      # título centrado
      )
    #dev.off()
    #ggsave(paste0(CarpetaOutput,"/Heatmap ",NomVariable," ",NomMetrica ,".png"),width = 16, height = 9, dpi = 100)
    ggsave(
      filename = paste0(CarpetaOutput, "/Heatmap_", NomVariable, "_", NomMetrica, ".png"),
      plot = p,
      width = 8,       # ~20 cm de ancho
      height = 6,      # ~15 cm de alto
      dpi = 300        # resolución alta
    )
    
    # Exporta también en PDF vectorial (ideal para revistas)
    ggsave(
      filename = paste0(CarpetaOutput, "/Heatmap_", NomVariable, "_", NomMetrica, ".pdf"),
      plot = p,
      width = 8,
      height = 6
    )
  }
  
}





## -------------------------------------------------------------------------

##### 12. Bloque deMapas de Calor #####

max_accuracy <- Estadisticas %>%
  group_by(Franja1, Franja2) %>%
  filter(Accuracy == max(Accuracy)) %>%
  ungroup()

max_accuracy_select=max_accuracy[,c(3,4,5,2)]
max_accuracy_sim = max_accuracy_select[,c(2,1,3,4)]
colnames(max_accuracy_sim)=colnames(max_accuracy_select)

max_accuracy_total= rbind(max_accuracy_select,max_accuracy_sim)

max_accuracy_total

ggplot(max_accuracy_total, aes(x = Franja1, y = Franja2, fill = Modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "#1f77b4", "Net_16_16_4" = "#ff7f0e", 
                               "Net_16_4" = "#2ca02c", "Net_32_32_4" = "#d62728", 
                               "Net_32_4" = "#9467bd", "Net_64_32_16_4" = "#8c564b", 
                               "Net_64_32_4" = "#e377c2", "Net_64_4" = "#7f7f7f",
                               "Net_32_16_8_4" = "#e300c2", "Net_32_32_16_4" = "#007f7f",
                               "Net_32_32_32_4" ="#ab7634")) +
  labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
       x = "Franja 1", y = "Franja 2") +
  theme_minimal()
ggsave(paste0(CarpetaOutput,"/Heatmap2 Modelos.png"),width = 16, height = 9, dpi = 100)


table(max_accuracy_total$Modelo)



## -------------------------------------------------------------------------

##### 13. Bloque de Mapas de Calor #####

modelosSelec= c("Net_16_16_16_4", "Net_16_16_4", "Net_16_4", "Net_32_32_4", "Net_32_4", 
           "Net_64_32_16_4", "Net_64_32_4", "Net_64_4", "Net_32_16_8_4", "Net_32_32_16_4",
           "Net_32_32_32_4")

modelosSelec=c("Net_16_16_4", "Net_16_4", "Net_32_32_4", "Net_32_4", 
              "Net_64_32_4", "Net_64_4")

Estadisticas$Modelo %in% modelosSelec

max_accuracy <- Estadisticas[Estadisticas$Modelo %in% modelosSelec,] %>%
  group_by(Franja1, Franja2) %>%
  filter(Accuracy == max(Accuracy)) %>%
  ungroup()

max_accuracy_select=max_accuracy[,c(3,4,5,2)]
max_accuracy_sim = max_accuracy_select[,c(2,1,3,4)]
colnames(max_accuracy_sim)=colnames(max_accuracy_select)

max_accuracy_total= rbind(max_accuracy_select,max_accuracy_sim)

max_accuracy_total

ggplot(max_accuracy_total, aes(x = Franja1, y = Franja2, fill = Modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "darkblue", "Net_16_16_4" = "blue", 
                               "Net_16_4" = "lightblue", "Net_32_32_4" = "blue", 
                               "Net_32_4" = "lightblue", "Net_64_32_16_4" = "darkblue", 
                               "Net_64_32_4" = "blue", "Net_64_4" = "lightblue",
                               "Net_32_16_8_4" = "darkblue", "Net_32_32_16_4" = "darkblue",
                               "Net_32_32_32_4" ="darkblue")) +
  geom_text(aes(label = round(Accuracy, 2)), color = "white") + 
  labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
       x = "Franja 1", y = "Franja 2") +
  theme_minimal()
ggsave(paste0(CarpetaOutput,"/Heatmap2 Capas.png"),width = 16, height = 9, dpi = 100)

ggplot(max_accuracy_total, aes(x = Franja1, y = Franja2, fill = Modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "#1f77b4", "Net_16_16_4" = "#ff7f0e", 
                               "Net_16_4" = "#2ca02c", "Net_32_32_4" = "#d62728", 
                               "Net_32_4" = "#9467bd", "Net_64_32_16_4" = "#8c564b", 
                               "Net_64_32_4" = "#e377c2", "Net_64_4" = "#7f7f7f",
                               "Net_32_16_8_4" = "#e300c2", "Net_32_32_16_4" = "#007f7f",
                               "Net_32_32_32_4" ="#ab7634")) +
  labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
       x = "Franja 1", y = "Franja 2") +
  theme_minimal()
ggsave(paste0(CarpetaOutput,"/Heatmap2 Modelos2.png"),width = 16, height = 9, dpi = 100)


table(max_accuracy_total$Modelo)


modelosSelec=c("Net_16_16_4", "Net_16_4", "Net_32_32_4", "Net_32_4", 
               "Net_64_32_4", "Net_64_4", "Net_32_32_32_4","Net_64_32_16_4","Net_16_16_16_4")

Estadisticas$Modelo %in% modelosSelec

max_accuracy <- Estadisticas[Estadisticas$Modelo %in% modelosSelec,] %>%
  group_by(Franja1, Franja2) %>%
  filter(Accuracy == max(Accuracy)) %>%
  ungroup()

max_accuracy$Modelo=as.factor(as.character(max_accuracy$Modelo) )

max_accuracy_select=max_accuracy[,c(3,4,5,2)]
max_accuracy_sim = max_accuracy_select[,c(2,1,3,4)]
colnames(max_accuracy_sim)=colnames(max_accuracy_select)

max_accuracy_total= rbind(max_accuracy_select,max_accuracy_sim)

max_accuracy_total

ggplot(max_accuracy_total, aes(x = Franja1, y = Franja2, fill = Modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "darkblue", "Net_16_16_4" = "blue", 
                               "Net_16_4" = "lightblue", "Net_32_32_4" = "blue", 
                               "Net_32_4" = "lightblue", "Net_64_32_16_4" = "darkblue", 
                               "Net_64_32_4" = "blue", "Net_64_4" = "lightblue",
                               "Net_32_16_8_4" = "darkblue", "Net_32_32_16_4" = "darkblue",
                               "Net_32_32_32_4" ="darkblue")) +
  labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
       x = "Franja 1", y = "Franja 2") +
  theme_minimal()
ggsave(paste0(CarpetaOutput,"/Heatmap2 Capas Definitivo.png"),width = 16, height = 9, dpi = 100)

ggplot(max_accuracy_total, aes(x = Franja1, y = Franja2, fill = Modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "#1f77b4", "Net_16_16_4" = "#ff7f0e", 
                               "Net_16_4" = "#2ca02c", "Net_32_32_4" = "#d62728", 
                               "Net_32_4" = "#9467bd", "Net_64_32_16_4" = "#8c564b", 
                               "Net_64_32_4" = "#e377c2", "Net_64_4" = "#7f7f7f",
                               "Net_32_16_8_4" = "#e300c2", "Net_32_32_16_4" = "#007f7f",
                               "Net_32_32_32_4" ="#ab7634")) +
  labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
       x = "Franja 1", y = "Franja 2") +
  theme_minimal()
ggsave(paste0(CarpetaOutput,"/Heatmap2 Modelos Definitivo.png"),width = 16, height = 9, dpi = 100)


table(max_accuracy_total$Modelo)
plot(table(max_accuracy_total$Modelo))


dataPlot = as.data.frame(table(max_accuracy_total$Modelo))
colnames(dataPlot)=c("Modelo","Frecuencia")

ggplot(dataPlot, aes(x = reorder(Modelo, -Frecuencia), y = Frecuencia)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frecuencia de aparición de cada modelo en max_accuracy_total",
       x = "Modelo",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0(CarpetaOutput,"/Histograma 1.png"),width = 16, height = 9, dpi = 100)

# Ordenar el dataframe por Frecuencia en orden descendente
dataPlot <- dataPlot[order(-dataPlot$Frecuencia), ]


values <- c(
  "Net_16_16_16_4" = "darkblue", 
  "Net_16_16_4" = "blue", 
  "Net_16_4" = "lightblue", 
  "Net_32_32_4" = "blue", 
  "Net_32_4" = "lightblue", 
  "Net_64_32_16_4" = "darkblue", 
  "Net_64_32_4" = "blue", 
  "Net_64_4" = "lightblue", 
  "Net_32_16_8_4" = "darkblue", 
  "Net_32_32_16_4" = "darkblue", 
  "Net_32_32_32_4" = "darkblue"
)

# Asignar los colores a cada fila del dataframe
dataPlot$Color <- values[dataPlot$Model]


# Crear el gráfico de barras con los valores numéricos en las barras
ggplot(dataPlot, aes(x = reorder(Modelo, -Frecuencia), y = Frecuencia,fill = Modelo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frecuencia), vjust = -0.5) +  # Añadir etiquetas encima de las barras
  scale_fill_manual(values = values) + 
  labs(
       x = "Model",
       y = "Number") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  coord_cartesian(clip = "off") 
ggsave(paste0(CarpetaOutput,"/Histograma 2.png"),width = 16, height = 9, dpi = 100)


## -------------------------------------------------------------------------

##### 13. Bloque de Análisis Franja #####


str(max_accuracy_total)

# Agregación por Franja1
agg_franja1 <- max_accuracy_total %>%
  group_by(Franja1) %>%
  summarise(
    Minimo = min(Accuracy),
    Q1 = quantile(Accuracy, 0.25),
    Media = mean(Accuracy),
    Mediana = median(Accuracy),
    Q3 = quantile(Accuracy, 0.75),
    Maximo = max(Accuracy),
    SD = sd(Accuracy)
  )

agg_franja1

# Transformar agg_franja1 a formato largo
agg_franja1_long <- agg_franja1 %>%
  pivot_longer(cols = c(Minimo, Q1, Media, Mediana, Q3, Maximo),
               names_to = "Estadistica",
               values_to = "Valor")

# Crear el gráfico de líneas con colores adecuados para publicación
ggplot(agg_franja1_long, aes(x = Franja1, y = Valor, color = Estadistica, group = Estadistica)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Franja", y = "Valor", title = "Estadísticas de Accuracy por Franja1") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Minimo" = "#1b9e77",
    "Q1" = "#d95f02",
    "Media" = "#7570b3",
    "Mediana" = "#e7298a",
    "Q3" = "#66a61e",
    "Maximo" = "#e6ab02"
  )) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_blank()
  )

agg_franja1_long2 =agg_franja1_long[agg_franja1_long$Estadistica %in% c("Media"),]
ggplot(agg_franja1_long2, aes(x = Franja1, y = Valor, color = Estadistica, group = Estadistica)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Franja", y = "Valor", title = "Estadísticas de Accuracy por Franja1") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Media" = "#7570b3"
  )) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    legend.title = element_blank()
  )


# Agregación por Modelo
agg_modelo <- max_accuracy_total %>%
  group_by(Modelo) %>%
  summarise(
    Minimo = min(Accuracy),
    Q1 = quantile(Accuracy, 0.25),
    Media = mean(Accuracy),
    Mediana = median(Accuracy),
    Q3 = quantile(Accuracy, 0.75),
    Maximo = max(Accuracy),
    SD = sd(Accuracy)
  )

agg_modelo

# Transformar agg_modelo a formato largo
agg_modelo_long <- agg_modelo %>%
  pivot_longer(cols = c(Minimo, Q1, Media, Mediana, Q3, Maximo),
               names_to = "Estadistica",
               values_to = "Valor")

# Crear el gráfico de líneas con colores adecuados para publicación
ggplot(agg_modelo_long, aes(x = Modelo, y = Valor, color = Estadistica, group = Estadistica)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Modelo", y = "Valor", title = "Estadísticas de Accuracy por Modelo") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Minimo" = "#1b9e77",
    "Q1" = "#d95f02",
    "Media" = "#7570b3",
    "Mediana" = "#e7298a",
    "Q3" = "#66a61e",
    "Maximo" = "#e6ab02"
  )) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )

# Convertir Modelo a factor ordenado por complejidad
agg_modelo$Modelo <- factor(agg_modelo$Modelo, levels = c(
  "Net_16_4", "Net_32_4", "Net_64_4",
  "Net_16_16_4", "Net_32_32_4", "Net_64_32_4",
  "Net_16_16_16_4", "Net_64_32_16_4"
))

# Transformar agg_modelo a formato largo
agg_modelo_long <- agg_modelo %>%
  pivot_longer(cols = c(Minimo, Q1, Media, Mediana, Q3, Maximo),
               names_to = "Estadistica",
               values_to = "Valor")

# Crear el gráfico de líneas con colores adecuados para publicación
ggplot(agg_modelo_long, aes(x = Modelo, y = Valor, color = Estadistica, group = Estadistica)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(x = "Modelo (Ordenado por Complejidad)", y = "Valor", title = "Estadísticas de Accuracy por Modelo") +
  theme_minimal() +
  scale_color_manual(values = c(
    "Minimo" = "#1b9e77",
    "Q1" = "#d95f02",
    "Media" = "#7570b3",
    "Mediana" = "#e7298a",
    "Q3" = "#66a61e",
    "Maximo" = "#e6ab02"
  )) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank()
  )
## -------------------------------------------------------------------------

print(max_accuracy_total, n=552)










## -------------------------------------------------------------------------

dev.off()






# Crear el scatterplot con ggplot2
ggplot(Estadisticas, aes(x = PrecisionMicro, y = RecallMicro, color = Modelo)) +
  geom_point( size = 2) +
  scale_color_manual(values = c("Net_16_16_16_4" = "darkblue", "Net_16_16_4" = "blue", 
                               "Net_16_4" = "lightblue", "Net_32_32_4" = "blue", 
                               "Net_32_4" = "lightblue", "Net_64_32_16_4" = "darkblue", 
                               "Net_64_32_4" = "blue", "Net_64_4" = "lightblue",
                               "Net_32_16_8_4" = "darkblue", "Net_32_32_16_4" = "darkblue",
                               "Net_32_32_32_4" ="darkblue")) +
  labs(
    x = "Precision",
    y = "Recall"
  ) +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12)
  )
ggsave(paste0(CarpetaOutput,"/PrecisionRecall_Micro.png"),width = 16, height = 9, dpi = 100)

# Crear el scatterplot con ggplot2
ggplot(Estadisticas, aes(x = PrecisionMacro, y = RecallMacro, color = Modelo)) +
  geom_point( size = 2) +
  scale_color_manual(values = c("Net_16_16_16_4" = "darkblue", "Net_16_16_4" = "blue", 
                                "Net_16_4" = "lightblue", "Net_32_32_4" = "blue", 
                                "Net_32_4" = "lightblue", "Net_64_32_16_4" = "darkblue", 
                                "Net_64_32_4" = "blue", "Net_64_4" = "lightblue",
                                "Net_32_16_8_4" = "darkblue", "Net_32_32_16_4" = "darkblue",
                                "Net_32_32_32_4" ="darkblue")) +
  labs(
    x = "Precision",
    y = "Recall"
  ) +
  theme(
    text = element_text(size = 14),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 12)
  )
ggsave(paste0(CarpetaOutput,"/PrecisionRecall_Macro.png"),width = 16, height = 9, dpi = 100)






## -------------------------------------------------------------------------

##### 16. Bloque de Graficos Artículo Balanced Accuracy #####

modelosSelec=c("Net_16_16_4", "Net_16_4", "Net_32_32_4", "Net_32_4", 
               "Net_64_32_4", "Net_64_4", "Net_32_32_32_4","Net_64_32_16_4","Net_16_16_16_4")

max_accuracy <- Estadisticas[Estadisticas$Modelo %in% modelosSelec,] %>%
  group_by(Franja1, Franja2) %>%
  filter(Accuracy == max(Accuracy)) %>%
  ungroup()

max_accuracy$Modelo=as.factor(as.character(max_accuracy$Modelo) )

max_accuracy_select=max_accuracy[,c(3,4,5,2)]
max_accuracy_sim = max_accuracy_select[,c(2,1,3,4)]
colnames(max_accuracy_sim)=colnames(max_accuracy_select)

max_accuracy_total= rbind(max_accuracy_select,max_accuracy_sim)

max_accuracy_total


max_balaccuracy <- Estadisticas[Estadisticas$Modelo %in% modelosSelec,] %>%
  group_by(Franja1, Franja2) %>%
  filter(BalAccuracyMicro == max(BalAccuracyMicro)) %>%
  ungroup()

max_balaccuracy$Modelo=as.factor(as.character(max_balaccuracy$Modelo) )

max_balaccuracy_select=max_balaccuracy[,c(3,4,58,2)]
max_balaccuracy_sim = max_balaccuracy_select[,c(2,1,3,4)]
colnames(max_balaccuracy_sim)=colnames(max_balaccuracy_select)

max_balaccuracy_total= rbind(max_balaccuracy_select,max_balaccuracy_sim)

max_balaccuracy_total$Model=max_balaccuracy_total$Modelo

max_balaccuracy_total

p<-ggplot(max_balaccuracy_total, aes(x = Franja1, y = Franja2, fill = Model)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "darkblue", "Net_16_16_4" = "blue", 
                               "Net_16_4" = "lightblue", "Net_32_32_4" = "blue", 
                               "Net_32_4" = "lightblue", "Net_64_32_16_4" = "darkblue", 
                               "Net_64_32_4" = "blue", "Net_64_4" = "lightblue",
                               "Net_32_16_8_4" = "darkblue", "Net_32_32_16_4" = "darkblue",
                               "Net_32_32_32_4" ="darkblue")) +
  labs(title = "",
       x = "Hourly Time Slot",
       y = "Hourly Time Slot")+
  #labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
  #     x = "Franja 1", y = "Franja 2") +
  theme_minimal(base_size = 16) +   # tamaño base de fuente
  theme(
    axis.title = element_text(size = 18, face = "bold"),   # títulos de ejes
    axis.text  = element_text(size = 14),                  # etiquetas de ejes
    legend.title = element_text(size = 16, face = "bold"), # título de leyenda
    legend.text  = element_text(size = 14),                # texto de leyenda
    plot.title   = element_text(size = 18, hjust = 0.5)    # título centrado
  )
# PNG alta resolución
ggsave(
  filename = paste0(CarpetaOutput, "/Heatmap2_Capas_Definitivo_BalAccuracyMicro.png"),
  plot = p,
  width = 8,     # ~20 cm
  height = 6,    # ~15 cm
  dpi = 300
)

# PDF vectorial
ggsave(
  filename = paste0(CarpetaOutput, "/Heatmap2_Capas_Definitivo_BalAccuracyMicro.pdf"),
  plot = p,
  width = 8,
  height = 6
)
ggplot(max_balaccuracy_total, aes(x = Franja1, y = Franja2, fill = Modelo)) +
  geom_tile(color = "white") +
  scale_fill_manual(values = c("Net_16_16_16_4" = "#1f77b4", "Net_16_16_4" = "#ff7f0e", 
                               "Net_16_4" = "#2ca02c", "Net_32_32_4" = "#d62728", 
                               "Net_32_4" = "#9467bd", "Net_64_32_16_4" = "#8c564b", 
                               "Net_64_32_4" = "#e377c2", "Net_64_4" = "#7f7f7f",
                               "Net_32_16_8_4" = "#e300c2", "Net_32_32_16_4" = "#007f7f",
                               "Net_32_32_32_4" ="#ab7634")) +
  labs(title = "Máximo Accuracy por Franja Horaria y Modelo", 
       x = "Franja 1", y = "Franja 2") +
  theme_minimal()
ggsave(paste0(CarpetaOutput,"/Heatmap2 Modelos Definitivo BalAccuracy.png"),width = 16, height = 9, dpi = 100)



table(max_accuracy_total$Modelo)

table(max_balaccuracy_total$Modelo)
plot(table(max_balaccuracy_total$Modelo))


dataPlot = as.data.frame(table(max_balaccuracy_total$Modelo))
colnames(dataPlot)=c("Model","Frequency")
dataPlot$Frequency=dataPlot$Frequency/2

ggplot(dataPlot, aes(x = reorder(Model, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Frecuencia de aparición de cada modelo en max_accuracy_total",
       x = "Modelo",
       y = "Frecuencia") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(paste0(CarpetaOutput,"/Histograma 1 BalAccuracy.png"),width = 16, height = 9, dpi = 100)

# Ordenar el dataframe por Frecuencia en orden descendente
dataPlot <- dataPlot[order(-dataPlot$Frequency), ]


values <- c(
  "Net_16_16_16_4" = "darkblue", 
  "Net_16_16_4" = "blue", 
  "Net_16_4" = "lightblue", 
  "Net_32_32_4" = "blue", 
  "Net_32_4" = "lightblue", 
  "Net_64_32_16_4" = "darkblue", 
  "Net_64_32_4" = "blue", 
  "Net_64_4" = "lightblue", 
  "Net_32_16_8_4" = "darkblue", 
  "Net_32_32_16_4" = "darkblue", 
  "Net_32_32_32_4" = "darkblue"
)

# Asignar los colores a cada fila del dataframe
dataPlot$Color <- values[dataPlot$Model]


# Crear el gráfico de barras con los valores numéricos en las barras
p<-ggplot(dataPlot, aes(x = reorder(Model, -Frequency), y = Frequency,fill = Model)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Frequency), vjust = -0.5, size = 3) +  # Añadir etiquetas encima de las barras
  scale_fill_manual(values = values) + 
  labs(
    x = "Model",
    y = "# 2-slot datasets") +
  theme_minimal(base_size = 16) +  # tamaño base de fuente
  theme(
    axis.title = element_text(size = 18, face = "bold"),   # títulos de ejes
    axis.text = element_text(size = 14),                   # etiquetas de ejes
    axis.text.x = element_text(angle = 45, hjust = 1),     # ejes inclinados
    legend.title = element_text(size = 16, face = "bold"), # título de leyenda
    legend.text = element_text(size = 14),                 # texto de leyenda
    plot.title = element_text(size = 18, hjust = 0.5)      # título centrado
  ) +
  coord_cartesian(clip = "off")

ggsave(
  filename = paste0(CarpetaOutput, "/Histograma_2_Bal_Accuracy.png"),
  plot = p,
  width = 8,     # ~20 cm
  height = 6,    # ~15 cm
  dpi = 300
)

# PDF vectorial (ideal para la revista)
ggsave(
  filename = paste0(CarpetaOutput, "/Histograma_2_Bal_Accuracy.pdf"),
  plot = p,
  width = 8,
  height = 6
)


## Gráfico para Graphical Abstract

# Crear el dataframe con los datos
df <- data.frame(
  Dataset = c("1-slot", "2-slot"),
  "Category 1" = c(0.8423, 0.8676),
  "Category 2" = c(0.7088, 0.7237),
  "Category 3" = c(0.5764, 0.7106),
  "Category 4" = c(0.7707, 0.8465)
)

# Convertir el dataframe a formato largo para ggplot2
df_long <- pivot_longer(df, cols = -Dataset, names_to = "Category", values_to = "Valor")

# Crear el gráfico de líneas
ggplot(df_long, aes(x = Category, y = Valor, group = Dataset, color = Dataset)) +
  geom_line(size = 1) + 
  geom_point(size = 3) + 
  labs(title = "",# "Comparación de Modelos por Categoría",
       x = "",
       y = "Balanced Accuracy",
       color = "Dataset") +
  ylim(0.50, 1) +
  scale_color_manual(values = c("1-slot" = "lightblue", "2-slot" = "darkblue")) +
  theme_minimal()

ggplot(df_long, aes(x = Category, y = Valor, group = Dataset, color = Dataset)) +
  geom_line(size = 2) +  # Aumenta el grosor de las líneas
  geom_point(size = 4) +  # Aumenta el tamaño de los puntos
  labs(title = "", 
       x = "",
       y = "Balanced Accuracy",
       color = "Dataset") +
  ylim(0.50, 1) +
  scale_color_manual(values = c("1-slot" = "red", "2-slot" = "black")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),  # Aumenta el tamaño de las etiquetas de los ejes
    axis.title = element_text(size = 18), # Aumenta el tamaño de los títulos de los ejes
    legend.text = element_text(size = 14), # Aumenta el tamaño del texto de la leyenda
    legend.title = element_text(size = 16) # Aumenta el tamaño del título de la leyenda
  )
ggsave(paste0(CarpetaOutput,"/Lineas Comparacion categorias.png"),width = 16, height = 9, dpi = 300)






## Segundo Gráfico evolución 1-slot to 2 slot


# Crear el dataframe
df <- data.frame(
  Model = c("Net_16_4", "Net_32_4", "Net_64_4", "Net_16_16_4", "Net_32_32_4", "Net_64_32_4",
            "Net_16_16_16_4", "Net_32_32_32_4", "Net_64_32_16_4", "Net_16_4", "Net_32_4", "Net_64_4",
            "Net_16_16_4", "Net_32_32_4", "Net_64_32_4", "Net_16_16_16_4", "Net_32_32_32_4", "Net_64_32_16_4"),
  Accuracy = c(0.6084, 0.5934, 0.5791, 0.6654, 0.6556, 0.6647, 0.6832, NA, 0.6943,
               0.6187, 0.6021, 0.6179, 0.6877, 0.6722, 0.6825, 0.7131, 0.7162, 0.7101),
  BalAccuracy = c(0.6943, 0.6745, 0.6807, 0.7426, 0.7308, 0.7348, 0.7538, NA, 0.7676,
                  0.7133, 0.6939, 0.7141, 0.7661, 0.7535, 0.7611, 0.7856, 0.7854, 0.7773),
  Dataset = c(rep("1-slot", 9), rep("2-slot", 9))
)

# Crear el gráfico
ggplot(df, aes(x = Accuracy, y = BalAccuracy, color = Dataset)) +
  geom_point(size = 4, alpha = 0.8) +  # Puntos grandes con transparencia
  scale_color_manual(values = c("1-slot" = "red", "2-slot" = "black")) + 
  labs(x = "Accuracy", y = "Balanced Accuracy", color = "Dataset") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# Guardar la imagen
ggsave(paste0(CarpetaOutput,"/scatter_accuracy_balaccuracy.png"), width = 8, height = 6, dpi = 300)



# Crear un dataframe con los pares de modelos para las flechas
arrows_df <- data.frame(
  x_start = df$Accuracy[1:9],       # Accuracy de "1-slot"
  y_start = df$BalAccuracy[1:9],    # BalAccuracy de "1-slot"
  x_end = df$Accuracy[10:18],       # Accuracy de "2-slot"
  y_end = df$BalAccuracy[10:18]     # BalAccuracy de "2-slot"
)
# Crear el gráfico con puntos y flechas
ggplot(df, aes(x = Accuracy, y = BalAccuracy, color = Dataset)) +
  geom_segment(data = arrows_df, aes(x = x_start, y = y_start, xend = x_end, yend = y_end),
               arrow = arrow(length = unit(0.3, "inches")), color = "green", linewidth = 1.2) +  
  geom_point(size = 4, alpha = 0.8) +  # Puntos grandes con transparencia
  scale_color_manual(values = c("1-slot" = "red", "2-slot" = "black")) + 
  labs(x = "Accuracy", y = "Balanced Accuracy", color = "Dataset") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14),  
    axis.title = element_text(size = 16),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14)
  )

# Guardar la imagen
ggsave(paste0(CarpetaOutput,"/scatter_accuracy_balaccuracy_arrows.png"), width = 8, height = 6, dpi = 300)


