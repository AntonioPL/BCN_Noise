## -------------------------------------------------------------------------
## SCRIPT: 4 Analisis No supervisado 1.R
## Descripción: Script para realizar análisis No supervisados 1
## FECHA: 13/03/2021
## PAQUETES NECESARIOS: dplyr, tidyr
## INTRUCCIONES: Modificar los Bloques 3 (Nombre Análisis) y 8 (Variables y Agregados). Opcionalmente el 7
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
library("tidyr")
library("stringr")

## -------------------------------------------------------------------------

##### 2. Bloque de definición de funciones auxiliares #####

crearDirectorio=function(subDir){
  if (file.exists(subDir)){
  } else {
    dir.create(file.path(getwd(), subDir))
  }
}

## -------------------------------------------------------------------------

##### 3. Bloque de parametros iniciales #####

Nombre_Analisis = "Analisis No Supervisado LdLeLnS clValid"

setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

crearDirectorio(Nombre_Analisis)

crearDirectorio(paste0(Nombre_Analisis,"/1 analisis clustering"))
crearDirectorio(paste0(Nombre_Analisis,"/2 resultados"))
crearDirectorio(paste0(Nombre_Analisis,"/3 parametros"))
crearDirectorio(paste0(Nombre_Analisis,"/4 elbow"))
crearDirectorio(paste0(Nombre_Analisis,"/5 asignaciones"))

colores=c("#000000","#FF00FF","#00FFFF","#A52A2A","#0000FF","#FFFF00","#00FF00","#FF0000","#808080","#FF0080","#80FF00","#0080FF")

## -------------------------------------------------------------------------

##### 4. Bloque de lectura de datos #####

creados = list.files("./Estadisticos Diarios/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]

if(exists("Data")) {rm(Data)}

for (fichero in creados){
  print(fichero)
  DataAux = read.csv(paste0("./Estadisticos Diarios/",fichero),stringsAsFactors = FALSE)
  if (exists("Data")){
    Data = rbind(Data,DataAux)
  } else{
    Data = DataAux
  }
}

## -------------------------------------------------------------------------

##### 5. Bloque de formateo de fecha #####

Data$Fecha=as.Date(Data$Fecha,format="%d/%m/%Y")

## -------------------------------------------------------------------------

##### 6. Bloque de filtrado de Analisis #####

Actual=Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C")
FechaAnalisis=as.Date("2019-12-31")
Sys.setlocale("LC_TIME", Actual)
PlazoAnalisis=365

DataFiltrado = Data[Data$Fecha<FechaAnalisis & Data$Fecha>=FechaAnalisis-PlazoAnalisis,]

## -------------------------------------------------------------------------

##### 7. Bloque de filtrado Manual si fuese necesario #####

Data2=DataFiltrado[!(DataFiltrado$Nulos==1440),]
DataFiltrado=DataFiltrado[DataFiltrado$Nulos/DataFiltrado$Total<0.01,]

## -------------------------------------------------------------------------

##### 8. Bloque de construcción de variables Analisis #####

NombreVariable1= "Ld"
NombreVariable2= "Le"
NombreVariable3= "Ln"
NombreVariable4= "sd(Lden)"

DataAnalisis=summarise(group_by(DataFiltrado, Codigo),
                       TotalV1 = sum(!is.na(Ld)),
                       Variable1 = 10*log10((1/(TotalV1))*sum(10^(Ld/10),na.rm = TRUE)),
                       TotalV2 = sum(!is.na(Le)),
                       Variable2 = 10*log10((1/(TotalV2))*sum(10^(Le/10),na.rm = TRUE)),
                       TotalV3 = sum(!is.na(Ln)),
                       Variable3 = 10*log10((1/(TotalV3))*sum(10^(Ln/10),na.rm = TRUE)),
                       TotalV4 = sum(!is.na(Ln)),
                       Variable4 = sd(LDEN,na.rm=TRUE)
)

DataAnalisis=DataAnalisis[,c("Codigo","Variable1","Variable2","Variable3","Variable4")]

## -------------------------------------------------------------------------

##### 9. Bloque Gráfico de Densidad Modelo RFM #####

dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis.png"),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2)
frame()
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3)
mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas.png"),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable2, factor = 3), DataAnalisis$Codigo)
frame()
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas y recta.png"),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable2, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
frame()
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis plano.png"),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2)
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas plano.png"),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable2, factor = 3), DataAnalisis$Codigo)
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas y recta plano.png"),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(0, 0, 0, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable2, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()



png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis 2.png"),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1, ylab=NombreVariable4)
frame()
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4)
mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas 2.png"),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1, ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
frame()
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable3, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas y recta 2.png"),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1, ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
frame()
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable3, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis plano 2.png"),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1,ylab=NombreVariable4)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas plano 2.png"),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1, ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable3, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()

png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas y recta plano 2.png"),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(0, 0, 0, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1, ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4)
text(jitter(DataAnalisis$Variable3, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()



png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas y recta plano Total.png"),width = 1024, height = 880)
par(mfrow=c(2, 3),oma = c(0, 0, 0, 0))
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=paste(NombreVariable1,"[dBA]"), ylab=paste(NombreVariable2,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(52,76),xlim=c(52,76))
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable2, factor = 3), DataAnalisis$Codigo)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=paste(NombreVariable1,"[dBA]"),ylab=paste(NombreVariable3,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(52,76),xlim=c(52,76))
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
abline(a=0,b=1)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=paste(NombreVariable2,"[dBA]"),ylab=paste(NombreVariable3,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(52,76),xlim=c(52,76))
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
smoothScatter(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=paste(NombreVariable1,"[dBA]"), ylab=paste(NombreVariable4,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
smoothScatter(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=paste(NombreVariable2,"[dBA]"),ylab=paste(NombreVariable4,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=paste(NombreVariable3,"[dBA]"),ylab=paste(NombreVariable4,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))
text(jitter(DataAnalisis$Variable3, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()


png(paste0(Nombre_Analisis,"/1 analisis clustering/1 Densidad Analisis con etiquetas y recta plano Total scatter.png"),width = 1024, height = 880)
par(mfrow=c(2, 3),oma = c(0, 0, 0, 0))
plot(DataAnalisis$Variable1,DataAnalisis$Variable2, xlab=NombreVariable1, ylab=NombreVariable2,cex.lab=1.5,yaxt="n",xaxt="n",pch=20)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable2, factor = 3), DataAnalisis$Codigo)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
abline(a=0,b=1)
plot(DataAnalisis$Variable1,DataAnalisis$Variable3, xlab=NombreVariable1,ylab=NombreVariable3,cex.lab=1.5,yaxt="n",xaxt="n",pch=20)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
abline(a=0,b=1)
plot(DataAnalisis$Variable2,DataAnalisis$Variable3, xlab=NombreVariable2,ylab=NombreVariable3,cex.lab=1.5,yaxt="n",xaxt="n",pch=20)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable3, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable1,DataAnalisis$Variable4, xlab=NombreVariable1, ylab=NombreVariable4,cex.lab=1.5,yaxt="n",xaxt="n",pch=20)
text(jitter(DataAnalisis$Variable1, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable2,DataAnalisis$Variable4, xlab=NombreVariable2,ylab=NombreVariable4,cex.lab=1.5,yaxt="n",xaxt="n",pch=20)
text(jitter(DataAnalisis$Variable2, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=NombreVariable3,ylab=NombreVariable4,cex.lab=1.5,yaxt="n",xaxt="n",pch=20)
text(jitter(DataAnalisis$Variable3, factor = 3),jitter(DataAnalisis$Variable4, factor = 3), DataAnalisis$Codigo)
abline(a=0,b=1)
axis(2,cex.axis=2)
axis(1,cex.axis=2)
#mtext("Density Plot", outer = TRUE, cex = 2)
dev.off()



## -------------------------------------------------------------------------

##### 10. Bloque de Clustering mediante Modelo #####

## SELECCIONAMOS LAS VARIABLES DE MODELADO
Variables=c("Variable1","Variable2","Variable3","Variable4")

## PREPARAMOS LOS DATOS MEDIANTE SU NORMALIZACIÓN
DataAnalisisNorm=scale(DataAnalisis[,Variables])

for (i in 1:12){
## CALCULAMOS LOS Clusters EN FUNCIÓN AL NÚMERO ELEGIDO
NumClusters=i
set.seed(1234)

Modelo=kmeans(DataAnalisisNorm,NumClusters)

## SELECCIONAMOS LOS GRUPOS
Clusters=Modelo$cluster

## MOSTRAMOS LA DISTRIBUCIÓN DE LOS GRUPOS
table(Clusters)

## MOSTRAMOS LOS DATOS REPRESENTATIVOS DE LOS GRUPOS
Resumen=aggregate(DataAnalisis[,-1], by = list(Clusters), mean)
Resumen

## -------------------------------------------------------------------------

##### 11. Bloque de Representación Gráfica clusters mediante Modelo RFM #####

#dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/2 Kmeans ",NumClusters," clusters.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(DataAnalisis$Variable1,DataAnalisis$Variable2,col=colores[Clusters], xlab=NombreVariable1, ylab=NombreVariable2,pch=19)
plot(c(0,max(DataAnalisis$Variable2)),c(0,max(DataAnalisis$Variable2)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(DataAnalisis$Variable2)),ylim=c(0,max(DataAnalisis$Variable2)))
legend(1,max(DataAnalisis$Variable2)/2-1,legend=c(1:NumClusters),yjust = 0.5,col=colores[c(1:NumClusters)],pch=15,cex=2)
plot(DataAnalisis$Variable1,DataAnalisis$Variable3,col=colores[Clusters], xlab=NombreVariable1,ylab=NombreVariable3,pch=19)
plot(DataAnalisis$Variable2,DataAnalisis$Variable3,col=colores[Clusters], xlab=NombreVariable2,ylab=NombreVariable3,pch=19)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/3 Centroides ",NumClusters," clusters.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(Resumen$Variable1,Resumen$Variable2,col=colores[c(1:NumClusters)], xlab=NombreVariable1, ylab=NombreVariable2,pch=15,cex=2)
plot(c(0,max(Resumen$Variable2)),c(0,max(Resumen$Variable2)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(Resumen$Variable2)),ylim=c(0,max(Resumen$Variable2)))
legend(1,max(Resumen$Variable2)/2-1,legend=c(1:NumClusters),yjust = 0.5,col=colores[c(1:NumClusters)],pch=15,cex=2)
plot(Resumen$Variable1,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable1,ylab=NombreVariable3,pch=15,cex=2)
plot(Resumen$Variable2,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable2,ylab=NombreVariable3,pch=15,cex=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/2 Kmeans ",NumClusters," clusters plano.png",sep=""),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
plot(DataAnalisis$Variable1,DataAnalisis$Variable2,col=colores[Clusters], xlab=NombreVariable1, ylab=NombreVariable2,pch=19)
plot(DataAnalisis$Variable1,DataAnalisis$Variable3,col=colores[Clusters], xlab=NombreVariable1,ylab=NombreVariable3,pch=19)
plot(DataAnalisis$Variable2,DataAnalisis$Variable3,col=colores[Clusters], xlab=NombreVariable2,ylab=NombreVariable3,pch=19)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/3 Centroides ",NumClusters," clusters plano.png",sep=""),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
plot(Resumen$Variable1,Resumen$Variable2,col=colores[c(1:NumClusters)], xlab=NombreVariable1, ylab=NombreVariable2,pch=15,cex=2)
plot(Resumen$Variable1,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable1,ylab=NombreVariable3,pch=15,cex=2)
plot(Resumen$Variable2,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable2,ylab=NombreVariable3,pch=15,cex=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()


png(paste(Nombre_Analisis,"/1 analisis clustering/2 Kmeans ",NumClusters," clusters 2.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(DataAnalisis$Variable1,DataAnalisis$Variable4,col=colores[Clusters], xlab=NombreVariable1, ylab=NombreVariable4,pch=19)
plot(c(min(DataAnalisis$Variable3),max(DataAnalisis$Variable3)),c(min(DataAnalisis$Variable4),max(DataAnalisis$Variable4)), type="n", axes=F, xlab="", ylab="",xlim=c(min(DataAnalisis$Variable3),max(DataAnalisis$Variable3)),ylim=c(min(DataAnalisis$Variable4),max(DataAnalisis$Variable4)))
legend(1,max(DataAnalisis$Variable2)/2-1,legend=c(1:NumClusters),yjust = 0.5,col=colores[c(1:NumClusters)],pch=15,cex=2)
plot(DataAnalisis$Variable2,DataAnalisis$Variable4,col=colores[Clusters], xlab=NombreVariable2,ylab=NombreVariable4,pch=19)
plot(DataAnalisis$Variable3,DataAnalisis$Variable4,col=colores[Clusters], xlab=NombreVariable3,ylab=NombreVariable4,pch=19)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/3 Centroides ",NumClusters," clusters 2.png",sep=""),width = 1024, height = 880)
par(mfrow=c(2, 2),oma = c(1, 0, 3, 0))
plot(Resumen$Variable1,Resumen$Variable2,col=colores[c(1:NumClusters)], xlab=NombreVariable1, ylab=NombreVariable2,pch=15,cex=2)
plot(c(0,max(Resumen$Variable2)),c(0,max(Resumen$Variable2)), type="n", axes=F, xlab="", ylab="",xlim=c(0,max(Resumen$Variable2)),ylim=c(0,max(Resumen$Variable2)))
legend(1,max(Resumen$Variable2)/2-1,legend=c(1:NumClusters),yjust = 0.5,col=colores[c(1:NumClusters)],pch=15,cex=2)
plot(Resumen$Variable1,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable1,ylab=NombreVariable3,pch=15,cex=2)
plot(Resumen$Variable2,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable2,ylab=NombreVariable3,pch=15,cex=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/2 Kmeans ",NumClusters," clusters plano 2.png",sep=""),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
plot(DataAnalisis$Variable1,DataAnalisis$Variable4,col=colores[Clusters], xlab=NombreVariable1,ylab=NombreVariable4,pch=19)
plot(DataAnalisis$Variable2,DataAnalisis$Variable4,col=colores[Clusters], xlab=NombreVariable2,ylab=NombreVariable4,pch=19)
plot(DataAnalisis$Variable3,DataAnalisis$Variable4,col=colores[Clusters], xlab=NombreVariable3,ylab=NombreVariable4,pch=19)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

png(paste(Nombre_Analisis,"/1 analisis clustering/3 Centroides ",NumClusters," clusters plano 2.png",sep=""),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
plot(Resumen$Variable1,Resumen$Variable4,col=colores[c(1:NumClusters)], xlab=NombreVariable1,ylab=NombreVariable4,pch=15,cex=2)
plot(Resumen$Variable2,Resumen$Variable4,col=colores[c(1:NumClusters)], xlab=NombreVariable2,ylab=NombreVariable4,pch=15,cex=2)
plot(Resumen$Variable3,Resumen$Variable4,col=colores[c(1:NumClusters)], xlab=NombreVariable3,ylab=NombreVariable4,pch=15,cex=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()





png(paste(Nombre_Analisis,"/1 analisis clustering/2 Kmeans ",NumClusters," clusters plano Total.png",sep=""),width = 1024, height = 800)
par(mfrow=c(2, 3),oma = c(1, 0, 3, 0))
plot(DataAnalisis$Variable1,DataAnalisis$Variable2,col=colores[Clusters], xlab=paste(NombreVariable1,"[dBA]"), ylab=paste(NombreVariable2,"[dBA]"),pch=19,cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(52,76),xlim=c(52,76))
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable1,DataAnalisis$Variable3,col=colores[Clusters], xlab=paste(NombreVariable1,"[dBA]"),ylab=paste(NombreVariable3,"[dBA]"),pch=19,cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(52,76),xlim=c(52,76))
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable2,DataAnalisis$Variable3,col=colores[Clusters], xlab=paste(NombreVariable2,"[dBA]"),ylab=paste(NombreVariable3,"[dBA]"),pch=19,cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(52,76),xlim=c(52,76))
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable1,DataAnalisis$Variable4,col=colores[Clusters], xlab=paste(NombreVariable1,"[dBA]"),ylab=paste(NombreVariable4,"[dBA]"),pch=19,cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable2,DataAnalisis$Variable4,col=colores[Clusters], xlab=paste(NombreVariable2,"[dBA]"),ylab=paste(NombreVariable4,"[dBA]"),pch=19,cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(DataAnalisis$Variable3,DataAnalisis$Variable4,col=colores[Clusters], xlab=paste(NombreVariable3,"[dBA]"),ylab=paste(NombreVariable4,"[dBA]"),pch=19,cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))

axis(2,cex.axis=2)
axis(1,cex.axis=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

#smoothScatter(DataAnalisis$Variable3,DataAnalisis$Variable4, xlab=paste(NombreVariable3,"[dBA]"),ylab=paste(NombreVariable4,"[dBA]"),cex.lab=1.5,yaxt="n",xaxt="n",ylim=c(1,4.4),xlim=c(52,76))


png(paste(Nombre_Analisis,"/1 analisis clustering/3 Centroides ",NumClusters," clusters plano Total.png",sep=""),width = 1024, height = 800)
par(mfrow=c(2, 3),oma = c(1, 0, 3, 0))
plot(Resumen$Variable1,Resumen$Variable2,col=colores[c(1:NumClusters)], xlab=NombreVariable1, ylab=NombreVariable2,pch=15,cex=2,cex.lab=1.5,yaxt="n",xaxt="n")
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(Resumen$Variable1,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable1,ylab=NombreVariable3,pch=15,cex=2,cex.lab=1.5,yaxt="n",xaxt="n")
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(Resumen$Variable2,Resumen$Variable3,col=colores[c(1:NumClusters)], xlab=NombreVariable2,ylab=NombreVariable3,pch=15,cex=2,cex.lab=1.5,yaxt="n",xaxt="n")
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(Resumen$Variable1,Resumen$Variable4,col=colores[c(1:NumClusters)], xlab=NombreVariable1,ylab=NombreVariable4,pch=15,cex=2,cex.lab=1.5,yaxt="n",xaxt="n")
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(Resumen$Variable2,Resumen$Variable4,col=colores[c(1:NumClusters)], xlab=NombreVariable2,ylab=NombreVariable4,pch=15,cex=2,cex.lab=1.5,yaxt="n",xaxt="n")
axis(2,cex.axis=2)
axis(1,cex.axis=2)
plot(Resumen$Variable3,Resumen$Variable4,col=colores[c(1:NumClusters)], xlab=NombreVariable3,ylab=NombreVariable4,pch=15,cex=2,cex.lab=1.5,yaxt="n",xaxt="n")
axis(2,cex.axis=2)
axis(1,cex.axis=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()

## -------------------------------------------------------------------------

##### 12. Bloque de Resultados Clusterizacion exportados a Excel #####

## CENTROS

Resumen$Contador=table(Clusters)
colnames(Resumen)=c("Cluster",NombreVariable1,NombreVariable2,NombreVariable3, NombreVariable4,"Contador")

NormalizacionMedia=apply(DataAnalisis[,Variables],MARGIN=2,FUN=mean)
names(NormalizacionMedia)=c(NombreVariable1,NombreVariable2,NombreVariable3,NombreVariable4)

NormalizacionSD=apply(DataAnalisis[,Variables],MARGIN=2,FUN=sd)
names(NormalizacionSD)=c(NombreVariable1,NombreVariable2,NombreVariable3,NombreVariable4)

Centroides=Modelo$centers
colnames(Centroides)=c(NombreVariable1,NombreVariable2,NombreVariable3,NombreVariable4)

#RFM_Clientes$CLUSTER_1=CLUSTERS
write.csv2(Resumen,file=paste(Nombre_Analisis,"/2 resultados/4 Resumen clusterizacion ",NumClusters," clusters.csv",sep=""),row.names=FALSE)
write.csv2(Centroides,file=paste(Nombre_Analisis,"/3 parametros/5 Centroides clusterizacion ",NumClusters," clusters.csv",sep=""),row.names=FALSE)
write.csv2(t(NormalizacionMedia),file=paste(Nombre_Analisis,"/3 parametros/6 Medias clusterizacion ",NumClusters," clusters.csv",sep=""),row.names=FALSE)
write.csv2(t(NormalizacionSD),file=paste(Nombre_Analisis,"/3 parametros/7 Desviaciones clusterizacion ",NumClusters," clusters.csv",sep=""),row.names=FALSE)

## -------------------------------------------------------------------------

##### 13. Bloque de asignacion de clusters #####

DataAnalisis_EXPLOTACION=DataAnalisis
DataAnalisis_EXPLOTACION$CLUSTER=Clusters
#head(DataAnalisis_EXPLOTACION)
write.csv2(DataAnalisis_EXPLOTACION,file=paste(Nombre_Analisis,"/5 asignaciones/Asignaciones ",NumClusters," clusters.csv",sep=""),row.names=FALSE)
}
## -------------------------------------------------------------------------

##### 14. Bloque de Metodo de seleccion de numero de clusters (Elbow Method) #####

IntraGrupos=rep(0,12)

for (i in 1:12){
  set.seed(1234)
  Modelo=kmeans(DataAnalisisNorm,i)
  IntraGrupos[i] <- sum(Modelo$withinss)
}

png(paste0(Nombre_Analisis,"/4 elbow/Errores Intragrupo.png"),width = 512, height = 440)
plot(1:12, IntraGrupos, type="b", xlab="Amount of Clusters", ylab="Sum of Squared Errors")
dev.off()
png(paste0(Nombre_Analisis,"/4 elbow/Errores Intragrupo2.png"),width = 400, height = 320)
plot(1:12, IntraGrupos, type="b", xlab="Amount of Clusters", ylab="Sum of Squared Errors")
dev.off()

IntraGrupos
SSE=IntraGrupos

SSE
SSE[2:12]/SSE[1:11]-1

## -------------------------------------------------------------------------

##### 15. Bloque de Metodo de seleccion de numero de clusters (silhouette Method) #####

resultados=rep(0,12)

library(cluster)
set.seed(1234)
d <- daisy(DataAnalisisNorm) 
for (i in 2:12)
{
  set.seed(1234)
  Modelo=kmeans(DataAnalisisNorm,i)
  y_cluster<-Modelo$cluster
  sk<-silhouette(Modelo$cluster, d)
  resultados[i] <- mean(sk[,3])
}

png(paste0(Nombre_Analisis,"/4 elbow/Silhouette.png"),width = 512, height = 440)
plot(2:12,resultados[2:12],type="o",col="blue",pch=0,xlab="Amount of Clusters",ylab="Silhouette")
dev.off()
png(paste0(Nombre_Analisis,"/4 elbow/Silhouette2.png"),width = 400, height = 320)
plot(2:12,resultados[2:12],type="o",col="blue",pch=0,xlab="Amount of Clusters",ylab="Silhouette")
dev.off()

## -------------------------------------------------------------------------

##### 16. Bloque de Metodo de seleccion de numero de clusters (clValid) #####

library(clValid)
library(mclust)
library(kohonen)

exprs <- DataAnalisisNorm
rownames(exprs) <- DataAnalisis$Codigo
head(exprs)
clmethods <- c("hierarchical","kmeans")
clmethods <- c("hierarchical","kmeans","pam")
clmethods <- c("hierarchical","kmeans", "diana", "fanny", "pam", "clara","som", "model", "sota")
clmethods <- c("hierarchical","kmeans","model","pam") # "som", "model", "sota")
set.seed(1234)
validation <- clValid(exprs, nClust = 2:12,clMethods = clmethods, validation = "internal")
# Summary



# Para el articulo de comparación:
clmethods <- c("hierarchical","kmeans", "diana", "fanny", "pam", "clara","som", "model", "sota")
set.seed(1234)
validation <- clValid(exprs, nClust = 3:20,clMethods = clmethods, validation = "internal")

summary(validation)
plot(validation)
str(validation)


validation@measures


plot(validation)

str(validation)

plot(validation,legend=FALSE)

## -------------------------------------------------------------------------

##### 17. Bloque de Análisis de Estabilidad #####

NUM_CLUSTERS = 4
Experimentos = 100

CENTROS=read.csv2(paste(Nombre_Analisis,"/3 parametros/5 Centroides clusterizacion ",NUM_CLUSTERS," clusters.csv",sep=""),stringsAsFactors = FALSE)
NORMALIZACION_MEDIA=read.csv2(paste(Nombre_Analisis,"/3 parametros/6 Medias clusterizacion ",NUM_CLUSTERS," clusters.csv",sep=""),stringsAsFactors = FALSE)
NORMALIZACION_SD=read.csv2(paste(Nombre_Analisis,"/3 parametros/7 Desviaciones clusterizacion ",NUM_CLUSTERS," clusters.csv",sep=""),stringsAsFactors = FALSE)

Nombres=c(NombreVariable1,NombreVariable2,NombreVariable3,NombreVariable4)
colnames(CENTROS)=Nombres
colnames(NORMALIZACION_MEDIA)=Nombres
colnames(NORMALIZACION_SD)=Nombres

set.seed(1234)
Modelo=kmeans(DataAnalisisNorm,NUM_CLUSTERS)

## SELECCIONAMOS LOS GRUPOS
Clusters=Modelo$cluster

## MOSTRAMOS LA DISTRIBUCIÓN DE LOS GRUPOS
table(Clusters)

## MOSTRAMOS LOS DATOS REPRESENTATIVOS DE LOS GRUPOS
Resumen=aggregate(DataAnalisis[,-1], by = list(Clusters), mean)
colnames(Resumen)=c("Numero",Nombres)
Resumen

distanciasC1=c()
distanciasC2=c()
distanciasC3=c()
distanciasC4=c()
set.seed(1234)

for (i in 1:Experimentos){
  Modelo=kmeans(DataAnalisisNorm,NUM_CLUSTERS)
  
  ## SELECCIONAMOS LOS GRUPOS
  Clusters=Modelo$cluster
  
  ## MOSTRAMOS LA DISTRIBUCIÓN DE LOS GRUPOS
  table(Clusters)
  
  ## MOSTRAMOS LOS DATOS REPRESENTATIVOS DE LOS GRUPOS
  Resumen2=aggregate(DataAnalisis[,-1], by = list(Clusters), mean)
  colnames(Resumen2)=c("Numero",Nombres)
  Resumen =rbind(Resumen,Resumen2)
  for (k in 1:NUM_CLUSTERS){
    Resumen2[[paste("DIST_CLUSTER_",k,sep="")]]=((Resumen2[[NombreVariable1]]-NORMALIZACION_MEDIA[[NombreVariable1]])/NORMALIZACION_SD[[NombreVariable1]]-CENTROS[[NombreVariable1]][k])^2+((Resumen2[[NombreVariable2]]-NORMALIZACION_MEDIA[[NombreVariable2]])/NORMALIZACION_SD[[NombreVariable2]]-CENTROS[[NombreVariable2]][k])^2+((Resumen2[[NombreVariable3]]-NORMALIZACION_MEDIA[[NombreVariable3]])/NORMALIZACION_SD[[NombreVariable3]]-CENTROS[[NombreVariable3]][k])^2+((Resumen2[[NombreVariable4]]-NORMALIZACION_MEDIA[[NombreVariable4]])/NORMALIZACION_SD[[NombreVariable4]]-CENTROS[[NombreVariable4]][k])^2
  }
  distancias=sapply(1:NUM_CLUSTERS,FUN=function(k){paste("DIST_CLUSTER_",k,sep="")})
  Resumen2$minimo=apply(Resumen2[,distancias],MARGIN=1,FUN=min,na.rm=TRUE)
  
  Resumen2_AUX=Resumen2[,distancias]==Resumen2$minimo
  Resumen2$CLUSTER=unname(unlist(apply(Resumen2_AUX,MARGIN=1,FUN=which)))
  print(table(Resumen2$CLUSTER))
  distanciasC1=c(distanciasC1,Resumen2$minimo[Resumen2$CLUSTER==1])
  distanciasC2=c(distanciasC2,Resumen2$minimo[Resumen2$CLUSTER==2])
  distanciasC3=c(distanciasC3,Resumen2$minimo[Resumen2$CLUSTER==3])
  distanciasC4=c(distanciasC4,Resumen2$minimo[Resumen2$CLUSTER==4])
}


distanciasC1

Resumen[c(1:4),]
Resumen[c(41:44),]


png(paste(Nombre_Analisis,"/1 analisis clustering/3 Centroides ",NUM_CLUSTERS," clusters plano 2 estabilidad.png",sep=""),width = 1024, height = 400)
par(mfrow=c(1, 3),oma = c(1, 0, 3, 0))
plot(Resumen[[NombreVariable1]],Resumen[[NombreVariable4]],col=colores[Resumen$Numero], xlab=NombreVariable1,ylab=NombreVariable4,pch=15,cex=2)
plot(Resumen[[NombreVariable2]],Resumen[[NombreVariable4]],col=colores[Resumen$Numero], xlab=NombreVariable2,ylab=NombreVariable4,pch=15,cex=2)
plot(Resumen[[NombreVariable3]],Resumen[[NombreVariable4]],col=colores[Resumen$Numero], xlab=NombreVariable3,ylab=NombreVariable4,pch=15,cex=2)
#mtext(paste("Kmeans Clustering",sep=""), outer = TRUE, cex = 2)
dev.off()


## -------------------------------------------------------------------------