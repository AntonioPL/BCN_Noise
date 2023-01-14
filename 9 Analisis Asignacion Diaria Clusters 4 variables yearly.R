## -------------------------------------------------------------------------
## SCRIPT: 9 Analisis Asignacion Diaria Clusters.R
## Descripción: Script para analizar la evolucion de los clusters
## FECHA: 23/05/2021
## PAQUETES NECESARIOS: dplyr, tidyr
## INTRUCCIONES: #########Modificar los Bloques 3 (Nombre Análisis) y 8 (Variables y Agregados). Opcionalmente el 7
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library("dplyr")
library("tidyr")
library("stringr")
library("ggplot2")

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

Nombre_Analisis = "Analisis No Supervisado LdLeLnS"


setwd("D:/Documentos, Trabajos y Demás/Doctorado UCAM/Datos BCN")

crearDirectorio(Nombre_Analisis)

crearDirectorio(paste0(Nombre_Analisis,"/6 asignaciones diarias"))
crearDirectorio(paste0(Nombre_Analisis,"/7 asignaciones mensuales"))
crearDirectorio(paste0(Nombre_Analisis,"/8 evolucion mensual"))
crearDirectorio(paste0(Nombre_Analisis,"/9 matriz de transicion"))

colores=c("#000000","#FF00FF","#00FFFF","#A52A2A","#FFFF00","#00FF00","#0000FF","#FF0000","#808080")

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

DataFiltrado[NombreVariable4]=DataFiltrado$SD

Nombres=c(NombreVariable1,NombreVariable2,NombreVariable3,NombreVariable4)

DataFiltrado=DataFiltrado[!is.na(DataFiltrado[[NombreVariable1]]),]
DataFiltrado=DataFiltrado[!is.na(DataFiltrado[[NombreVariable2]]),]
DataFiltrado=DataFiltrado[!is.na(DataFiltrado[[NombreVariable3]]),]

## -------------------------------------------------------------------------

##### 9. Bloque de asignación de clusters a los datos diarios periodo #####



NUM_CLUSTERS=4

CENTROS=read.csv2(paste(Nombre_Analisis,"/3 parametros/5 Centroides clusterizacion ",NUM_CLUSTERS," clusters.csv",sep=""),stringsAsFactors = FALSE)
NORMALIZACION_MEDIA=read.csv2(paste(Nombre_Analisis,"/3 parametros/6 Medias clusterizacion ",NUM_CLUSTERS," clusters.csv",sep=""),stringsAsFactors = FALSE)
NORMALIZACION_SD=read.csv2(paste(Nombre_Analisis,"/3 parametros/7 Desviaciones clusterizacion ",NUM_CLUSTERS," clusters.csv",sep=""),stringsAsFactors = FALSE)

colnames(CENTROS)=Nombres
colnames(NORMALIZACION_MEDIA)=Nombres
colnames(NORMALIZACION_SD)=Nombres
  
for (k in 1:NUM_CLUSTERS){
  DataFiltrado[[paste("DIST_CLUSTER_",k,sep="")]]=((DataFiltrado[[NombreVariable1]]-NORMALIZACION_MEDIA[[NombreVariable1]])/NORMALIZACION_SD[[NombreVariable1]]-CENTROS[[NombreVariable1]][k])^2+((DataFiltrado[[NombreVariable2]]-NORMALIZACION_MEDIA[[NombreVariable2]])/NORMALIZACION_SD[[NombreVariable2]]-CENTROS[[NombreVariable2]][k])^2+((DataFiltrado[[NombreVariable3]]-NORMALIZACION_MEDIA[[NombreVariable3]])/NORMALIZACION_SD[[NombreVariable3]]-CENTROS[[NombreVariable3]][k])^2+((DataFiltrado[[NombreVariable4]]-NORMALIZACION_MEDIA[[NombreVariable4]])/NORMALIZACION_SD[[NombreVariable4]]-CENTROS[[NombreVariable4]][k])^2
}
  
distancias=sapply(1:NUM_CLUSTERS,FUN=function(k){paste("DIST_CLUSTER_",k,sep="")})
DataFiltrado$minimo=apply(DataFiltrado[,distancias],MARGIN=1,FUN=min,na.rm=TRUE)
  
DataFiltrado_AUX=DataFiltrado[,distancias]==DataFiltrado$minimo
DataFiltrado$CLUSTER=unname(unlist(apply(DataFiltrado_AUX,MARGIN=1,FUN=which)))
print(table(DataFiltrado$CLUSTER))
  

DataAnalisisClustersDiarios=summarise(group_by(DataFiltrado, Codigo),
                       TotalCluster1 = sum(CLUSTER==1),
                       TotalCluster2 = sum(CLUSTER==2),
                       TotalCluster3 = sum(CLUSTER==3),
                       TotalCluster4 = sum(CLUSTER==4),
                       TotalCluster5 = sum(CLUSTER==5),
                       TotalCluster6 = sum(CLUSTER==6),
                       TotalCluster7 = sum(CLUSTER==7),
                       TotalCluster8 = sum(CLUSTER==8),
                       TotalCluster9 = sum(CLUSTER==9),
                       TotalCluster10 = sum(CLUSTER==10),
                       TotalClusterPorc1 = 100*sum(CLUSTER==1)/n(),
                       TotalClusterPorc2 = 100*sum(CLUSTER==2)/n(),
                       TotalClusterPorc3 = 100*sum(CLUSTER==3)/n(),
                       TotalClusterPorc4 = 100*sum(CLUSTER==4)/n(),
                       TotalClusterPorc5 = 100*sum(CLUSTER==5)/n(),
                       TotalClusterPorc6 = 100*sum(CLUSTER==6)/n(),
                       TotalClusterPorc7 = 100*sum(CLUSTER==7)/n(),
                       TotalClusterPorc8 = 100*sum(CLUSTER==8)/n(),
                       TotalClusterPorc9 = 100*sum(CLUSTER==9)/n(),
                       TotalClusterPorc10 = 100*sum(CLUSTER==10)/n()
)

DataAnalisisClustersDiarios

write.csv2(DataAnalisisClustersDiarios,file=paste(Nombre_Analisis,"/6 asignaciones diarias/Asignaciones ",NUM_CLUSTERS," clusters.csv",sep=""),row.names=FALSE)


## -------------------------------------------------------------------------

##### 10. Bloque de asignación de clusters a los datos mensuales #####








## -------------------------------------------------------------------------

##### 11. Bloque de asignación de nuevas fechas #####

Fechas=c("2018-12-31","2019-01-31","2019-02-28","2019-03-31","2019-04-30","2019-05-31","2019-06-30","2019-07-31","2019-08-31","2019-09-30","2019-10-31","2019-11-30","2019-12-31","2020-01-31","2020-02-28","2020-03-31","2020-04-30","2020-05-31","2020-06-30","2020-07-31","2020-08-31","2020-09-30","2020-10-31","2020-11-30","2020-12-31")

rm(Resultados)
for (i in 1:length(Fechas)){
  Fecha=Fechas[i]
  Sys.setlocale("LC_TIME", "C")
  FechaAnalisis=as.Date(Fecha)
  Sys.setlocale("LC_TIME", Actual)
  
  DataFiltrado = Data[Data$Fecha<FechaAnalisis & Data$Fecha>=FechaAnalisis-PlazoAnalisis,]

  DataFiltrado=DataFiltrado[DataFiltrado$Nulos/DataFiltrado$Total<0.01,]
  
  
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

  
  DataAnalisisFecha=DataAnalisis[,c("Codigo","Variable1","Variable2","Variable3","Variable4")]
  colnames(DataAnalisisFecha)=c("Codigo",NombreVariable1,NombreVariable2,NombreVariable3,NombreVariable4)
  DataAnalisisFecha=DataAnalisisFecha[!is.na(DataAnalisisFecha[[NombreVariable1]]),]
  DataAnalisisFecha=DataAnalisisFecha[!is.na(DataAnalisisFecha[[NombreVariable2]]),]
  DataAnalisisFecha=DataAnalisisFecha[!is.na(DataAnalisisFecha[[NombreVariable3]]),]
  DataAnalisisFecha=DataAnalisisFecha[!is.na(DataAnalisisFecha[[NombreVariable4]]),]
  
  for (k in 1:NUM_CLUSTERS){
    DataAnalisisFecha[[paste("DIST_CLUSTER_",k,sep="")]]=((DataAnalisisFecha[[NombreVariable1]]-NORMALIZACION_MEDIA[[NombreVariable1]])/NORMALIZACION_SD[[NombreVariable1]]-CENTROS[[NombreVariable1]][k])^2+((DataAnalisisFecha[[NombreVariable2]]-NORMALIZACION_MEDIA[[NombreVariable2]])/NORMALIZACION_SD[[NombreVariable2]]-CENTROS[[NombreVariable2]][k])^2+((DataAnalisisFecha[[NombreVariable3]]-NORMALIZACION_MEDIA[[NombreVariable3]])/NORMALIZACION_SD[[NombreVariable3]]-CENTROS[[NombreVariable3]][k])^2+((DataAnalisisFecha[[NombreVariable4]]-NORMALIZACION_MEDIA[[NombreVariable4]])/NORMALIZACION_SD[[NombreVariable4]]-CENTROS[[NombreVariable4]][k])^2
}
  
  distancias=sapply(1:NUM_CLUSTERS,FUN=function(k){paste("DIST_CLUSTER_",k,sep="")})
  DataAnalisisFecha$minimo=apply(DataAnalisisFecha[,distancias],MARGIN=1,FUN=min,na.rm=TRUE)
  
  DataAnalisisFecha_AUX=DataAnalisisFecha[,distancias]==DataAnalisisFecha$minimo
  DataAnalisisFecha$CLUSTER=unname(unlist(apply(DataAnalisisFecha_AUX,MARGIN=1,FUN=which)))
  print(Fecha)
  distribucion=rep.int(0,NUM_CLUSTERS)
  for (i in 1:NUM_CLUSTERS){
    distribucion[i]=sum(DataAnalisisFecha$CLUSTER==i)
  }
  #distribucion=table(DataAnalisisFecha$CLUSTER)
  if (exists("Resultados")){
    #ResultadosAux=data.frame(Fecha=Fecha ,valor=distribucion)
    #Resultados=rbind(Resultados,ResultadosAux)
    Resultados=rbind(Resultados,data.frame(t(distribucion)))
  } else{
    #Resultados=data.frame(Fecha=Fecha ,valor=distribucion)
    Resultados=data.frame(t(distribucion))
  }
  print(table(DataAnalisisFecha$CLUSTER))
  write.csv2(DataAnalisisFecha,file=paste(Nombre_Analisis,"/8 evolucion mensual/Asignaciones ",NUM_CLUSTERS," clusters ",Fecha,".csv",sep=""),row.names=FALSE)
}

Resultados$Fecha=Fechas

write.csv2(Resultados,file=paste(Nombre_Analisis,"/8 evolucion mensual/Asignaciones ",NUM_CLUSTERS," clusters Total.csv",sep=""),row.names=FALSE)


## -------------------------------------------------------------------------

##### 12. Bloque de representación gráfica #####

Resultados$Fecha=as.Date(Resultados$Fecha)

png(paste(Nombre_Analisis,"/8 evolucion mensual/Asignaciones ",NUM_CLUSTERS,".png",sep=""),width = 500, height = 250)
ggplot(Resultados,aes(x=Fecha))+
  geom_line(aes(y=X1,color="Cluster 1"))+
  geom_point(aes(y=X1,color="Cluster 1"))+
  geom_line(aes(y=X2,color="Cluster 2"))+
  geom_point(aes(y=X2,color="Cluster 2"))+
  geom_line(aes(y=X3,color="Cluster 3"))+
  geom_point(aes(y=X3,color="Cluster 3"))+
  geom_line(aes(y=X4,color="Cluster 4"))+
  geom_point(aes(y=X4,color="Cluster 4"))+
  scale_colour_manual("", values = c("Cluster 1" = colores[1], "Cluster 2" = colores[2], "Cluster 3" = colores[3], "Cluster 4" = colores[4])) +
  xlab("Date") + ylab("Amount of Nodes per cluster")+
  labs(fill = "Cluster")+
  #theme_classic()+
  theme(axis.text.x = element_text(angle = 90))+
  scale_y_continuous(breaks = seq(0,max(Resultados[,c(1:NUM_CLUSTERS)]),by=2))+
  scale_x_date(date_breaks = "months" , date_labels = "%m-%Y")+
  geom_vline(xintercept = as.Date("15/03/2020",format="%d/%m/%Y"),color="red")+
  geom_vline(xintercept = as.Date("21/06/2020",format="%d/%m/%Y"),color="red")
dev.off()
## -------------------------------------------------------------------------
##### 14. Bloque de Matriz de Transicion #####

# introducir las fechas a comparar en formato YYYYMMDD
fecha_1="2019-12-31"
fecha_2="2020-06-30"

Nodes_Fecha_1=read.csv2(paste(Nombre_Analisis,"/8 evolucion mensual/Asignaciones ",NUM_CLUSTERS," clusters ",fecha_1,".csv",sep=""))
Nodes_Fecha_2=read.csv2(paste(Nombre_Analisis,"/8 evolucion mensual/Asignaciones ",NUM_CLUSTERS," clusters ",fecha_2,".csv",sep=""))

Nodes_Fecha_1$CLUSTER_1=Nodes_Fecha_1$CLUSTER
Nodes_Fecha_2$CLUSTER_2=Nodes_Fecha_2$CLUSTER

Nodes_VENTAS_1_2=merge(Nodes_Fecha_1[,c("Codigo", "CLUSTER_1")],Nodes_Fecha_2[,c("Codigo", "CLUSTER_2")],all.x=TRUE,all.y=TRUE)
Nodes_VENTAS_1_2$CLUSTER_1[is.na(Nodes_VENTAS_1_2$CLUSTER_1)]=0
Nodes_VENTAS_1_2$CLUSTER_2[is.na(Nodes_VENTAS_1_2$CLUSTER_2)]=0

TABLA=table(Nodes_VENTAS_1_2$CLUSTER_1,Nodes_VENTAS_1_2$CLUSTER_2)
TABLA2=round(100*TABLA/sum(TABLA),2)
TABLA3=round(100*TABLA/apply(TABLA,MARGIN=1,FUN=sum),2)
TABLA4=round(t(100*t(TABLA)/apply(TABLA,MARGIN=2,FUN=sum)),2)

write.csv2(TABLA,paste0(Nombre_Analisis,"/9 matriz de transicion/matriz de transicion ",fecha_1," ",fecha_2,".csv"),row.names = TRUE)
write.csv2(TABLA2,paste0(Nombre_Analisis,"/9 matriz de transicion/matriz de transicion relativa ",fecha_1," ",fecha_2,".csv"),row.names = TRUE)
write.csv2(TABLA3,paste0(Nombre_Analisis,"/9 matriz de transicion/matriz de transicion relativa salida ",fecha_1," ",fecha_2,".csv"),row.names = TRUE)
write.csv2(TABLA4,paste0(Nombre_Analisis,"/9 matriz de transicion/matriz de transicion relativa llegada ",fecha_1," ",fecha_2,".csv"),row.names = TRUE)

## -------------------------------------------------------------------------