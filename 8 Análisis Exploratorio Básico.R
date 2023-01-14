## -------------------------------------------------------------------------
## SCRIPT: Analisis Exploratorio Básico.R
## Descripción: Script para formatear los estadisticos
## FECHA: 16/05/2021
## Paquetes Necesarios: ggplot2
## -------------------------------------------------------------------------

## -------------------------------------------------------------------------

##### 1. Bloque de inicializacion de librerias #####

library(ggplot2)
library(hrbrthemes)

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

crearCarpeta("Analisis Exploratorio Basico")

## -------------------------------------------------------------------------

##### 4. Bloque de selección de ficheros #####

creados = list.files("./Estadisticos Diarios/")

elementos = grep(".csv",x = creados)

creados=creados[elementos]

dev.off()

## -------------------------------------------------------------------------

##### 5. Bloque de creacion de graficos #####

for (i in 1:length(creados)){
  print(paste0("procesando fichero ",creados[i]))
  
  df <- try(read.csv(paste0("./Estadisticos Diarios/",creados[i]),stringsAsFactors = FALSE))
  tipos <- sapply(df, class)
  Statistics=c()
  value=c()
  for (j in 1:length(tipos)){
    if (tipos[j]=="numeric"){
      Statistics =c(Statistics,rep(colnames(df)[j],length(df[,j]))) 
      value =c(value,df[,j])
    }
  }
  Statistics = gsub("LDEN","Lden",Statistics)
  data = data.frame(Statistics=Statistics,value=value,stringsAsFactors = FALSE)
  
  data=data[!is.na(data$value),]
  variables=c("Lden","Ld","Le","Ln")
  
  data=data[data$Statistics %in% variables,]
  data$Statistics = ordered(data$Statistics, levels = variables)
  data$color=as.numeric(as.factor(data$Statistics))
  p1 <- ggplot(data, aes(x=Statistics, y=value, fill=Statistics)) + geom_violin() +theme(legend.position="none") +
    xlab("Statistic")+ylab("Sound Pressure Level [dBA]") +ylim(50,90)
  png(paste0("./Analisis Exploratorio Basico/Violin ",df$Codigo[1],".png"),width = 1024, height = 880)
  print(p1+ theme_gray(base_size=36))
  dev.off()
  
  
  p2 <- ggplot(data, aes(x=value, group=Statistics, fill=Statistics)) +
    geom_density(adjust=1.5, alpha=.4) +ylim(0,0.35)+xlim(50,90)
    theme_ipsum() + xlab("Sound Pressure Level [dBA]")+ylab("Probability Density")
  png(paste0("./Analisis Exploratorio Basico/Density ",df$Codigo[1],".png"),width = 1024, height = 880)
  print(p2+ theme_gray(base_size=36))
  dev.off()
  
  
  
  p3 <- ggplot(data, aes(x=Statistics, y=value, color=Statistics)) +
    geom_boxplot()
  png(paste0("./Analisis Exploratorio Basico/Boxplot ",df$Codigo[1],".png"),width = 1024, height = 880)
  print(p3+ theme_gray(base_size=36))
  dev.off()
}
## -------------------------------------------------------------------------