library(dplyr)
library(ggplot2)
library(ggpubr)

ruta <- getwd()
ruta <- paste(ruta, "EP02 Datos.csv", sep = "/")

archivo <- read.csv2(ruta)

negros <- archivo %>% filter(Raza == "Negra")

mediaPreEntrenamiento <-mean(negros$Previo)
medioPostEntrenamiento <- mean(negros$Posterior)

t.test(negros$Previo, alternative="less", conf.level = 0.95, mu=14.3)
