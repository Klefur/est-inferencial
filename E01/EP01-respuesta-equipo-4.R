# Integrantes
# Nicolás Aguilera
# Lucas Mesias
# Joaquín Saldivia

library(dplyr)
library(ggplot2)
library(ggpubr)

# ¿Se distribuye de igual manera la situación ocupacional
# de los hombres que viven en áreas rurales y quienes viven en áreas urbanas de la RM?

setwd("C:/Code/Inferencial/e01")
datos <- read.csv2("EP01 Datos Casen 2017.csv", )

datos_hombre <- datos %>% filter(sexo == "Hombre")

tabla <- xtabs(~zona + ch1, data = datos_hombre)

proporciones_fila <- prop.table(tabla, margin=1)
proporciones_fila <- addmargins(proporciones_fila, margin=2)
proporciones_fila
contingencia <- as.data.frame(proporciones_fila)

g1 <- ggplot(contingencia, aes(fill = zona, y = Freq, x = ch1))
g1 <- g1 + geom_bar(position = "dodge", stat = "identity")
g1 <- g1 + labs(y = "Frecuencia") + ggtitle("Barras agrupadas")
g1 <- g1 + theme_pubr() + theme( axis.text.x = element_text(angle=90))
g1

# Por lo observado en el grafico g1 se determina que si se distribuye de igual
# manera la situacion ocupacional entre los hombres rurales y urbanos pero que
# de forma interna la distribución es desigual concentrandose en asalariado. 
