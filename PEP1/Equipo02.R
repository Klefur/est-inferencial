library(tidyverse)
library(ggpubr)
library(ez)

datos <- read.csv("EI-2023-2-PE1-Datos.csv")

# Pregunta 1

set.seed(127)

indices <- sample(nrow(datos), 100)
muestra <- datos[indices, ]

caloria <-muestra %>% filter(programa == "calorias")
grasa <-muestra %>% filter(programa == "grasas")

perdida_caloria <- caloria$peso.inicial - caloria$peso.final 
summary(perdida_caloria)

perdida_grasa <- grasa$peso.inicial- grasa$peso.final 
summary(perdida_grasa)

shapiro.test(perdida_caloria)
ggqqplot(perdida_caloria)

shapiro.test(perdida_grasa)
ggqqplot(perdida_grasa)

# Se verifican condiciones para usar t test
# Cantidad de datos < 30, siguen una distribución normal y son datos independientes
# por lo que ocupamos la prueba t

# se definen las hipotesis
# H0: Las medias perdida de peso de ambos son iguales
# Ha: La media perdida de peso de calorias es mayor a grasas

test_caloria <- t.test(perdida_caloria, mu = 2.250, alternative = "greater", conf.level = .95)
print(test_caloria)

test_grasa <- t.test(perdida_grasa, mu = 1.250,  alternative = "greater", conf.level = .95)
print(test_grasa)

comparacion <- t.test(perdida_caloria, perdida_grasa, alternative = "greater", conf.level = .95)
print(comparacion)

# con un 95% de confianza se rechaza la hipotesis nula en favor de la alternativa,
# lo que indica que el programa de perdida de peso con dieta de calorias hace 
# bajar mas de 2250 gramos ante un grupo de tamaño similar del programa de 
# bajo en grasas que bajo 1250 gramos


# Pregunta 2

grasa <- datos %>% filter(programa == "grasas")
caloria <- datos %>% filter(programa == "calorias")
carbo <- datos %>% filter(programa == "carbos")
control <- datos %>% filter(programa == "control")

# Como nos piden hacer contraste de medias, vamos a usar anova ya que tenemos
# 4 grupos

# por lo que verificamos condiciones:

perdida_grasa <- grasa$peso.inicial - grasa$peso.final
perdida_caloria <- caloria$peso.inicial - caloria$peso.final
perdida_carbo <- carbo$peso.inicial - carbo$peso.final
perdida_control <- control$peso.inicial - control$peso.final

shapiro.test(perdida_grasa)
ggqqplot(perdida_grasa)

shapiro.test(perdida_caloria)
ggqqplot(perdida_caloria)

shapiro.test(perdida_carbo)
ggqqplot(perdida_carbo)

shapiro.test(perdida_control)
ggqqplot(perdida_control)


# Los datos son independientes
# Todos siguen una distribución normal
# Tenemos un tamaño de datos de al menos 30

# Se cumples todas las condiciones por lo que continuamos

# Se definen las siguientes hipotesis:
# H0: la perdida de grasa en promedio es igual para todos los programas
# Ha: hay diferencias entre el promedio de perdida de grasa entre los programas

# formateamos los datos
perdida_peso = datos$peso.inicial - datos$peso.final
id <- datos$id
programa <- datos$programa
data_format <- data.frame(id, programa, perdida_peso)

prueba <- ezANOVA(data = data_format, dv = perdida_peso, between = programa, wid = id, return_aov = TRUE)
prueba$`Levene's Test for Homogeneity of Variance`
prueba$ANOVA

g <- ezPlot(data = data_format, dv = perdida_peso, between = programa, wid = id
            , y_lab = "Perdida de peso por programa", x = programa)
print(g)
