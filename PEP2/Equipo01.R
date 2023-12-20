#PEP2 Equipo 1
library(lmtest)
library(car)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(boot)

archivo <- read.csv2("EI-2023-2-PE2-Datos-Equipo01.csv", sep = ",")

# Pregunta 1

# Queremos ver si es posible predecir la variable X1 y como se pide 
# evaluar con todas las variables realizaremos una regresion lineal multiple

set.seed(300)

# Definimos los conjuntos de entrenamiento y de prueba
entrenamiento <- head(archivo, 40)
prueba <- tail(archivo, 10)

# Creamos el modelo de RML con tadas la variables
completo <- lm(X1 ~ ., data = entrenamiento)
summary(completo)

# Comprobamos la multilinealidad
vifs <- vif(completo)
vifs
1/vifs

# Observamos que las variables X2 y X3 nos dan un problema de multicolinealidad
# por lo que las eliminamos del modelo
modelo2 <- lm(X1 ~ . - X2 -X3, data = entrenamiento)
summary(modelo2)

# Comprobamos la multilinealidad
vifs2 <- vif(modelo2)
vifs2
1/vifs2

# Como ya no hay problemas de multicolinealidad evaluamos los demas condiciones

# Comprobar independencia de los residuos
durbinWatsonTest(modelo2)
# Hay independencia de los los residuos con un intervalo de confiazna de 95%

# Comprobar normalidad de los residuos
shapiro.test(modelo2$residuals)
# No hay normalidad de los residuos con un W = 0.919 y p value = 0.007 

# Homocedasticidad Gráfico
plot(modelo2$fitted.values, resid(modelo2), xlab="Fitted values", ylab="Residuals", main="Homoscedasticity Check")
abline(h=0, col="red")

# Homocedasticidad 
ncvTest(modelo2)
# Existe homocedasticidad con un intervalo de confianza de 95%

# Normalidad de los residuos
qqnorm(modelo2$residuals)
qqline(modelo2$residuals, col="red")

# Evaluación del poder predictivo del modelo con validación cruzada
# Error con datos de entrenamiento
mean(modelo2$residuals ** 2)

predicciones <- predict(modelo2, prueba)

# Calcular error
error <- archivo$X1 - predicciones
mean(error ** 2)

mean(error ** 2) / mean(modelo2$residuals ** 2)

# la razon de los errores es el triple, indicando que el modelo esta sobre ajustado
# a los valores de entrenamiento.
# Por lo tanto no es posible predecir la variable X1 con el resto de variables, 
# eliminando un maximo de 3 y evitando problemas de multicolinealidad

