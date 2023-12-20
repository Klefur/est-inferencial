
library(lmtest)
library(car)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(boot)
# C:/Users/lucas/Documents/Git/EI/Ep8
# Se leen los datos con la codificación adecuada
datos <- read.csv2("EP09 Datos.csv")

# Verificar las primeras filas de los datos para asegurarse de que se leyeron correctamente
head(datos)

# La semilla selecionada es 2789 por lo que se procederá a filtrar los datos en una muestra de 50 hombres.

set.seed(2789)

# Parte RLS 
muestra <- datos %>% filter(Gender =='1')
muestra2 <- muestra[sample(nrow(muestra), 80), ]
muestra_ac <- muestra2[51:80, ]
muestra2 <- muestra2[0:50, ]

# Se seleccionan las 8 variables predictoras 
predictores_aleatorios <- sample(muestra2 ,8)

predictores_aleatorios

# Se seleciona la variable de Navel.Girth	, yq que el grosor a la altura del ombligo	podria ser una variable 
# influyente en el Peso.

# Construir un modelo de regresión lineal simple con el predictor seleccionado
modelo_simple <- lm(Weight ~ Navel.Girth, data=muestra2)
summary(modelo_simple)

# Evaluación de las condiciones para RLS en el gráfico
ggplot(muestra2, aes(x = Navel.Girth, y = Weight)) +
  geom_point() +
  geom_smooth(method = lm)

#Condiciones : 

# Se tiene un R cuadrado de 0,6365 dando así un R correlación de0,798 indicando una fuerte correlación positiva entre las dos variables.

# Evaluación de las condiciones para RLS
# Linealidad
ggplot(muestra2, aes(x = Navel.Girth, y = Weight)) +
  geom_point() +
  geom_smooth(method = lm)

# Normalidad de los residuos
shapiro.test(modelo_simple$residuals)
#Con W = 0,98848 y p value = 0,9044 . Evaluando los datos se puede decir que los residuos del modelo tienen una distribución normal.

# Homocedasticidad 1
bptest(modelo_simple)
#Con BP= 0,99603 y p value = 0.3183. Evalando los datos se puede decir que no hay una tendencia en los residuos y que el modelo parece tener risuduos con varianzas constantes .

# Independencia

durbinWatsonTest(modelo_simple)
#Con estadístico D-W = 2,325 y p value = 0,25 .Evaluando los datos se puede decir que al ser un valor cercano a 2, se tiene que no hay autocorrelación , cumpliendo con la idenpendecia.

# 
# Para interpretar estos resultados en el contexto del análisis, se puede concluir que, 
# basado en las pruebas estadísticas, el modelo de regresión lineal simple no viola las suposiciones de normalidad,
# homocedasticidad e independencia de los residuos. Esto significa que las estimaciones y pruebas inferenciales que
# realices con este modelo son estadísticamente válidas bajo el marco de la regresión lineal.

# Parte de RML
nulo <- lm(Weight ~ 1, data = muestra2)
colnames(predictores_aleatorios)
completo <- lm(Weight ~ Navel.Girth + Hip.Girth + Gender + Elbows.diameter + Wrists.diameter + Chest.diameter + Forearm.Girth + Ankle.Minimum.Girth + Ankles.diameter, data = muestra2)

# Selección hacia adelante utilizando AIC como criterio
modelo_seleccionado <- step(nulo, direction="forward", scope= list(upper = completo), trace = 0)
summary(modelo_seleccionado)

# Linealidad
plot(muestra2$Navel.Girth, modelo_seleccionado$fitted.values, xlab="Navel Girth", ylab="Fitted values", main="Linearity Check")
abline(lm(modelo_seleccionado$fitted.values ~ muestra2$Navel.Girth), col="red")


# Ahora evaluamos las condiciones del modelo

# Comprobar independencia de los residuos
durbinWatsonTest(modelo_seleccionado)
# p value de  0,538

# Comprobar normalidad de los residuos
shapiro.test(modelo_seleccionado$residuals)
# Con W = 0,9675 y p value= 0,1829

# Homocedasticidad 
ncvTest(modelo_seleccionado)
# Con Chisquare = 9,628342 y p value = 0,001916

# Comprobar la multilinealidad
vifs <- vif(modelo_seleccionado)
vifs
# Navel.Girth       Forearm.Girth           Hip.Girth      Chest.diameter Ankle.Minimum.Girth 
# 3.292884            2.120205            4.445103            2.358246            1.786130 
print(1/vifs)

# Homocedasticidad Gráfico
plot(modelo_seleccionado$fitted.values, resid(modelo_seleccionado), xlab="Fitted values", ylab="Residuals", main="Homoscedasticity Check")
abline(h=0, col="red")

# Normalidad de los residuos
qqnorm(modelo_seleccionado$residuals)
qqline(modelo_seleccionado$residuals, col="red")

# Evaluación del poder predictivo del modelo con validación cruzada
# Error con datos de entrenamiento
print(mean(modelo_seleccionado$residuals ** 2))
# 8.56

# Usar muestras nuevas
predicciones <- predict(modelo_seleccionado, muestra_ac)

# Calcular error
error <- muestra_ac$Weight - predicciones
print(mean(error ** 2))
# Más de el doble del error de los datos de entrenamiento, el modelo está sobre ajustado a los datos