# Cargar las librerías necesarias
library(tidyverse)
library(caret)
library(pROC)
library(leaps)
library(car)

# 1 -Definir la semilla para la reproducibilidad de los resultados
set.seed(20847)

# Cargar los datos desde un archivo CSV
datos <- read.csv2("EP09 Datos.csv")

# Calcular el Índice de Masa Corporal (IMC) y determinar el estado nutricional
datos$IMC <- datos$Weight / ((datos$Height / 100) ** 2)
datos$EN <- ifelse(datos$IMC < 25, "No sobrepeso", "Sobrepeso")
datos$EN <- factor(datos$EN)

# 2 -Seleccionar muestras de individuos con y sin sobrepeso
sobrepeso <- datos %>% filter(EN == "Sobrepeso") %>% sample_n(50)
normal <- datos %>% filter(EN == "No sobrepeso") %>% sample_n(50)
muestra <- rbind(sobrepeso, normal)

# 3 - Usando las herramientas del paquete leaps, realizar una búsqueda exhaustiva 
# para seleccionar entre dos y ocho predictores que ayuden a estimar la variable 
# Peso (Weight), obviamente sin considerar las nuevas variables IMC ni EN, y luego 
# utilizar las funciones del paquete caret para construir un modelo de regresión lineal
# múltiple con los predictores escogidos y evaluarlo usando bootstrapping.

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
# Regresión lineal múltiple para la variable peso.

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

# Preparar los datos para el análisis, eliminando columnas no necesarias
datos.peso <- muestra %>% select(-c(IMC, EN))

# Seleccionar los mejores predictores usando el método de regresiones de subconjuntos
preliminar <- regsubsets(Weight ~ ., data = datos.peso, nbest = 1, nvmax = 8, method = "exhaustive")
plot(preliminar) 

# Construir un modelo de regresión lineal usando los predictores seleccionadoscomo Waist.Girth,Forearm.Girth y Height
modelo.peso <- train(Weight ~ Waist.Girth + Forearm.Girth + Height, data = datos.peso, method = "lm", trControl = trainControl(method = "boot", number = 1999))
print(summary(modelo.peso))

# El modelo obtenido presenta un R^2 ajustado de 0,899. Esto significa que el
# modelo obtenido explica el 89,9% de la variabilidad de los datos.

# Realizar predicciones con el modelo y calcular el error
predicciones.peso <- predict(modelo.peso, datos.peso)
error.peso <- datos.peso$Weight - predicciones.peso
rmse.peso <- sqrt(mean(error.peso ** 2))
cat("RMSE:", rmse.peso, "\n\n")  


# La raíz del error cuadrático medio para el modelo es de 4,153. Esto indica que
# los valores predichos se asemejan bastante a los valores observados, por lo
# que el modelo es bueno.

# Obtener residuos y estadísticas de influencia para evaluar la calidad del modelo
eval.rlm.peso <- data.frame(predicted.probabilities = fitted(modelo.peso$finalModel))
eval.rlm.peso$std.residuals <- rstandard(modelo.peso$finalModel)
eval.rlm.peso$studentized.residuals <- rstudent(modelo.peso$finalModel)
eval.rlm.peso$cooks.distance <- cooks.distance(modelo.peso$finalModel)
eval.rlm.peso$dfbeta <- dfbeta(modelo.peso$finalModel)
eval.rlm.peso$dffit <- dffits(modelo.peso$finalModel)
eval.rlm.peso$leverage <- hatvalues(modelo.peso$finalModel)
eval.rlm.peso$covariance.ratios <- covratio(modelo.peso$finalModel)

#----------------------------------
# Calculando residuos estandarizados
std_residuos <- rstandard(modelo.peso$finalModel)

# Identificando observaciones sospechosas 
sospechosos1 <- which(abs(std_residuos) > 1.96)

# Mostrando los índices de las observaciones sospechosas
cat("Observaciones con residuos estandarizados fuera del 95% de intervalo de confianza:", sospechosos1, "\n")

# Las observaciones con residuos estandarizados fuera del 95% son 26 y 47
#-----------------------------

# Calculando la distancia de Cook
cooks_distancias <- cooks.distance(modelo.peso$finalModel)

# Identificando observaciones sospechosas (distancia de Cook mayor que 1)
sospechosos2 <- which(cooks_distancias > 1)

# Mostrando los índices de las observaciones sospechosas
cat("Observaciones con distancia de Cook mayor que 1:", sospechosos2, "\n")

# Distancia de cook es 0
#------------------------------

# Calculando el apalancamiento promedio
# Aquí, k es el número de predictores y n es el número de observaciones
k <- ncol(datos.peso) - 1 # Restamos 1 para no contar la columna de la variable dependiente
n <- nrow(datos.peso)
apalancamiento_promedio <- (k + 1) / n

# Calculando el apalancamiento de cada observación
leverage_values <- hatvalues(modelo.peso$finalModel)

# Identificando observaciones con apalancamiento superior al doble del promedio
sospechosos3 <- which(leverage_values > 2 * apalancamiento_promedio)

# Mostrando los índices de las observaciones sospechosas y el apalancamiento promedio
cat("- Observaciones con apalancamiento fuera de rango (promedio = ", apalancamiento_promedio, "): ", sep = "")
print(sospechosos3)

# Apalancamiento promedio de 0.25 y no hay indices sospechosos
#--------------------------------

# Calculando DFBeta para todas las observaciones
dfbetas_valores <- dfbeta(modelo.peso$finalModel)

# Identificando las observaciones con al menos un DFBeta mayor o igual a 1
sospechosos4 <- apply(dfbetas_valores, 1, function(x) any(abs(x) >= 1))

# Obteniendo los índices de las observaciones sospechosas
indices_sospechosos4 <- which(sospechosos4)

# Mostrando los índices de las observaciones sospechosas
cat("- Observaciones con DFBeta mayor o igual a 1: ")
print(indices_sospechosos4)

# Indices de las observaciones sospechosas : 5   7  18  26  27  47  48  52  53  74  84  98  99 

# -----------------------------------
# Definiendo los límites inferior y superior para la razón de covarianza
CVRi_lower <- 1 - 3 * apalancamiento_promedio
CVRi_upper <- 1 + 3 * apalancamiento_promedio

# Calculando la razón de covarianza para cada observación
covariance_ratios <- covratio(modelo.peso$finalModel)

# Identificando observaciones cuya razón de covarianza está fuera de los límites recomendados
sospechosos5 <- which(covariance_ratios < CVRi_lower | covariance_ratios > CVRi_upper)

# Mostrando los índices de las observaciones sospechosas y los límites de razón de covarianza
cat("- Observaciones con razón de covarianza fuera de rango ([", CVRi_lower, ", ", CVRi_upper, "]): ", sep = "")
print(sospechosos5)

# - Observaciones con razón de covarianza fuera de rango ([0.25, 1.75]) : 0

# -----------------------------------------
sospechosos <- c(sospechosos1, sospechosos2, sospechosos3, sospechosos4,sospechosos5)

sospechosos <- sort(unique(sospechosos))
cat("\nResumen de observaciones sospechosas:\n")

print(round(eval.rlm.peso[sospechosos,c("cooks.distance", "leverage", "covariance.ratios")],3))

# cooks.distance leverage covariance.ratios
# X1           0.001    0.045             1.090
# X26          0.317    0.068             0.502
# X47          0.078    0.064             0.919


# Aunque se han identificado ciertas observaciones como potencialmente atípicas, es importante destacar que los valores de la
# distancia de Cook para estas observaciones están considerablemente alejados del umbral crítico de 1. En este contexto, esto sugiere que 
# su impacto en el modelo no es tan significativo como para generar preocupación.

# ------------------------------------------

# Realizando la prueba de Durbin-Watson en el modelo final
test_dw <- durbinWatsonTest(modelo.peso$finalModel)

# Mostrando los resultados de la prueba de Durbin-Watson
cat("\nIndependencia de los residuos\n")
print(test_dw)

# Puesto que la prueba de Durbin-Watson entrega p = 0,606, podemos concluir que
# los residuos son independientes.

# Podemos concluir que el modelo obtenido es confiable.


#°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
# Regresión lineal múltiple para la variable IMC.

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

# 4- Haciendo un poco de investigación sobre el paquete caret, en particular cómo hacer Recursive
# Feature Elimination (RFE), construir un modelo de regresión lineal múltiple para predecir la
# variable IMC que incluya entre 10 y 20 predictores, seleccionando el conjunto de variables que 
# maximice R2 y que use cinco repeticiones de validación cruzada de cinco pliegues para evitar el 
# sobreajuste (obviamente no se debe considerar las variables Peso, Estatura ni estado nutricional 
# –Weight, Height, EN respectivamente).

# Descartar columnas inútiles
datos.imc <- muestra %>% select(-c(Weight, Height, EN))

# Separar variable de respuesta de los predictores.
IMC <- datos.imc[["IMC"]]
datos.imc[["IMC"]] <- NULL

control <- rfeControl(functions = lmFuncs, method="repeatedcv",
                number=5, repeats=5, verbose = FALSE)

modelo.imc <- rfe(datos.imc, IMC, rfeControl = control, sizes = 10:20,
                  metric = "Rsquared")

print(modelo.imc)
cat("Variables seleccionadas:\n")
print(modelo.imc[["optVariables"]])

print(modelo.imc)
print(ggplot(modelo.imc))

# El gráfico muestra que R^2 se maximiza para 17 variables, con un valor de
# 86.83% (con una raíz del error cuadrático medio de 1,043).

# Se puede decir que el modelo obtenido se ajusta bien a los datos.


# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°
# Regresión logística múltiple .

# °°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°

# 5- Usando RFE, construir un modelo de regresión logística múltiple para la variable EN que
# incluya el conjunto, de entre dos y seis, predictores que entregue la mejor curva ROC y que 
## utilice validación cruzada dejando uno fuera para evitar el sobreajuste (obviamente no se debe
# considerar las variables Peso, Estatura –Weight y Height respectivamente– ni IMC).

# Descartar columnas inútiles
datos.en <- muestra %>% select(-c(IMC, Weight, Height))

# Separar variable de respuesta de los predictores.
EN <- datos.en[["EN"]]
datos.en[["EN"]] <- NULL

control.seleccion <- rfeControl(functions = lrFuncs, method = "LOOCV",
                                number = 1, verbose = FALSE)

control.entrenamiento <- trainControl(method = "none", classProbs = TRUE,
                                      summaryFunction = twoClassSummary)

modelo.en <- rfe(datos.en, EN, metric = "ROC", rfeControl = control.seleccion,
                 trControl = control.entrenamiento, sizes = 2:6)

print(modelo.en)

# Podemos ver que el modelo considera 3 variables: Chest.Girth , Bicep.GIrth y Navel.Girth
# Podemos ver gráficamente cómo varía el área bajo la curva ROC en cada
# iteración.
print(ggplot(modelo.en))

# El gráfico muestra que el área bajo la curva ROC se maximiza para 3 variables,Chest.Girth, Bicep.Girth, Navel.Girth
# con un valor de 91,16% (86% de sensibilidad y 84% de especificidad).

# Evaluar calidad predictiva del modelo.
predicciones <- predict(modelo.en, datos.en)[["pred"]]
cat("Calidad predictiva del modelo\n\n")
print(confusionMatrix(predicciones, EN))
#---------------------------------

# Verificando que las 3 variables escogidas esten deacuerdo al modelo 
data.en=cbind(datos.en, EN)
mm<-glm(EN~Chest.Girth+Bicep.Girth+Navel.Girth, data = data.en, family = "binomial")
vif(mm)

# Se puede apreciar que las 3 variables son buenas para el modelo, evidenciado por el vif.


