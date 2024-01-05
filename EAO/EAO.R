library(tidyverse)
library(ez)
library(boot)
library(ggpubr)
library(car)
library(lmtest)
library(leaps)
library(pROC)

# Pregunta 1

datos = read.csv2("EI-2023-2-EAO-P1-Datos-PM.csv", sep = ",")
datos$V0.5 = as.numeric(datos$V0.5)
datos$V0.6 = as.numeric(datos$V0.6)
datos$V0.7 = as.numeric(datos$V0.7)
datos$V0.8 = as.numeric(datos$V0.8)
datos$V0.9 = as.numeric(datos$V0.9)

# Definimos hipotesis

# H0: La media del RMSE es igual para todos los porcentajes de las series de tiempo
# Ha: La media del RMSE es distinta para al menos uno de los porcentajes de las series de tiempo

# Denotando:

# H0: μ_RMSE_1 = μ_RMSE_2 = μ_RMSE_3 = μ_RMSE_4 = μ_RMSE_5
# Ha: μ_RMSE_1 != μ_RMSE_2 = μ_RMSE_3 != μ_RMSE_4 = μ_RMSE_5

# Ya que estamos comparando la media de más de 2 grupos, remuestraremos el 
# estadistico F

# Pasamos los datos a formato largo
datos_combinados <- data.frame(
  porcentaje = rep(c("V0.5", "V0.6", "V0.7", "V0.8", "V0.9"), each = 37),
  rmse = c(datos$V0.5, datos$V0.6, datos$V0.7, datos$V0.8, datos$V0.9),
  Day = factor(rep(datos$Day, 5))
)
datos_combinados$porcentaje = factor(datos_combinados$porcentaje)

# Realizamos una prueba ANOVA correlacionada ya que trabajamos sobre el mismo modelo.
anova_resultado <- ezANOVA(datos_combinados, dv = rmse, within = porcentaje,
                           wid = Day, return_aov = TRUE)
valor_observado <- anova_resultado[["ANOVA"]][["F"]]

# Generar permutaciones.
R <- 1999

# Verificar condición de normalidad.
g <- ggqqplot(datos_combinados, "rmse", facet.by = "porcentaje", color = "porcentaje")
print(g)
# Todos cumplen con el requisito de normalidad

# Función para obtener una permutación.
obtiene_permutacion <- function(i, df_ancho) {
  df_ancho[, 2:6] <- t(apply(df_ancho[, 2:6], 1, sample))
  return(df_ancho)
}

# Obtener permutaciones.
permutaciones <- lapply(1:R, obtiene_permutacion, datos)

# Función para obtener el estadístico F para una matriz de datos con formato ancho.
obtiene_F <- function(df_ancho) {
  df_largo <- df_ancho %>% pivot_longer ( c ( "V0.5", "V0.6", "V0.7", "V0.8", "V0.9" ),
                                                 names_to = "porcentaje" ,
                                                 values_to = "rmse" )
  
  df_largo$porcentaje = factor(df_largo$porcentaje)
  df_largo$Day = factor(df_largo$Day)
  
  anova_resultado <- ezANOVA(df_largo, dv = rmse, within =  porcentaje, wid = Day, return_aov = TRUE)
  return(anova_resultado[["ANOVA"]][["F"]])
}

# Genera distribución de estadísticos F con las permutaciones.
distribucion <- sapply(permutaciones, obtiene_F)

# Obtener valor p.
p <- (sum(distribucion > valor_observado) + 1) / (R + 1)
cat("ANOVA de una vía para muestras independientes con permutaciones \n")
cat("p =", p, "\n\n")

# Análisis post-hoc.

# Función para calcular la media de las diferencias para dos columnas de una
# matriz de datos en formato ancho.
obtiene_media_difs <- function(df_ancho, columna_1, columna_2) {
  media <- mean(df_ancho[[columna_1]] - df_ancho[[columna_2]])
  return(media)
}

# Obtiene las medias de las diferencias observadas
dif_obs_0.5_0.6 <- obtiene_media_difs(datos, "V0.5", "V0.6")
dif_obs_0.5_0.7 <- obtiene_media_difs(datos, "V0.5", "V0.7")
dif_obs_0.5_0.8 <- obtiene_media_difs(datos, "V0.5", "V0.8")
dif_obs_0.5_0.9 <- obtiene_media_difs(datos, "V0.5", "V0.9")
dif_obs_0.6_0.7 <- obtiene_media_difs(datos, "V0.6", "V0.7")
dif_obs_0.6_0.8 <- obtiene_media_difs(datos, "V0.6", "V0.8")
dif_obs_0.6_0.9 <- obtiene_media_difs(datos, "V0.6", "V0.9")
dif_obs_0.7_0.8 <- obtiene_media_difs(datos, "V0.7", "V0.8")
dif_obs_0.7_0.9 <- obtiene_media_difs(datos, "V0.7", "V0.9")
dif_obs_0.8_0.9 <- obtiene_media_difs(datos, "V0.8", "V0.9")

# Obtiene las distribuciones de las medias de las diferencias permutadas
dist_medias_0.5_0.6 <- sapply(permutaciones, obtiene_media_difs, "V0.5", "V0.6")
dist_medias_0.5_0.7 <- sapply(permutaciones, obtiene_media_difs, "V0.5", "V0.7")
dist_medias_0.5_0.8 <- sapply(permutaciones, obtiene_media_difs, "V0.5", "V0.8")
dist_medias_0.5_0.9 <- sapply(permutaciones, obtiene_media_difs, "V0.5", "V0.9")
dist_medias_0.6_0.7 <- sapply(permutaciones, obtiene_media_difs, "V0.6", "V0.7")
dist_medias_0.6_0.8 <- sapply(permutaciones, obtiene_media_difs, "V0.6", "V0.8")
dist_medias_0.6_0.9 <- sapply(permutaciones, obtiene_media_difs, "V0.6", "V0.9")
dist_medias_0.7_0.8 <- sapply(permutaciones, obtiene_media_difs, "V0.7", "V0.8")
dist_medias_0.7_0.9 <- sapply(permutaciones, obtiene_media_difs, "V0.7", "V0.9")
dist_medias_0.8_0.9 <- sapply(permutaciones, obtiene_media_difs, "V0.8", "V0.9")

# Obtener valores p.
num <- sum(abs(dist_medias_0.5_0.6) > abs(dif_obs_0.5_0.6)) + 1
den <- R + 1
p_0.5_0.6 <- num / den

num <- sum(abs(dist_medias_0.5_0.7) > abs(dif_obs_0.5_0.7)) + 1
den <- R + 1
p_0.5_0.7 <- num / den

num <- sum(abs(dist_medias_0.5_0.8) > abs(dif_obs_0.5_0.8)) + 1
den <- R + 1
p_0.5_0.8 <- num / den

num <- sum(abs(dist_medias_0.5_0.9) > abs(dif_obs_0.5_0.9)) + 1
den <- R + 1
p_0.5_0.9 <- num / den

num <- sum(abs(dist_medias_0.6_0.7) > abs(dif_obs_0.6_0.7)) + 1
den <- R + 1
p_0.6_0.7 <- num / den

num <- sum(abs(dist_medias_0.6_0.8) > abs(dif_obs_0.6_0.8)) + 1
den <- R + 1
p_0.6_0.8 <- num / den

num <- sum(abs(dist_medias_0.6_0.9) > abs(dif_obs_0.6_0.9)) + 1
den <- R + 1
p_0.6_0.9 <- num / den

num <- sum(abs(dist_medias_0.7_0.8) > abs(dif_obs_0.7_0.8)) + 1
den <- R + 1
p_0.7_0.8 <- num / den

num <- sum(abs(dist_medias_0.7_0.9) > abs(dif_obs_0.7_0.9)) + 1
den <- R + 1
p_0.7_0.9 <- num / den

num <- sum(abs(dist_medias_0.8_0.9) > abs(dif_obs_0.8_0.9)) + 1
den <- R + 1
p_0.8_0.9 <- num / den

cat("\n\n")
cat("Análisis post-hoc (permutaciones) para la diferencia de las medias\n")
cat("--------------------------------------------------------------\n")
cat("Valores p:\n")

cat(sprintf("0.5 - 0.6: %.3f\n", p_0.5_0.6))
cat(sprintf("0.5 - 0.7: %.3f\n", p_0.5_0.7))
cat(sprintf("0.5 - 0.8: %.3f\n", p_0.5_0.8))
cat(sprintf("0.5 - 0.9: %.3f\n", p_0.5_0.9))
cat(sprintf("0.6 - 0.7: %.3f\n", p_0.6_0.7))
cat(sprintf("0.6 - 0.8: %.3f\n", p_0.6_0.8))
cat(sprintf("0.6 - 0.9: %.3f\n", p_0.6_0.9))
cat(sprintf("0.7 - 0.8: %.3f\n", p_0.7_0.8))
cat(sprintf("0.7 - 0.9: %.3f\n", p_0.7_0.9))
cat(sprintf("0.8 - 0.9: %.3f\n", p_0.8_0.9))

cat("\nDiferencias observadas:\n")

cat(sprintf("0.5 - 0.6: %.3f\n", dif_obs_0.5_0.6))
cat(sprintf("0.5 - 0.7: %.3f\n", dif_obs_0.5_0.7))
cat(sprintf("0.5 - 0.8: %.3f\n", dif_obs_0.5_0.8))
cat(sprintf("0.5 - 0.9: %.3f\n", dif_obs_0.5_0.9))
cat(sprintf("0.6 - 0.7: %.3f\n", dif_obs_0.6_0.7))
cat(sprintf("0.6 - 0.8: %.3f\n", dif_obs_0.6_0.8))
cat(sprintf("0.6 - 0.9: %.3f\n", dif_obs_0.6_0.9))
cat(sprintf("0.7 - 0.8: %.3f\n", dif_obs_0.7_0.8))
cat(sprintf("0.7 - 0.9: %.3f\n", dif_obs_0.7_0.9))
cat(sprintf("0.8 - 0.9: %.3f\n", dif_obs_0.8_0.9))

# La evaluacion anova nos indica con un p-value de 0.002 que se rechaza la hipotesis nula,
# en favor de la alternativa por lo que realizamos post-hoc podemos observar en que grupos
# existen las diferencias, y vemos que son entreL
# V0.5 - V0.8, V0.5 - V0.9, V0.6 - V0.9 y V0.7 - V0.9. 
# concluyendo que el porcentaje de las series de tiempo si afecta en el rendimiento
# del modelo.

# Pregunta 2

set.seed(103)

datos <- read.csv2("EI-2023-2-EAO-P2-Datos-BC.csv", sep = ",")
datos[["diagnosis"]] <-  factor(datos[["diagnosis"]])
datos_num <- as.data.frame(lapply(datos, function(x) as.numeric(as.character(x))))
datos_num$diagnosis <- datos$diagnosis
datos <- datos_num
entrenamiento_index <- sample.int(n=nrow(datos) , size=200, replace=FALSE)
entrenamiento<- datos[entrenamiento_index, ]
prueba <- datos [-entrenamiento_index, ]
prueba_index <- sample.int(n=nrow(prueba) , size=200, replace=FALSE)
prueba <- prueba[prueba_index, ]

#Vamos a desordenar la muestra para que no queden ordenados los grupos
entrenamiento <- entrenamiento[sample(1:nrow(entrenamiento)), ]
prueba <- prueba[sample(1:nrow(prueba)), ]

# Verificamos que no cometimos algún error con las muestras
stopifnot(all(entrenamiento$Id == unique(entrenamiento$Id)))
stopifnot(all(prueba$Id == unique(prueba$Id)))
stopifnot(!any(entrenamiento$Id %in% prueba))
#Se eligen predictores que otorgue el menor BIC posible y se prueban con vif
preliminar <- regsubsets(diagnosis ~ ., data = entrenamiento, nbest = 1, nvmax = 15, method = "exhaustive")
plot(preliminar)

modelo_inicial <- glm(diagnosis ~ smoothness_se, data = entrenamiento, family = binomial(link = "logit"))
modelo <- update(modelo_inicial, . ~ . + radius_worst)
vif(modelo)
modelo <- update(modelo, . ~ . + texture_mean)
vif(modelo)
#Se observa que las variables predictoras cumplen con el requisito de tener VIF < 5


#Se procede a comprobar de las relaciones entre predictores y variable de respuesta.
xm1 <- data.frame(Logit = log(fitted(modelo)/(1-fitted(modelo))),
                  smoothness_se = entrenamiento[["smoothness_se"]],
                  radius_worst = entrenamiento[["radius_worst"]],
                  texture_mean = entrenamiento[["texture_mean"]])
xm1.l <- pivot_longer(xm1, -Logit, names_to = "Predictor", values_to = "Valor")
pxm1 <- ggscatter(data = xm1.l, x = "Logit", y = "Valor", conf.int = TRUE) +
  geom_smooth(method = "loess") + 
  theme_bw() +
  facet_wrap(~ Predictor, scales = "free_y")
print(pxm1)

#Se puede observar que la variable predictora radius_worst si sigue una relación lineal
# evidente exceptuando ciertos valores atípicos, en cambio las otras variables, no tienen
# relación lineal con la variable predictora, teniendo gran error al predecirlos, con 
# anterior se concluye que se viola la condición de relaciones lineales entre predictores y variable de respuesta.

# Revisamos que los residuos sean independientes
cat("\nPrueba de Durbin y Watson:\n")
print(durbinWatsonTest(modelo))
#Se observa con un p-value de 0.198 por lo que no hay razones para sospechar que los residuos
#no sean independientes

#Revisamos que los residuos tengan una distribución normal
xm2 <- data.frame(Indice = 1:nrow(entrenamiento), diagnosis = entrenamiento$diagnosis,
                  Residuo.estandarizado = rstandard(modelo))
pxm2 <- ggscatter(data = xm2, x = "Indice", y = "Residuo.estandarizado", color = "diagnosis")
print(pxm2)

shapiro.test(xm2$Residuo.estandarizado)
hist(xm2$Residuo.estandarizado)
#Con el p-value obtenido menor a 0.05 se rechaza la hipótesis nula a favor de la alternativa
#que indica que los residuos del modelo no siguen una distribución normal.
#Adicionalmente con tal de verificar la afirmación se genera el histograma de los residuos,
# y se puede observar que los valores relativamente siguen la distribución normal, pero
# se presentan problemas dadas las colas que tiene y un desbalance hacia los negativos.
# Este aspecto no es tan grave ya que la gran mayoría de los datos si siguen relativamente bien
# la distribución normal.

#Se revisa homocedasticidad
print(bptest(modelo))
#Con el p-value se determina que existe heterocedasticidad en el modelo, lo cual puede
# afectar la precisión y calidad del modelo.

# Observaciones con distancia de Cook mayor a uno.
eval.rlogs <- data.frame(standardized.residuals = rstandard(modelo))
eval.rlogs[["cooks.distance"]] <- cooks.distance(modelo)
sospechosos <- which(eval.rlogs[["cooks.distance"]] > 1)
cat("- Residuos con distancia de Cook mayor que 1: ")
print(sospechosos)
#Se observa que no existen datos con distancia de cook problemática.

#Se procede a evaluar la calidad predictiva del modelo primero con los valores de entrenamiento y
# después con valores de prueba.
ROC_entrenamiento <- roc(entrenamiento$diagnosis, predict(modelo, entrenamiento))
plot(ROC_entrenamiento)
ROC_prueba <- roc(prueba$diagnosis, predict(modelo, prueba))
plot(ROC_prueba)


#Se puede observar que el modelo tiene un buen poder predictivo con respecto a
# los datos de entrenamiento alejándose de la diagonal lo que indica que se posee buena sensibilidad y especificidad
# determinándose que el modelo no tiene tendencias sobre su aceptación o negación.
# Posteriormente se realiza la curva de ROC con los valores de prueba usando el modelo y se observa un
# buen comportamiento, teniendo una buena capacidad de detectar tanto casos positivos como negativos,
# por lo que no se tiene una tendencia fuerte a ninguna de estas, otorgándonos una buena calidad predictiva.

#Conclusion: Si bien se observan problemas en la verificación de condiciones como la homocedasticidad, linealidad
# y normalidad de los residuos que afectan al modelo y su confiabilidad, al momento de generar las pruebas de calidad
# de predicción, el modelo se comporta correctamente, teniendo una buena sensibilidad y especificidad tanto en los datos
# de entrenamiento y en los de prueba, por lo que el modelo si posee una buena calidad de predicción inclusive con datos fuera
# de su construcción.

