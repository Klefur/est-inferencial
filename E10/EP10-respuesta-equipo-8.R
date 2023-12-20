# Cargar librerías necesarias
library(pROC)

# Leer y preparar los datos
archivo <- read.csv2("EP09 Datos.csv")
archivo$Height <- archivo$Height / 100
archivo$IMC <- archivo$Weight / archivo$Height^2
archivo$EN <- ifelse(archivo$IMC < 25, 1, 0)
archivo$EN <- factor(archivo$EN)

# 1-Se selecciona la semilla 
set.seed(7708)

# 2- Se filtran los datos a 90 mujeres 
mujeres <- archivo[archivo$Gender == 0, ]
sobrepeso <- mujeres[mujeres$EN == 1, ]
normal <- mujeres[mujeres$EN == 0, ]

sobrepeso45 <- sobrepeso[sample(nrow(sobrepeso), 45), ]
sobrepeso30 <- sobrepeso45[1:30, ]
sobrepeso15 <- sobrepeso45[31:45, ]

normal30 <- normal[1:30, ]
normal15 <- normal[31:45, ]

# Combinar conjuntos de entrenamiento y prueba
entrenamiento <- rbind(sobrepeso30, normal30)
prueba <- rbind(sobrepeso15, normal15)


# Antiguo
#Hip.Girth + Gender + Elbows.diameter + Wrists.diameter + 
#Chest.diameter + Forearm.Girth + Ankle.Minimum.Girth + Ankles.diameter

#Chest.Girth

# 4-Se seleccionar Chest.Girth una variable que podría influir en el EN
# 5-Crear regresión logística 
modelo <- glm(EN ~ Chest.Girth, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo))

# Definir el umbral para la clasificación
umbral <- 0.5

# Evaluar el modelo con el conjunto de entrenamiento
probs_e <- predict(modelo, entrenamiento, type = "response")
preds_e <- ifelse(probs_e >= umbral, 1, 0)

# Generar la curva ROC para el conjunto de entrenamiento
ROC_e <- roc(response = entrenamiento$EN, predictor = probs_e)
plot(ROC_e)

# Crear matriz de confusión para el conjunto de entrenamiento
table(Prediction = preds_e, Actual = entrenamiento$EN)

# Evaluar el modelo con el conjunto de prueba
probs_p <- predict(modelo, prueba, type = "response")
preds_p <- ifelse(probs_p >= umbral, 1, 0)

# Generar la curva ROC para el conjunto de prueba
ROC_p <- roc(response = prueba$EN, predictor = probs_p)
plot(ROC_p)

# Crear matriz de confusión para el conjunto de prueba
table(Prediction = preds_p, Actual = prueba$EN)

# 6- Seleccionar variables adicionales y agregarlas al modelo
# Variables antiguas: Hip.Girth, Gender, Elbows.diameter, Wrists.diameter, Chest.diameter,
# Forearm.Girth, Ankle.Minimum.Girth, Ankles.diameter
# Seleccionar al menos dos de estas para agregar al modelo
modelo_ampliado <- glm(EN ~ Chest.Girth + Hip.Girth + Wrists.diameter, family = binomial(link = "logit"), data = entrenamiento)
print(summary(modelo_ampliado))

# Evaluar la confiabilidad y ajuste del modelo ampliado
# Puede incluir evaluación de residuos, multicolinealidad, etc.

# 7- Evaluar el poder predictivo del modelo ampliado con el conjunto de prueba
probs_p_ampliado <- predict(modelo_ampliado, prueba, type = "response")
preds_p_ampliado <- ifelse(probs_p_ampliado >= umbral, 1, 0)

# Generar la curva ROC para el modelo ampliado con el conjunto de prueba
ROC_p_ampliado <- roc(response = prueba$EN, predictor = probs_p_ampliado)
plot(ROC_p_ampliado)

# Crear matriz de confusión para el modelo ampliado con el conjunto de prueba
confusion_matrix_ampliado <- table(Prediction = preds_p_ampliado, Actual = prueba$EN)

# Calcular sensibilidad y especificidad a partir de la matriz de confusión
verdaderos_positivos <- confusion_matrix_ampliado[2, 2]
falsos_negativos <- confusion_matrix_ampliado[2, 1]
verdaderos_negativos <- confusion_matrix_ampliado[1, 1]
falsos_positivos <- confusion_matrix_ampliado[1, 2]

sensibilidad_ampliado <- verdaderos_positivos / (verdaderos_positivos + falsos_negativos)
especificidad_ampliado <- verdaderos_negativos / (verdaderos_negativos + falsos_positivos)

print(paste("Sensibilidad:", sensibilidad_ampliado))
print(paste("Especificidad:", especificidad_ampliado))

