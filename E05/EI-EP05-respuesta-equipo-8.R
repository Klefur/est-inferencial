library(tidyverse)
library(nlme)
library(emmeans)
library(ggpubr)
library(ez)

# Carga el archivo CSV con read.csv()
data <- read.csv("EP05 Datos.csv", header = TRUE) # Cambia "tu_archivo.csv" por el nombre de tu archivo

# HIPOTESIS
# H0: El tiempo de consulta promedio para un problema difícil es igual para las 3 áreas
# HA: El tiempo de consulta promedio para un problema difícil es diferente para al menos un área.

# Filtramos por dificultad
dificultad_alta <- data[data$dificultad == "Alta", ]

# CONDICIONES PARA ANOVA
# 1. El tiempo está en escala lineal
# 2. Se asumen independientes ya que los experimentos no influyen entre sí

# Crear el data frame en formato ancho.
fisica <- dificultad_alta[dificultad_alta$area == "Física",]
arquitectura <- dificultad_alta[dificultad_alta$area == "Arquitectura",]
musica <- dificultad_alta[dificultad_alta$area == "Música",]

fisica_t <- c(fisica$tiempo)
arquitectura_t <- c(arquitectura$tiempo)
musica_t <- c(musica$tiempo)
datos <- data.frame(fisica_t, arquitectura_t, musica_t)

# Llevar data frame a formato largo.
datos <- datos %>% pivot_longer(c("fisica_t", "arquitectura_t", "musica_t"),
                                names_to = "area",
                                values_to = "tiempo")

datos[["area"]] <- factor(datos[["area"]])
datos[["instancia"]] <- factor(1:nrow(datos))

alfa <- 0.025

# Comprobación de normalidad.
g <- ggqqplot(datos,
              x = "tiempo",
              y = "area",
              color = "area")
g <- g + facet_wrap(~ area)
g <- g + remove("x_ticks") + remove("x_text")
g <- g + remove("y_ticks") + remove("y_text")
g <- g + remove("axis_title")
print(g)
# Se asume razonablemente que las poblaciones siguen una distribución normal ya que
# la mayoría de los puntos de la muestra para cada área se encuentran dentro de la
# franja de color.

# Procedimiento ANOVA con ezANOVA().
cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
anova <- ezANOVA(
  data = datos,
  dv = tiempo,
  between = area,
  wid = instancia,
  return_aov = TRUE)

print(anova)
# Si se define alfa=0.025, entonces p=0.001 < 0.025=alfa y por lo tanto rechazamos la hipotesis
# nula em favor de la hipotesis alternativa, es decir, hay evidencia suficiente para
# afirmar que existen diferencias en el tiempo que tardan los usuarios para formular
# una consulta para un problema de dificultad dificil en al menos una de las áreas estudiadas.

# Gráfico del tamaño del efecto.
g2 <- ezPlot(
  data = datos,
  dv = tiempo,
  wid = instancia,
  between = area,
  y_lab = "Tiempo promedio de prompt por area en dificultad alta",
  x = area)

print(g2)

# Post-hoc
# Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos[["tiempo"]], datos[["area"]],
                              p.adj = "bonferroni", pool.sd = TRUE, 
                              paired = FALSE, conf.level = 1 - alfa)
cat("Corrección de Bonferroni\n")
print(bonferroni)

# Procedimiento post-hoc de Holm. 
holm <- pairwise.t.test(datos[["tiempo"]], datos[["area"]],
                              p.adj = "holm", pool.sd = TRUE, 
                              paired = FALSE, conf.level = 1 - alfa)
cat("\nCorrección de Holm\n")
print(holm)

# Tanto Bonferroni como Holm evidencian que existe una diferencia entre los tiempos
# en que los usuarios formulan una consulta en nivel dificil entre fisica y musica

# Procedimiento post-hoc HSD de Tukey.
anova2 <- aov(tiempo ~ area, data = datos)
tukey <- TukeyHSD(anova2, "area", ordered = TRUE, conf.level = 1 - alfa)
print(tukey)
# La misma conclusión, existen diferencias entre física y música.