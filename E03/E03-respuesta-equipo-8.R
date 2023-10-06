# Si el ingeniero está seguro de que el verdadero volumen medio no puede ser 
# inferior a 10 litros y piensa rechazar la hipótesis nula cuando la muestra
# presente una media mayor a 10,2 litros, ¿cuál es la probabilidad de que 
# cometa un error de tipo I? Para responder, generen un gráfico de la 
# distribución muestral de las medias hipotetizada en donde se marque la zona 
# correspondiente a la probabilidad solicitada, para luego, basándose en este 
# gráfico, calcular el área correspondiente. Tome como ejemplo los scripts 
# presentados en la lectura sobre poder estadístico.

library(ggpubr)
library(dplyr)
library(tidyr)
library(pwr)
library(ggplot2)

# Dado que el ingeniero está seguro que la media no puede ser menor a 10
# y que desea rechazar la hipótesis nula cuando la media es > 10.2, entonces,
# se utiliza una prueba de una cola, es decir, solo se considera el lado derecho
# de la distribución para el alpha.

n = 100
media_nula = 10
sigma = 1

# Dado que conocemos la desviación de la población, podemos estimar la desviación
# de la muestra con la siguiente fórmula:
SE = sigma / sqrt(n)

# Crear un dataframe con datos de la distribución normal
df <- data.frame(x = seq(9, 11, by = 0.01), 
                 y = dnorm(seq(9, 11, by = 0.01), mean = media_nula, sd = SE))

# Crear el gráfico de la distribución normal
p <- ggplot(df, aes(x = x, y = y)) +
  geom_line() +  # Agregar una línea para representar la distribución normal
  theme_minimal() +  # Estilo minimalista del gráfico
  
  # Rellenar el área cuando la media es mayor a 10.2
  geom_area(data = subset(df, x > 10.2), aes(x = x, y = y), fill = "blue", alpha = 0.3) +
  
  # Línea vertical en la media
  geom_vline(xintercept = media_nula, linetype = "dashed", color = "red") +
  
  # Etiquetas y título
  labs(x = "Valor", y = "Densidad", 
       title = "Distribución Normal con Media 10 y Desviación Estándar 0.1") +
  
  # Personalización del tema
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Mostrar el gráfico
print(p)

# Luego calculamos la probabilidad asociada a la cola derecha cuando la media es
# mayor a 10.2 con la función pnorm, que equivale a la probabilidad de cometer
# error de tipo 1:
alfa <- pnorm(10.2, media_nula, SE, lower.tail = FALSE)
cat("P(Error tipo 1) =", alfa)

# --------------------------------------------------------------------

# Si el verdadero volumen medio de los bidones fuera de 10,1 litros, ¿cuál sería
# la probabilidad de que el ingeniero, que obviamente no conoce este dato, 
# cometa un error de tipo II? Para responder, agregue al gráfico anterior la 
# verdadera distribución muestral de las medias y marquen (con otro color) la 
# zona correspondiente a la probabilidad solicitada, para luego, basándose en 
# este gráfico, calcular el área correspondiente. También hay ejemplos de este 
# procedimiento en la lectura sobre poder estadístico. 

# Definir la media del efecto
media_efecto <- 10.1
# Definir los parámetros de la segunda distribución normal
media2 <- 10.1

# Crear un dataframe con datos de la segunda distribución normal
df2 <- data.frame(x = seq(9, 11, by = 0.01), 
                  y = dnorm(seq(9, 11, by = 0.01), mean = media_efecto, sd = SE))

# Crear el gráfico de la distribución normal con ambas curvas
p <- ggplot() +
  geom_line(data = df, aes(x = x, y = y), color = "red") +
  geom_area(data = subset(df, x >= 10.2), aes(x = x, y = y), fill = "red", alpha = 0.5) +
  geom_line(data = df2, aes(x = x, y = y), color = "blue") +
  geom_area(data = subset(df2, x <= 10.2), aes(x = x, y = y), fill = 'blue', alpha = 0.5) +
  theme_minimal() +
  labs(x = "Valor", y = "Densidad", 
       title = "Distribuciones Normales con Media 10 y 10.1, Desviación Estándar 0.1") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "none")

# Mostrar el gráfico
print(p)

# Luego, debemos calcular el área azul que es la probabilidad de cometer un error
# tipo II
beta <- pnorm(10.2, mean = media_efecto, sd = SE, lower.tail = TRUE)

# Luego beta (probabilidad de cometer error tipo II) es:

cat("P(Error tipo II) = ", beta)

# Lo cual tiene sentido debido a que la probabilidad de cometer error de tipo I
# es 0.023 por lo que la probabilidad de cometer error de tipo II debería ser 
# bastante grande, en este caso alrededor del 84%.

# --------------------------------------------------

# Como no se conoce el verdadero volumen medio, genere un gráfico del poder
# estadístico con las condiciones anteriores, pero suponiendo que el verdadero
# volumen medio podría variar de 10 a 10,4 litros. Hay un ejemplo de este tipo
# de gráfico en la lectura sobre poder estadístico. 

# Se genera un vector con un rango de valores para la efecto.

efecto <- seq(9, 11, 0.01)
efecto_delta <- efecto - media_nula

# Se consideran 1 escenario para calcular el poder estadistico:
# 1. Una muestra de tamaño 100 y significancia 0.05

alfa <- 0.05

# Calculamos el poder estadistico.

n100_alfa <- power.t.test(n = 100,
                           delta = efecto_delta,
                           sd = 1,
                           sig.level = alfa,
                           type = "one.sample",
                           alternative = "two.sided")$power

# Construir matriz de datos en formato ancho.
datos <- data.frame(efecto_delta, n100_alfa)

# Se lleva a formato largo.
datos <- datos %>% pivot_longer(!"efecto_delta",
                                names_to = "fuente",
                                values_to = "poder")

# Se formatea fuente como variable categorica.
niveles <- c("n100_alfa")

etiquetas <- c("n = 100, alfa = 0.05")

datos[["fuente"]] <- factor(datos[["fuente"]], levels = niveles, labels = etiquetas)

# Graficar curvas de poder
g <- ggplot(datos, aes(efecto_delta, poder, colour = factor(fuente)))
g <- g + geom_line()
g <- g + labs(colour = "")
g <- g + ylab("Poder estadistico")
g <- g + xlab("Tamaño del efecto")
g <- g + scale_color_manual(values = c("red"))
g <- g + theme_pubr()
g <- g + ggtitle("Curvas de poder para prueba t unilateral")
g <- g + geom_vline(xintercept = 0, linetype = "dashed")

print(g)

# Se observa que el instante en el que es minimo es en el instante que es 0.05.

# ----------------------------------------------

# Considerando un volumen medio de 10 litros, ¿cuántos bidones deberían 
# revisarse para conseguir un poder estadístico de 0,7 y 
# un nivel de significación de 0,05?

poder <- 0.7
alfa <- 0.05
diferencia <- media_efecto - media_nula

resultado <- power.t.test(n = NULL,
                          delta = diferencia,
                          sd = 1,
                          sig.level = alfa,
                          power = poder,
                          type = "one.sample",
                          alternative = "two.sided")

n_requerido <- ceiling(resultado[["n"]])

print(n_requerido)

# Se necesitan 620 bidones para conseguir un poder estadistico de 0.7

# ----------------------------------------------

# ¿Y si el ingeniero fuese muy exigente y quisiera reducir la probabilidad de
# cometer un error de tipo I a un 1% solamente?

poder <- 0.7
alfa <- 0.01
diferencia <- media_efecto - media_nula

resultado <- power.t.test(n = NULL,
                          delta = diferencia,
                          sd = 1,
                          sig.level = alfa,
                          power = poder,
                          type = "one.sample",
                          alternative = "two.sided")

n_requerido <- ceiling(resultado[["n"]])

print(n_requerido)

# Si es que se pone mas estricto, se necesitarian de 965 bidones para obtener
# un poder de 0,7