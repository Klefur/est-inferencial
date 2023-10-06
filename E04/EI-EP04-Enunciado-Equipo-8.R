# PREGUNTA 1
# Una popular discoteca quiere analizar el interés que generan los concursos de 
# baile entre sus visitantes más asiduos. Para ello ha invitado a sus mejores 
# clientes, 8 hombres y 10 mujeres, a un evento privado con la posibilidad de 
# participar en un concurso. 6 mujeres y 6 hombres decidieron participar. ¿Influye el
# género en la participación en concursos de baile?

# Construimos la tabla de contingencia
# Crear un dataframe con los datos
discoteca_data <- data.frame(
  Genero = c(rep("Hombre", 8), rep("Mujer", 10)),
  Participacion = c(rep("Participante", 6), rep("No Participante", 2), 
                    rep("Participante", 6), rep("No Participante", 4))
)

# Crear la tabla de contingencia
tabla_contingencia <- table(discoteca_data$Genero, discoteca_data$Participacion)

# Mostrar la tabla de contingencia
print(tabla_contingencia)

# Luego, en base al problema, lo más adecuado pareciera usar la prueba Chi-cuadrado
# de independencia, por lo que necesitamos verificar las condiciones. Sin embargo,
# al calcular el tamaño esperado de la muestra para los nombres no participantes:

E <- 8*6 / 16
print(E)

# Lo cual es menor a 5, por lo que usaremos la prueba fisher con un nivel de confianza
# del 0.05 con las siguientes hipótesis:
# H0: los generos no influyen en la participación del concurso
# Ha: los géneros si influyen en la participación del concurso

alfa <- 0.05
fisher.test(tabla_contingencia, 1-alfa)

# Como el p-value=0.638 > alfa, entonces no se puede rechazar h0, y por lo tanto
# no hay evidencia suficiente para decir que el género influye en la participación
# del baile

# --------------------------------------------------------

# Pregunta 2
# Un afamado nutricionista ha desarrollado un nuevo producto para adelgazar. Ha decidido probarlo con 20
# voluntarios con tendencia a engordar y que siguen una estricta dieta por un periodo de 3 meses. Tras el
# periodo de prueba:
# 5 participantes se mantuvieron en el nivel de sobrepeso.
# 4 participantes se mantuvieron en el nivel de obesidad.
# 8 participantes pasaron de tener sobrepeso a la condición de obesidad.
# 3 participantes pasaron de ser obesos a tener sobrepeso.
# ¿Qué se puede decir acerca del nuevo producto para adelgazar?

# Crear un dataframe con los datos
medicamento <- data.frame(
  Antes = c(rep("Sobrepeso", 13), rep("Obesidad", 7)),
  Despues = c(rep("Sobrepeso", 5), rep("Obesidad", 8), 
              rep("Obesidad", 4), rep("Sobrepeso", 3))
)

# Crear la tabla de contingencia
tabla_medicamento <- table(medicamento$Antes, medicamento$Despues)

# Mostrar la tabla de contingencia
print(tabla_medicamento)

# Dado que la variable involucrada es dicotómica y se quiere medir el impacto del
# nuevo producto para adelgazar, dado si se produce o no un cambio significativo en 
# las personas, usaremos el test de mcNemar donde las hipótesis son:

# h0: no hay cambio de peso significativa en las personas
# ha: hay cambio de peso significativo en las personas

mcnemar.test(tabla_medicamento)

# Dado que el p-value=0.2278, si consideramos un nivel de significancia de alfa=0.05
# podemos concluir que no hay cambios significativos en el peso de las personas
# al usar el medicamento, por lo que no podemos rechazar la h0.

# ------------------------------------------------------------------------------
# Pregunta 3
# En noviembre de 2019, se realizó un estudio acerca de la aprobación al presidente
# Sebastián Piñera entre 431 profesores y estudiantes de una prestigiosa 
# universidad, obteniéndose los resultados que se muestran en la tabla. 
# ¿Son similares las opiniones de ambos segmentos de la comunidad universitaria?

# Se usara hi-cuadrado de independencia ya que permite evaluar si hay una 
# asociación significativa entre las dos variables

aprueba <- c(177, 77, 21)
rechaza <- c(87, 85, 36)

tabla <- as.table(rbind(aprueba, rechaza))

dimnames(tabla) <- list(opinion = c("Aprueba", "Rechaza"),
                        asociacion = c("Estudiante", "Profesor", "Funcionario"))
print(tabla)

# Hacer prueba chi-cuadrado de independencia.
# Se definen las hipotesis
# H0 = las variables opinion y asociacion son independientes
# HA = las variables opinion y asociacion estan relacionadas
prueba <- chisq.test(tabla)
cat("\nLa prueba internamente calcula los valores esperados:\n")
esperados <- round(prueba[["expected"]], 3)
print(esperados)

cat("\nResultado de la prueba:\n")
print(prueba)

# Con una significancia de 0.05 no tenemos evidencia suficiente para rechazar 
# la hipótesis nula en este estudio.

# -----------------------------------------------------------------------------
# Pregunta 4
library(readxl)
library(tidyverse)
library(RVAideMemoire)
library(rcompanion)

# La Facultad de Ingeniería desea saber si existe diferencia significativa en el 
# desempeño de los estudiantes en asignaturas críticas de primer semestre. 
# Para ello, le ha entregado un archivo de datos que, para 3 asignaturas, 
# indica si una muestra de 50 estudiantes aprobó o reprobó. ¿Qué puede concluir 
# la Facultad? Indicación: obtenga la muestra a partir del archivo “EP04 Datos.csv” 
# que se encuentra en el directorio compartido, usando la semilla 347. 
# Considere un nivel de significación α=0,05.

set.seed(347)

alfa <- 0.05

# se utiliza la prueba de cochran ya que trabajamos con mas de dos observaciones
# pareadas.
# por lo que definimos como hipotesis:

# H0: la proporcion de exito es la misma para todos los grupos.
# Ha: la proporcion de exito es distinta en almenos un grupo.

# verificamos las condinciones y vemos que cumple con todo.

# matriz de datos.
datos <- read_excel("EP04 Datos.xls")

datos <- datos %>% pivot_longer(c("Calculo", "Algebra", "Fisica"),
                                names_to = "Ramos",
                                values_to = "Resultado")

datos[["Id"]] <- factor(datos[["Id"]])
datos[["Ramos"]] <- factor(datos[["Ramos"]])

# Hacer prueba Q de Cochran.
prueba <- cochran.qtest(Resultado ~ Ramos | Id,
                        data = datos, alpha = alfa)

# con una significancia de 0.05, la investigacion no presenta sufienciente
# evidencia para rechazar la hipotesis nula.