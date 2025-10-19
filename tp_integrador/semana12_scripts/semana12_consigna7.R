
# Santiago Octavio Varela - <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-10-18

# PyE - TPI - Semana 12, consigna 7

# readxl para la lectura del archivo
if(!require(readxl)) install.packages("readxl")
library(readxl)

# Leemos el archivo de excel propuesto.
archivo <- "assets/TUPAD-2025-EST-TPI-planilla5.xlsx"
datos <- read_excel(archivo)

# a. Construimos la tabla de frecuencias en base a `ESTATURA CM.`

# Seleccionamos los datos de la columna y omitimos valores nulos si los hubiera
estaturas <- na.omit(datos$`ESTATURA CM.`)

# Cantidad total de observaciones
n <- length(estaturas)

# Número óptimo de intervalos (clases) siguiendo la regla de Sturges
k <- ceiling(1 + 3.322 * log10(n))

# Mensaje informativo sobre los intervalos
print(paste("Número óptimo de intervalos (k):", k))

# Calculamos el rango y la amplitud
rango <- range(estaturas)
amplitud <- ceiling((rango[2] - rango[1]) / k)

# Creamos los intervalos (breaks) y las clases (clases)
breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut(estaturas, breaks = breaks, right = FALSE)

# Creamos la tabla de frecuencias completa
tabla_estaturas <- table(clases)
f_acum_estaturas <- cumsum(tabla_estaturas)
f_rel_estaturas <- prop.table(tabla_estaturas)
f_rel_acum <- cumsum(f_rel_estaturas)

# Construimos el dataframe final de la tabla
tabla_estaturas_frecuencias <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_estaturas),
  Frec_Acumulada = as.vector(f_acum_estaturas),
  Frec_Relativa = round(as.vector(f_rel_estaturas), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)

# Mostramos la tabla de frecuencias
print("Tabla de frecuencias para 'ESTATURA CM.'")
print(tabla_estaturas_frecuencias)

# CÁLCULO DE MEDIA Y DESVIACIÓN ESTÁNDAR PARA DATOS AGRUPADOS

# Definir la variable para los mensajes posteriores
variable_continua <- "ESTATURA CM."

# Vector de frecuencias absolutas (ya calculado)
frecuencias <- as.vector(tabla_estaturas)

# Total de observaciones (ya calculado)
n_total <- n

# Vector de límites inferiores de cada intervalo
cortes <- breaks[-length(breaks)]

# Vector de marcas de clase (punto medio de cada intervalo)
marca_clase <- (cortes + cortes + amplitud) / 2

# Cálculo de la media para datos agrupados
media_agrupada <- sum(marca_clase * frecuencias) / n_total

# Cálculo de la varianza para datos agrupados
varianza_agrupada <- sum(frecuencias * (marca_clase - media_agrupada)^2) / (n_total - 1)

# Cálculo del desvío estándar para datos agrupados
desvio_agrupado <- sqrt(varianza_agrupada)

# ---
# MUESTRA DE RESULTADOS
# ---

# Mostrar resultados de los cálculos principales
message("\n Resultados de Media y Desviación Estándar - VARIABLE CONTINUA (", variable_continua, ")")

# Tabla resumen con las medidas calculadas
stats_agrupados <- data.frame(
  Media_Agrupada = round(media_agrupada, 2),
  Desvio_Estandar_Agrupado = round(desvio_agrupado, 2)
)

print(stats_agrupados, row.names = FALSE)

# Mensaje final de esta sección
message("\n Fin de los cálculos de media y desvío estándar para nuestra base de datos. Estos valores son aproximaciones basadas en los datos agrupados.")

message("\n CÁLCULO DE PROBABILIDAD (PUNTO 7.a)")


# a. Calcular la probabilidad de que un estudiante tenga una estatura mayor o igual que 179 cm.
# Usamos la función pnorm() que calcula la probabilidad en una distribución normal.

# Definimos el valor de la estatura que nos interesa y lo almacenamos en `estatura_limite`
estatura_limite <- 179

# Calculamos la probabilidad P(X >= 179), almacenando el resultado en `probabilidad_a`
# Usamos los valores de media y desvío calculados previamente.
# El argumento `lower.tail` es `FALSE` porque P(X > xi),

probabilidad_a <- pnorm(estatura_limite, 
                        mean = media_agrupada, 
                        sd = desvio_agrupado, 
                        lower.tail = FALSE)


# MUESTRA DEL RESULTADO (PUNTO 7.a)

message("\n Resultados para el Ejercicio 7.a")

# Mostramos el resultado de una forma clara y comprensible
print(paste("La probabilidad de que un estudiante mida 179 cm o más es de:", round(probabilidad_a, 4)))


# GRÁFICO (PUNTO 7.a)
# Creamos una secuencia de valores para graficar la curva normal
# Esto va a ser válido también para los casos b. y c.
x_curva <- seq(media_agrupada - 4 * desvio_agrupado, media_agrupada + 4 * desvio_agrupado, length = 400)
y_curva <- dnorm(x_curva, mean = media_agrupada, sd = desvio_agrupado)

# Dibujamos la curva de la distribución para este caso "Mayor que"
plot(x_curva, y_curva, type = 'l', lwd = 2, col = "black",
     main = paste("Probabilidad P(X >= ", estatura_limite, ") =", round(probabilidad_a, 4)),
     xlab = "Estatura (cm)", ylab = "Densidad")

# Creamos la secuencia de valores para el área sombreada (la cola derecha)
x_sombreado_a <- seq(estatura_limite, media_agrupada + 4 * desvio_agrupado, length = 100)
y_sombreado_a <- dnorm(x_sombreado_a, mean = media_agrupada, sd = desvio_agrupado)

# Añadimos el polígono sombreado
polygon(c(estatura_limite, x_sombreado_a, media_agrupada + 4 * desvio_agrupado), c(0, y_sombreado_a, 0), col = "skyblue")


# CÁLCULO DE PROBABILIDAD (PUNTO 7.b)

# b. Calcular la probabilidad de que un estudiante tenga una estatura comprendida entre 147 cm. y 172 cm.
# Para esto, calculamos P(X <= 172) y le restamos P(X <= 147).

# Definimos los límites inferior y superior del intervalo
estatura_inferior <- 147
estatura_superior <- 172

# Calculamos la probabilidad restando las probabilidades acumuladas, usando nuevamente `pnorm` y almacenando el resultado en `probabilidad_b `
# pnorm() por defecto usa lower.tail = TRUE, que sirve para P(X <= x)
probabilidad_b <- pnorm(estatura_superior, mean = media_agrupada, sd = desvio_agrupado) - 
  pnorm(estatura_inferior, mean = media_agrupada, sd = desvio_agrupado)

# MUESTRA DEL RESULTADO (PUNTO 7.b)

message("\n Resultados para el Ejercicio 7.b")

# Mostramos el resultado de forma clara
print(paste("La probabilidad de que un estudiante mida entre 147 cm y 172 cm es de:", round(probabilidad_b, 4)))

# GRÁFICO (PUNTO 7.b)
# Dibujamos la curva de la distribución para este caso "Entre"
plot(x_curva, y_curva, type = 'l', lwd = 2, col = "black",
     main = paste("Probabilidad P(", estatura_inferior, "<= X <=", estatura_superior, ") =", round(probabilidad_b, 4)),
     xlab = "Estatura (cm)", ylab = "Densidad")

# Creamos la secuencia de valores para el área sombreada (el área central)
x_sombreado_b <- seq(estatura_inferior, estatura_superior, length = 100)
y_sombreado_b <- dnorm(x_sombreado_b, mean = media_agrupada, sd = desvio_agrupado)

# Añadimos el polígono sombreado
polygon(c(estatura_inferior, x_sombreado_b, estatura_superior), c(0, y_sombreado_b, 0), col = "skyblue")



# CÁLCULO DE VALOR (PUNTO 7.c)

# c. Hallar el valor que excede al 97,5% de las estaturas.
# Según la interpretación literal, buscamos un valor `x` que sea mayor que el 97.5% de los datos. Esto corresponde al percentil 97.5.
# La probabilidad acumulada a la izquierda de este valor es 0.975.

# Definimos la probabilidad de la cola izquierda que vamos a buscar, que será almacenada en `probabilidad_c`.

probabilidad_c <- 0.975

# Calculamos el valor de la estatura correspondiente a esa probabilidad acumulada.
estatura_c <- qnorm(probabilidad_c, 
                            mean = media_agrupada, 
                            sd = desvio_agrupado, 
                            lower.tail = TRUE)

# MUESTRA DEL RESULTADO (PUNTO 7.c)

message("\n Resultados para el Ejercicio 7.c")

# El mensaje ahora sí coincide directamente con la consigna.
print(paste("El valor de estatura que excede al 97.5% de los estudiantes es:", round(estatura_c, 2), "cm."))

# GRÁFICO (PUNTO 7.c)

# Dibujamos la curva de la distribución para este caso "Menor que"
plot(x_curva, y_curva, type = 'l', lwd = 2, col = "black",
     main = "Valor que excede al 97.5% de las Estaturas",
     xlab = "Estatura (cm)", ylab = "Densidad")

# Creamos la secuencia de valores para el área sombreada (el 97.5% a la izquierda)
x_sombreado_c <- seq(media_agrupada - 4 * desvio_agrupado, estatura_c, length = 200)
y_sombreado_c <- dnorm(x_sombreado_c, mean = media_agrupada, sd = desvio_agrupado)

# Añadimos el polígono sombreado
polygon(c(media_agrupada - 4 * desvio_agrupado, x_sombreado_c, estatura_c), c(0, y_sombreado_c, 0), col = "skyblue")

# Añadimos una línea vertical para marcar el valor exacto
abline(v = estatura_c, col = "red", lwd = 2, lty = 2)

# Añadimos texto para indicar el valor y la probabilidad
text(x = estatura_c, y = max(y_curva) * 0.5, 
     labels = paste("Percentil 97.5\n", round(estatura_c, 2), "cm"), 
     pos = 4, col = "red") # pos=4 lo coloca a la derecha
