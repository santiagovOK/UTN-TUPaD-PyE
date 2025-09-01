# Santiago Octavio Varela - <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-09-01

# PyE - TPI - Semana 5, consignas 3-4 (continuación de la semana 3, lo nuevo de esta semana está a partir de TENDENCIA CENTRAL)

# Construir la/s Tabla/s de Frecuencias y calcular todas las frecuencias de las siguientes variables:

# a. Tiempo en horas semanales dedicadas al estudio. (Determinar la cantidad optima de intervalos a utilizar)
# b. Nivel de satisfacción con la Carrera.
# c. A partir de la tabla obtenida en el punto a. realizar la interpretación de todas las frecuencias correspondientes al cuarto intervalo en el contexto del caso planteado.
# d. A partir de la tabla obtenida en el punto b. realizar la interpretación de todas las frecuencias correspondientes a la categoría “Satisfecho”

# Instalamos los paquetes necesarios

if(!require(readxl)) install.packages("readxl")
library(readxl)

# Leemos el archivo de excel propuesto `TUPAD-2025-EST-TPI-planilla5.xlsx`
# Almacenamos los datos en `datos`

archivo <- "assets/TUPAD-2025-EST-TPI-planilla5.xlsx"
datos <- read_excel(archivo)

# a. Construimos la tabla de frecuencias en base a `TIEMPO SEMANAL en HS. DEDIC. EST.`

# Usamos los datos del archivo Excel

tiempo_semanal <- datos$`TIEMPO SEMANAL en HS. DEDIC. EST.`

# Cantidad total de observaciones

n <- length(tiempo_semanal)

# Número óptimo de intervalos (clases) siguienbdo la regla de Sturges
k <- ceiling(1 + 3.322 * log10(n))


print(paste("Número óptimo de intervalos:", k))

# Calculamos el rango y amplitud basado en los datos del .xlsx
rango <- range(tiempo_semanal)
amplitud <- ceiling((rango[2] - rango[1]) / k)

# Creamos los intervalos basados en los datos
breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut(tiempo_semanal, breaks = breaks, right = FALSE)

# Verificamos las primeras clases

head(clases)

# Creamos la tabla de frecuencias

tabla_tiempo_semanal <- table(clases)
f_acum_tiempos <- cumsum(tabla_tiempo_semanal)
f_rel_tiempos <- prop.table(tabla_tiempo_semanal)
f_rel_acum <- cumsum(f_rel_tiempos)

# Construimos la tabla final

tabla_tiempo_semanal_frecuencias <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_tiempo_semanal),
  Frec_Acumulada = as.vector(f_acum_tiempos),
  Frec_Relativa = round(as.vector(f_rel_tiempos), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)

# Mostramos la tabla

print("Tabla de frecuencias para 'TIEMPO SEMANAL en HS. DEDIC. EST.' \n ")
print(tabla_tiempo_semanal_frecuencias)

## VARIABLES ADECUADAS PARA MEDIDAS DE TENDENCIA CENTRAL

# Vector de frecuencias absolutas (ya calculado anteriormente)
frecuencias <- as.vector(tabla_tiempo_semanal)

# Vector de frecuencias acumuladas (ya calculado anteriormente)
f_acum <- as.vector(f_acum_tiempos)

# Vector de límites inferiores de cada intervalo (cortes)
cortes <- breaks[-length(breaks)]  # Removemos el último límite superior

# Vector de marcas de clase (punto medio de cada intervalo)
marca_clase <- (cortes + cortes + amplitud) / 2

# Definir variable continua para mensajes
variable_continua <- "TIEMPO SEMANAL en HS. DEDIC. EST."

# Medidas de Tendencia Central

# Cálculos para la variable continua en "TIEMPO SEMANAL en HS. DEDIC. EST."

# Cálculo de la media
# Media para variable continua agrupada

media_continua <- sum(marca_clase * frecuencias) / sum(frecuencias)

# Cálculo de la moda

# Índice de la clase modal (mayor frecuencia)
i_modal <- which.max(frecuencias)

# Límite inferior del intervalo modal
l_m <- cortes[i_modal]

# Frecuencia del intervalo modal
f_m <- frecuencias[i_modal]

# Frecuencia del intervalo anterior
f_1 <- ifelse(i_modal == 1, 0, frecuencias[i_modal - 1])

# Frecuencia del intervalo siguiente
f_2 <- ifelse(i_modal == length(frecuencias), 0, frecuencias[i_modal + 1])

# Cálculo de la moda usando la fórmula de interpolación
moda_continua <- l_m + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud 

# Cálculo de la mediana

# Total de frecuencias
n_total <- sum(frecuencias)

# Mitad del total
n_2 <- n_total / 2

# Índice de la clase que contiene la mediana
clase_mediana_index <- which(f_acum >= n_2)[1]

# Límite inferior del intervalo de la mediana
L <- cortes[clase_mediana_index]

# Frecuencia acumulada anterior a la clase de la mediana
F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index - 1])

# Frecuencia de la clase de la mediana
f_mediana <- frecuencias[clase_mediana_index]

# Cálculo de la mediana continua usando interpolación
mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud

# Medidas de dispersión

# Varianza agrupada
varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)

# Desvío estándar agrupado
desvio_continua <- sqrt(varianza_continua)

# Coeficiente de variación (%)
coef_var_continua <- (desvio_continua / media_continua) * 100

# Mostrar resultados agrupados
message("\n Resultados medidas de tendencia central - VARIABLE CONTINUA (", variable_continua, ")")

# Tabla resumen de medidas estadísticas

continua_stats <- data.frame(
  Media = round(media_continua, 4),
  Moda = round(moda_continua, 4),
  Mediana = round(mediana_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_Estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

print(continua_stats, row.names = FALSE)

# Mensaje final

message("\n Fin de resultados medidas de tendencia central - VARIABLE CONTINUA (", variable_continua, ")")

message("\n Comenzamos con el análisis completo de: SATISFACCIÓN CON LA CARRERA")

# -------------- ------------------------------- --------------------------

# b. Construimos la tabla de frecuencias en base a `SATISFACCIÓN CON LA CARRERA`

# Usamos los datos del archivo Excel

tabla_satisfaccion <- table(datos$`SATISFACCIÓN CON LA CARRERA`)

# Establecemos variables para almacenar los cálculos de las frecuencias y luego construir la tabla

f_acum_satisfaccion <- cumsum(tabla_satisfaccion)
f_rel_satisfaccion <- prop.table(tabla_satisfaccion)
f_rel_acum_satisfaccion <- cumsum(f_rel_satisfaccion)

# Construimos la tabla con data.frame 

tabla_frecuencias_satisfaccion <- data.frame(
  
  Satisfacción = names(tabla_satisfaccion),
  Frec = as.vector(tabla_satisfaccion),
  Frec_acum = as.vector(f_acum_satisfaccion),
  Frec_Rel = round(as.vector(f_rel_satisfaccion), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum_satisfaccion), 3)
  
)

# Mostramos la tabla de frecuencia en la consola

print("Tabla de frecuencias para 'SATISFACCIÓN CON LA CARRERA' \n ")
print(tabla_frecuencias_satisfaccion)

# MEDIDAS DE TENDENCIA CENTRAL para el caso "SATISFACCIÓN CON LA CARRERA" (Variable categórica)
# Solo calculamos

# Variables de soporte para la variable categórica
categorias_satisfaccion <- names(tabla_satisfaccion)
freq_satisfaccion <- as.vector(tabla_satisfaccion)
freq_acum_satisfaccion <- as.vector(f_acum_satisfaccion)
n_total_satisfaccion <- sum(freq_satisfaccion)

# Definir variable categórica para mensajes
variable_categorica <- "SATISFACCIÓN CON LA CARRERA"

# Cálculo de la MODA para variable categórica
# La moda es la categoría con mayor frecuencia
indice_moda_cat <- which.max(freq_satisfaccion)
moda_categorica <- categorias_satisfaccion[indice_moda_cat]
freq_moda_cat <- freq_satisfaccion[indice_moda_cat]

# Cálculo de la MEDIANA para variable categórica ordinal
# Posición de la mediana
posicion_mediana_cat <- n_total_satisfaccion / 2

# Encontrar la categoría que contiene la mediana
indice_mediana_cat <- which(freq_acum_satisfaccion >= posicion_mediana_cat)[1]
mediana_categorica <- categorias_satisfaccion[indice_mediana_cat]

# Cálculo de CUARTILES para variable categórica ordinal
# Q1 (25%)
posicion_q1_cat <- n_total_satisfaccion * 0.25
indice_q1_cat <- which(freq_acum_satisfaccion >= posicion_q1_cat)[1]
q1_categorica <- categorias_satisfaccion[indice_q1_cat]

# Q3 (75%)
posicion_q3_cat <- n_total_satisfaccion * 0.75
indice_q3_cat <- which(freq_acum_satisfaccion >= posicion_q3_cat)[1]
q3_categorica <- categorias_satisfaccion[indice_q3_cat]

# Mostrar resultados para variable categórica
message("\n Medidas de tendencia central para SATISFACCIÓN CON LA CARRERA (", variable_categorica, ")")

# Creamos la tabla de medidas estadísticas
categorica_stats <- data.frame(
  Moda = moda_categorica,
  Freq_Moda = freq_moda_cat,
  Mediana = mediana_categorica,
  Q1 = q1_categorica,
  Q3 = q3_categorica
)


# Imprimimos la tabla final con las medidas estadísticas
print(categorica_stats, row.names = FALSE)

