# Santiago Octavio Varela - <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-09-04

# PyE - TPI - Semana 5, consignas 3-4 (continuación de la semana 3, lo nuevo de esta semana está a partir de TENDENCIA CENTRAL en cada caso)

# Decidí mantener la porción de código respecto al calculo de frecuencias de semanas atrás para reutilizar variables si era necesario, pero también como referencia en cuanto a los datos

# Instalamos los paquetes necesarios

# ggplot2 para la creación de gráficas en la consigna 4
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# readxl para la lectura del archivo
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

# Número óptimo de intervalos (clases) siguiendo la regla de Sturges
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

# Definir variable continua para mensajes posteriores
variable_continua <- "TIEMPO SEMANAL en HS. DEDIC. EST."

# -
# Medidas de TENDENCIA CENTRAL
# -

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

# Calculamos los cuartiles
cuartiles <- quantile(tiempo_semanal, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Almacenamos en variables cada cuartil individual
Q1 <- cuartiles[1]  # Primer cuartil (25%)
Q2 <- cuartiles[2]  # Segundo cuartil (50% = mediana)
Q3 <- cuartiles[3]  # Tercer cuartil (75%)

# Rango - Diferencia entre máximo y mínimo
rango_continua <- max(tiempo_semanal) - min(tiempo_semanal) # 25 - 0 = 25

# Varianza agrupada para datos continuos
varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)

# Desviación Estándar agrupada
desvio_continua <- sqrt(varianza_continua)

# Rango Intercuartílico 
IQR_continua <- IQR(tiempo_semanal, na.rm = TRUE)

# Mostrar resultados agrupados
message("\n Resultados medidas de tendencia central - VARIABLE CONTINUA (", variable_continua, ")")

# Tabla resumen de medidas estadísticas

continua_stats <- data.frame(
  Media = round(media_continua, 4),
  Mediana = round(mediana_continua, 4),
  Moda = round(moda_continua, 4),
  
  # Cuartiles
  Q1 = round(Q1, 4),
  Q2 = round(Q2, 4), # Mediana
  Q3 = round(Q3, 4),
  
  # Medidas de dispersión
  Rango = round(rango_continua, 4),
  IQR = round(IQR_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_Estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

print(continua_stats, row.names = FALSE)

# Mensaje final

message("\n Fin de resultados medidas de tendencia central - VARIABLE CONTINUA (", variable_continua, ")")

# -
# Construcción del histograma en función de las frecuencias absolutas
# -

# Agrupa datos numéricos en intervalos (bins) y muestra su frecuencia.

# ggplot2 ya fue cargado previamente - VER AL COMIENZO DEL SCRIPT

# R BASE
hist(tiempo_semanal,
     breaks = breaks,           # usamos los mismos breaks calculados anteriormente
     col = "skyblue",           # color de las barras
     main = "Histograma de Tiempo Semanal en Horas Dedicadas al Estudio", # título del gráfico
     xlab = "Tiempo Semanal (horas)",    # etiqueta eje x
     ylab = "Frecuencia Absoluta",       # etiqueta eje y
     freq = TRUE)               # TRUE: muestra frecuencias absolutas en c/barra

# GGPLOT2
# Usamos los mismos cortes calculados anteriormente para mantener consistencia
ggplot(data.frame(tiempo = tiempo_semanal), aes(x = tiempo)) +
  geom_histogram(breaks = breaks,       # Usamos los breaks ya calculados
                 closed = "left",       # intervalos [a,b) como en cut()
                 fill = "skyblue", color = "white") +
  labs(title = "Histograma de Tiempo Semanal en Horas Dedicadas al Estudio",
       x = "Tiempo Semanal (horas)", 
       y = "Frecuencia Absoluta") +
  theme_minimal()

message("\n Fin del creación del histograma para (", variable_continua, ") en base a sus frecuencias absolutas y los intervalos previamente creados")

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

# - 
# MEDIDAS DE TENDENCIA CENTRAL para el caso "SATISFACCIÓN CON LA CARRERA" (Variable categórica)
# -

# Variables específicas para este caso

# Vector de frecuencias absolutas para variable categórica
frecuencias_cat <- as.vector(tabla_satisfaccion)

# Vector de frecuencias acumuladas para variable categórica
f_acum_cat <- as.vector(f_acum_satisfaccion)

# Vector de categorías (niveles de satisfacción)
categorias <- names(tabla_satisfaccion)

# Definir variable categórica para mensajes
variable_categorica <- "SATISFACCIÓN CON LA CARRERA"

# Moda

# Índice de la categoría con mayor frecuencia
indice_moda_cat <- which.max(frecuencias_cat)

# Moda: categoría con mayor frecuencia
moda_categorica <- categorias[indice_moda_cat]

# Frecuencia de la moda
frec_moda_cat <- frecuencias_cat[indice_moda_cat]

# Cálculo de la Mediana categórica
# Total de observaciones
n_total_cat <- sum(frecuencias_cat)

# Posición de la mediana (n/2)
pos_mediana <- n_total_cat / 2

# Índice de la categoría que contiene la mediana
indice_mediana_cat <- which(f_acum_cat >= pos_mediana)[1]

# Mediana: categoría correspondiente
mediana_categorica <- categorias[indice_mediana_cat]

# Cálculo de Cuartiles y Rango Intercuartil
# Convertir variable categórica a numérica para cálculos de cuartiles
datos_numericos_cat <- as.numeric(as.factor(datos[[variable_categorica]]))

# Cuartiles usando la función quantile
cuartiles <- quantile(datos_numericos_cat, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Convertir cuartiles numéricos de vuelta a categorías
q1_cat <- categorias[round(cuartiles[1])]
q2_cat <- categorias[round(cuartiles[2])]  # Mediana
q3_cat <- categorias[round(cuartiles[3])]

# Mostrar resultados categóricos
message("\n📊 Resultados - VARIABLE CATEGÓRICA (", variable_categorica, ")")

# Tabla resumen de medidas estadísticas categóricas
categorica_stats <- data.frame(
  Moda = moda_categorica,
  Frecuencia_Moda = frec_moda_cat,
  Mediana = mediana_categorica,
  Q1 = q1_cat,
  Q2 = q2_cat, # Mediana
  Q3 = q3_cat
)

print(categorica_stats, row.names = FALSE)

# -
# Gráfico circular (Torta) para la consigna 4 - muestra proporciones relativas de cada categoría respecto al total.
# -

# R BASE
porcentajes_satisfaccion <- round(as.vector(f_rel_satisfaccion) * 100, 1) # convertimos a porcentajes
nombres_satisfaccion <- names(tabla_satisfaccion)

pie(porcentajes_satisfaccion,         # partes del total
    labels = paste(nombres_satisfaccion, "\n", porcentajes_satisfaccion, "%"), # etiquetas con porcentajes
    col = rainbow(length(nombres_satisfaccion)),    # asigna automáticamente colores distintos
    main = "Nivel de Satisfacción con la Carrera") # título del gráfico

# GGPLOT2
df_satisfaccion <- data.frame(
  Satisfaccion = nombres_satisfaccion, 
  porcentaje = porcentajes_satisfaccion
)

ggplot(df_satisfaccion, aes(x = "", y = porcentaje, fill = Satisfaccion)) +
  geom_bar(stat = "identity", width = 1) +  # barras proporcionales
  coord_polar("y") +                        # convierte barras en sectores circulares
  labs(title = "Nivel de Satisfacción con la Carrera") +
  theme_void()

