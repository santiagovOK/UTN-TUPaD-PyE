 # Santiago Octavio Varela / @santiagovOK (GitHub) <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-08-20

# Probabilidad y Estadística - Unidad 3: Analizar y Resumir Datos - Comisión 14


# Limpiar entorno

rm(list = ls())

# Instalamos los paquetes necesarios

if(!require(readxl)) install.packages("readxl")
library(readxl)

# Leemos el archivo de excel propuesto `Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx`
# Almacenamos los datos en `datos`

archivo <- "assets/Datos_Práctica_Tabla_de_Frecuencias_en_R_(V_Nominal).xlsx"
datos <- read_excel(archivo)

# 1 - Tabla de Frecuencias y medidas de tendencia central para `Tickets_Soporte` (Variable Discreta)
# Tabla de frecuencias con cuatro columnas: Tickets, Frecuencia absoluta (Frec), Frecuencia Acumulada (Frec_Acum) , Frecuencia Relativa (Frec_Rel)


variable_discreta <- "Tickets_Soporte"

message("\n Resultados - Variable Discreta (", variable_discreta, ")")
message("Resumen básico:")

faltantes <- sum(is.na(datos[[variable_discreta]])) # Cuenta los valores faltantes
cat("Datos faltantes:", faltantes, "\n")
print(summary(datos[[variable_discreta]])) # Muestra resumen estadístico básico

# Calculo de estadísticos para datos no agrupados

media_discreta <- mean(datos[[variable_discreta]], na.rm = TRUE)
mediana_discreta <- median(datos[[variable_discreta]], na.rm = TRUE)


frecuencias_discreta<- table(datos[[variable_discreta]])

moda_discreta <- as.numeric(names(frecuencias_discreta[frecuencias_discreta == max(frecuencias_discreta)]))

# Calculo alternativo para la moda

# install.packages("modeest")
# library(modeest)
# moda_discreta_alt <- mlv(datos[[variable_discreta]], method = "mfv") # `"mfv" = most likely value`

varianza_discreta <- var(datos[[variable_discreta]], na.rm = TRUE)
desvio_estandar_discreta <- sd(datos[[variable_discreta]], na.rm = TRUE)
coef_var_discreta <- (desvio_estandar_discreta / media_discreta) * 100


tabla_discreta_tickets <- data.frame(
#  Tickets = names(variable_discreta),
  Media = round(media_discreta, 4),
  Mediana = round(mediana_discreta, 4),
  Moda = paste(moda_discreta, collapse = ", "),
  Varianza = round(varianza_discreta, 4),
  Desvio_Estandar = round(desvio_estandar_discreta, 4),
  Coef_Variacion_pct = round(coef_var_discreta, 4)
  
)

message("Tabla Discret Tickets")
print(tabla_discreta_tickets, row.names = FALSE)

# Cuartiles y RIC

cuartiles <- quantile(datos[[variable_discreta]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
rango_intercuartil <- IQR(datos[[variable_discreta]], na.rm = TRUE)

message("Cuartiles y RIC")

print(cuartiles)
print(rango_intercuartil) # RIC

# -----------------------------------------------

# 2 - Tabla de Frecuencias y medidas de tendencia central para `Tiempo_Conexion_Min` (por modificar...)

# -----------------------------------------------

variable_continua <- "Tiempo_Conexion_Min" # Variable continua a analizar

# Agrupamiento por la regla de Sturgges con intervalos enteros

# Agrupamiento por regla de Sturges con intervalos enteros

# Número de intervalos según la regla de Sturges
k <- ceiling(1 + 3.322 * log10(nrow(datos)))

# Valor mínimo y máximo del variable continua
min_val <- floor(min(datos[[variable_continua]], na.rm = TRUE))
max_val <- ceiling(max(datos[[variable_continua]], na.rm = TRUE))

# Amplitud de los intervalos
amplitud <- ceiling((max_val - min_val) / k)

# Tope máximo ajustado al número de intervalos
max_tope <- min_val + amplitud * k

# Secuencia de cortes para los intervalos
cortes <- seq(min_val, max_tope, by = amplitud)

# ---

# Crear columna con intervalos

datos$clases <- cut(datos[[variable_continua]], breaks = cortes, right = FALSE, include.lowest = TRUE)

# Marca de clase - punto intermedio de cada intervalo

marca_clase <- (head(cortes, -1) + tail(cortes, -1)) / 2

# Tabla de frecuencias 

# Tabla de frecuencias

# Frecuencia absoluta por clase
tabla_clases <- table(datos$clases)
f_acum <- cumsum(tabla_clases)
f_rel <- prop.table(tabla_clases)
f_rel_acum <- cumsum(f_rel)

# Mostrar tabla de frecuencias
tabla_frecuencia <- data.frame(
  Intervalo = names(tabla_clases),
  Marca = as.vector(marca_clase),
  Frec_Abs = as.vector(tabla_clases),
  Frec_Acum = as.vector(f_acum),
  Frec_Rel = round(as.vector(f_rel), 4),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 4)
)

message("\n Tabla de Frecuencias - Variable Continua (", variable_continua, ")")
print(tabla_frecuencia, row.names = FALSE)

# Tendencia central

# Cálculo de la media

# Vector de frecuencias absolutas
frecuencias <- as.vector(tabla_clases)

# Media para variable continua
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

mediana_continua <- L + ((n_2 - F_anterior) / f_mediana) * amplitud # Cálculo de la mediana continua usando interpolación

# Medidas de dispersión

# Varianza agrupada
varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)

# Desvío estándar agrupado
desvio_continua <- sqrt(varianza_continua)

# Coeficiente de variación (%)
coef_var_continua <- (desvio_continua / media_continua) * 100

# Mostrar resultados agrupados
message("\n📊 Resultados AGRUPADOS - VARIABLE CONTINUA (", variable_continua, ")")

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

