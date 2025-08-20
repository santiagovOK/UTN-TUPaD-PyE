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

print(tabla_discreta_tickets, row.names = FALSE)

# Cuartiles y RIC

cuartiles <- quantile(datos[[variable_discreta]], probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
rango_intercuartil <- IQR(datos[[variable_discreta]], na.rm = TRUE)

print(cuartiles)
print(rango_intercuartil) # RIC

# 2 - Tabla de Frecuencias y medidas de tendencia central para `Tiempo_Conexion_Min` (por modificar...)

# Usamos los datos reales del archivo Excel
#tiempo_conexion <- datos$Tiempo_Conexion_Min

# Cantidad total de observaciones
#n <- length(tiempo_conexion)

# Número de clases según la regla de Sturges
# k <- ceiling(1 + 3.322 * log10(n))
# k

# Calculamos el rango y amplitud basado en los datos del .xlsx
# rango <- range(tiempo_conexion)
# amplitud <- ceiling((rango[2] - rango[1]) / k)

# rango
# amplitud

# Creamos los intervalos basados en los datos reales
# breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
# clases <- cut(tiempo_conexion, breaks = breaks, right = FALSE)

# Verificamos las primeras clases
# head(clases)

# Creamos la tabla de frecuencias
# tabla_tiempos <- table(clases)
# f_acum_tiempos <- cumsum(tabla_tiempos)
# f_rel_tiempos <- prop.table(tabla_tiempos)
# f_rel_acum <- cumsum(f_rel_tiempos)

# Construimos la tabla final
#tabla_frecuencias_tiempo <- data.frame(
#  Intervalo = levels(clases),
#  Frecuencia = as.vector(tabla_tiempos),
#  Frec_Acumulada = as.vector(f_acum_tiempos),
#  Frec_Relativa = round(as.vector(f_rel_tiempos), 3),
#  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
#)

# Mostramos la tabla corregida
#tabla_frecuencias_tiempo
