# Santiago Octavio Varela - <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-08-19

# PyE - TPI - Semana 3, consigna 2

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
