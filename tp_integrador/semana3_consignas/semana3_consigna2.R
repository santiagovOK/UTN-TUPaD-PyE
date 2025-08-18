# Santiago Octavio Varela - <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-08-18

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
