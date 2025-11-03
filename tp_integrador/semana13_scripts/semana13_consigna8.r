# Santiago Octavio Varela - <santiago.varela@tupad.utn.edu.ar>

# Última actualización: 2025-11-03

# PyE - TPI - Semana 13, consigna 8

# Cargar la librería para leer archivos CSV
if(!require(readxl)) install.packages("readxl")
library(readxl)

# Definir el nombre del archivo de datos
archivo_datos <- "assets/TUPAD-2025-EST-TPI-planilla5.xlsx"

# Definir la columna que vamos a analizar
columna_interes <- "PESO KG."

# Definir los parámetros del muestreo
cantidad_muestras <- 6
tamano_muestra <- 20

# Fijar una semilla aleatoria
# Esto hace que los resultados aleatorios sean reproducibles
set.seed(123)

# 1. Carga de datos

# Cargar los datos. 
datos_poblacion <- read_excel(archivo_datos)

# 2. Cálculo del Parámetro (Población)

print("Análisis Poblacional")

# Calcular el promedio de 'PESO KG.' para toda la población (N=250)
mu_poblacional <- mean(datos_poblacion[[columna_interes]], na.rm = TRUE)

# Usamos paste0() para unir texto y variables sin espacios extra
# Usamos round() para formatear el número a 2 decimales
print(paste0("Parámetro (μ) - Promedio de 'PESO KG.' de la población: ", round(mu_poblacional, 2), " kg"))
print("--------------------------------------------------")


# 3. Muestreo y Cálculo de Estadísticos (Muestras)

print("Análisis Muestral (Muestreo Aleatorio Simple)")
print(paste0("Tomando ", cantidad_muestras, " muestras de tamaño n=", tamano_muestra, ":"))

# Crear un vector para guardar los 6 promedios muestrales
promedios_muestrales <- numeric(cantidad_muestras)

# Crear un vector de la población de pesos (quitando los NA)
poblacion_pesos <- na.omit(datos_poblacion[[columna_interes]])

# Loop para tomar cada muestra y calcular su promedio
for (i in 1:cantidad_muestras) {
  
  # 1. Tomar la muestra aleatoria
  muestra_i <- sample(poblacion_pesos, size = tamano_muestra, replace = FALSE)
  
  # 2. Calcular el promedio de esa muestra
  promedio_muestra_i <- mean(muestra_i)
  
  # 3. Guardar el resultado
  promedios_muestrales[i] <- promedio_muestra_i
  
  # 4. Imprimir el resultado de esta muestra usando print(paste0(...))
  print(paste0("Muestra ", i, " (n=20) -> Promedio (x̄_", i, "): ", round(promedio_muestra_i, 2), " kg"))
}

# 4. Comparación y Conclusiones


print("Datos Finales")

# Imprimir el parámetro de nuevo para comparar fácilmente
print(paste0("Valor del Parámetro (μ) Poblacional: ", round(mu_poblacional, 2), " kg"))

# Imprimir todos los estadísticos muestrales juntos
print("Valores de los Estadísticos Muestrales:")
print(round(promedios_muestrales, 2)) 