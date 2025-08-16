# Santiago Octavio Varela / @santiagovOK (GitHub) <santiago.varela@tupad.utn.edu.ar>
# Probabilidad y Estadística - Unidad 2: Organizar Datos - Comisión 14

# Consigna 1 - En base al archivo Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx

# A continuación, encontrarás un archivo que simula los resultados de una encuesta aplicada a 103   programadores y programadoras que trabajan de forma remota en distintas empresas de desarrollo    de   software. El objetivo del relevamiento fue conocer sus hábitos laborales y herramientas más   utilizadas:

# La plataforma de comunicación más utilizada.
# La cantidad de tickets de soporte que resolvieron en una semana.
# El tiempo promedio de conexión diaria a los sistemas de trabajo.

# Utilizá este archivo para construir tablas de frecuencia para cada una de las variables.        Importante:la variable Plataforma_Trabajo es una variable categórica nominal, por lo que:       Debés    construir únicamente las columnas de frecuencia absoluta y frecuencia relativa.

# No corresponde calcular la frecuencia acumulada, ya que no existe un orden entre las      categorías.

# En cambio, para las variables numéricas Tickets_Soporte y Tiempo_Conexion sí corresponde          construir las tres columnas:frecuencia absoluta, frecuencia relativa y frecuencia acumulada.     En   el caso de Tiempo_Conexion, deberás agrupar los datos en clases utilizando la función      cut().


# Instalamos los paquetes necesarios

if(!require(readxl)) install.packages("readxl")
library(readxl)

# Leemos el archivo de excel propuesto `Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx`
# Almacenamos los datos en `datos`

archivo <- "assets/Datos_Práctica_Tabla_de_Frecuencias_en_R_(V_Nominal).xlsx"
datos <- read_excel(archivo)

# 1 - Tabla de Frecuencias para `Plataforma_Trabajo` - Variable Categórica nominal ;
# Tabla de frecuencias con tres columnas: Plataforma, Frecuencia absoluta (Frec), Frecuencia Relativa (Frec_Rel)

tabla_plataforma_trabajo <- table(datos$Plataforma_Trabajo)

f_rel_plataforma_trabajo <- prop.table(tabla_plataforma_trabajo)

tabla_frecuencias_plataforma <- data.frame(
  
  Plataforma = names(tabla_plataforma_trabajo),
  Frec = as.vector(tabla_plataforma_trabajo),
  Frec_Rel = round(as.vector(f_rel_plataforma_trabajo), 3)
    
    
)

# Mostramos la tabla de frecuencia en la consola

tabla_frecuencias_plataforma

# 2 - Tabla de Frecuencias para `Tickets_Soporte`
# Tabla de frecuencias con cuatro columnas: Tickets, Frecuencia absoluta (Frec), Frecuencia Acumulada (Frec_Acum) , Frecuencia Relativa (Frec_Rel)

tabla_tickets_soporte <- table(datos$Tickets_Soporte)

frec_acum_tickets_soporte <- cumsum(tabla_tickets_soporte)
frec_rel_tickets_soporte <- prop.table(tabla_tickets_soporte)

tabla_frecuencias_tickets <- data.frame(
  Tickets = names(tabla_tickets_soporte),
  Frec = as.vector(tabla_tickets_soporte),
  Frec_Acum = as.vector(frec_acum_tickets_soporte),
  Frec_Rel = round(as.vector(frec_rel_tickets_soporte), 3)
  
)

tabla_frecuencias_tickets

# 3 - Tabla de Frecuencias para `Tiempo_Conexion_Min`

# Usamos los datos reales del archivo Excel
tiempo_conexion <- datos$Tiempo_Conexion_Min

# Cantidad total de observaciones
n <- length(tiempo_conexion)

# Número de clases según la regla de Sturges
k <- ceiling(1 + 3.322 * log10(n))
k

# Calculamos el rango y amplitud basado en los datos del .xlsx
rango <- range(tiempo_conexion)
amplitud <- ceiling((rango[2] - rango[1]) / k)

rango
amplitud

# Creamos los intervalos basados en los datos reales
breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut(tiempo_conexion, breaks = breaks, right = FALSE)

# Verificamos las primeras clases
head(clases)

# Creamos la tabla de frecuencias
tabla_tiempos <- table(clases)
f_acum_tiempos <- cumsum(tabla_tiempos)
f_rel_tiempos <- prop.table(tabla_tiempos)
f_rel_acum <- cumsum(f_rel_tiempos)

# Construimos la tabla final
tabla_frecuencias_tiempo <- data.frame(
  Intervalo = levels(clases),
  Frecuencia = as.vector(tabla_tiempos),
  Frec_Acumulada = as.vector(f_acum_tiempos),
  Frec_Relativa = round(as.vector(f_rel_tiempos), 3),
  Frec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)

# Mostramos la tabla corregida
tabla_frecuencias_tiempo
