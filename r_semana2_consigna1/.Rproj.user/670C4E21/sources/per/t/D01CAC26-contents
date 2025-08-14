# Consigna 1 - En base al archivo Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx

#A continuación, encontrarás un archivo que simula los resultados de una encuesta aplicada a 103 programadores y programadoras que trabajan de forma remota en distintas empresas de desarrollo de software. El objetivo del relevamiento fue conocer sus hábitos laborales y herramientas más utilizadas.

#La plataforma de comunicación más utilizada.
#La cantidad de tickets de soporte que resolvieron en una semana.
#El tiempo promedio de conexión diaria a los sistemas de trabajo.

#Utilizá este archivo para construir tablas de frecuencia para cada una de las variables. Importante:la variable Plataforma_Trabajo es una variable categórica nominal, por lo que:Debés construir únicamente las columnas de frecuencia absoluta y frecuencia relativa.
#No corresponde calcular la frecuencia acumulada, ya que no existe un orden entre las categorías.
#En cambio, para las variables numéricas Tickets_Soporte y Tiempo_Conexion sí corresponde construir las tres columnas:frecuencia absoluta, frecuencia relativa y frecuencia acumulada. En el caso de Tiempo_Conexion, deberás agrupar los datos en clases utilizando la función cut().


# Instalamos los paquetes necesarios
if(!require(readxl)) install.packages("readxl")
library(readxl)

# Leemos el archivo de excel propuesto `Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx`
# Cargamos los datos en `datos`

archivo <- "/assets/Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx"
datos <- read_excel(archivo)


