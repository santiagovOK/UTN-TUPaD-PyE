# ============================================
# MÉTODOS GRÁFICOS EN R: BASE Y GGPLOT2
# ============================================

# 1. DIAGRAMA DE PUNTOS (Dotplot)
# --------------------------------------------
# Este gráfico es ideal para muestras pequeñas,
# porque permite ver cada dato individual y cómo se agrupan.

# R BASE
datos <- c(2.2, 4.1, 3.5, 4.5, 3.2, 3.7, 3.0, 2.6, 3.4, 1.6,
           3.1, 3.3, 3.8, 3.1, 4.7, 3.7, 2.5, 4.3, 3.4, 3.6)

stripchart(datos,              # podría se también df$datos
           method = "stack",   # apila los valores repetidos en columnas
           pch = 19,           # forma del punto: círculo sólido
           col = "blue",       # color de los puntos
           main = "Diagrama de puntos",   # título principal
           xlab = "Tiempo de ejecución (s)") # etiqueta del eje X

# GGPLOT2
# La "gramática de gráficos" en ggplot2 se arma en capas:
#   1. Datos (data) (siempre dentro de un dataframe)
#   2. Estéticas (aes)
#   3. Geometría (geom_)
#   4. Opcional: etiquetas, temas, ajustes de diseño
library(ggplot2)

ggplot(data.frame(tiempo = datos), aes(x = tiempo)) + # datos + estética
  geom_dotplot(binwidth = 0.1,      # ancho de agrupamiento
               fill = "blue",       # color de relleno de los puntos
               stackratio = 1.5,    # proporción en el apilamiento vertical
               dotsize = 0.9) +     # tamaño relativo de los puntos
  labs(title = "Diagrama de puntos", 
       x = "Tiempo de ejecución (s)") # etiquetas


# 2. DIAGRAMA DE TALLO Y HOJAS
# --------------------------------------------
# Divide los números en "tallos" (parte entera) y "hojas" (parte decimal).
# Es útil para ver distribución y valores exactos al mismo tiempo.
# Disponible solo en R base.

stem(datos, scale = 1) # o también df$datos 


# 3. GRÁFICO DE BARRAS
# --------------------------------------------
# Representa variables cualitativas mostrando la frecuencia de cada categoría.

# R BASE
categorias <- c("Negro", "Azul", "Amarillo", "Rojo")
frecuencias <- c(4, 5, 5, 6)

barplot(frecuencias, #frecuencias de cada categoría podrías ser también df$frecuencias
        names.arg = categorias, # etiquetas de cada barra
        col = "orange",         # color de las barras
        main = "Preferencias de color", # título
        ylab = "Frecuencia")    # etiqueta del eje Y

# GGPLOT2
df_colores <- data.frame(color = categorias, frecuencia = frecuencias)

ggplot(df_colores, aes(x = color, y = frecuencia, fill = color)) +
  geom_bar(stat = "identity") +   # usamos valores dados, no conteo automático
  labs(title = "Preferencias de color", y = "Frecuencia") +
  theme_minimal()                 # estilo limpio y moderno


# 4. GRÁFICO CIRCULAR (Torta)
# --------------------------------------------
# Muestra proporciones relativas de cada categoría respecto al total.

# R BASE
porcentajes <- c(45, 30, 15, 10) # también podría ser df$porcentaje
nombres <- c("Windows", "Linux", "macOS", "Otros") # o df$sistema

pie(porcentajes,         # partes del total
    labels = nombres,    # etiquetas de cada sector
    col = rainbow(4),    # asigna automáticamente 4 colores distintos
    main = "Sistemas Operativos") #título del gráfico

# GGPLOT2
df_so <- data.frame(SO = nombres, porcentaje = porcentajes)

ggplot(df_so, aes(x = "", y = porcentaje, fill = SO)) + #fill = SO, hace que cada sector se pinte según la categoría
  geom_bar(stat = "identity", width = 1) +  # barras proporcionales
  coord_polar("y") +                        # convierte barras en sectores circulares
  labs(title = "Sistemas Operativos") +
  theme_void()                              # elimina ejes y grillas


# 5. HISTOGRAMA
# --------------------------------------------
# Agrupa datos numéricos en intervalos (bins) y muestra su frecuencia.

# R BASE
hist(datos,
     breaks = 10,               # número de intervalos
     col = "skyblue",           # color de las barras
     main = "Histograma de tiempos", # título del gráfico
     xlab = "Tiempo",                # etiqueta eje x
     freq = TRUE)               # TRUE: muestra frecuencias absolutas en c/barra

# GGPLOT2
# Vamos a forzar los mismos cortes que hist() de R base, 
# pero no es estrictamente necesario
h <- hist(datos, breaks = 10, plot = FALSE)
brks <- h$breaks 

ggplot(data.frame(tiempo = datos), aes(x = tiempo)) +
  geom_histogram(breaks = brks, # Podríamos usar binwidth y forzar el ancho
                 closed = "right",         # igual que hist(): (a,b]
                 fill = "skyblue", color = "white") +
  labs(title = "Histograma de tiempos",
       x = "Tiempo (s)", y = "Frecuencia")




# 6. POLÍGONO DE FRECUENCIAS
# --------------------------------------------
# Similar al histograma, pero une con líneas los puntos medios de cada barra.

# R BASE
histograma <- hist(datos, plot = FALSE) # guardamos histograma sin dibujar

plot(histograma$mids, histograma$counts,
     type = "l",             # tipo línea
     col = "red",            # color de la línea
     main = "Polígono de frecuencias",
     xlab = "Tiempo",
     ylab = "Frecuencia")

# GGPLOT2
ggplot(data.frame(tiempo = datos), aes(x = tiempo)) +
  geom_freqpoly(breaks = brks,  # ancho de intervalos
                color = "red",   # color de la línea
                linewidth = 1) + # grosor de la línea
  labs(title = "Polígono de frecuencias",
       x = "Tiempo (s)",
       y = "Frecuencia")


# 7. BOXPLOT
# --------------------------------------------
# Resume la distribución: mediana, cuartiles y posibles valores atípicos.

# R BASE
boxplot(datos,
        main = "Boxplot de tiempos",
        ylab = "Tiempo (s)",
        col = "lightgreen") # color de la caja
        

# GGPLOT2 – Boxplot simple
ggplot(data.frame(tiempo = datos), aes(x = "", y = tiempo)) +
  geom_boxplot(fill = "lightgreen",
               width = 0.4,        # ancho de la caja
               whiskerwidth = 0.8, # ancho de los topes (ggplot2 >= 3.4)
               linewidth = 0.7,
               outlier.shape = 19, outlier.size = 2, 
               outlier.color = "red") +
  labs(title = "Boxplot de tiempos", x = NULL, y = "Tiempo (s)") +
  theme_classic()

# GGPLOT2 – Boxplot comparativo por grupo
# Aquí comparamos rendimiento por cantidad de cilindros en el dataset mtcars.
ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Consumo por cilindros",
       x = "Cilindros",
       y = "Millas por galón")