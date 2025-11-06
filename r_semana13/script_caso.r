
# DEFINICIÓN DE LOS DATOS

horas <- c(10, 12, 8, 14, 6, 16, 9, 13)
incidencias <- c(6, 8, 4, 9, 3, 10, 5, 8)


# 1. DIAGRAMA DE DISPERSIÓN 
# Generamos el gráfico de dispersión para visualizar la relación

plot(horas, incidencias,
     main = "Relación entre Capacitación e Incidencias",
     xlab = "Horas de Capacitación",
     ylab = "Incidencias Resueltas",
     pch = 19,     # Tipo de punto (círculo sólido)
     col = "blue") # Color de los puntos


# 2. COEFICIENTE DE CORRELACIÓN DE PEARSON

# Calculamos el coeficiente 'r' para medir la fuerza y dirección

coef_correlacion <- cor(horas, incidencias)

# Imprimimos el resultado

print(paste("Coeficiente de Correlación (r):", round(coef_correlacion, 4)))


# 3. RECTA DE REGRESIÓN LINEAL
# Calculamos el modelo lineal (incidencias en función de horas)
# La fórmula 'incidencias ~ horas' significa Y ~ X

modelo <- lm(incidencias ~ horas)

# Mostramos un resumen completo del modelo
# Obtención los coeficientes y el valor R^2
print("--- Resumen del Modelo de Regresión ---")
summary(modelo)

# Añadir la recta de regresión al gráfico anterior
abline(modelo, col = "red", lty = 2) # lty = 2 es línea punteada


# 4. PREDICCIÓN PARA 11 HORAS
# Creamos un data.frame con el nuevo valor que queremos predecir
# Debe tener el mismo nombre de variable que el modelo ("horas")

nuevas_horas <- data.frame(horas = 11)

# Usamos la función predict() sobre nuestro modelo
prediccion <- predict(modelo, newdata = nuevas_horas)

# Imprimimos la predicción
print(paste("Predicción para 11 horas:", round(prediccion, 2), "incidencias"))


# 5. EVALUACIÓN DEL MODELO (COEFICIENTE R²)
# El valor R² ya se mostró en el 'summary(modelo)' del paso 3. Lo guardamos en una variable.

r_cuadrado <- summary(modelo)$r.squared

print(paste("Coeficiente de Determinación (R²):", round(r_cuadrado, 4)))