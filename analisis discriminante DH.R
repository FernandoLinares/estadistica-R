
library(readxl)
DH <- read_excel("C:/Users/Usuario/Downloads/DH.xlsx")
View(DH)

str(DH)

#comporbamos valores nulos
colSums(is.na(DH))

#tomamos solo los datos numericos
datos_num <- DH[,1:28]

# cargar el paquete MASS si aún no está instalado
library(dplyr)
library(MASS)
library(car)

# Ajustar el modelo de análisis discriminante lineal
modelo_lda <- lda(riesgo_hipertension ~ ., data = datos_num)

# Resumen del modelo
summary(modelo_lda)


# Predecir las clases
predicciones <- predict(modelo_lda)
print(predicciones)

# Matriz de confusión
tabla_confusion <- table(predicciones$class, datos_num$riesgo_hipertension)
print(tabla_confusion)

# Precisión del modelo
precision <- sum(diag(tabla_confusion)) / sum(tabla_confusion)
print(precision)

# Sensibilidad (tasa de verdaderos positivos)
sensibilidad <- tabla_confusion[2, 2] / sum(tabla_confusion[2, ])
print(paste("Sensibilidad (TPR):", sensibilidad))

# Especificidad (tasa de verdaderos negativos)
especificidad <- tabla_confusion[1, 1] / sum(tabla_confusion[1, ])
print(paste("Especificidad (TNR):", especificidad))

# Tasa de falsos positivos (FPR)
fpr <- 1 - especificidad
print(paste("Tasa de Falsos Positivos (FPR):", fpr))



library(ROCR)

# Calcula las probabilidades de clase
probabilidades <- predict(modelo_lda, type = "response")$posterior[, 2]

# Calcula la curva ROC
roc_obj <- prediction(probabilidades, datos_num$riesgo_hipertension)
perf <- performance(roc_obj, "tpr", "fpr")
plot(perf, main = "Curva ROC")



# nuevos datos
library(readxl)
datosnuevos <- read_excel("C:/Users/Usuario/Downloads/datosnuevos.xlsx")


# Hacer predicciones con el modelo
predicciones_nuevas <- predict(modelo_lda, newdata = datosnuevos)

# Mostrar las predicciones
print(predicciones_nuevas)

# Acceder a la primera predicción
primera_prediccion <- predicciones_nuevas$class[1:10]

# Mostrar la primera predicción
print(primera_prediccion)

library(openxlsx)
# Crear un data frame con las primeras 500 predicciones
datos_prediccion <- data.frame(observacion = 1:500, clasificacion = primera_prediccion)

# Guardar el data frame en un archivo Excel
write.xlsx(datos_prediccion, file = "predicciones.xlsx", rowNames = FALSE)


library(readxl)
predicciones <- read_excel("C:/Users/Usuario/Desktop/seminario/trabajo seminario en r/predicciones.xlsx")

library(ggplot2)

# Binarizar datos_reales en 0 y 1 usando un umbral
umbral <- 0.5
predicciones$Datos_Reales <- ifelse(predicciones$datos_reales >= umbral, 1, 0)

# Graficar
ggplot(predicciones, aes(x = observacion, y = clasificacion, color = factor(Datos_Reales))) +
  geom_point(size = 3) +  # Puntos de datos
  geom_abline(intercept = 0, slope = 1, color = "blue") +  # Línea de clasificación ideal
  labs(x = "Observación", y = "Clasificación") +  # Etiquetas de los ejes
  scale_color_manual(values = c("red", "blue"), labels = c("0", "1")) +  # Etiqueta de colores
  theme_minimal()  # Estilo del gráfico



# Contar puntos mal clasificados
predicciones$MalClasificados <- ifelse(predicciones$clasificacion != predicciones$datos_reales, 1, 0)

# Calcular el total de puntos mal clasificados
total_mal_clasificados <- sum(predicciones$MalClasificados)

# Mostrar el total de puntos mal clasificados
total_mal_clasificados

