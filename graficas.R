

library(readxl)
DH <- read_excel("C:/Users/Usuario/Downloads/DH.xlsx")


str(DH)

#comporbamos valores nulos
colSums(is.na(DH))

#tomamos solo los datos numericos
datos_num <- DH[,1:28]

library(ggplot2)

# Ejemplo de gráfico de dispersión entre dos variables
ggplot(DH, aes(x = edad, y = concentracion_hemoglobina)) +
  geom_point() +
  labs(x = "Edad", y = "Concentración de Hemoglobina",
       title = "Relación entre Edad y Concentración de Hemoglobina")

ggplot(DH, aes(x = edad, y = riesgo_hipertension)) +
  geom_point() +
  labs(x = "Edad", y = "riesgo de hipertension",
       title = "Relación entre Edad y el riesgo de hipertension")

ggplot(DH, aes(x = peso, y = estatura)) +
  geom_point() +
  labs(x = "Peso", y = "Estatura",
       title = "Relación entre peso y estatura")

# Ejemplo de histograma para una variable
ggplot(DH, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Edad", y = "Frecuencia",
       title = "Distribución de Edades")

ggplot(DH, aes(x = peso)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(x = "Peso", y = "Frecuencia",
       title = "Distribución del Peso")
# Ejemplo de gráfico de caja para una variable
ggplot(DH, aes(y = edad)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(y = "Edad",
       title = "Distribución de Edades")
ggplot(DH, aes(y = peso)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(y = "Peso",
       title = "Distribución del Peso")

library(corrplot)

# Calculamos la matriz de correlación
cor_mat <- cor(datos_num)

# Gráfico de mapa de calor de correlación
corrplot(cor_mat, method = "color", type = "upper",
         addCoef.col = "black", tl.col = "black",
         tl.srt = 45, diag = FALSE)

# Graficar la matriz de correlación
corrplot(correlaciones, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Graficar la matriz de correlación
corrplot(correlaciones, method = "color", type = "upper", tl.col = "black", tl.srt = 45, tl.cex = 0.7)

