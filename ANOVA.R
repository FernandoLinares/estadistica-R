library(readxl)
DH <- read_excel("C:/Users/Usuario/Downloads/DH.xlsx")


str(DH)

# Carga de paquete necesario
library(car)


# Selecciona todas las variables numéricas excepto 'riesgo_hipertension'
variables_dependientes <- colnames(DH)[!colnames(DH) %in% c("riesgo_hipertension")]

# Almacena los resultados del análisis de covarianza para cada variable dependiente
results <- list()

# Itera sobre cada variable dependiente y realiza un análisis de covarianza
for (var in variables_dependientes) {
  # Asegúrate de que no haya valores faltantes en estas variables
  DH_subset <- na.omit(DH[c("riesgo_hipertension", var)])
  
  # Ejecución del análisis de covarianza
  cov_anova <- aov(as.formula(paste(var, "~ riesgo_hipertension")), data = DH_subset)
  
  # Almacena los resultados
  results[[var]] <- summary(cov_anova)
}

# Muestra los resultados
results

