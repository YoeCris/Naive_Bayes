# Librerías necesarias
library(readr)
library(dplyr)

# Función para cargar y preparar los datos como factores
preparar_datos <- function(filepath) {
  data <- read_delim(filepath)
  
  # Filtrar los datos para seleccionar solo los registros de Puno (supongamos que los códigos comienzan con '21')
  data <- subset(data, grepl("^21", id_ubigeo_f00))
  
  # Eliminar las columnas no necesarias
  data <- data %>% select(-id_persona, -fecha_Contacto, -fecha_sintomas, -id_ubigeo_f00, -otros_sintomas, -nauseas, -anosmia_hiposmia, -dolor_pecho)
  
  # Convertir todas las variables de síntomas en factores
  data <- data %>% mutate(across(c("tos", "cefalea", "congestion_nasal", "dificultad_respiratoria", 
                                   "dolor_garganta", "fiebre", "diarrea", "dolor_abdominal", 
                                   "dolor_articulaciones", "dolor_muscular"), as.factor))
  
  # Convertir la variable Flag_sospechoso a factor para la clasificación
  data$Flag_sospechoso <- as.factor(data$Flag_sospechoso)
  
  return(data)
}

# Función para dividir los datos en entrenamiento y prueba
dividir_datos <- function(data) {
  set.seed(123)  # Fijar semilla para reproducibilidad
  trainIndex <- sample(1:nrow(data), 0.8 * nrow(data))
  trainData <- data[trainIndex, ]
  testData <- data[-trainIndex, ]
  return(list(trainData = trainData, testData = testData))
}

# Función para calcular probabilidades a priori
calcular_probabilidades_a_priori <- function(trainData) {
  positivo <- trainData[trainData$Flag_sospechoso == 1, ]
  negativo <- trainData[trainData$Flag_sospechoso == 0, ]
  
  P_sospechoso <- nrow(positivo) / nrow(trainData)
  P_no_sospechoso <- nrow(negativo) / nrow(trainData)
  
  return(list(P_sospechoso = P_sospechoso, P_no_sospechoso = P_no_sospechoso, positivo = positivo, negativo = negativo))
}

# Función para calcular las probabilidades condicionales para una observación tratada como factor
calcular_probabilidades_condicionales <- function(observacion, positivo, negativo, variables) {
  P_X_given_sospechoso <- 1
  P_X_given_no_sospechoso <- 1
  
  for (var in variables) {
    x <- as.character(observacion[[var]])
    
    # Tabla de contingencia para calcular la probabilidad condicional P(variable = x | sospechoso)
    tabla_sospechoso <- table(positivo[[var]]) + 1  # Aplicar suavizado de Laplace (sumar 1)
    tabla_no_sospechoso <- table(negativo[[var]]) + 1  # Aplicar suavizado de Laplace
    
    P_X_sospechoso <- tabla_sospechoso[x] / sum(tabla_sospechoso)
    P_X_no_sospechoso <- tabla_no_sospechoso[x] / sum(tabla_no_sospechoso)
    
    P_X_given_sospechoso <- P_X_given_sospechoso * P_X_sospechoso
    P_X_given_no_sospechoso <- P_X_given_no_sospechoso * P_X_no_sospechoso
  }
  
  return(list(P_X_given_sospechoso = P_X_given_sospechoso, P_X_given_no_sospechoso = P_X_given_no_sospechoso))
}

# Función para hacer una predicción tratada como factor
predecir <- function(observacion, P_sospechoso, P_no_sospechoso, positivo, negativo, variables) {
  probs <- calcular_probabilidades_condicionales(observacion, positivo, negativo, variables)
  P_X_given_sospechoso <- probs$P_X_given_sospechoso
  P_X_given_no_sospechoso <- probs$P_X_given_no_sospechoso
  
  # Calcular P(X)
  P_X <- (P_X_given_sospechoso * P_sospechoso) + (P_X_given_no_sospechoso * P_no_sospechoso)
  
  # Calcular las probabilidades posteriores
  P_sospechoso_given_X <- (P_X_given_sospechoso * P_sospechoso) / P_X
  P_no_sospechoso_given_X <- (P_X_given_no_sospechoso * P_no_sospechoso) / P_X
  
  # Realizar la predicción
  if (P_sospechoso_given_X > P_no_sospechoso_given_X) {
    return(1)  # Sospechoso
  } else {
    return(0)  # No sospechoso
  }
}

# Función para evaluar el modelo y calcular la precisión
evaluar_modelo <- function(testData, P_sospechoso, P_no_sospechoso, positivo, negativo, variables) {
  predicciones <- c()
  
  for (i in 1:nrow(testData)) {
    observacion <- testData[i, ]
    prediccion <- predecir(observacion, P_sospechoso, P_no_sospechoso, positivo, negativo, variables)
    predicciones <- c(predicciones, prediccion)
  }
  
  # Crear tabla de contingencia (matriz de confusión)
  predicciones <- as.factor(predicciones)
  actuales <- testData$Flag_sospechoso
  tabla_contingencia <- table(Predicted = predicciones, Actual = actuales)
  
  print("Tabla de contingencia:")
  print(tabla_contingencia)
  
  # Calcular la precisión
  precision <- sum(diag(tabla_contingencia)) / sum(tabla_contingencia)
  print(paste("Precisión del modelo: ", round(precision * 100, 2), "%", sep = ""))
}

# Función para hacer una predicción personalizada con valores asignados manualmente
hacer_prediccion_personalizada <- function(valores_personalizados, P_sospechoso, P_no_sospechoso, positivo, negativo, variables) {
  prediccion <- predecir(valores_personalizados, P_sospechoso, P_no_sospechoso, positivo, negativo, variables)
  
  if (prediccion == 1) {
    print("Predicción: Sospechoso")
  } else {
    print("Predicción: No sospechoso")
  }
}

# MAIN: Ejecutar el flujo de trabajo

# Ruta del archivo CSV (asegúrate de cambiarla si es necesario)
filepath <- "C:/Users/Dell/OneDrive/Escritorio/COVID.csv"

# Paso 1: Preparar los datos
data <- preparar_datos(filepath)

# Paso 2: Dividir los datos en entrenamiento y prueba
datos_divididos <- dividir_datos(data)
trainData <- datos_divididos$trainData
testData <- datos_divididos$testData

# Paso 3: Calcular las probabilidades a priori
probs_priori <- calcular_probabilidades_a_priori(trainData)
P_sospechoso <- probs_priori$P_sospechoso
P_no_sospechoso <- probs_priori$P_no_sospechoso
positivo <- probs_priori$positivo
negativo <- probs_priori$negativo

# Variables a analizar (sin nauseas, anosmia_hiposmia, dolor_pecho)
variables <- c("tos", "cefalea", "congestion_nasal", "dificultad_respiratoria", 
               "dolor_garganta", "fiebre", "diarrea", "dolor_abdominal", 
               "dolor_articulaciones", "dolor_muscular")

# Paso 4: Evaluar el modelo
evaluar_modelo(testData, P_sospechoso, P_no_sospechoso, positivo, negativo, variables)

# Paso 5: Hacer predicciones personalizadas
valores_personalizados <- data.frame(
  tos = as.factor(1),  # Tiene tos
  cefalea = as.factor(0),  # No tiene cefalea
  congestion_nasal = as.factor(0),  # Tiene congestión nasal
  dificultad_respiratoria = as.factor(0),  # Tiene dificultad respiratoria
  dolor_garganta = as.factor(0),  # No tiene dolor de garganta
  fiebre = as.factor(0),  # Tiene fiebre
  diarrea = as.factor(0),  # No tiene diarrea
  dolor_abdominal = as.factor(0),  # No tiene dolor abdominal
  dolor_articulaciones = as.factor(0),  # No tiene dolor en articulaciones
  dolor_muscular = as.factor(1)  # Tiene dolor muscular
)

hacer_prediccion_personalizada(valores_personalizados, P_sospechoso, P_no_sospechoso, positivo, negativo, variables)
