#Abrir archivo de varibles
library(readxl)

# Leer la primera hoja del archivo Excel
chihuahuense <- read_excel("Chihuahuense.xlsx")

# Instalar y cargar paquetes necesarios
library(tidyr)
library(ggplot2)
library(dplyr)
library(terra)
library(openair)

##### Modelos de regresión lineal multiple ####################

mod <-lm(CO ~ PH + NA_CMOL + K + CA + MG + BIO2 +BIO3 + BIO4 + BIO5 + BIO9 + BIO13 + BIO17 + BIO18, data=chihuahuense) #Se define un modelo en el que se indica que CO es la variable dependiente
summary(mod) #Resumen del modelo
anova(mod)

#histograma de los residuales
hist(residuals(mod))
par(mfrow=c(1,3))#para abrir un cuadro de gráficos de una fila con tres columnas
plot(mod, which=c(1,2,4,5), caption = list("Residuos vs Ajustados","Q-Q normal", " ", " ","Residuos vs Apalancamiento", "")) #dibujamos los gráficos de resumen del comportamiento de los errores
par(mfrow=c(1,1)) #volvemos a dejar el cuadro de gráficos con una sola fila y una sola columna

############# Stepwise Forward Selection (selección hacia adelante) #############
modelo_forward <- step(lm(CO ~ 1, data = chihuahuense),  # Modelo nulo (solo el intercepto)
                       scope = formula(mod),  # Alcance del modelo completo
                       direction = "forward")  # Especifica que es selección hacia adelante

# Resumen del modelo resultante
summary(modelo_forward)


########### Stepwise Backward Elimination (eliminación hacia atrás) ##########################
modelo_backward <- step(mod,  # Comienza con el modelo completo
                        direction = "backward")  # Especifica que es eliminación hacia atrás

# Resumen del modelo resultante
summary(modelo_backward)

########## Stepwise Both (Selección hacia adelante y atrás) #################
modelo_both <- step(mod, 
                    direction = "both")  # Ambos, hacia adelante y hacia atrás

# Resumen del modelo resultante
summary(modelo_both)

# Graficar los residuos del modelo resultante
par(mfrow = c(2, 2))
plot(modelo_forward)
plot(modelo_backward)
plot(modelo_both)

###############verificación de multicolinealidad ##############

# Si aún no lo tienes instalado
#install.packages("car")
library(car)

# Calcular el VIF para los modelos
vif(modelo_backward)  # Valores > 10 indican multicolinealidad
vif(modelo_forward)
vif(modelo_both)

# Cargar las librerías necesarias
#install.packages("caret")
library(caret)

# Definir la configuración para k-fold cross-validation (k=10 es común)
control <- trainControl(method = "cv", number = 10)  # 10 folds

# Ajustar el modelo con validación cruzada para modelo both
modelo_cv_both <- train(CO ~ BIO4 + BIO13 + BIO17, 
                   data = chihuahuense, 
                   method = "lm", 
                   trControl = control)

# Resumen del modelo con validación cruzada
print(modelo_cv_both)

# Ajustar el modelo con validación cruzada para modelo forward
modelo_cv_fw <- train(CO ~ BIO18 + BIO4 + BIO17, 
                        data = chihuahuense, 
                        method = "lm", 
                        trControl = control)

# Resumen del modelo con validación cruzada
print(modelo_cv_fw)

# Ajustar el modelo con validación cruzada para modelo backward
modelo_cv_bw <- train(CO ~ BIO4 + BIO13 + BIO17, 
                      data = chihuahuense, 
                      method = "lm", 
                      trControl = control)

# Resumen del modelo con validación cruzada
print(modelo_cv_bw)


####################     Validación Manual (Train-Test Split)    ########################3
# Dividir el conjunto de datos en entrenamiento (70%) y prueba (30%)
set.seed(123)  # Para reproducibilidad
trainIndex <- createDataPartition(chihuahuense$CO, p = 0.7, list = FALSE)

# Crear conjunto de entrenamiento y prueba
train_data <- chihuahuense[trainIndex, ]
test_data <- chihuahuense[-trainIndex, ]

# Ajustar el modelo con los datos de entrenamiento para modelo both
mod_train_both <- lm(CO ~ BIO4 + BIO13 + BIO17, data = train_data)

# Predecir en el conjunto de prueba
predicciones_both <- predict(mod_train_both, newdata = test_data)

# Evaluar el desempeño del modelo both
actual_both <- test_data$CO
rmse_both <- sqrt(mean((predicciones_both - actual_both)^2))  # Calcular el RMSE
rsq_both <- cor(predicciones_both, actual_both)^2  # Calcular el R²

# Mostrar los resultados
cat("RMSE:", rmse_both, "\n")
cat("R²:", rsq_both, "\n")

# Ajustar el modelo con los datos de entrenamiento para modelo forward
mod_train_fw <- lm(CO ~ BIO18 + BIO4 + BIO17, data = train_data)

# Predecir en el conjunto de prueba
predicciones_fw <- predict(mod_train_fw, newdata = test_data)

# Evaluar el desempeño del modelo forward
actual_fw <- test_data$CO
rmse_fw <- sqrt(mean((predicciones_fw - actual_fw)^2))  # Calcular el RMSE
rsq_fw <- cor(predicciones_fw, actual_fw)^2  # Calcular el R²

# Mostrar los resultados
cat("RMSE:", rmse_fw, "\n")
cat("R²:", rsq_fw, "\n")

# Ajustar el modelo con los datos de entrenamiento para modelo backward
mod_train_bw <- lm(CO ~ BIO4 + BIO13 + BIO17, data = train_data)

# Predecir en el conjunto de prueba
predicciones_bw <- predict(mod_train_bw, newdata = test_data)

# Evaluar el desempeño del modelo backward
actual_bw <- test_data$CO
rmse_bw <- sqrt(mean((predicciones_bw - actual_bw)^2))  # Calcular el RMSE
rsq_bw <- cor(predicciones_bw, actual_bw)^2  # Calcular el R²

# Mostrar los resultados
cat("RMSE:", rmse_bw, "\n")
cat("R²:", rsq_bw, "\n")

#### Como nuestros modelos no tienen el mejor desempeño, podemos explorar algunas transformaciones

#Transformación

RegMat$Log1pSOC <- log1p(RegMat$SOC)

mod5 <-lm(Log1pSOC~Moisture_Stress_Index,data=RegMat ) #Se define un modelo en el que se indica que SOC es la variable dependiente y Moisture_Stress_Index es la independiente

plot(RegMat$Log1pSOC ~ RegMat$Moisture_Stress_Index)#Volvemso a dibujar la nube de puntos
abline(mod5,col="magenta")#agragamos la línea de regres

summary(mod5) #Resumen del modelo

par(mfrow=c(1,3))#para abrir un cuadro de gráficos de una fila con tres columnas
plot(mod5, which=c(1,2,5), caption = list("Residuos vs Ajustados","Q-Q normal", " ", " ","Residuos vs Apalancamiento", "")) #dibujamos los gráficos de resumen del comportamiento de los errores
par(mfrow=c(1,1)) #volvemos a dejar el cuadro de gráficos con una sola fila y una sola columna



