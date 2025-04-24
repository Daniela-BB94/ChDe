
#Abrir archivo de varibles
install.packages("readxl")
library(readxl)

# Leer la primera hoja del archivo Excel
chihuahuense2 <- read_excel("base_Chihuahuense2.xlsx")

# Instalar y cargar paquetes necesarios
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyr)
library(ggplot2)
library(dplyr)

# Muestra las primeras filas del data frame
head(chihuahuense2) 
# Muestra la estructura del data frame
str(chihuahuense2)   

# Filtrar los datos para cada categoría
df_Ar <- subset(chihuahuense2, AMBIENTE == "Ar")
df_HAr <- subset(chihuahuense2, AMBIENTE == "HAr")
df_SAM <- subset(chihuahuense2, AMBIENTE == "SAM")

# Revisar los data frames
head(df_Ar)
head(df_HAr)
head(df_SAM)

# Revisar la estructura de los data frames
str(df_Ar)
str(df_HAr)
str(df_SAM)

# Añadir una columna 'Categoria' para identificar cada data frame
df_Ar$Categoria <- "Ar"
df_HAr$Categoria <- "HAr"
df_SAM$Categoria <- "SAM"

# Combinar los data frames en uno solo
datos_combinados <- bind_rows(df_Ar, df_HAr, df_SAM)

#Especificar el orden deseado de las categorias
orden_categorias <- c("HAr","Ar", "SAM")

#Convertir la columna "Categoria" en un factor con el orden deseado
datos_combinados$Categoria <- factor(datos_combinados$Categoria, levels= orden_categorias)

# Crear el histograma de pH
ggplot(datos_combinados, aes(x = PH, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 0.8, alpha = 0.7) +
  labs(title = "Histograma de pH por Ambiente",
       x = "PH",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$PH, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de Ca
ggplot(datos_combinados, aes(x = `CA_CMOL.L-`, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 4, alpha = 0.7) +
  labs(title = "Histograma de Ca por Ambiente",
       x = "CA_CMOL.L-",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$`CA_CMOL.L-`, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de MO
ggplot(datos_combinados, aes(x = MO, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 0.7, alpha = 0.7) +
  labs(title = "Histograma de MO por Ambiente",
       x = "%",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$MO, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de CO
ggplot(datos_combinados, aes(x = CO, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 3, alpha = 0.7) +
  labs(title = "Histograma de CO por Ambiente",
       x = "%",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$CO, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de contenidos de CO
ggplot(datos_combinados, aes(x = `CO_KG.M-2`, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 3, alpha = 0.7) +
  labs(title = "Histograma de CO por Ambiente",
       x = "KG.M-2",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$`CO_KG.M-2`, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de CE
ggplot(datos_combinados, aes(x = C_E, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.7) +
  labs(title = "Histograma de CE por Ambiente",
       x = "CE",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$C_E, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de BIO2
ggplot(datos_combinados, aes(x = BIO2, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 0.6, alpha = 0.7) +
  labs(title = "Rango de temperatura media diurna",
       x = "º C",
       y = "Frecuencia",
       fill = "Ambiente") +
  scale_y_continuous(limits = c(0, max(datos_combinados$BIO2, na.rm = TRUE))) + # Ajustar la escala del eje Y
  theme_minimal()

# Crear el histograma de BIO3
ggplot(datos_combinados, aes(x = BIO3, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 2, alpha = 0.7) +
  labs(title = "Isotermalidad por ambiente",
       x = "Isotermalidad",
       y = "Frecuencia",
       fill = "Ambiente") +
    theme_minimal()

# Crear el histograma de BIO4
ggplot(datos_combinados, aes(x = BIO4, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 48, alpha = 0.8) +
  labs(title = "Estacionalidad de la temperatura por Ambiente",
       x = "Estacionalidad de la Temperatura",
       y = "Frecuencia",
       fill = "Ambiente") +
    theme_minimal()

# Crear el histograma de BIO5
ggplot(datos_combinados, aes(x = BIO5, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 2, alpha = 0.7) +
  labs(title = "Temperatura máxima del mes más cálido",
       x = "º C",
       y = "Frecuencia",
       fill = "Ambiente") +
  theme_minimal()

# Crear el histograma de BIO9
ggplot(datos_combinados, aes(x = BIO9, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth =1, alpha = 0.7) +
  labs(title = "Temperatura promedio del trimestre más seco",
       x = "º C",
       y = "Frecuencia",
       fill = "Ambiente") +
  theme_minimal()

# Crear el histograma de BIO13
ggplot(datos_combinados, aes(x = BIO13, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 25, alpha = 0.7) +
  labs(title = "Precipitación del mes más lluvioso",
       x = "mm",
       y = "Frecuencia",
       fill = "Ambiente") +
  theme_minimal()

# Crear el histograma de BIO17
ggplot(datos_combinados, aes(x = BIO17, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 5, alpha = 0.7) +
  labs(title = "Precipitación del trimestre más seco",
       x = "mm",
       y = "Frecuencia",
       fill = "Ambiente") +
  theme_minimal()

# Crear el histograma de BIO18
ggplot(datos_combinados, aes(x = BIO18, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 25, alpha = 0.7) +
  labs(title = "Precipitación del trimestre más cálido",
       x = "mm",
       y = "Frecuencia",
       fill = "Ambiente") +
  theme_minimal()

# Crear el histograma de pendientes
ggplot(datos_combinados, aes(x = PEN_GRADOS, fill = Categoria)) +
  geom_histogram(position = "dodge", binwidth = 25, alpha = 0.7) +
  labs(title = "Análisis de pendiente",
       x = "grados",
       y = "Frecuencia",
       fill = "Ambiente") +
  theme_minimal()

# Crear el gráfico de violín con medias y barras de variación de pH
ggplot(datos_combinados, aes(x = Categoria, y = PH, fill = Categoria)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "pH",
       x = "Ambiente",
       y = "PH",
       fill = "Ambiente") +
  theme_minimal() 


# Crear el gráfico de violín con media y barras de variación de Ca
ggplot(datos_combinados, aes(x = Categoria, y = `CA_CMOL.L-`, fill = Categoria)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Concentración de Ca",
       x = "Ambiente",
       y = "CMOL.L-",
       fill = "Ambiente") +
  theme_minimal() 

# Crear el gráfico de violín con media y barras de variación de MO
ggplot(datos_combinados, aes(x = Categoria, y = MO, fill = Categoria)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Materia orgánica",
       x = "Ambiente",
       y = "%",
       fill = "Ambiente") +
  theme_minimal() 

# Crear el gráfico de violín con media y barras de variación de CO
ggplot(datos_combinados, aes(x = Categoria, y = `CO_KG.M-2`, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Almacen de carbono orgánico",
       x = "Ambiente",
       y = "Kg.m-2") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de CE
ggplot(datos_combinados, aes(x = Categoria, y = C_E, fill = Categoria)) +
  geom_violin(alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Gráfico de Violín de CE por Ambiente",
       x = "Ambiente",
       y = "CE") +
  theme_minimal() +
  theme(legend.position = "none")

# Crear el gráfico de violín con media y barras de variación de BIO2
ggplot(datos_combinados, aes(x = Categoria, y = BIO2, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Rango de temperatura media diurna",
       x = "Ambiente",
       y = "º C") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO3
ggplot(datos_combinados, aes(x = Categoria, y = BIO3, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Isotermalidad",
       x = "Ambiente",
       y = "Isotermalidad") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO4
ggplot(datos_combinados, aes(x = Categoria, y = BIO4, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Estacionalidad de la temperatura",
       x = "Ambiente",
       y = "Estacionalidad") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO5
ggplot(datos_combinados, aes(x = Categoria, y = BIO5, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Temperatura máxima del mes más cálido",
       x = "Ambiente",
       y = "º C") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO9
ggplot(datos_combinados, aes(x = Categoria, y = BIO9, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Temperatura promedio del trimestre más seco",
       x = "Ambiente",
       y = "º C") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO13
ggplot(datos_combinados, aes(x = Categoria, y = BIO13, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Precipitación del mes más lluvioso",
       x = "Ambiente",
       y = "mm") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO17
ggplot(datos_combinados, aes(x = Categoria, y = BIO17, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Precipitación del trimestre más seco",
       x = "Ambiente",
       y = "mm") +
  theme_minimal()

# Crear el gráfico de violín con media y barras de variación de BIO18
ggplot(datos_combinados, aes(x = Categoria, y = BIO18, fill = Categoria)) +
  geom_violin(trim=FALSE, alpha = 0.7) +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange", color = "black", position = position_dodge(0.9)) +
  labs(title = "Precipitación del trimestre más cálido",
       x = "Ambiente",
       y = "mm") +
  theme_minimal()

######################CORRELACION SPEARMAN#####################################

##Para poder visualizar una matriz de correlación
install.packages("corrplot")
library(corrplot)

# Seleccionar las variables edáficas para la matriz de correlación
vars_eda <- chihuahuense2 %>% select(CO, PH, C_E, `NA_CMOL.L-`, `K_CMOL.L-1`, `CA_CMOL.L-`, `MG_CMOL.L-`)

# Calcular la matriz de correlación de Spearman
cor_mat_eda <- cor(vars_eda, method = "spearman", use = "complete.obs")

# Visualizar la matriz de correlación con corrplot
corrplot(cor_mat_eda, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)

# Seleccionar las variables climáticas para la matriz de correlación
vars_clim <- chihuahuense2 %>% select(BIO2,  BIO3,  BIO4,  BIO5,  BIO9, BIO13, BIO17, BIO18)

# Calcular la matriz de correlación de Spearman
cor_mat_clim <- cor(vars_clim, method = "spearman", use = "complete.obs")

# Visualizar la matriz de correlación con corrplot
corrplot(cor_mat_clim, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)

#################CORRELACIÓN SPERMAN POR AMBIENTE######################

# Seleccionar las variables de interés
vars_gral <- chihuahuense2 %>% select(PH, CO, `CA_CMOL.L-`, `NA_CMOL.L-`, `MG_CMOL.L-`, `K_CMOL.L-1` , BIO2,  BIO4, BIO9, BIO13, BIO17, BIO18)
summary(cor_mat_eda)

# Calcular la matriz de correlación de Spearman
cor_mat_gral <- cor(vars_gral, method = "spearman", use = "complete.obs")

# Visualizar la matriz de correlación con corrplot
corrplot(cor_mat_gral, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)

#######SPEARMAN ÁRIDO
# Seleccionar las variables de interés
vars_Ar <- df_Ar %>% select(PH, CO, `CA_CMOL.L-`, `NA_CMOL.L-`, `MG_CMOL.L-`, `K_CMOL.L-1` , BIO2,  BIO4, BIO9, BIO13, BIO17, BIO18)

# Calcular la matriz de correlación de Spearman
cor_mat_Ar <- cor(vars_Ar, method = "spearman", use = "complete.obs")

# Visualizar la matriz de correlación con corrplot
corrplot(cor_mat_Ar, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)

######SPEARMAN HIPERÁRIDO
# Seleccionar las variables de interés
vars_HAr <- df_HAr %>% select(PH, CO, `CA_CMOL.L-`, BIO2,  BIO4, BIO9, BIO13, BIO17, BIO18)

# Calcular la matriz de correlación de Spearman
cor_mat_HAr <- cor(vars_HAr, method = "spearman", use = "complete.obs")

# Visualizar la matriz de correlación con corrplot
corrplot(cor_mat_HAr, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)

#####SPEARMAN SEMIHUMEDO
# Seleccionar las variables de interés
vars_SAM <- df_SAM %>% select(PH, CO, `CA_CMOL.L-`, `NA_CMOL.L-`, `MG_CMOL.L-`, `K_CMOL.L-1` , BIO2,  BIO4, BIO9, BIO13, BIO17, BIO18)

# Calcular la matriz de correlación de Spearman
cor_mat_SAM <- cor(vars_SAM, method = "spearman", use = "complete.obs")

# Visualizar la matriz de correlación con corrplot
corrplot(cor_mat_SAM, method = "number", type = "upper", tl.col = "black", tl.cex = 0.8)

################ ANALISIS DE KRUSKAL-WALLIS PARA TODAS LAS VARIABLES ################

library(dunn.test)
# Realizar el test de Kruskal-Wallis
kw_test <- kruskal.test(CO~ AMBIENTE, data = chihuahuense2)

# Mostrar resultados del test de Kruskal-Wallis
print(kw_test)

# Si el p-valor es menor que 0.05, realizar pruebas post-hoc de Dunn
if(kw_test$p.value < 0.05) {
  dunn_test <- dunn.test(chihuahuense2$CO, chihuahuense2$AMBIENTE, method = "bonferroni")
  print(dunn_test)
}


# Lista de variables continuas que deseas comparar
variables_continuas <- c("CO", "PH", "CA_CMOL.L-", "NA_CMO.L-" , "BIO2", "BIO4", "BIO9", "BIO13", "BIO17", "BIO18")

# Verificar las columnas antes de proceder
print(colnames(data))

# Realizar el test de Kruskal-Wallis para cada variable continua
for (var in variables_continuas) {
  cat("Resultados del test de Kruskal-Wallis para", var, ":\n")
  
  # Corregir la llamada a la variable
  kruskal_result <- kruskal.test(as.formula(paste(var, "~ AMBIENTE")), data = chihuahuense2)
  
  print(kruskal_result)
  cat("\n-----------------------------\n")
}

###############     REGRESIÓN LINEAL MULTIPLE     #######################

# Realizar una regresión lineal múltiple
# Asegúrate de que los nombres de las columnas coincidan exactamente con los de tu archivo
modelo <- lm(CO ~ PH + BIO2 + BIO4 + BIO9 + BIO17 + BIO18, data = chihuahuense2)

# Resumen de la regresión
summary(modelo)

#Revisar multicolinearidad
# Instalar el paquete si no lo tienes
install.packages("car")
library(car)

# Calcular el VIF
vif(modelo)

# Graficar los residuos del modelo
par(mfrow = c(2, 2))
plot(modelo)

# Realizar una regresión lineal múltiple
# Asegúrate de que los nombres de las columnas coincidan exactamente con los de tu archivo
modelo1 <- lm(CO ~ BIO4 +BIO5 + BIO17 + BIO18, data = chihuahuense2)

# Resumen de la regresión
summary(modelo1)

#Revisar multicolinearidad

# Calcular el VIF
vif(modelo1)

# Graficar los residuos del modelo
par(mfrow = c(2, 2))
plot(modelo1)

# Asegúrate de que los nombres de las columnas coincidan exactamente con los de tu archivo
modelo2 <- lm(CO ~ PH, data = chihuahuense2)

# Resumen de la regresión
summary(modelo2)

#Revisar multicolinearidad

# Calcular el VIF
vif(modelo2)

# Graficar los residuos del modelo
par(mfrow = c(2, 2))
plot(modelo2)

# Asegúrate de que los nombres de las columnas coincidan exactamente con los de tu archivo
modelo3 <- lm(CO ~ BIO4 + BIO17 + BIO18, data = chihuahuense2)

# Resumen de la regresión
summary(modelo3)

#Revisar multicolinearidad

# Calcular el VIF
vif(modelo3)

# Graficar los residuos del modelo
par(mfrow = c(2, 2))
plot(modelo1)

##Análsis de Componentes Principales (PCA) para variables edáficas y climáticas seleccionadas###########

# Instalar y cargar los paquetes necesarios
install.packages("ggplot2")
install.packages("factoextra")

library(ggplot2)
library(dplyr)
library(factoextra)

# Seleccionar las variables para el PCA
vars <- chihuahuense2 %>% select(PH, CO, `NA_CMOL.L-`, `K_CMOL.L-1`, `CA_CMOL.L-`,`MG_CMOL.L-`, BIO2, BIO4, BIO9, BIO13, BIO18)

# Escalar los datos (importante para PCA cuando las variables están en diferentes unidades)
vars_scaled <- scale(vars)

# Realizar el análisis de componentes principales (PCA)
pca_result <- prcomp(vars_scaled, center = TRUE, scale. = TRUE)

# Mostrar un resumen del PCA
summary(pca_result)

# Graficar la varianza explicada por cada componente principal
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

# Graficar los individuos (muestras) en el espacio de los primeros dos componentes principales
fviz_pca_ind(pca_result,
             geom.ind = "point", # Mostrar los puntos de los individuos
             col.ind = "cos2",   # Color basado en la calidad de la representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)       # Evitar que las etiquetas se superpongan

# Graficar las variables en el espacio de los componentes principales
fviz_pca_var(pca_result,
             col.var = "contrib", # Color basado en la contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)        # Evitar que las etiquetas se superpongan

# Graficar tanto individuos como variables en el espacio del PCA
fviz_pca_biplot(pca_result, repel = TRUE,
                col.var = "red", # Color para las variables
                col.ind = "blue") # Color para los individuos

# Agregar los resultados del PCA al dataframe original
pca_ind <- as.data.frame(pca_result$x)

# Añadir la columna "AMBIENTE" para colorear los puntos
pca_ind$AMBIENTE <- chihuahuense2$AMBIENTE

# Visualización del PCA con colores según ambiente
fviz_pca_ind(pca_result,
             geom.ind = "point",  # Mostrar los puntos (individuos)
             col.ind = pca_ind$AMBIENTE,  # Color de los puntos según "AMBIENTE"
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),  # Paleta de colores
             addEllipses = TRUE,  # Añadir elipses de confianza por grupo
             legend.title = "Ambiente") + 
  theme_minimal()

# Obtener los scores de los individuos (proyecciones en los componentes principales)
ind_scores <- as.data.frame(pca_result$x)

# Mostrar los primeros scores
head(ind_scores)

# Añadir los scores de los individuos al dataframe original
chihuahuense_with_scores <- cbind(chihuahuense2, ind_scores)

# Mostrar el dataframe con los scores añadidos
head(chihuahuense_with_scores)

# Visualización de los individuos en el espacio PCA (PC1 y PC2) coloreados por ambiente
fviz_pca_ind(pca_result, 
             geom.ind = "point", 
             col.ind = chihuahuense2$AMBIENTE, # Color por ambiente
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Añadir elipses de confianza para cada grupo
             legend.title = "Ambiente") + 
  theme_minimal()

#############ANOVA de una vía para los scores################################

# ANOVA de una vía para PC1
anova_PC1 <- aov(PC1 ~ AMBIENTE, data = chihuahuense_with_scores)

# Mostrar el resumen del ANOVA para PC1
summary(anova_PC1)

# ANOVA de una vía para PC2
anova_PC2 <- aov(PC2 ~ AMBIENTE, data = chihuahuense_with_scores)

# Mostrar el resumen del ANOVA para PC2
summary(anova_PC2)

# Prueba post-hoc de Tukey HSD para PC1
TukeyHSD(anova_PC1)

# Prueba post-hoc de Tukey HSD para PC2
TukeyHSD(anova_PC2)

# Visualización de los scores de PC1 por AMBIENTE
ggplot(chihuahuense_with_scores, aes(x = AMBIENTE, y = PC1, color = AMBIENTE)) +
  geom_boxplot() +
  labs(title = "Scores de PC1 por AMBIENTE", x = "AMBIENTE", y = "PC1") +
  theme_minimal()

# Visualización de los scores de PC2 por AMBIENTE
ggplot(chihuahuense_with_scores, aes(x = AMBIENTE, y = PC2, color = AMBIENTE)) +
  geom_boxplot() +
  labs(title = "Scores de PC2 por AMBIENTE", x = "AMBIENTE", y = "PC2") +
  theme_minimal()

####################PCA de variables edáficas###########################

vars_edafica <- chihuahuense2 %>% select(PH, CO, `NA_CMOL.L-`, `K_CMOL.L-1`, `CA_CMOL.L-`,`MG_CMOL.L-`)

# Escalar los datos (importante para PCA cuando las variables están en diferentes unidades)
vars_scaled_eda <- scale(vars_edafica)

# Realizar el análisis de componentes principales (PCA)
pca_result_eda <- prcomp(vars_scaled_eda, center = TRUE, scale. = TRUE)

# Mostrar un resumen del PCA
summary(pca_result_eda)

# Graficar la varianza explicada por cada componente principal
fviz_eig(pca_result_eda, addlabels = TRUE, ylim = c(0, 100))

# Graficar los individuos (muestras) en el espacio de los primeros dos componentes principales
fviz_pca_ind(pca_result_eda,
             geom.ind = "point", # Mostrar los puntos de los individuos
             col.ind = "cos2",   # Color basado en la calidad de la representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)       # Evitar que las etiquetas se superpongan

# Graficar las variables en el espacio de los componentes principales
fviz_pca_var(pca_result_eda,
             col.var = "contrib", # Color basado en la contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)        # Evitar que las etiquetas se superpongan


# Graficar tanto individuos como variables en el espacio del PCA
fviz_pca_biplot(pca_result_eda, repel = TRUE,
                col.var = "red", # Color para las variables
                col.ind = "blue") # Color para los individuos

# Agregar los resultados del PCA al dataframe original
pca_ind_eda <- as.data.frame(pca_result_eda$x)

# Añadir la columna "AMBIENTE" para colorear los puntos
pca_ind_eda$AMBIENTE <- chihuahuense2$AMBIENTE

# Visualización del PCA con colores según ambiente
fviz_pca_ind(pca_result_eda,
             geom.ind = "point",  # Mostrar los puntos (individuos)
             col.ind = pca_ind$AMBIENTE,  # Color de los puntos según "AMBIENTE"
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),  # Paleta de colores
             addEllipses = TRUE,  # Añadir elipses de confianza por grupo
             legend.title = "Ambiente") + 
  theme_minimal()

# Obtener los scores de los individuos (proyecciones en los componentes principales)
ind_scores_eda <- as.data.frame(pca_result_eda$x)

# Mostrar los primeros scores
head(ind_scores_eda)

# Añadir los scores de los individuos al dataframe original
chihuahuense_with_scores_eda <- cbind(chihuahuense2, ind_scores_eda)

# Mostrar el dataframe con los scores añadidos
head(chihuahuense_with_scores_eda)

# Visualización de los individuos en el espacio PCA (PC1 y PC2) coloreados por ambiente
fviz_pca_ind(pca_result_eda, 
             geom.ind = "point", 
             col.ind = chihuahuense2$AMBIENTE, # Color por ambiente
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Añadir elipses de confianza para cada grupo
             legend.title = "Ambiente") + 
  theme_minimal()

#############ANOVA de una vía para los scores################################

# ANOVA de una vía para PC1
anova_PC1_eda <- aov(PC1 ~ AMBIENTE, data = chihuahuense_with_scores_eda)

# Mostrar el resumen del ANOVA para PC1
summary(anova_PC1_eda)

# ANOVA de una vía para PC2
anova_PC2_eda <- aov(PC2 ~ AMBIENTE, data = chihuahuense_with_scores_eda)

# Mostrar el resumen del ANOVA para PC2
summary(anova_PC2_eda)

# Prueba post-hoc de Tukey HSD para PC1
TukeyHSD(anova_PC1_eda)

# Prueba post-hoc de Tukey HSD para PC2
TukeyHSD(anova_PC2_eda)

# Visualización de los scores de PC1 por AMBIENTE
ggplot(chihuahuense_with_scores_eda, aes(x = AMBIENTE, y = PC1, color = AMBIENTE)) +
  geom_boxplot() +
  labs(title = "Scores de PC1 por AMBIENTE", x = "AMBIENTE", y = "PC1") +
  theme_minimal()

# Visualización de los scores de PC2 por AMBIENTE
ggplot(chihuahuense_with_scores_eda, aes(x = AMBIENTE, y = PC2, color = AMBIENTE)) +
  geom_boxplot() +
  labs(title = "Scores de PC2 por AMBIENTE", x = "AMBIENTE", y = "PC2") +
  theme_minimal()

####################PCA de variables climáticas###########################

vars_clima <- chihuahuense2 %>% select(BIO2, BIO4, BIO9, BIO13,BIO18)

# Escalar los datos (importante para PCA cuando las variables están en diferentes unidades)
vars_scaled_clim <- scale(vars_clima)

# Realizar el análisis de componentes principales (PCA)
pca_result_clim <- prcomp(vars_scaled_clim, center = TRUE, scale. = TRUE)

# Mostrar un resumen del PCA
summary(pca_result_clim)

# Graficar la varianza explicada por cada componente principal
fviz_eig(pca_result_clim, addlabels = TRUE, ylim = c(0, 100))

# Graficar los individuos (muestras) en el espacio de los primeros dos componentes principales
fviz_pca_ind(pca_result_clim,
             geom.ind = "point", # Mostrar los puntos de los individuos
             col.ind = "cos2",   # Color basado en la calidad de la representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)       # Evitar que las etiquetas se superpongan

# Graficar las variables en el espacio de los componentes principales
fviz_pca_var(pca_result_clim,
             col.var = "contrib", # Color basado en la contribución
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)        # Evitar que las etiquetas se superpongan


# Graficar tanto individuos como variables en el espacio del PCA
fviz_pca_biplot(pca_result_clim, repel = TRUE,
                col.var = "red", # Color para las variables
                col.ind = "blue") # Color para los individuos

# Agregar los resultados del PCA al dataframe original
pca_ind_clim <- as.data.frame(pca_result_clim$x)

# Añadir la columna "AMBIENTE" para colorear los puntos
pca_ind_clim$AMBIENTE <- chihuahuense2$AMBIENTE

# Visualización del PCA con colores según ambiente
fviz_pca_ind(pca_result_clim,
             geom.ind = "point",  # Mostrar los puntos (individuos)
             col.ind = pca_ind$AMBIENTE,  # Color de los puntos según "AMBIENTE"
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),  # Paleta de colores
             addEllipses = TRUE,  # Añadir elipses de confianza por grupo
             legend.title = "Ambiente") + 
  theme_minimal()

# Obtener los scores de los individuos (proyecciones en los componentes principales)
ind_scores_clim <- as.data.frame(pca_result_clim$x)

# Mostrar los primeros scores
head(ind_scores_clim)

# Añadir los scores de los individuos al dataframe original
chihuahuense_with_scores_clim <- cbind(chihuahuense2, ind_scores_clim)

# Mostrar el dataframe con los scores añadidos
head(chihuahuense_with_scores_clim)

# Visualización de los individuos en el espacio PCA (PC1 y PC2) coloreados por ambiente
fviz_pca_ind(pca_result_clim, 
             geom.ind = "point", 
             col.ind = chihuahuense2$AMBIENTE, # Color por ambiente
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Añadir elipses de confianza para cada grupo
             legend.title = "Ambiente") + 
  theme_minimal()

#############ANOVA de una vía para los scores################################

# ANOVA de una vía para PC1
anova_PC1_clim <- aov(PC1 ~ AMBIENTE, data = chihuahuense_with_scores_clim)

# Mostrar el resumen del ANOVA para PC1
summary(anova_PC1_clim)

# ANOVA de una vía para PC2
anova_PC2_clim <- aov(PC2 ~ AMBIENTE, data = chihuahuense_with_scores_clim)

# Mostrar el resumen del ANOVA para PC2
summary(anova_PC2_clim)

# Prueba post-hoc de Tukey HSD para PC1
TukeyHSD(anova_PC1_clim)

# Prueba post-hoc de Tukey HSD para PC2
TukeyHSD(anova_PC2_clim)

# Visualización de los scores de PC1 por AMBIENTE
ggplot(chihuahuense_with_scores_clim, aes(x = AMBIENTE, y = PC1, color = AMBIENTE)) +
  geom_boxplot() +
  labs(title = "Scores de PC1 por AMBIENTE", x = "AMBIENTE", y = "PC1") +
  theme_minimal()

# Visualización de los scores de PC2 por AMBIENTE
ggplot(chihuahuense_with_scores_clim, aes(x = AMBIENTE, y = PC2, color = AMBIENTE)) +
  geom_boxplot() +
  labs(title = "Scores de PC2 por AMBIENTE", x = "AMBIENTE", y = "PC2") +
  theme_minimal()


#Para la vizualización de la CORRELACIÓN SPEARMAN#
# Carga de librerías necesarias
library(ggplot2)

# Gráfico de dispersión para todo el conjunto de datos entre CO y pH
ggplot(chihuahuense2, aes(x = PH, y = `CO_KG.M-2`)) +
  geom_point(aes(color = AMBIENTE), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black", formula = y ~ poly(x, 1)) +
  labs(title = "Correlación de Spearman",
       x = "pH", y = "CO (Kg M-2)") +
  theme_minimal()

#PRUEBA POST-HOC KRUSKAL-WALLIS#
#instalar el programa de visualización#
install.packages("dunn.test")
library(dunn.test)

# Realizar el test de Kruskal-Wallis
kw_test <- kruskal.test(`CO_KG.M-2`~ AMBIENTE, data = chihuahuense2)

# Mostrar resultados del test de Kruskal-Wallis
print(kw_test)

# Si el p-valor es menor que 0.05, realizar pruebas post-hoc de Dunn
if(kw_test$p.value < 0.05) {
  dunn_test <- dunn.test(chihuahuense2$`CO_KG.M-2`, chihuahuense2$AMBIENTE, method = "bonferroni")
  print(dunn_test)
}


# HACER CORELACIONES PARA CADA CATEGORIA DE AMBIENTE

#Correlaciones SPEARMAN"
# Cálculo de la correlación de Spearman por cada categoría de AMBIENTE para CO y Ca
cor_spearman_cat_Ca <- chihuahuense2 %>%
  group_by(AMBIENTE) %>%
  summarize(correlacion_spearman = cor(`CA_CMOL.L-`, `CO_KG.M-2`, method = "spearman", use = "complete.obs"))

print(cor_spearman_cat_Ca)

#VISUALIZACIÓN DE CORRELACIÓN SPEARMAN PARA CADA AMBIENTE DE CO Y Ca
# Gráfico de dispersión por categorías
ggplot(chihuahuense2, aes(x = `CA_CMOL.L-`, y = `CO_KG.M-2`)) +
  geom_point(aes(color = AMBIENTE), alpha = 0.6) +
  facet_wrap(~AMBIENTE) +
  geom_smooth(method = "lm", se = FALSE, formula = y ~ poly(x, 1)) +
  labs(title = "Correlación de Spearman por Ambiente",
       x = "Ca 2+", y = "CO (Kg M-2)") +
  theme_minimal()


# Filtrar los datos para cada categoría y añadir la columna 'Categoria'
df_Ar <- chihuahuense2 %>% filter(AMBIENTE == "Ar") %>% mutate(Categoria = "Ar")
df_HAr <- chihuahuense2 %>% filter(AMBIENTE == "HAr") %>% mutate(Categoria = "HAr")
df_SAM <- chihuahuense 2%>% filter(AMBIENTE == "SAM") %>% mutate(Categoria = "SAM")

# Combinar los data frames en uno solo
datos_combinados <- bind_rows(df_Ar, df_HAr, df_SAM)

# Especificar el orden deseado de las categorías
orden_categorias <- c("HAr", "Ar", "SAM")

# Convertir la columna 'Categoria' en un factor con el orden deseado
datos_combinados$Categoria <- factor(datos_combinados$Categoria, levels = orden_categorias)

# Calcular la correlación para cada categoría
correlaciones <- datos_combinados %>%
  group_by(Categoria)%>%
  summarize(correlacion = cor(`CO_KG.M-2`, PH, use = "complete.obs"))

print(correlaciones)


###
##
#########PRUEBA DE NORMALIDAD Shapiro-Wilk
#pH
shapiro_test_result <- shapiro.test(datos_combinados$PH)
print(shapiro_test_result)

# Crear el histograma
ggplot(datos_combinados, aes(x = PH)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de PH",
       x = "PH",
       y = "Frecuencia") +
  theme_minimal()

#Prueba de normalidad CO
shapiro_test_result <- shapiro.test(datos_combinados$`CO_KG.M-2`)
print(shapiro_test_result)

# Crear el histograma
ggplot(datos_combinados, aes(x = `CO_KG.M-2`)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de CO",
       x = "CO Kg/m^2",
       y = "Frecuencia") +
  theme_minimal()

#Prueba de normalidad Ca
shapiro_test_result <- shapiro.test(datos_combinados$`CA_CMOL.L-`)
print(shapiro_test_result)

# Crear el histograma
ggplot(datos_combinados, aes(x = `CA_CMOL.L-`)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de Ca",
       x = "Ca cmol/L",
       y = "Frecuencia") +
  theme_minimal()

#Como los datos no fueron normales debido al que el valor de p es menos que 0.05
#en todas las variables, procedemos a hacer transformaciones de los datos

library(MASS)

# Leer el archivo Excel
datos <- read_excel("/mnt/data/base_Chihuahuense2.xlsx")

# Transformación logarítmica
datos_combinados$PH_log <- log(datos_combinados$PH)


# Transformación raíz cuadrada
datos_combinados$PH_sqrt <- sqrt(datos_combinados$PH)

# Transformación inversa
datos_combinados$PH_inv <- 1 / datos_combinados$PH

# Transformación Box-Cox (necesita valores positivos)
boxcox_transform <- boxcox(PH ~ 1, data = datos_combinados, lambda = seq(-2, 2, by = 0.1))
lambda <- boxcox_transform$x[which.max(boxcox_transform$y)]
datos_combinados$PH_boxcox <- (datos_combinados$PH^lambda - 1) / lambda

# Prueba de normalidad Shapiro-Wilk para las transformaciones
shapiro.test(datos_combinados$PH_log)
shapiro.test(datos_combinados$PH_sqrt)
shapiro.test(datos_combinados$PH_inv)
shapiro.test(datos_combinados$PH_boxcox)

# Crear histogramas para evaluar visualmente
ggplot(datos_combinados, aes(x = PH_log)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Histograma de PH (Log Transformación)",
       x = "PH (Log)",
       y = "Frecuencia") +
  theme_minimal()

ggplot(datos_combinados, aes(x = PH_sqrt)) +
  geom_histogram(binwidth = 0.5, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Histograma de PH (Raíz Cuadrada Transformación)",
       x = "PH (Raíz Cuadrada)",
       y = "Frecuencia") +
  theme_minimal()

ggplot(datos_combinados, aes(x = PH_inv)) +
  geom_histogram(binwidth = 0.01, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Histograma de PH (Inversa Transformación)",
       x = "PH (Inversa)",
       y = "Frecuencia") +
  theme_minimal()

ggplot(datos_combinados, aes(x = PH_boxcox)) +
  geom_histogram(binwidth = 0.5, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Histograma de PH (Box-Cox Transformación)",
       x = "PH (Box-Cox)",
       y = "Frecuencia") +
  theme_minimal()

