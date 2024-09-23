#install.packages("FrF2")
library(FrF2)

#Generar el diseño factorial fraccionado 2^(5-1) con dos réplicas
Experimento <- FrF2(nruns = 16, nfactors = 5, replications = 2, 
               factor.names = c("Corriente", "Electrolitos", "Tiempo", "pH", "Antibiotico"))


### CAMBIAR CUANDO SE TENGA VECTOR DE DATOS ###
#Definir el número de observaciones
num_observations <- 16 * 2  # 16 corridas * 2 réplicas
set.seed(202407)  #Fijar la semilla para reproducibilidad
Concentraciones <- runif(num_observations, min = 0, max = 100)

Experimento_resp<-add.response(Experimento,Concentraciones)

#Gráfica de efectos individuales
MEPlot(Experimento_resp)

#Gráfica de efectos asociados
IAPlot(Experimento_resp)

#Ajustar un modelo lineal al diseño factorial fraccionado
Modelo <- lm(Concentraciones ~ (.)^2, data = Experimento_resp)
summary(Modelo)

#Análisis de varianza
Anova<-aov(Concentraciones ~ (.)^2, data = Experimento_resp)
summary(Anova)

# Verificación de la normalidad de los residuales
residuales <- residuals(Modelo)

# Gráfico Q-Q para los residuales
qqnorm(residuales)
qqline(residuales)


# Prueba de Shapiro-Wilk para normalidad
shapiro.test(residuales)


# Histograma de los residuales
hist(residuales, main = "Histograma de los residuales estandarizados", xlab = "Residuales")

# Gráfico de residuales vs valores ajustados
plot(fitted(Modelo), residuales, xlab = "Valores ajustados", ylab = "Residuales")
title("Residuales vs Valores ajustados")
abline(h = 0, col = "red")

# Gráfica de los efectos estimados
efectos <- 2 * Modelo$coef[2:length(Modelo$coef)]
efectos

# Gráfico normal de probabilidades para los efectos
qqnorm(efectos)
qqline(efectos)

