library(rjags)
library(corrplot)
library(polycor)
library(glm2)
library(psych)
library(GGally)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(car)
library(nortest)
library(MASS)
library(caTools)



#### Cargamos la base de datos
info <- read.csv("C:/Users/Itzel/Downloads/calories-exercise - exercise.csv")
head(info)
summary(info)

gender <- as.factor(info$Gender)
age <- info$Age
height <- info$Height
weight <- info$Weight
duration <- info$Duration
heart_rate <- info$Heart_Rate
temp <- info$Body_Temp
calories <- info$Calories

### dataframe
datos <- data.frame(gender, age, height, weight, duration, heart_rate, temp, calories)

##Analisis descriptivo
multi.hist(x = datos[2:8], dcol = c("blue", "red"), 
           main = c("Age", "Height", "Weight","Duration", "Heart rate",
                    "Body Temp", "Calories"))
boxplot(datos[2:8], main = c("Age", "Height", "Weight","Duration", 
                       "Heart rate","Body Temp", "Calories"))

ggpairs(datos[2:8], lower = list(continuous = "smooth"), 
        diag = list(continuous = "barDiag"), axisLabels = "none")

#### Proponemos el modelo
modelo <- lm(calories ~ gender + age + height + weight + duration + heart_rate + temp)
summary(modelo)

#Usamos el stpwise
step(object = modelo, direction = "both", trace = 1)

#Obtenemos lo intervalos de confianza del modelo con el stepwise l cual sigue igual
confint(modelo)

#Relacion lineal entre los predictores numericos y la variable respuesta:
plot1 <- ggplot(data = datos[2:8], aes(age, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos[2:8], aes(height, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos[2:8], aes(weight, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos[2:8], aes(duration, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot5 <- ggplot(data = datos[2:8], aes(heart_rate, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot6 <- ggplot(data = datos[2:8], aes(temp, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4,plot5, plot6)

#Tabla ANOVA
anova(modelo)
#Veamos el vif
vif(modelo)

##########################      AQUI SE PROPONE EL MODELO SIN LA VARIABLE PESO       ############################

#Modelo sin la variable peso
modelo2 <- lm(calories ~ gender + age + height + duration + heart_rate + temp )
#Realizamos el resumen
summary(modelo2)

#Usamos el stpwise
step(object = modelo2, direction = "both", trace = 1)
#Y tambien vif
vif(modelo2)
confint(modelo2)
#Anova
anova(modelo2)

#Residuales
residuales2 <-rstandard(modelo2)

#Realizamos el histograma
hist(residuales2, probability = TRUE, main = "Residuales del modelo",
     col ="light blue", ylim = c(0,0.05))
lines(density(residuales2), col = "blue", lwd = 2)

#Pruebas de normalidad
#H0: Los residuales se distribuyen Normal
ad.test(residuales2)
lillie.test(residuales2)
cvm.test(residuales2)
# p-value < 0.05 para las tres pruebas. Se rechaza normalidad

#Prueba de correlacion
#H0: La correlacion de los residuales es igual a cero

#Graficamos la autocorrelacion de los residuales 
acf(residuales2, main = "Correlograma de los residuales")

dwtest(modelo2)
#p-value > 0.05 No rechazamos H0
#La correlacion es cero


#Pruebas de homocedasticidad
#H0: Los residuales tienen Varianza constante
bptest(modelo2)
ncvTest(modelo2)
#p-value < 0.05 Rechazamos H0.
#La varianza no es constante

###################---------Modelo sin variable peso y transformado------#############################

powerTransform(modelo2)

##grafica
boxcox(modelo2)

bc<-boxcox(modelo2)
lambda <- bc$x[which.max(bc$y)]

#Como lambda = 0.5093, y lambda= 0.5050505, entonces la transformacion
# a las variables respuesta y regresoras debe ser la raiz cuadrada

### cargamos los datos
edad_sqrt <- sqrt(age)
altura_sqrt <- sqrt(height)
duracion_sqrt <- sqrt(duration)
frec_card_sqrt <- sqrt(heart_rate)
temp_sqrt <- sqrt(temp)
cal_sqrt <- sqrt(calories)

#### dataframe

datos2 <- data.frame(gender, edad_sqrt, altura_sqrt, duracion_sqrt, 
                     frec_card_sqrt, temp_sqrt, cal_sqrt)


modelo_trans <- lm(cal_sqrt ~ gender + edad_sqrt + altura_sqrt + 
                     duracion_sqrt + frec_card_sqrt + temp_sqrt)

#Realizamos el resumen
summary(modelo_trans)

### valores de calorias tranformados vs ajustados por el modelo clasico
plot(cal_sqrt, modelo_trans$fitted.values)

#Usamos el stpwise
step(object = modelo_trans, direction = "both", trace = 1)
#Y tambien vif
vif(modelo_trans)
confint(modelo_trans)
#Anova
anova(modelo_trans)

#Residuales
residuales_trans <-rstandard(modelo_trans)

#Realizamos el histograma
hist(residuales_trans, probability = TRUE, main = "Residuales del modelo",
     col ="light blue")
lines(density(residuales_trans), col = "blue", lwd = 2)

#Pruebas de normalidad
#H0: Los residuales se distribuyen Normal
ad.test(residuales_trans)
lillie.test(residuales_trans)
cvm.test(residuales_trans)
# p-value < 0.05 para las tres pruebas. Se rechaza normalidad

#Prueba de correlacion
#H0: La correlacion de los residuales es igual a cero

#Graficamos la autocorrelacion de los residuales 
acf(residuales_trans, main = "Correlograma de los residuales")

dwtest(modelo_trans)
#p-value > 0.05 No rechazamos H0
#La correlacion es cero


#Pruebas de homocedasticidad
#H0: Los residuales tienen Varianza constante
bptest(modelo_trans)
ncvTest(modelo_trans)
#p-value < 0.05 Rechazamos H0.
#La varianza no es constante


