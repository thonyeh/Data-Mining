library(ISLR)
library(ggplot2)
library(knitr)
data("Weekly")
kable(head(Weekly))
summary(Weekly)

cor_matrix <- round(cor(Weekly[,-9], method = "pearson" ), digits = 4)
cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- ""
as.data.frame(cor_matrix)

ggplot(data = Weekly, aes(x = Year, y = Volume)) +
  geom_point(color = "firebrick") +
  geom_smooth(method = "lm") +
  theme_bw()

#representación mediante Histograma de cada variable para cada dirección 
par(mfcol = c(2, 6))
for (i in 2:7) {
  variable <- names(Weekly)[i]
  rango <- seq(min(Weekly[, i]), max(Weekly[, i]), le = 50)
  for (k in 1:2) {
    grupo <- levels(Weekly$Direction)[k]
    x <- Weekly[Weekly$Direction == grupo, variable]
    hist(x, proba = T, col = grey(0.8), main = grupo, xlab = variable)
    lines(rango, dnorm(rango, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

#representación de cuantiles normales de cada variable para cada dirección 
for (i in 2:7) {
  variable <- names(Weekly)[i]
  rango <- seq(min(Weekly[, i]), max(Weekly[, i]), le = 50)
  for (k in 1:2) {
    grupo <- levels(Weekly$Direction)[k]
    x <- Weekly[Weekly$Direction == grupo, variable]
    qqnorm(x, main = paste(grupo, variable), pch = 19, col = i)
    qqline(x)
  }
}

#Contraste de normalidad Shapiro-Wilk para cada variable en cada dirección
library(reshape2)
library(knitr)
library(dplyr)
Weekly_tidy <- melt(Weekly[,-1], value.name = "valor")
Weekly_tidy %>% group_by(Direction, variable) %>% 
  summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value)



#1.b

modelo_logistico <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly, family = "binomial")
summary(modelo_logistico)

contrasts(Weekly$Direction)


#1.c
predicciones <- predict(object = modelo_logistico, type = "response")
head(predicciones)

prediccion <- data.frame(probabilidad = predicciones, clase = rep(NA,length(predicciones)))
prediccion[prediccion$probabilidad < 0.5,"clase"] <- "Down"
prediccion[prediccion$probabilidad > 0.5,"clase"] <- "Up"
head(prediccion)

table(clase_predicha = prediccion$clase, clase_real = Weekly$Direction)

paste( "% de acierto:", mean(prediccion$clase == Weekly$Direction))

paste( "% de error:",mean(prediccion$clase != Weekly$Direction))

#1.d

train_data <- Weekly[Weekly$Year < 2009,]
test_data <- Weekly[!(Weekly$Year < 2009),]

# Se crea el modelo de regresión logística
modelo <- glm(Direction ~ Lag2, data = train_data, family = "binomial")
summary(modelo)
# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)

# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- "Up"
predicciones2[predicciones2 != "Up"] <- "Down"

table(clase_predicha = predicciones2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones2 != test_data$Direction))


#1.e

library(MASS)
modelo_lda <- lda(Direction ~ Lag2, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$Direction))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$Direction))

test_data$prediccion <- predicciones_lda$class
test_data$acierto <- ifelse(test = test_data$Direction == test_data$prediccion,
                            yes = "Si", no = "No")
ggplot(data = test_data, aes(x = Lag1, y = Lag2, color = Direction,
                             shape = acierto)) +
  geom_point() +
  scale_shape_manual(values = c("No" = 4, "Si" = 19 )) + 
  theme_bw()

#1.f

modelo_qda <- qda(Direction ~ Lag2, data = train_data)
modelo_qda

predicciones_qda <- predict(object = modelo_qda, test_data)
head(predicciones_qda$class)

head(predicciones_qda$posterior)

table(clase_predicha = predicciones_qda$class,
      clase_real =  test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones_qda$class == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones_qda$class != test_data$Direction))

#1.g
library(class)
# En caso de empate entre los vecinos, se elige uno aleatoriamente.
# Esto influye en la reproducibilidad.

set.seed(604)
prediccion_knn2 <- knn(train = matrix(train_data[, "Lag2"]),
                       test = matrix(test_data[, "Lag2"]) ,
                       cl = train_data[,"Direction"], k = 1 )
head(prediccion_knn2)

table(clase_predicha = prediccion_knn2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$Direction))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$Direction))


#1.h

#logistica y LDA

#1.i

# LOGISTICO LAG1 LAG2
modelo <- glm(Direction ~ Lag1 + Lag2, data = train_data, family = "binomial")
summary(modelo)
# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)

# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- "Up"
predicciones2[predicciones2 != "Up"] <- "Down"

table(clase_predicha = predicciones2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones2 != test_data$Direction))


#1.e

library(MASS)
modelo_lda <- lda(Direction ~ Lag1 + Lag2, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$Direction))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$Direction))

test_data$prediccion <- predicciones_lda$class
test_data$acierto <- ifelse(test = test_data$Direction == test_data$prediccion,
                            yes = "Si", no = "No")
ggplot(data = test_data, aes(x = Lag1, y = Lag2, color = Direction,
                             shape = acierto)) +
  geom_point() +
  scale_shape_manual(values = c("No" = 4, "Si" = 19 )) + 
  theme_bw()

#1.f

modelo_qda <- qda(Direction ~ Lag1 + Lag2, data = train_data)
modelo_qda

predicciones_qda <- predict(object = modelo_qda, test_data)
head(predicciones_qda$class)

head(predicciones_qda$posterior)

table(clase_predicha = predicciones_qda$class,
      clase_real =  test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones_qda$class == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones_qda$class != test_data$Direction))

#1.g
library(class)
# En caso de empate entre los vecinos, se elige uno aleatoriamente.
# Esto influye en la reproducibilidad.

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("Lag1", "Lag2")],
                       test = test_data[,c("Lag1", "Lag2")] ,
                       cl = train_data[,"Direction"], k = 3 )
head(prediccion_knn2)

table(clase_predicha = prediccion_knn2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$Direction))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$Direction))


set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("Lag1", "Lag2")],
                       test = test_data[,c("Lag1", "Lag2")] ,
                       cl = train_data[,"Direction"], k = 6 )
head(prediccion_knn2)

table(clase_predicha = prediccion_knn2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$Direction))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$Direction))


# LOGISTICO LAG1 LAG2 LAG1*LAG2
modelo <- glm(Direction ~ Lag1*Lag2, data = train_data, family = "binomial")
summary(modelo)
# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)

# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- "Up"
predicciones2[predicciones2 != "Up"] <- "Down"

table(clase_predicha = predicciones2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones2 != test_data$Direction))


#1.e

library(MASS)
modelo_lda <- lda(Direction ~ Lag1*Lag2, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$Direction))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$Direction))

test_data$prediccion <- predicciones_lda$class
test_data$acierto <- ifelse(test = test_data$Direction == test_data$prediccion,
                            yes = "Si", no = "No")
ggplot(data = test_data, aes(x = Lag1, y = Lag2, color = Direction,
                             shape = acierto)) +
  geom_point() +
  scale_shape_manual(values = c("No" = 4, "Si" = 19 )) + 
  theme_bw()

#1.f

modelo_qda <- qda(Direction ~ Lag1*Lag2, data = train_data)
modelo_qda

predicciones_qda <- predict(object = modelo_qda, test_data)
head(predicciones_qda$class)

head(predicciones_qda$posterior)

table(clase_predicha = predicciones_qda$class,
      clase_real =  test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones_qda$class == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones_qda$class != test_data$Direction))

#1.g
library(class)
# En caso de empate entre los vecinos, se elige uno aleatoriamente.
# Esto influye en la reproducibilidad.

set.seed(604)
train_data$Lag1Lag2 <- train_data$Lag1*train_data$Lag2
test_data$Lag1Lag2 <- test_data$Lag1*test_data$Lag2

prediccion_knn2 <- knn(train = train_data[,c("Lag1", "Lag2","Lag1Lag2")],
                       test = test_data[,c("Lag1", "Lag2","Lag1Lag2")] ,
                       cl = train_data[,"Direction"], k = 3 )
head(prediccion_knn2)

table(clase_predicha = prediccion_knn2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$Direction))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$Direction))


set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("Lag1", "Lag2","Lag1Lag2")],
                       test = test_data[,c("Lag1", "Lag2","Lag1Lag2")] ,
                       cl = train_data[,"Direction"], k = 6 )
head(prediccion_knn2)

table(clase_predicha = prediccion_knn2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$Direction))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$Direction))


hist(log(train_data$Volume))

modelo <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + log(Volume), data = Weekly, family = "binomial")
summary(modelo)
# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)

# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- "Up"
predicciones2[predicciones2 != "Up"] <- "Down"

table(clase_predicha = predicciones2, clase_real = test_data$Direction)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$Direction))


# % de errores
paste("% de error:", mean(predicciones2 != test_data$Direction))

##PREGUNTA 2

#2.a
Auto <- data.frame(Auto)
Auto$mpg1 <- rep(0,dim(Auto)[1])

for ( i in 1:dim(Auto)[1]) {
  if (Auto$mpg[i] > median(Auto$mpg) ){ Auto$mpg1[i] = 1}
}

Auto$mpg1 <- factor(Auto$mpg1)
head(Auto)

#2.b
library(ggplot2)
library(scatterplot3d)
library(GGally)

p <- ggpairs(Auto[,-9],aes(color = factor(mpg1)))+theme_bw()

for(i in 1:p$nrow){
  for (j in 1:p$ncol) {
    p[i,j] <- p[i,j] + scale_fill_manual(values = c("#00AFBB","#E7B800"))+
      scale_color_manual(values = c("#00AFBB","#E7B800"))
  }
}

p

#2.c
set.seed(604)
m = sample(1:dim(Auto)[1],250)

train_data <- Auto[m,]
test_data <- Auto[-m,]
summary(Auto)
#2.d

library(MASS)

modelo <- glm(mpg1 ~ horsepower + weight + year + origin, data = train_data, family = "binomial")

summary(modelo)
# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)


# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- 1
predicciones2[predicciones2 <= 0.5] <- 0
predicciones2 <- as.factor(predicciones2)


table(clase_predicha = predicciones2, clase_real = test_data$mpg1)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$mpg1))


# % de errores
paste("% de error:", mean(predicciones2 != test_data$mpg1))





modelo_lda <- lda(mpg1 ~ horsepower + weight + year + origin, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$mpg1)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$mpg1))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$mpg1))


###2.e

modelo_qda <- qda(mpg1 ~ horsepower + weight + year + origin, data = train_data)
modelo_qda

predicciones_qda <- predict(object = modelo_qda, test_data)
head(predicciones_qda$class)

head(predicciones_qda$posterior)

table(clase_predicha = predicciones_qda$class,
      clase_real =  test_data$mpg1)

# % de aciertos
paste("% de acierto:", mean(predicciones_qda$class == test_data$mpg1))


# % de errores
paste("% de error:", mean(predicciones_qda$class != test_data$mpg1))




#2.g

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("horsepower", "weight","year","origin")],
                       test = test_data[,c("horsepower", "weight","year","origin")] ,
                       cl = train_data[,"mpg1"], k = 6 )



table(clase_predicha = prediccion_knn2, clase_real = test_data$mpg1)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$mpg1))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$mpg1))


# PREGUNTA 3

#3.
Boston$indice <- rep(0,dim(Boston)[1])

for ( i in 1:dim(Boston)[1]) {
  if (Boston$crim[i] > median(Boston$crim) ){ Boston$indice[i] = 1}
}

Boston$indice <- factor(Boston$indice)
head(Boston)

# datos entrenamiento
set.seed(604)
m = sample(1:dim(Boston)[1],400)

train_data <- Boston[m,]
test_data <- Boston[-m,]

#2.b

p <- ggpairs(Boston,aes(color = factor(indice)))+theme_bw()

for(i in 1:p$nrow){
  for (j in 1:p$ncol) {
    p[i,j] <- p[i,j] + scale_fill_manual(values = c("#00AFBB","#E7B800"))+
      scale_color_manual(values = c("#00AFBB","#E7B800"))
  }
}

p

######

# log

modelo <- glm(indice ~ .-crim, data = train_data, family = "binomial")
summary(modelo)

# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)


# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- 1
predicciones2[predicciones2 <= 0.5] <- 0
predicciones2 <- as.factor(predicciones2)

table(clase_predicha = predicciones2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones2 != test_data$indice))


#lda

modelo_lda <- lda(indice ~ .-crim, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$indice))

#KNN k=1

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,2:14],
                       test = test_data[,2:14] ,
                       cl = train_data[,"indice"], k = 1 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

#KNN k=3

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,2:14],
                       test = test_data[,2:14] ,
                       cl = train_data[,"indice"], k = 3 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))


#KNN k=6

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,2:14],
                       test = test_data[,2:14] ,
                       cl = train_data[,"indice"], k = 6 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))


######

######

# log

modelo <- glm(indice ~ .-crim-zn-chas-rm, data = train_data, family = "binomial")
summary(modelo)

# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)


# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- 1
predicciones2[predicciones2 <= 0.5] <- 0
predicciones2 <- as.factor(predicciones2)

table(clase_predicha = predicciones2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones2 != test_data$indice))


#lda

modelo_lda <- lda(indice ~ .-crim-zn-chas-rm, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$indice))

#KNN k=1

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c(3,5,7:14)],
                       test = test_data[,c(3,5,7:14)] ,
                       cl = train_data[,"indice"], k = 1 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

#KNN k=3

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c(3,5,7:14)],
                       test = test_data[,c(3,5,7:14)] ,
                       cl = train_data[,"indice"], k = 3 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

#KNN k=6

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c(3,5,7:14)],
                       test = test_data[,c(3,5,7:14)] ,
                       cl = train_data[,"indice"], k = 6 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

######

library(MASS)

# logistico completo
modelo_logistico <- glm(indice ~ zn + indus + chas + nox + rm +
                          age + dis + rad + tax + ptratio + black +
                          lstat + medv, data = train_data, family = "binomial")

summary(modelo_logistico)

# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones <- predict(object = modelo_logistico, newdata = test_data, type = "response")


# Se considera como threshold de clasificación el 0.5
predicciones[predicciones > 0.5] <- 1
predicciones[predicciones <= 0.5] <- 0
predicciones <- as.factor(predicciones)

table(clase_predicha = predicciones, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones != test_data$indice))

stepAIC(modelo_logistico)

# log
modelo <- glm(indice ~ zn + nox + dis + rad + tax + ptratio + black +
                          lstat + medv, data = train_data, family = "binomial")
summary(modelo)

# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)


# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- 1
predicciones2[predicciones2 <= 0.5] <- 0
predicciones2 <- as.factor(predicciones2)

table(clase_predicha = predicciones2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones2 != test_data$indice))


#lda

modelo_lda <- lda(indice ~ zn + nox + dis + rad + tax + ptratio + black +
                    lstat + medv, data = train_data)
modelo_lda

predicciones_lda <- predict(object = modelo_lda, test_data)
head(predicciones_lda$class)
head(predicciones_lda$posterior)

table(clase_predicha = predicciones_lda$class,clase_real =  test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones_lda$class == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones_lda$class != test_data$indice))

#KNN k=1

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("zn","nox","dis","rad","tax","ptratio","black",
                                             "lstat","medv")],
                       test = test_data[,c("zn","nox","dis","rad","tax","ptratio","black",
                                           "lstat","medv")] ,
                       cl = train_data[,"indice"], k = 1 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

#KNN k=3

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("zn","nox","dis","rad","tax","ptratio","black",
                                             "lstat","medv")],
                       test = test_data[,c("zn","nox","dis","rad","tax","ptratio","black",
                                           "lstat","medv")] ,
                       cl = train_data[,"indice"], k = 3 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

#KNN k=6

set.seed(604)
prediccion_knn2 <- knn(train = train_data[,c("zn","nox","dis","rad","tax","ptratio","black",
                                             "lstat","medv")],
                       test = test_data[,c("zn","nox","dis","rad","tax","ptratio","black",
                                           "lstat","medv")] ,
                       cl = train_data[,"indice"], k = 6 )

table(clase_predicha = prediccion_knn2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(prediccion_knn2 == test_data$indice))

# % de errores
paste("% de error:", mean(prediccion_knn2 != test_data$indice))

#### log 2
modelo <- glm(indice ~ zn + nox + dis + rad + tax + ptratio + black +
                lstat + medv, data = train_data, family = "binomial")
summary(modelo)

# Se realizan las predicciones para el set de datos no empleado en la creación
# del modelo
predicciones2 <- predict(object = modelo, newdata = test_data, type = "response")
head(predicciones2)


# Se considera como threshold de clasificación el 0.5
predicciones2[predicciones2 > 0.5] <- 1
predicciones2[predicciones2 <= 0.5] <- 0
predicciones2 <- as.factor(predicciones2)

table(clase_predicha = predicciones2, clase_real = test_data$indice)

# % de aciertos
paste("% de acierto:", mean(predicciones2 == test_data$indice))

# % de errores
paste("% de error:", mean(predicciones2 != test_data$indice))

#importancia
importance <- importance(modelo)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance

rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables

ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()


