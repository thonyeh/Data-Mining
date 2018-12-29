# PC1

library(ggplot2)
library(ggExtra)
library(ggridges)
library(ggplot2)
library(naniar)
library(data.table)
library("imputeR")
library(mice)
library(VIM)

####------------ Ejercicio 2


datos_iris <- read.csv(file.choose())
datos_iris <- data.table(datos_iris[,2:6])

head(datos_iris)
summary(datos_iris)
str(datos_iris)
attach(datos_iris)
datos_iris[, mean(Sepal.Width), Species]

d1 <- datos_iris[ Sepal.Length > 0 , mean(Sepal.Length), Species]
d2 <- datos_iris[ Sepal.Width  > 0 , mean(Sepal.Width), Species]
d3 <- datos_iris[ Petal.Length > 0, mean(Petal.Length), Species]
d4 <- datos_iris[ Petal.Width  > 0, mean(Petal.Width), Species]

d <- data.table(d1,d2[,2],d3[,2],d4[,2])
colnames(d) <- c("Species","Mean Sepal.Length","Mean Sepal.Width","Mean Petal.Length","Mean Petal.Width")
d

# Scatterplot Matrices from the lattice Package 
library(lattice)
splom(datos_iris[,c(1:4)], groups = Species,
      panel=panel.superpose, 
      key=list(title="Species",
               columns=3,
               bg =list(pch=Species[1:3],
                           col=c("red", "green3", "blue")[class(iris$Species)]),
               text=list(c("Setosa","Versicolor","Virginica"))))

pairs(datos_iris[,c(1:4)],
      main = "Species", pch = 21, 
      bg = c("red", "green3", "blue")[unclass(Species)])#,
      #text=list(c("Setosa","Versicolor","Virginica")))

install.packages("scatterplot3d")

library(scatterplot3d)
library(GGally)
library(ggplot2)
ggpairs(datos_iris[,-5])+ theme_bw()


p <- ggpairs(datos_iris, aes(color = Species))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
  }
}
p



p1=ggplot(datos_iris, aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) + geom_point(size=6,alpha=0.6) +   theme(legend.position="none")
p1
p2=ggplot(datos_iris, aes(x=Petal.Length, y=Petal.Width, color=Species, shape=Species)) + geom_point(size=6,alpha=0.6) +   theme(legend.position="none")
p2



ggplot(datos_iris, aes(x = Sepal.Length , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(datos_iris, aes(x = Sepal.Width  , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(datos_iris, aes(x = Petal.Length  , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(datos_iris, aes(x = Petal.Width  , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

table(is.na(datos_iris))==ncol(datos_iris)*nrow(datos_iris)
str(datos_iris)

options(scipen=999)
theme_set(theme_bw())
gg <- ggplot(datos_iris, aes(x=Sepal.Length, y=Sepal.Width,fill = Species)) + 
  geom_point(aes(col=Species, size=Species)) + #aes=asignaciones est�ticas
  geom_smooth(method="loess", se=F)
gg

gg2 <- ggplot(datos_iris, aes(x=Petal.Length, y=Petal.Width,fill = Species)) + 
  geom_point(aes(col=Species, size=Species)) + #aes=asignaciones est�ticas
  geom_smooth(method="loess", se=F)
gg2

p3 <- ggplot(datos_iris, aes(y = Sepal.Width, x = Sepal.Length,fill = Species)) 
p3 + geom_point(aes(color = Species)) + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) + facet_wrap(~Species) + ggtitle("Sepal.Length vs Sepal.Width") + xlab("Sepal.Length") + ylab("Sepal.Width")

p4 <- ggplot(datos_iris, aes(y = Petal.Width, x = Petal.Length,fill = Species)) 
p4 + geom_point(aes(color = Species)) + geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) + facet_wrap(~Species) + ggtitle("Petal.Length vs Petal.Width") + xlab("Petal.Length") + ylab("Petal.Width") 


# IMPUTACION

vis_miss(datos_iris[,1:4])
gg_miss_var(datos_iris[,1:4],show_pct = TRUE)
gg_miss_var(datos_iris[,1:5],show_pct = TRUE,facet = Species)


md.pattern(datos_iris)

#Verifique los datos por valores perdidos.
sapply(completedData, function(x) sum(is.na(x)))

ggpairs(completedData[,-5])+ theme_bw()

attach(completedData)
p2 <- ggpairs(completedData, aes(color = Species))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
  }
}
p2





aggr_plot <- aggr(datos_iris, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(datos_iris), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tempData <- mice(datos_iris,m=5,maxit=50,meth='pmm',seed=500)
summary(tempData)

completedData <- complete(tempData,1)
str(completedData)

xyplot(tempData,Sepal.Width ~ Sepal.Length ,pch=18,cex=1)
p1=ggplot(completedData, aes(x=Sepal.Length, y=Sepal.Width, color=Species, shape=Species)) + geom_point(size=6,alpha=0.6) +   theme(legend.position="none")
p1


xyplot(tempData,Petal.Width ~ Petal.Length ,pch=18,cex=1)
p2=ggplot(completedData, aes(x=Petal.Length, y=Petal.Width, color=Species, shape=Species)) + geom_point(size=6,alpha=0.6) +   theme(legend.position="none")
p2

class()

Rmse(completedData[,1:4],as.data.frame(datos_iris)[,1:4], iris[,1:4], norm = TRUE)

missdata <- SimIm(datos_iris[,1:4], 0.1)
# imputacion de los valores perdidos por LASSO
impdata <- impute(datos_iris[,1:4], lmFun = "lassoR")
# calcular el RMSE normalizado para la imputaci�n
Rmse(impdata$imp, missdata, parkinson, norm = TRUE)


ggplot(completedData, aes(x = Sepal.Length , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(completedData, aes(x = Sepal.Width  , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(completedData, aes(x = Petal.Length  , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(completedData, aes(x = Petal.Width  , y = Species, fill = Species)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")


#------Ejercicio 3
diabetes <- read.csv(file.choose(), header=TRUE, sep=",")
head(diabetes)
str(diabetes)
original <- diabetes
summary(diabetes)

#Verifique los datos por valores perdidos.
sapply(diabetes, function(x) sum(is.na(x)))

#Como no hay errores, se agregará algunos NA en el conjunto de datos, pero
#antes voy a duplicar el conjunto de datos original para evaluar la 
#precisión de la imputación más adelante.

original <- diabetes


#Ahora se agregará algunas fallas en algunas variables.

dim(diabetes)
set.seed(10)
diabetes[sample(1:nrow(diabetes), 20), "glucose"] <- NA
diabetes[sample(1:nrow(diabetes), 20), "insulin"] <- NA
diabetes[sample(1:nrow(diabetes), 20), "sspg"] <- NA
diabetes[sample(1:nrow(diabetes), 5), "class"] <- NA

#Confirme la presencia de errores en el conjunto de datos.

sapply(diabetes, function(x) sum(is.na(x)))

vis_miss(diabetes)
gg_miss_var(datos_iris[,1:4],show_pct = TRUE)
gg_miss_var(datos_iris[,1:5],show_pct = TRUE,facet = Species)

aggr_plot <- aggr(datos_iris, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(datos_iris), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
md.pattern(diabetes)


#El siguiente paso es transformar las variables en factores o numéricos.
#Por ejemplo, fumar y la educación son variables categóricas, mientras 
#que el nivel de colesterol es continuo


#Veamos la estructura del conjunto de datos

str(diabetes)

#Todo se ve bien, entonces continuemos con la imputación

############
#Imputación

#Ahora que el conjunto de datos está listo para la imputación, 
#llamaremos al paquete de los ratones. El código siguiente es
#estándar y no necesita cambiar nada además del nombre del 
#conjunto de datos.


library(mice)
init = mice(diabetes, maxit=0) 
meth = init$method
predM = init$predictorMatrix

#Para imputar los valores que faltan, el paquete de mice usa 
#un algoritmo de tal manera que usa información de otras variables 
#en el conjunto de datos para predecir e imputar los valores perdidos. 
#Por lo tanto, es posible que no desee utilizar ciertas variables como 
#predictores. Por ejemplo, la variable de ID no tiene ningún valor

#El siguiente código eliminará la variable como predictor, pero aún as�? 
#será imputado. Solo a t�?tulo ilustrativo selecciono la variable BMI 
#para que no se incluya como predictor durante la imputación.

predM[,c("BMI")]=0

#Si desea omitir una variable de la imputación, use el siguiente código.
#Tenga en cuenta que esta variable se usará para la predicción.
#predictivo.


#Ahora, especifique los métodos para imputar los valores faltantes. 
#Existen métodos espec�?ficos para variables continuas, binarias y 
#ordinales. Establec�? diferentes métodos para cada variable. Puede
#agregar más de una variable en cada método.

meth[c("glucose")]="norm" 
meth[c("insulin")]="norm" 
meth[c("sspg")]="norm"
meth[c("class")]="polyreg"

#Ahora es el momento de ejecutar la imputación múltiple (m = 5).

set.seed(103)
imputed = mice(diabetes, method=meth, predictorMatrix=predM, m=15)

#Crea un conjunto de datos después de la imputación.

imputed <- complete(imputed)

#Verificar si hay errores en el conjunto de datos imputados.
sapply(imputed, function(x) sum(is.na(x)))


#irmi
imputed.irmi <- irmi(diabetes,modelFormulas = meth) 
summary(imputed.irmi)

#imputeR
imputed.R <- data.table(impute(diabetes[,1:3], lmFun = "lassoR")$imp)
sapply(imputed.R, function(x) sum(is.na(x)))

#Exactitud

#En este ejemplo, conocemos los valores reales de los datos 
#faltantes, ya que agregué las pérdidas. Esto indica que podemos
#verificar la precisión de la imputación. Sin embargo, debemos 
#reconocer que este es un conjunto de datos simulado, y por lo tanto,
#las variables no tienen ningún significado cient�?fico y no están 
#correlacionadas entre s�?. Por lo tanto, espero una tasa de precisión
#menor para esta imputación.


M <- c(mean(original$glucose[is.na(diabetes$glucose)]),
       mean(imputed$glucose[is.na(diabetes$glucose)]),
       #mean(imputed.irmi$glucose[is.na(diabetes$glucose)]),
       mean(imputed.R$glucose[is.na(diabetes$glucose)]))
M <- rbind(M,c(mean(original$insulin[is.na(diabetes$insulin)]),
               mean(imputed$insulin[is.na(diabetes$insulin)]),
               #mean(imputed.irmi$insulin[is.na(diabetes$insulin)]),
               mean(imputed.R$insulin[is.na(diabetes$insulin)])))
M <- rbind(M,c(mean(original$sspg[is.na(diabetes$sspg)]),
               mean(imputed$sspg[is.na(diabetes$sspg)]),
               #mean(imputed.irmi$sspg[is.na(diabetes$sspg)]),
               mean(imputed.R$sspg[is.na(diabetes$sspg)])))
colnames(M) <- c("original","mice","imputeR")
rownames(M) <- c("Mean.glucose","Mean.insulin","Mean.sspg")
M

actual4 <- original$class[is.na(diabetes$class)] 
table(actual)
table(predicted)

predicted <- imputed$glucose[is.na(diabetes$glucose)]
mean(actual4)
mean(predicted)

# insuline
actual <- original$insulin[is.na(diabetes$insulin)] 
predicted <- imputed$insulin[is.na(diabetes$insulin)] 
table(actual)
table(predicted)

#La media real y pronosticada para el colesterol son aproximadas, 
#lo que muestra una alta precisión de imputación, mientras que 
#para fumar es baja.



ggplot(original, aes(x = glucose , y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(original, aes(x = insulin , y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(original, aes(x = sspg , y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

ggplot(imputed, aes(x = glucose , y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(imputed, aes(x = insulin , y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
ggplot(imputed, aes(x = sspg , y = class, fill = class)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")




sapply(imputed, function(x) sum(is.na(x)))

attach(original)
p <- ggpairs(original, aes(color = class))+ theme_bw()
# Change color manually.
# Loop through each plot changing relevant scales
for(i in 1:p$nrow) {
  for(j in 1:p$ncol){
    p[i,j] <- p[i,j] + 
      scale_fill_manual(values=c("#00AFBB", "#E7B800", "#FC4E07")) +
      scale_color_manual(values=c("#00AFBB", "#E7B800", "#FC4E07"))  
  }
}
p




#----preg 4
library(ggplot2)
data("msleep", package = "ggplot2")

aggr_plot <- aggr(msleep, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(msleep), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
md.pattern(diabetes)

str(msleep)

msleep <- data.table(msleep)
datos <- msleep %>%
  mutate(name = as.factor(name)) %>% 
  mutate(genus = as.factor(genus)) %>% 
  mutate(vore = as.factor(vore)) %>%
  mutate(order = as.factor(order)) %>%
  mutate(conservation = as.factor(conservation))
str(datos)

summary(datos)

p=ggplot(msleep, aes(x=awake, y=sleep_total, color=vore, size=vore)) + geom_point() +   theme(legend.position="none")
p
ggMarginal(p, type="histogram")
ggMarginal(p, type="boxplot")

ggplot(msleep, aes(x = sleep_total, y = vore, fill = vore)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")



vis_miss(msleep)




library(mice)
init = mice(diabetes, maxit=0) 
meth = init$method
predM = init$predictorMatrix


meth[c("name")]=""
meth[c("genus")]=""
meth[c("vore")]=""
meth[c("order")]=""
meth[c("conservation")]="polyreg"
meth[c("sleep_total")]="norm" 
meth[c("sleep_rem")]="norm" 
meth[c("sleep_cycle")]="norm"
meth[c("awake")]="norm"
meth[c("brainwt")]="norm"
meth[c("bodywt")]=""


set.seed(103)
imputed = mice(datos, seed=103)

fit <- with(imputed, lm(sleep_total ~ awake + factor(vore)))
print(pool(fit))
round(summary(pool(fit)), 2)


summary(imputed)



# ejercicio 5
library(data.table)
dat <- read.csv(url("https://goo.gl/iR3diF"), header=TRUE, sep=" ")
dat <- data.table(dat)
str(dat)
summary(dat)

sapply(dat, function(x) sum(is.na(x)))

library(VIM)
aggr_plot<-aggr(dat, col=c('navyblue','red'), 
                numbers=TRUE, sortVars=TRUE, 
                labels=names(dat), cex.axis=.7, 
                gap=3, 
                ylab=c("Histogram of missing data",
                       "Pattern"))



plot(dat)
##----- Mice
library(mice)
init = mice(dat, maxit=0) 
meth = init$method
predM = init$predictorMatrix

meth[c("illiteracyFemale")]="norm" 
meth[c("illiteracyMale")]="norm" 
meth[c("economicActivityFemale")]="norm"
meth[c("economicActivityMale")]="norm" 
meth[c("GDPperCapita")]="pmm" 
meth[c("infantMortality")]="pmm"
meth[c("lifeFemale")]="norm" 
meth[c("lifeMale")]="norm" 
meth[c("educationFemale")]="norm"
meth[c("educationMale")]="norm" 
meth[c("contraception")]="pmm" 
meth[c("tfr")]="norm"
meth[c("region")]="polyreg"

set.seed(13)
imputed = mice(dat, method=meth, predictorMatrix=predM, m=20)
imputed <- complete(imputed)

##-----ImputeR
library("imputeR")
imputed.R <- data.table(impute(dat[,2:13], lmFun = "lassoR")$imp)

sapply(imputed.R, function(x) sum(is.na(x)))

M <- c(mean(imputed$tfr[is.na(dat$tfr)]),
       mean(imputed.R$tfr[is.na(dat$tfr)]))
M <- rbind(M,c(mean(imputed$contraception[is.na(dat$contraception)]),
               mean(imputed.R$contraception[is.na(dat$contraception)])))
M <- rbind(M,c(mean(imputed$educationMale[is.na(dat$educationMale)]),
               mean(imputed.R$educationMale[is.na(dat$educationMale)])))
M <- rbind(M,c(mean(imputed$educationFemale[is.na(dat$educationFemale)]),
               mean(imputed.R$educationFemale[is.na(dat$educationFemale)])))
M <- rbind(M,c(mean(imputed$lifeMale[is.na(dat$lifeMale)]),
               mean(imputed.R$lifeMale[is.na(dat$lifeMale)])))
M <- rbind(M,c(mean(imputed$lifeFemale[is.na(dat$lifeFemale)]),
               mean(imputed.R$lifeFemale[is.na(dat$lifeFemale)])))
M <- rbind(M,c(mean(imputed$infantMortality[is.na(dat$infantMortality)]),
               mean(imputed.R$infantMortality[is.na(dat$infantMortality)])))
M <- rbind(M,c(mean(imputed$GDPperCapita[is.na(dat$GDPperCapita)]),
               mean(imputed.R$GDPperCapita[is.na(dat$GDPperCapita)])))
M <- rbind(M,c(mean(imputed$economicActivityMale[is.na(dat$economicActivityMale)]),
               mean(imputed.R$economicActivityMale[is.na(dat$economicActivityMale)])))
M <- rbind(M,c(mean(imputed$economicActivityFemale[is.na(dat$economicActivityFemale)]),
               mean(imputed.R$economicActivityFemale[is.na(dat$economicActivityFemale)])))
M <- rbind(M,c(mean(imputed$illiteracyMale[is.na(dat$illiteracyMale)]),
               mean(imputed.R$illiteracyMale[is.na(dat$illiteracyMale)])))
M <- rbind(M,c(mean(imputed$illiteracyFemale[is.na(dat$illiteracyFemale)]),
               mean(imputed.R$illiteracyFemale[is.na(dat$illiteracyFemale)])))

colnames(M) <- c("mice","imputeR")
dim(M)
rownames(M) <- c("Mean.tfr","Mean.contraception","Mean.educationMale",
                 "Mean.educationFemale","Mean.lifeMale","Mean.lifeFemale",
                 "Mean.infantMortality","Mean.GDPperCapita","Mean.economicActivityMale",
                 "Mean.economicActivityFemale","Mean.illiteracyMale","Mean.illiteracyFemale")
M







