############# An?lisis de datos en Operadores de tarjetas de cr?dito

############# Definiendo directorio de trabajo
setwd("~/sistemas/statistics/machineL/")

############# Instalaci?n de paquetes
install.packages("mlbench")
install.packages("caret")
install.packages("e1071")

library(mlbench)
library(caret)
library(e1071)

############# Carga del dataset
mydata <- read.csv("database.csv")
View(mydata)
### 1. Saldo de la cuenta bancaria
### 2. Sexo
.
.
.
### 24. Variable a predecir. En el siguiente mes figura con deuda (1) o no figura con deuda (0)

############# Verifica el baance de las variables
skewness(mydata$LIMIT_BAL)
histogram(mydata$LIMIT_BAL)

### Sumarizar los datos
summary(mydata)
str(mydata)

### Calcula los par?metros de preprocesamiento.
preprocessParam <- preProcess(mydata, method=c("BoxCox"))
print(preprocessParam)

### Transforma el dataset usando los par?metros
transformed <- predict(preprocessParam, mydata)
mydata <- transformed

### Sumarizar nuevamente los datos
str(mydata)
skewness(mydata$LIMIT_BAL)
histogram(mydata$LIMIT_BAL)

### Transformar las variables categ?ricas
mydata$default.payment.next.month <- factor(mydata$default.payment.next.month)
mydata$SEX <- as.factor(mydata$SEX)
mydata$EDUCATION <- as.factor(mydata$EDUCATION)
mydata$MARRIAGE <- as.factor(mydata$MARRIAGE)
mydata = na.omit(mydata)
summary(mydata)
str(mydata)


### Divisi?n de los datos entrenamiento y test.
# Se comienza a construir el modelo predictivo

row <- nrow(mydata)
row
set.seed(12345)
trainindex <- sample(row, 0.7*row, replace = FALSE)
training <- mydata[trainindex,]
validation <- mydata[-trainindex,]

trainingx <- training
trainingy <- training[,24]
validationx <- validation[,-24]
validationy <- validation[,24]

#FEATURE ENGINEERING
install.packages("randomForest")
library(randomForest)
rfmodel <- randomForest(training$default.payment.next.month ~ ., data = training, ntree=500)
varImpPlot(rfmodel)


### Construcci?n del modelo con KNN (Modelo simple de Machine Learning)
### Prepara los datasets con las mejores variables predictoras

trainingx <- training[,-c(2,3,4,5,24)] # variables predictoras (las mejores seg?n RANDOM FOREST)
trainingy <- training[,24] # "Y" variable a predecir
validationx <- validation[,-c(2,3,4,5,24)]
validationy <- validation[,24]

### Determinamos el modelo KNN
knnModel <- train(x=trainingx, y=trainingy, method = "knn",
                  preProc = c("center","scale"),
                  tuneLength = 10)
knnModel

plot(knnModel$results$k,
     knnModel$results$Accuracy,
     type = "o",
     xlab = "N?mero de vecinos m?s pr?ximos",
     ylab = "Accuracy",
     main = "Modelo KNN para predicci?n de CTC")

### Haciendo las previsiones
knnPred = predict(knnModel, newdata = validationx)
knnPred

columna24 <- mydata$default.payment.next.month
columna24







