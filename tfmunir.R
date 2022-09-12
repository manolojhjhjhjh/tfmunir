
## Leer los datos
## fichero txt se importa con read.csv
datos=read.csv("Tetuan.csv", header=TRUE)
datos<-datos[,-1] ##quitamos variable identificador
dim(datos)
attach(datos)
head(datos)

names(datos)
summary(datos)
dim(datos)
x<-datos[,c(1,2,3,4,5)] ##variables auxiliares
###zona 1 
x1=datos[,c(1,2,3,4,5,6)]
y1<-(datos[,c(6)])

###zona 2
x2=datos[,c(1,2,3,4,5,7)]
y2<-(datos[,c(7)])

head(datos2)
###zona 3 
x3=datos[,c(1,2,3,4,5,8)]
y3<-datos[,c(8)]



###analisis de variables.
dim(datos)      #NUMERO DE FILAS Y COLUMNAS
summary(x)  #RESUMEN NUMERICO
data=(x)
summary(data)
####hipotesis modelo lineal
###modelo lineal y contrastes


det(cor(x)) #determinante matriz de correlación
corrplot(cor(x), method="number", type="upper")
library(psych)
#pairs((x))
library(corrplot)
#boxplot(x) ##gráfico de caja y bigotes.
attach(data.frame(data))

data=data.frame(data)

modelo_lm1 <- lm(y1 ~., data = data) ##modelo lineal 1
modelo_lm1$model
 
modelo_lm2 <- lm(y2 ~., data = data) ##modelo lineal 1
modelo_lm2

modelo_lm3 <- lm(y3 ~., data = data) ##modelo lineal 1
modelo_lm3


summary(modelo_lm1) ##resumen de los diferentes modelos creados
summary(modelo_lm2)
summary(modelo_lm3)


shapiro.test(modelo_lm1$residuals[1:4999]) ##test de Shapiro Wilks solo admite 5000 elementos 

shapiro.test(modelo_lm2$residuals[1:4999])

shapiro.test(modelo_lm3$residuals[1:4999]) 


datoss<-datos[1:2000,] 

###support vector machine


library(e1071) ##creamos los tres modelos con SVM, 

svm1 <- svm(formula = Zone.1.Power.Consumption ~ ., data = x1, kernel = "linear",
                  cost = 10, scale = FALSE)



svm2 <- svm(formula = Zone.2..Power.Consumption ~ ., data = x2, kernel = "linear",
            cost = 10, scale = FALSE)



svm3 <- svm(formula = Zone.3..Power.Consumption ~ ., data = x3, kernel = "linear",
            cost = 10, scale = FALSE)



### REGRESIÃ"N SVM


summary(svm1)
summary(svm2)
summary(svm3)








predictedYsvm1<-predict(svm1,x1) ##realizamos las respectivas predicciones usando los modelos creados.
predictedYsvm2<-predict(svm2,x2)
predictedYsvm3<-predict(svm3,x3)

library(Metrics)


rmse(Zone.1.Power.Consumption,predictedYsvm1) ##comparamos los valores predichos por los modelos con los originales a través del error cuadrático medio
rmse(Zone.2..Power.Consumption,predictedYsvm2)
rmse(Zone.3..Power.Consumption,predictedYsvm3)
### Afinamos el epsilon y el cost, que son los parámetros que nos muestra si llamamos a modelsvm.




tuneResult1<-tune.svm(Zone.1.Power.Consumption ~ ., data = x1,epsilon=seq(0,1,0.1),cost=2^(2:6))

tunesvm1<-tuneResult1$best.model
summary(tunesvm1)
predictedYtune1<-predict(tunesvm1,x1)
rmse(Zone.1.Power.Consumption,predictedYtune1)





tuneResult2<-tune.svm(Zone.2..Power.Consumption ~ ., data = x2,epsilon=seq(0,1,0.1),cost=2^(2:6))

tunesvm2<-tuneResult2$best.model
summary(tunesvm2)
predictedYtune2<-predict(tunesvm2,x2)
rmse(Zone.2..Power.Consumption,predictedYtune2)


tuneResult3<-tune.svm(Zone.3..Power.Consumption ~ ., data = x3,epsilon=seq(0,1,0.1),cost=2^(2:6))

tunesvm3<-tuneResult3$best.model
summary(tunesvm3)
predictedYtune3<-predict(tunesvm3,x3)
rmse(Zone.3..Power.Consumption,predictedYtune3)









### redes neuronales 


n=nrow(datos)
indices<- 1:n
inditest<- sample(indices,trunc(n*0.85)+1)
indient<-setdiff(indices,inditest)

##creamos una función de ajuste para evaluar el modelo

Ajuste<- function(y,pred,titulo)
{
  residuos=y-pred
  plot(y,pred,main=titulo,ylab=expression(hat(y)))
  abline(a=0,b=1,col="blue",lwd=2)
  grid()
  MSE= mean(residuos^2)
  RMSE= sqrt(MSE)
  R2= cor(y,pred)^2
  return(list(MSE=MSE,RMSE=RMSE,R2=R2))
}


#III) MODELO nnet EN REGRESION 
#################################
#linout=TRUE para que la función de activación de 
#la neurona de salida sea la identidad. La combinación lineal directamente

##zona 1

library(caret)
modeloPM = train(Zone.1.Power.Consumption ~.,data=datos,subset=indient,
                 method = "nnet",linout=TRUE, 
                 trControl = trainControl(method = "cv"), 
                 preProcess = c("center","scale"), 
                 tuneGrid = expand.grid(size=5:15,
                                        decay=c(0,0.5,0.1)) )



summary(modeloPM)  ##resumen del modelo creado
plot(modeloPM)
modeloPM
modeloPM$finalModel
summary(modeloPM$finalModel) ##resumen del modelo final creado
coef(modeloPM$finalModel)
predPM_ent=predict(modeloPM,datos[indient,]) ##predicción modelo creado a los datos de entrenamiento
predPM_test=predict(modeloPM,newdata=datos[inditest,])  ##predicción modelo creado a los datos test
predPM=predict(modeloPM,datos)

Ajuste(datos$Zone.1.Power.Consumption,predPM,
        "Perceptrón Multicapas")

Ajuste(datoss$Zone.1.Power.Consumption[inditest],predPM_test,
       "Test, Perceptrón Multicapas")





###zona 2

modeloPM2 = train(Zone.2..Power.Consumption ~.,data=datos,subset=indient,
                 method = "nnet",linout=TRUE, 
                 trControl = trainControl(method = "cv"), 
                 preProcess = c("center","scale"), 
                 tuneGrid = expand.grid(size=5:15,
                                        decay=c(0,0.5,0.1)) )



summary(modeloPM2)
plot(modeloPM2)
modeloPM2
modeloPM2$finalModel
summary(modeloPM2$finalModel)
coef(modeloPM2$finalModel)
predPM2_ent=predict(modeloPM2,datoss[indient,])
predPM_test=predict(modeloPM2,newdata=datos[inditest,])
predPM2=predict(modeloPM2,datos)

Ajuste(datos$Zone.2..Power.Consumption,predPM2,
       "Perceptrón Multicapas")

Ajuste(datoss$Zone.1.Power.Consumption[inditest],predPM_test,
       "Test, Perceptrón Multicapas")




###zona 3

modeloPM3 = train(Zone.3..Power.Consumption ~.,data=datos,subset=indient,
                  method = "nnet",linout=TRUE, 
                  trControl = trainControl(method = "cv"), 
                  preProcess = c("center","scale"), 
                  tuneGrid = expand.grid(size=5:15,
                                         decay=c(0,0.5,0.1)) )



summary(modeloPM3)
plot(modeloPM3)
modeloPM3
modeloPM3$finalModel
summary(modeloPM3$finalModel)
coef(modeloPM3$finalModel)
predPM3_ent=predict(modeloPM3,datos[indient,])
predPM_test=predict(modeloPM3,newdata=datos[inditest,])
predPM3=predict(modeloPM3,datos)

Ajuste(datos$Zone.3..Power.Consumption,predPM3,
       "Perceptrón Multicapas")

Ajuste(datos$Zone.1.Power.Consumption[inditest],predPM_test,
       "Test, Perceptrón Multicapas")







###series temporales
library(RCurl)
##dividimos los datos en quincenas, como disponemos de un año esta bien esta división para poder realizar un estudio de los 
##elemntos de la serie temporal
medias1 = vector()
p=vector()
l=0
for (i in 1:24){
  p=rbind(p,sum(Zone.1.Power.Consumption[((i-1)*2160):(i*2160)])/2160)
  medias1 = rbind(medias1,p[i])
}
medias1=ts(medias1)
medias1
plot(medias1,main="Consumo de la zona 1 por quincenas en 2017",xlab="quincena nº",ylab="Consumo")
lines(x,l11,col="green")
grid()





medias2 = vector()
p=vector()
l=0
for (i in 1:24){
  p=rbind(p,sum(Zone.2..Power.Consumption[((i-1)*2160):(i*2160)])/2160)
  medias2 = rbind(medias2,p[i])
}
medias2


medias2=ts(medias2)
medias2
plot(medias2,main="Consumo de la zona 2 por quincenas en 2017",xlab="quincena nº",ylab="Consumo")
lines(x,l11,col="green")
grid()







medias3 = vector()
p=vector()
l=0
for (i in 1:24){
  p=rbind(p,sum(Zone.3..Power.Consumption[((i-1)*2160):(i*2160)])/2160)
  medias3 = rbind(medias3,p[i])
}
medias3


medias3=ts(medias3)
medias3
plot(medias3,main="Consumo de la zona 3 por quincenas en 2017",xlab="quincena nº",ylab="Consumo")
lines(x,l11,col="green")
grid()



p=vector()
pp=vector()
l1=vector()
for (i in 1:21){
  p=rbind(p,(sum(medias3[i:(i+3)]))/4)
  l1 = rbind(l1,p[i])
}
l1

pp=vector()
l11=vector()
for (i in 1:20){
  p=rbind(p,(sum(l1[i:(i+1)]))/2)
  l11 = rbind(l11,p[i])
}
l11

medias11=medias3[3:22]
length(medias11)
s=medias11/l11

sum(medias1[1:4])/4






#install.packages(«quantmod»)
library(quantmod)
#install.packages(«tseries»)
library(tseries)
#install.packages(«fImport»)
library(fImport)



library(RCurl)

library(rjson)
#install.packages(«parsedate»)
library(parsedate)
require(tseries)

library(Imtest)

###mejor modelo arima para cada una de las tres zonas con la función adf.test


adf.test(Zone.1.Power.Consumption)
parametros1=auto.arima(Zone.1.Power.Consumption)
parametros1
summary(parametros1)
coeftest(parametros1)
parametros1$coef
qqnorm(parametros1$residuals)
shapiro.test(parametros1$residuals[a]) ##test de Shapiro-Wilks a los residuos, pero como solo admite 5000 unidades, en este caso no nos quedamos con los 5000 primeros como antes, sino que para que sea aleatorio y nos los del primer periodo solo, cogemos una muestra de índices aleatoria a.
parametros1$model









adf.test(Zone.2..Power.Consumption)
parametros2=auto.arima(Zone.2..Power.Consumption)
parametros2
summary(parametros2)
coeftest(parametros2)
parametros2$coef
qqnorm(parametros2$residuals)
shapiro.test(parametros2$residuals[a])
parametros2$model






adf.test(Zone.3..Power.Consumption)
parametros3=auto.arima(Zone.3..Power.Consumption)
parametros3
summary(parametros3)
coeftest(parametros3)
parametros3$coef
qqnorm(parametros3$residuals)
shapiro.test(parametros3$residuals[a])
parametros3$model

## por último realizamos las respectivas gráficas de los pronósticos realizados con los mejores modelos arima que se han creado.
plot(forecast(auto.arima(Zone.1.Power.Consumption)), main="",  sub = "Figura 20: Valores pronosticados zona 1")
plot(forecast(auto.arima(Zone.2..Power.Consumption)), main="",  sub = "Figura 20: Valores pronosticados zona 2")
plot(forecast(auto.arima(Zone.3..Power.Consumption)), main="",  sub = "Figura 20: Valores pronosticados zona 3")







