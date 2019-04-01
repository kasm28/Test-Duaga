library(readxl)
#Los primeros datos a abrir serán los de la pestaña CSD
#Solo debemos ir a ala ubicacion del archivo y se abre automaticamente
data1 <- as.data.frame(read_excel(file.choose(), sheet = "CSD", range = "A4:BK55"))
head(data1)

summary(data1)
data1$`ENE 2014` <- as.integer(data1$`ENE 2014`)
summary(data1$`ENE 2014`)
data1$`ENE 2014` <- as.numeric(data1$`ENE 2014`)
summary(data1$`ENE 2014`)
class(data1)

sapply(data1, class)

colnames(data1)

data1[,2:63] <- sapply(data1[,2:63],as.numeric)
sapply(data1, class)
#podria hacerse de otra manera pero nos harà perder los nombres de los productos en la 1ra columna
#data_num <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))

class(data1)

data_t <- as.data.frame(t(data1))
summary(data_t)
class(data_t)
sapply(data_t, class)

#los nombres de las columnas (productos) estan en la primera fila
colnames(data_t)<- as.character(unlist(data_t[1,]))
data_t = data_t[-1,]
class(data_t)
summary(data_t)
sapply(data_t, class)

#Convirtiendo las variables de factor a numericas para poder trabajar con ellas como series de tiempo
data_t[,1:51] <- sapply(as.character(unlist(data_t[,1:51])),as.numeric)
sapply(data_t, class)
summary(data_t)
View(data_t)
colnames(data_t)

#"COCA-COLA"      col 1                         "COCA-COLA LIGHT"   col 2
# "SPRITE TOTAL"   col 4

dtseries <- data_t[,c(1,2,4)]

dtseries$fecha <- exogenas$Date
#x11()
#plot(x = dtseries$fecha,y = dtseries$`COCA-COLA`,type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Comportamiento Ventas Coca-Cola 2014-2018"  )
#x11()
#plot(x = dtseries$fecha,y = dtseries$`COCA-COLA LIGHT`, col="blue", type = "o", xlab = "Date", ylab = "Volumen Ventas", main = "Comportamiento Ventas Coca-Cola Light 2014-2018"  )
#x11()
#plot(x = dtseries$fecha,y = dtseries$`SPRITE TOTAL`, col="blue", type = "o", xlab = "Date", ylab = "Volumen Ventas", main = "Comportamiento Ventas Sprite 2014-2018"  )
#Solo la de Coca-Cola parece ser estacionaria pero tiene comportamientos 
#anormales en cada fin e inicio de año, las de Coca-Cola Light van a la baja 
#con clara tendencia negativa y parecen tener un componente estacional  
#y Sprite tambien negativo pero se comporta bastante .... con un poco de 
#estacionalidad hasta 2016
dtseries <- dtseries[,-4]


cocacolats <- ts(data = dtseries$`COCA-COLA`,frequency = 12, start = c(2014,1))
plot(cocacolats)
dim(
  as.matrix(cocacolats)
)

cocalight_ts <- ts(data = dtseries$`COCA-COLA LIGHT`,frequency = 12, start = c(2014,1))
sprite_ts <- ts(data = dtseries$`SPRITE TOTAL`, frequency = 12, start = c(2014,1))

#Solo la de Coca-Cola parece ser estacionaria pero tiene comportamientos 
#irregular en cada fin e inicio de año, las de Coca-Cola Light van a la baja 
#con clara tendencia negativa y parecen tener un componente estacional  
#y Sprite tambien negativo pero se comporta bastante irregular con un poco de 
#estacionalidad hasta 2016



#Training and Testing Datasets

cocacolats_tr <- window(cocacolats, start = c(2014,1), end = c(2018, 10))
dim(as.matrix(cocacolats_tr))
cocacolats_tstg <- window(cocacolats, start = c(2018,11))
dim(as.matrix(cocacolats_tstg))
x11()
plot.ts(cocacolats_tr, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola 2014-2018")
plot.ts(cocacolats_tstg, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola 2018-2019")



#data_ts <- ts(data = dtseries, start = c(2014,1), frequency = 12 )
#dim(as.matrix(data_ts))
#training <- window(data_ts, start = c(2014,1), end = c(2018, 10))
#dim(as.matrix(training))
#testing <- window(data_ts, start = c(2018,11))
#dim(as.matrix(testing))
#x11()
#plot.ts(training, type = "o", xlab = "Date", ylab = "Volumen Ventas", main = "Comportamiento Ventas 2014-2018")
#plot.ts(testing, type = "o", xlab = "Date", ylab = "Volumen Ventas", main = "Comportamiento Ventas 2018(11&12)-2019(1&2)")


#### SARIMA     ####

install.packages("forecast")
library(forecast)
arima_cocacola <- auto.arima(cocacolats_tr, trace = TRUE, test = "kpss", ic = "aic")
summary(arima_cocacola)
#No hay problema de autocorrelacion
confint(arima_cocacola)

#Residual Diagnostics

plot.ts(arima_cocacola$residuals)
Box.test(arima_cocacola$residuals, lag=20, type="Ljung-Box")
par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(arima_cocacola$residuals, lag.max = 24)
pacf(arima_cocacola$residuals)

Box.test(arima_cocacola$residuals^2, lag=20, type="Ljung-Box")
install.packages("tseries")

library(tseries)
jarque.bera.test(arima_cocacola$residuals)

arima_cocacola.forecast <- forecast(arima_cocacola, h=8)
arima_cocacola.forecast
plot(arima_cocacola.forecast, xlab="Years", ylab="Ventas(p)")

install.packages("TSPred")
library(TSPred)
plotarimapred(cocacolats, arima_cocacola.forecast, xlim = c(2014,2019), range.percent = 0.05)
accuracy(arima_cocacola.forecast, cocacolats)

########## El anterior no es un buen modelo 
# intentemos ver que pasa si hacemos con log de la serie de tiempo
#cocalog<-log(cocacolats)
#plot(cocalog)
#dim(
#  as.matrix(cocalog)
#)

#cocalog_tr <- window(cocalog, start = c(2014,1), end = c(2018, 10))
#dim(as.matrix(cocalog_tr))
#cocalog_tstg <- window(cocalog, start = c(2018,11))
#dim(as.matrix(cocalog_tstg))

#library(forecast)
#arima_cocalog <- auto.arima(cocalog_tr, trace = TRUE, test = "kpss", ic = "aic")
#summary(arima_cocalog)
#plot.ts(arima_cocalog$residuals)
#Box.test(arima_cocalog$residuals, lag=20, type="Ljung-Box")

#arima_cocalog.forecast <- forecast(arima_cocalog, h=8)
#arima_cocalog.forecast
#plot(arima_cocalog.forecast, xlab="Years", ylab="Ventas(p)")
#library(TSPred)
#plotarimapred(cocacolats, arima_cocalog.forecast, xlim = c(2014,2019), range.percent = 0.05)



#para graficos de autocorrelaciones simple y parcial 
#una vez ya son estacionarias nuestras series
#par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)







###        Importamos los datos a incluir en el modelo como exógenas      ######################################

exogenas <- as.data.frame(read_excel("C:/Users/flamingo/Downloads/prueba.xlsx",sheet = "EXOGENAS", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))
class(exogenas)
sapply(exogenas, class)




################################################################################################################
###        SERIES DE TIEMPO PARA ESTIMAR PARTICIPACION EN EL MERCADO DEL BANCO CAJA SOCIAL            ##########
################################################################################################################


BCS <- as.data.frame(read_excel("C:/Users/flamingo/Downloads/prueba.xlsx", sheet = "Bancos", range = "B1:C63", col_types = c("date", "numeric")))
class(BCS)
sapply(BCS, class)

banco <- ts(BCS$MONTO, start = c(2014,1), frequency = 12)
plot(banco)
#parece ser una serie estacionaria
par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(banco, lag(30))
pacf(banco, lag.max = 30)
#ARIMA(3,0,3)

modelbanco<- arima(banco, order = c(3,0,3))
modelbanco
tsdiag(modelbanco)

plot.ts(modelbanco$residuals)
Box.test(modelbanco$residuals, lag=30, type="Ljung-Box")
acf(modelbanco$residuals, lag.max = 30)
pacf(modelbanco$residuals)


rm(arimabanco)
sarimabanco <- auto.arima(banco)
sarimabanco

arimabanco.forecast <- forecast(modelbanco, h=12)
arimabanco.forecast

plot(arimabanco.forecast, xlab="Years", ylab="Ventas(p)")

library(TSPred)
plotarimapred(banco, arimabanco.forecast, xlim = c(2014,2020), range.percent = 1)
accuracy(arimabanco.forecast, banco)

