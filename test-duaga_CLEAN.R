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
#debemos covertir el formato de las vairbales en númerico para poder trabajar
#con ellos como serie de tiempo
data1[,2:63] <- sapply(data1[,2:63],as.numeric)
sapply(data1, class)
#podria hacerse de otra manera pero nos harà perder los nombres de los productos en la 1ra columna
#data_num <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))

class(data1)
#transponemos la matriz de datos para procesarla como serie de tiempo
data_t <- as.data.frame(t(data1))
summary(data_t)
class(data_t)
sapply(data_t, class)

#los nombres de las columnas (productos) estan en la primera fila
#por lo que debemos covertirlos en titulos de las variables y 
#una vez convertidos eliminamos esa fila que los contenia
colnames(data_t)<- as.character(unlist(data_t[1,]))
data_t = data_t[-1,]
class(data_t)
summary(data_t)
sapply(data_t, class)

#Convirtiendo las variables de factor a numericas para poder trabajar 
#con ellas como series de tiempo
data_t[,1:51] <- sapply(as.character(unlist(data_t[,1:51])),as.numeric)
sapply(data_t, class)
summary(data_t)
View(data_t)
colnames(data_t)

#"COCA-COLA"      col 1                         "COCA-COLA LIGHT"   col 2
# "SPRITE TOTAL"   col 4
#Quiero un dataset con solo los 3 productos solicitados para analisis
dtseries <- data_t[,c(1,2,4)]

#dtseries$fecha <- exogenas$Date
#dtseries <- dtseries[,-4]
#ahora puedo eliminar elementos que no usaré
rm(data1)
rm(data_t)

#Lo convertimos a serie de tiempo para poder graficar y 
#observar sus comportamientos
ventas_ts <- ts(dtseries, start = c(2014,1), frequency = 12)
plot(ventas_ts, plot.type = "single", col = 1:3)
legend("topleft", colnames(ventas_ts), col = 1:3, lty = 1)

#Creamos una serie de tiempo para cada uno de nuestros productos y empezar 
#a procesar nuestras ts de manera individual

cocacola_ts <- ts(data = dtseries$`COCA-COLA`,frequency = 12, start = c(2014,1))
plot(cocacola_ts, type = "o", col="blue", xlab = "Date", ylab = "Vol. Ventas", main = "Ventas Coca-Cola 2014-2019")
dim(as.matrix(cocacola_ts))

cocalight_ts <- ts(data = dtseries$`COCA-COLA LIGHT`,frequency = 12, start = c(2014,1))
plot(cocalight_ts, type = "o", col="blue", xlab = "Date", ylab = "Vol. Ventas", main = "Ventas Coca-Cola Light 2014-2019")
dim(as.matrix(cocalight_ts))

sprite_ts <- ts(data = dtseries$`SPRITE TOTAL`, frequency = 12, start = c(2014,1))
plot(sprite_ts, type = "o", col="blue", xlab = "Date", ylab = "Vol. Ventas", main = "Ventas Sprite 2014-2019")
dim(as.matrix(sprite_ts))

#Solo la serie de Coca-Cola parece ser estacionaria pero tiene comportamientos 
#irregular en cada fin e inicio de año, las de Coca-Cola Light van a la baja 
#con clara tendencia negativa y parecen tener un componente estacional  
#y Sprite tambien negativo pero se comporta bastante irregular con un poco de 
#estacionalidad hasta 2016
#ciclos desiguales, su tendencia no es clara no son estacionarias

mean(cocacola_ts)
sd(cocacola_ts)
install.packages("moments")
library(moments)
kurtosis(cocacola_ts)#datos bastante dispersos

library(tseries)
library(forecast)

adf.test(cocacola_ts)#la serie no es estacionaria
#se rechaza la hipotesis nula de estationary ya que p-valor>0.05 y el DF es negativo
acf(cocacola_ts)#q=2
pacf(cocacola_ts)#p=0

arima_coca <- arima(cocacola_ts, order = c(0,1,2))
arima_coca
plot(arima_coca$residuals, type="l")

#quiero compararlo con uno auto gnerado por R
sarima_coca<- auto.arima(cocacola_ts)
sarima_coca
plot(sarima_coca$residuals, type="l")

#comprobamos la significancia y estacionaridad 
#de la seie mediante la prueba de Ljung Box
library(stats)

Box.test(arima_coca$residuals, lag=20, type="Ljung-Box")
#los residuos se comportan estacionaria porque p-value > 0.05 
#y no rechazo la h nula que es estacionariedad
rm(sarima_coca)

tsdiag(arima_coca)#esto lo compararemos con otros diagnosticos de modelos
#implementados mas adelante
tsdiag(sarima_coca)


###        Importamos los datos a incluir en los modelos como exógenas      ######################################

#Seleccionamos el archivo de excel completo
exogenas <- as.data.frame(read_excel(file.choose(),sheet = "EXOGENAS", col_types = c("date", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric")))
class(exogenas)
sapply(exogenas, class)
exogenas <- exogenas[,-1]
exogenas_ts <- ts(exogenas, start = c(2014,1), frequency = 12)


#################     COCA - COLA            #####################################

#Training and Testing Datasets

cocacolats_tr <- window(cocacola_ts, start = c(2014,1), end = c(2018, 10))
dim(as.matrix(cocacolats_tr))
cocacolats_tstg <- window(cocacola_ts, start = c(2018,11))
dim(as.matrix(cocacolats_tstg))
x11()
plot.ts(cocacolats_tr, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola 2014-2018")
plot.ts(cocacolats_tstg, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola 2018-2019")


####      SARIMA     ####

install.packages("forecast")
library(forecast)
arima_cocacola <- auto.arima(cocacolats_tr, trace = TRUE, test = "kpss", ic = "bic")
summary(arima_cocacola)
confint(arima_cocacola)#No hay problema de autocorrelacion


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

par()

# Grafiquemos los pronosticos vs. lo real 
arima_cocacola.forecast <- forecast(arima_cocacola, h=8)
arima_cocacola.forecast
plot(arima_cocacola.forecast, xlab="Years", ylab="Ventas(p)")

install.packages("TSPred")
library(TSPred)
plotarimapred(cocacola_ts, arima_cocacola.forecast, xlim = c(2014,2019), range.percent = 0.05, ylab="Volumen de Litros", xlab="Año", main = "Vol. Ventas Coca-Cola")
accuracy(arima_cocacola.forecast, cocacola_ts)

tsdiag(arima_cocacola)#bastante buenos ya que los p-value estan bastante alejados de
#la region de autocorrelacion y no estacionaridad de la serie

### Prueba de inclusion de exogenas para Coca-Cola
install.packages("TSA")
library(TSA)
#Series: cocacolats_tr 
#ARIMA(1,1,0)(1,1,0)[12] 
sarimax_cocacola <- arimax(log(cocacola_ts), order=c(1,1,0), seasonal=list(order=c(1,1,0), period=12), xreg=exogenas$IPC, method='ML')
sarimax_cocacola
confint(sarimax_cocacola)


#################     COCA-COLA LIGHT          #####################################

#Training and Testing Datasets

cocalight_tr <- window(cocalight_ts, start = c(2014,1), end = c(2018, 10))
dim(as.matrix(cocalight_tr))
cocalight_tstg <- window(cocalight_ts, start = c(2018,11))
dim(as.matrix(cocalight_tstg))
x11()
plot.ts(cocalight_tr, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola Light 2014-2018")
plot.ts(cocalight_tstg, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola Light 2018-2019")


####      SARIMA     ####


library(forecast)
arima_light <- auto.arima(cocalight_tr, trace = TRUE, test = "kpss", ic = "bic")
summary(arima_light)
#No hay problema de autocorrelacion
confint(arima_light)

#Residual Diagnostics

plot.ts(arima_light$residuals)
Box.test(arima_light$residuals, lag=20, type="Ljung-Box")
par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(arima_light$residuals, lag.max = 24)
pacf(arima_light$residuals)

Box.test(arima_light$residuals^2, lag=20, type="Ljung-Box")

library(tseries)
jarque.bera.test(arima_light$residuals)


# Grafiquemos los pronosticos vs. lo real 
arima_light.forecast <- forecast(arima_light, h=8)
arima_light.forecast
plot(arima_light.forecast, xlab="Years", ylab="Ventas(p)")

library(TSPred)
plotarimapred(cocalight_ts, arima_light.forecast, xlim = c(2014,2019), range.percent = 0.05, ylab="Volumen de Litros", xlab="Año", main = "Vol. Ventas Coca-Cola Light")
accuracy(arima_light.forecast, cocalight_ts)

tsdiag(arima_light)


### Prueba de inclusion de exogenas para Coca-Cola Light

library(TSA)
#Series: cocalight_tr 
#ARIMA(0,1,1)(1,0,0)[12] with drift 
sarimax_cocalight <- arimax(log(cocalight_ts), order=c(0,1,1), seasonal=list(order=c(1,0,0), period=12), xreg=exogenas$IPC, method='ML')
sarimax_cocalight
confint(sarimax_cocalight)#solo el parametro del componente estacional
#nos arroja significativo al incluir variables exogenas al modelo
#el de la exogena no es significativo lo que implica que IPC no afecta el pronostico


#################           SPRITE          #####################################

#Training and Testing Datasets

sprite_tr <- window(sprite_ts, start = c(2014,1), end = c(2018, 10))
dim(as.matrix(sprite_tr))
sprite_tstg <- window(sprite_ts, start = c(2018,11))
dim(as.matrix(sprite_tstg))
x11()
plot.ts(sprite_tr, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola Light 2014-2018")
plot.ts(sprite_tstg, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola Light 2018-2019")


####      SARIMA     ####


library(forecast)
arima_sprite <- auto.arima(sprite_tr, trace = TRUE, test = "kpss", ic = "bic")
summary(arima_sprite)
#No hay problema de autocorrelacion
confint(arima_sprite)#y el parametro estimado es significativo

#Residual Diagnostics

plot.ts(arima_sprite$residuals)#se comportan estacionarios
Box.test(arima_sprite$residuals, lag=20, type="Ljung-Box")
#par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(arima_sprite$residuals, lag.max = 24)
pacf(arima_sprite$residuals)

Box.test(arima_sprite$residuals^2, lag=20, type="Ljung-Box")

x11()
library(tseries)
jarque.bera.test(arima_sprite$residuals)


# Grafiquemos los pronosticos vs. lo real 
arima_sprite.forecast <- forecast(arima_sprite, h=8)
arima_sprite.forecast
plot(arima_sprite.forecast, xlab="Years", ylab="Ventas(p)")

library(TSPred)
plotarimapred(sprite_ts, arima_sprite.forecast, xlim = c(2014,2019), range.percent = 0.05, ylab="Volumen de Litros", xlab="Año", main = "Vol. Ventas Sprite")
accuracy(arima_light.forecast, sprite_ts)

tsdiag(arima_sprite)


### Prueba de inclusion de exogenas para Sprite

library(TSA)
#Series: sprite_tr 
#ARIMA(0,1,0)(0,0,1)[12]
sarimax_sprite <- arimax(log(sprite_ts), order=c(0,1,0), seasonal=list(order=c(0,0,1), period=12), xreg=exogenas$IPC, method='ML')
sarimax_sprite
confint(sarimax_sprite)#solo el parametro del componente estacional
#nos arroja significativo al incluir variables exogenas al modelo
#el de la exogena no es significativo lo que implica que IPC no afecta el pronostico




################################################################################################################
###  BANCO  - SERIES DE TIEMPO PARA ESTIMAR PARTICIPACION EN EL MERCADO DEL BANCO CAJA SOCIAL            ##########
##################################################################################################################


BCS <- as.data.frame(read_excel(file.choose(), sheet = "Bancos", range = "B1:C63", col_types = c("date", "numeric")))
class(BCS)
sapply(BCS, class)

#La convierto en un objeto serie de tiempo
banco <- ts(BCS$MONTO, start = c(2014,1), frequency = 12)
plot(banco)
abline(reg = lm(banco~time(banco)))
#parece ser una serie estacionaria - la suavizaremos y 
#sin embargo sacaré una diferencia para ver su comportamiento
plot(log(banco))
#Se comporta de manera bastante similar a la original
plot(diff(log(banco)))
#Esta ya muestra un comportamiento mucho mas estacionario
banco_diff <- diff(log(banco))
acf(banco)#MA(1)=q
pacf(banco)#AR(0)=p
#DIF(O) or que a estos no les sacamos diferencia para hacerlo media cero
#ARIMA(0,0,1) MODELO DE DATA ORIGINAL
tsdiag(arima(banco, order = c(0,0,1)))#Los valores p de este modelo parecen 
#no permitir decir la estacionaridad de la serie en 1ra diff

#par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(banco_diff, lag(30))#q value MA(2)
pacf(banco_diff, lag.max = 30)#p value AR(1)
#ARIMA(1,1,2) MODELO DE LAS DIFERENCIAS DE LOS LOGS
tsdiag(arima((log(banco)), order = c(1,1,2))) 

#Lo comparamos con un arima autogenerado por R
sarimabanco <- auto.arima(banco)
sarimabanco

xarimabanco<-auto.arima(log(banco))
xarimabanco

#MODELAMOS CON EL AUTOGENERADO MA(2)SMA(1)12
arimabanco.forecast <- forecast(sarimabanco, h=12)
arimabanco.forecast
#las predicciones deben ser las mimas que usando el comamndo predict
predicciones.sarima <- predict(sarimabanco, n.ahead=12)
predicciones.sarima


plot(arimabanco.forecast, xlab="Years", ylab="Ventas(p)")

library(TSPred)
plotarimapred(banco, arimabanco.forecast, xlim = c(2014,2020), range.percent = .001, xlab="Año", ylab="Monto(particip.)",main = "Participacion Banco Caja Social a 2020")
accuracy(arimabanco.forecast, banco)

tsdiag(sarimabanco)#para este modelo los p.value de los errores si permiten
#aceptar la validez de las predicciones por lo que es buen predictor de la
#participacion en el mercado por parte del BancoCajaSocial



#################################################################################
#######                 Secuencia de Números                         ############ 


#el vector debe llegar hasta n=500
n <- 500
#definimos el vector a rellenar
v3 <- c(1:n)
#a partir de 102 aumenta de 2, luego de a 3, luego de 4 y por ultimo de a 5
counts <- c(2,3,4,5)
#a partir de 102 el aumento debe hacerse t veces  
times<- c(50,33,26,20)

for(i in 102:lenght(v3)){
  for (t in times){
    for (c in counts){
      if (!i %% c) {
        r <- function(i){
        }
        valor<- c(c+i)
      }
      print(valor)
      v3[i] <- rep(valor, each=t)
    }
    
  }
  next
}
