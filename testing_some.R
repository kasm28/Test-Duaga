########## El anterior no es un buen modelo 
# intentemos ver que pasa si hacemos con log de la serie de tiempo
#cocalog<-log(cocacolats)
cocadiff<-diff(cocacolats)
#plot(cocadiff)
#dim(as.matrix(cocadiff))
acf(ts(cocadiff, freq=1))
pacf(ts(cocadiff, freq=1))


library(forecast)
arima_cocadiff <- auto.arima(cocadiff, trace = TRUE, test = "kpss", ic = "aic")
summary(arima_cocadiff)
confint(arima_cocadiff)
plot.ts(arima_cocadiff$residuals)
Box.test(arima_cocadiff$residuals, lag=20, type="Ljung-Box")

arima_cocadiff.forecast <- forecast(arima_cocalog, h=8)
arima_cocadiff.forecast
plot(arima_cocadiff.forecast, xlab="Years", ylab="Ventas(p)")
library(TSPred)
plotarimapred(cocacolats, arima_cocadiff.forecast, xlim = c(2014,2019), range.percent = 0.05)




#################     SPRITE          #####################################

sprite_log <- log(sprite_ts)
sprite_ts_diff <- ts(diff(sprite_log), start = c(2014,1), frequency = 12)
class(sprite_ts_diff)
plot(sprite_ts_diff)
plot(sprite_log)

par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(sprite_ts_diff)
pacf(sprite_ts_diff)

model1<- arima(sprite_log, order = c(6,0,1))
model1
tsdiag(model1)


#Training and Testing Datasets

spritelog_tr <- window(sprite_log, start = c(2014,1), end = c(2018, 10))
dim(as.matrix(spritelog_tr))
spritelog_tstg <- window(sprite_log, start = c(2018,11))
dim(as.matrix(spritelog_tstg))
x11()
plot.ts(spritediff_tr, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola Light 2014-2018")
plot.ts(spritediff_tstg, type = "o", col="blue", xlab = "Date", ylab = "Volumen Ventas", main = "Ventas Coca-Cola Light 2018-2019")


####      SARIMA     ####


library(forecast)
arima_spritelog <- auto.arima(spritelog_tr, trace = TRUE, test = "kpss", ic = "bic")
summary(arima_spritediff)
#No hay problema de autocorrelacion
confint(arima_spritediff)

#Residual Diagnostics

plot.ts(arima_spritelog$residuals)
Box.test(arima_spritelog$residuals, lag=20, type="Ljung-Box")
par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(arima_spritediff$residuals, lag.max = 24, freq=1)
pacf(arima_spritediff$residuals, freq=1)

Box.test(arima_spritediff$residuals^2, lag=20, type="Ljung-Box")

x11()
library(tseries)
jarque.bera.test(arima_spritediff$residuals)


# Grafiquemos los pronosticos vs. lo real 
arima_spritelog.forecast <- forecast(model1, h=8)
arima_spritelog.forecast
plot(arima_spritelog.forecast, xlab="Years", ylab="Ventas(p)")

library(TSPred)
plotarimapred(sprite_log, arima_spritelog.forecast, xlim = c(2014,2020), range.percent = 0.05)
accuracy(arima_spritelog.forecast, sprite_log)

tsdiag(arima_spritediff)
rm(sprite_log)
rm(sprite_ts_diff)
rm(spritediff_tr)
rm(spritediff_tstg)


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

#par(mfrow = c(2,1), mar= c(3,3,3,1) +.1)
acf(banco_diff, lag(30))#q value MA(2)
pacf(banco_diff, lag.max = 30)#p value AR(1)
#ARIMA(1,1,2) MODELO DE LAS DIFERENCIAS DE LOS LOGS

#Modelamos primero con el de data original MA(1)
modelbanco<- arima(banco, order = c(0,0,1))
modelbanco
tsdiag(modelbanco)

plot.ts(modelbanco$residuals)
Box.test(modelbanco$residuals, lag=30, type="Ljung-Box")
acf(modelbanco$residuals, lag.max = 30)
pacf(modelbanco$residuals)

#Hacemos las predicciones con el modelo MA(1)
pred_banco <- predict(modelbanco, n.ahead=12)
pred_banco


#arimabanco.forecast <- forecast(modelbanco, h=12)
#arimabanco.forecast

#ARIMA(1,1,2) MODELO DE LAS DIFERENCIAS DE LOS LOGS
modelbancodiff<- arima(log(banco), order = c(1,1,2))
modelbancodiff
tsdiag(modelbancodiff)

plot.ts(modelbancodiff$residuals)
Box.test(modelbancodiff$residuals, lag=30, type="Ljung-Box")
acf(modelbancodiff$residuals, lag.max = 30)
pacf(modelbancodiff$residuals)

#Hacemos las predicciones con el modelo AR(1)I(1)MA(2)
pred_banco_diff <- predict(modelbancodiff, n.ahead=12)
pred_banco_diff
pred_banco_112 <-round(2.718^pred_banco_diff$pred, 2)

ts.plot(banco, pred_banco$pred)
ts.plot(banco, pred_banco_112)
