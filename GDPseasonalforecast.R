library(readxl)
setwd('C:/Users/Mahe/Desktop/8th Sem Project/Data')
gdp.data<-read_excel('GDPdatewise.xlsx')
gdp.data2<-gdp.data[,2]
gdptimeseries<-ts(gdp.data2,frequency = 4,start=c(2011,1))
plot.ts(gdptimeseries, main='GDP (In percentage) over years',lwd=3)
library(tseries)
adf.test(gdptimeseries)
acf(gdptimeseries)
pacf(gdptimeseries)
gdptimeseriescomponents<-decompose(gdptimeseries)
plot(gdptimeseriescomponents)
library('sarima')
plot(gdptimeseriescomponents)

model1<-arima(gdptimeseries,order = c(0,0,0),seasonal = list(order=c(1,0,1),period=3))
AIC(model1)
sum(residuals(model1)^2)

library(forecast)
autoPred1 = forecast(model1, h=3)
plot(autoPred1)

model2<-arima(gdptimeseries,order = c(0,0,0),seasonal = list(order=c(1,0,2),period=3))
AIC(model2)
sum(residuals(model2)^2)

autoPred2 = forecast(model2, h=3)
plot(autoPred2)

model3<-arima(gdptimeseries,order = c(0,0,0),seasonal = list(order=c(2,0,1),period=3))
AIC(model3)
sum(residuals(model3)^2)

autoPred3 = forecast(model3, h=3)
plot(autoPred3)

model4<-arima(gdptimeseries,order = c(0,0,0),seasonal = list(order=c(2,0,2),period=3))
AIC(model4)
sum(residuals(model4)^2)

autoPred4 = forecast(model4, h=5)
plot(autoPred4, ylab='GDP in percentage',lwd=3)
model4

