library(readxl)
library(knitr)
library(forecast)
library(tseries)
library(urca)
library(stargazer)
library(ggthemes)
library(tidyverse)
library(vars)

rm(list=ls())
Data<-read_excel("ForcastData.xls")

NHousesS <- ts(Data$NHS, start = c(2003,01),frequency = 12)
ts.plot(NHousesS)

ggplot(Data)+
  geom_line(aes(x=Month,y=NHS),color="hotpink")+
  labs(y= "Unemployment Rate", title="Unemployment Rate since 2016")+theme_dark()


summary(ur.df(Data$NHS, type="trend", selectlags = "AIC", lags=20))
summary(ur.df(Data$NHS, type="drift", selectlags = "AIC", lags=20))
summary(ur.df(Data$NHS, type="none", selectlags = "AIC", lags=20))

#Calculate the Q number of significant spikes
acf(Data$NHS)
#Calculate the P number of significant spikes
pacf(Data$NHS)

#ggplot versions
ggAcf(Data$NHS)
ggAcf(Data$NHS)

#ARMA Process
ggAcf(diff(Data$NHS))
ggPacf(diff(Data$NHS))

auto.arima(Data$NHS)
#ARIMA (2,1,5)
auto.arima(diff(Data$NHS))
#ARIMA (2,0,5) with zero mean


NHS_arima<- Arima(NHousesS, order=c(2,0,5),include.mean = F)#Order goes like c(P,D,Q) D 1 is stationary, 2 is non-stationary
tsdiag(NHS_arima)
f_NHS<-forecast(NHS_arima,h=12)
autoplot(f_NHS)
f_NHS



