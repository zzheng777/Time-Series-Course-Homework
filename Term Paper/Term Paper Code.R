#Ze Zheng
#April 19th 2021


library(readxl)
library(forecast)
library(tseries)
library(urca)
library(stargazer)
library(ggthemes)
library(tidyverse)
library(vars)
library(ggplot2)

UR5S<-read_excel("UN5S.xls")


ggplot(UR5S)+
  geom_line(aes(x=Month, y=ILUR), color="red")+
  geom_line(aes(x=Month, y=MEUR), color = "blue")+
  geom_line(aes(x=Month, y=OHUR), color = "green")+
  geom_line(aes(x=Month, y=PAUR), color = "orange")+
  geom_line(aes(x=Month, y=ALUR), color = "brown")+
  labs(x= "Month", y="Employment Rate")


summary(ur.df(UR5S$ILUR, type="trend", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$ILUR, type="drift", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$ILUR, type="none", selectlags = "AIC", lags=36))


#$\tau_\tau$ = 2.8335 less than 3.42 Fail to Reject Move to $H_{0,2}$
#$\tau_2$ =2.7295 less than 2.87  Fail to reject Move to $H_{0,6}$
#$\phi_1$ =3.7399  less than 4.59 Fail to reject Move to $H_{0,9}$
#$\tau$ = 0.6056 less than 1.95 Fail to reject Pure Random Walk I(1)

summary(ur.df(UR5S$OHUR, type="trend", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$OHUR, type="drift", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$OHUR, type="none", selectlags = "AIC", lags=36))

#$\tau_\tau$ = 2.9916 less than 3.41 Fail to Reject Move to $H_{0,2}$
#$\tau_2$ =2.6564 less than 2.86  Fail to reject Move to $H_{0,6}$
#$\phi_1$ =3.5312  less than 4.59 Fail to reject Move to $H_{0,9}$
#$\tau$ = 0.8941 less than 1.95 Fail to reject Pure Random Walk I(1)

summary(ur.df(UR5S$MEUR, type="trend", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$MEUR, type="drift", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$MEUR, type="none", selectlags = "AIC", lags=36))

#$\tau_\tau$ = 2.7572 less than 3.41 Fail to Reject Move to $H_{0,2}$
#$\tau_2$ =2.6048 less than 2.86  Fail to reject Move to $H_{0,6}$
#$\phi_1$ =3.4049  less than 4.59 Fail to reject Move to $H_{0,9}$
#$\tau$ = 0.834 less than 1.95 Fail to reject Pure Random Walk I(1)

summary(ur.df(UR5S$ALUR, type="trend", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$ALUR, type="drift", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$ALUR, type="none", selectlags = "AIC", lags=36))

#$\tau_\tau$ = 2.5779 less than 3.41 Fail to Reject Move to $H_{0,2}$
#$\tau_2$ =1.9171 less than 2.86  Fail to reject Move to $H_{0,6}$
#$\phi_1$ =1.8849  less than 4.59 Fail to reject Move to $H_{0,9}$
#$\tau$ = 0.9403 less than 1.95 Fail to reject Pure Random Walk I(1)

summary(ur.df(UR5S$PAUR, type="trend", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$PAUR, type="drift", selectlags = "AIC", lags=36))
summary(ur.df(UR5S$PAUR, type="none", selectlags = "AIC", lags=36))

#$\tau_\tau$ = 2.5516 less than 3.41 Fail to Reject Move to $H_{0,2}$
#$\tau_2$ =2.4723 less than 2.86  Fail to reject Move to $H_{0,6}$
#$\phi_1$ =3.057  less than 4.59 Fail to reject Move to $H_{0,9}$
#$\tau$ = 0.6045 less than 1.95 Fail to reject Pure Random Walk I(1)

civ1<-lm(ILUR~OHUR+PAUR+ALUR+MEUR,data=UR5S)

stargazer(civ1,type="text", report=c("vcst"))

UR5S$e<-resid(civ1)

ggplot(UR5S)+
  geom_line(aes(x=Month, y=e),color="black")+
  xlab("Date")+
  ylab("Residuals")+
  theme_minimal()

summary(ur.df(UR5S$e, type="none", selectlags = "AIC", lags=36))
#$\tau_\tau$ = 2.1169 is greater than 1.95 Reject Null Hypothesis, No Cointergration (WTF Surely I thought there would be)


