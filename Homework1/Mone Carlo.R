#Ze Zheng

#Monte Carlo Simulation

install.packages("tidyverse")
install.packages("stargazer")
install.packages("urca")
library(tidyverse)
library(stargazer)
library(urca)

u1 <- rnorm(5000,0,1)
u2 <- rnorm(5000,0,2)
v1 <- u1
v2 <- u2

t<-(1:5000)

# Randam Waklk Equation 1
for (i in 2:5000) v1[i]<-v1[i-1]+u1[i] 

# Randam Waklk Equation 2

for (i in 2:5000) v2[i]<-v2[i-1]+u2[i] 
ggplot()+
  geom_line(aes(x=t,y=v1),col="white")+
  geom_line(aes(x=t,y=v2),col="pink")+
  theme_dark()+
  geom_vline(xintercept = 500,color="yellow")


# Random Walk 1 and Random Walk 2 Regression (w1~w2)
reg1<-lm(v1~v2)

# Stargazer Regression Result
stargazer(reg1, report="vcs", type="text",
          omit.stat = c("ser", "f"),
          ci=TRUE, omit.table.layout = "n")
summary(reg1)
sum= summary(reg1)
#Generating f-stats
fstat<-sum$fstatistic
fstat # value, numf, dendf find from fstats

# f-stats only for value
fstat<-sum$fstatistic["value"]
fstat

fstat<-sum$r.squared
fstat # R^2
fstat<-sum$adj.r.squared
fstat # adjusted R^2

qf(0.95,1,9926) #Compare (qf)Critial Value with Fstats

#Clears environment so that previous code does not conflict with new code
rm(list=ls())

n<-250 # the sample size per replication is 250
r<-10000 # the number of replications is 10,000
b0<-1 # the intercept in the regression equation
b1<-0.5 # the slope in the regression equation
su<-2 # the standard deviation of the error term
rsq<-numeric(r) # a spot to save R-squared
x<-rnorm(n,4,1) # x in the regression equation

for(j in 1:r){
  u<-rnorm(n,0,su)
  y<-b0+b1*x+u
  reg<-lm(y~x)
  t_x[j]<-coef(summary(reg))["x",3]
  rsq[j]<-summary(reg)$r.squared
}
sum=summary(reg)

fstat<-sum$fstatistic


rej<-ifelse(fstat>=qf(0.95,2,247),1,0) #is fstat greater than critical value (prob, m, t-k-1)
summary(rej) #viewing results from hypothesis test

plot(density(rsq))

hrsq<-ifelse(rsq>=.46,1,0)

summary(hrsq)
summary(rsq) #viewing the value for r-squared
