#1. LOAD LIBRARIES
library(tseries)
library(fBasics)
library(forecast)
library(zoo)
library(vars)
library(rugarch)
setwd("C:\\Users\\patrick.carey\\Desktop\\Forecasting\\")
data=read.csv("NSDisplay.csv", stringsAsFactors = F) 
data$Week <- as.Date(data$Week, "%m/%d/%Y")
#create subset
plot(d)
#create time series
x_ts <- ts(d$x, start = c(2014, 1), frequency=52) #52 for weekly data, 365 daily, 12 monthly
y_ts <- ts(d$x, start = c(2014, 1), frequency=52)
z_ts <- ts(d$x, start = c(2014, 1), frequency=52)
# create the log difference transformation 
x_ts <- diff(log(x_ts))

####View Multivariate possibilities
#d_ts <- data.frame(impression_ts[,1], clicks_ts[,1], appt_ts[,1], cost_ts[,1], trend_ts[,1])
d_ts <- data.frame(x_ts, y_ts, z_ts)
cor(d_ts)
#automatic selection method;
VARselect(d_ts, season=52, lag.max=3,type="const")
m1 = VAR(d_ts, p = 3, type = "const")
coeftest(m1)


############ Model Making
x <- impression_ts # put variable of interest here
basicStats(x) # basic descriptive statistics
x <- diff(log(x_ts))
x <- log(x)
x <- diff(x)
x<- diff(x)#second difference
x<- diff(x, 52) #seasonal
par(mfcol=c(1,1))
acf(coredata((x)), lag.max =105)
pacf(coredata((x)), lag.max =105)
plot(x)
par(mfcol=c(1,2))
hist(x, prob=T)
#normal curve
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
lines(xfit, yfit, col="blue", lwd=2)
#
basicStats(x)
qqnorm(x)
qqline(x, col=2)
par(mfcol=c(1,1))
### Auto Arima
auto.arima(x, max.order=10, d=1, D=1,max.d=2, max.D=2, trace=T, allowdrift = T, seasonal=T,stepwise=F, xreg=diff(log(impression_ts)))
auto.arima(x, max.order=10,trace=T, allowdrift = T, seasonal=T, allowmean=F,stepwise=F,xreg=diff(log(impression_ts)))
#Arima
#m1trend <- Arima(coredata(diff(trend_ts)),order=c(3,1,1), seasonal=list(order=c(0,1,0),period=51)) #model from trend data
#m1 <- Arima(diff(log(cost_ts)),order=c(2,0,1), include.mean=F)#,seasonal=list(order=c(0,1,0),period=52))
#m1 <- Arima(diff(log(cost_ts)),order=c(2,1,2),seasonal=list(order=c(0,1,0),period=52))
m1 <- Arima(diff(log(cost_ts)),order=c(3,1,4),seasonal=list(order=c(0,1,0),period=52), xreg=diff(log(impression_ts)))
#m1 <- Arima(diff(log(cost_ts)),order=c(2,1,2),seasonal=list(order=c(0,1,0),period=52))
coeftest(m1)
summary(m1)
acf(m1$residuals, lag.max = 53)
pacf(m1$residuals, lag.max = 53)

plot(forecast(m1trend, 52))
plot(forecast(m1trend, 4))

# backtest
source("backtest.R")
backtest(m1, x,h=1, orig=83)
help(plot)

forecast(m1trend,52)
