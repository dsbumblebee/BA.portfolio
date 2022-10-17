### ARMA disney

### 1 import data
library(quantmod)
library(fBasics)
library(tseries)
library(fUnitRoots)
library(forecast)
# load data
# 2010-01-01 : Disney acquire Mavel 
getSymbols('DIS', from = '2010-01-01', to='2021-11-30')
# change period ( daily -> weekly )
DIS <- to.weekly(DIS)

### 2 EDA
head(DIS)
## plot Price data
chartSeries(DIS, theme='white')

## change data   ( price --> log return)
# adjusted price data
price <- DIS$DIS.Adjusted
# log price
logprice <- log(as.numeric(price)) #as.numeric(): transform to numeric format
# log return
logrtn <- diff(logprice) 

## plot price distribution 
# probability density function of log return. 
d1 <- density(price)  # obtain density estimate
plot(d1$x, d1$y, xlab = 'price', ylab='density', type='l') #type :line
title('Possibility Density Function')

# plotting normal distribution 
range(price)
x = seq(0, 200, 0.1)
y1 = dnorm(x, mean(price), stdev(price))
lines(x, y1, lty=2)  # add plot , lty =2 : dot line

# plotting Log return data  
par(mfcol=c(2,1))           # multi plot 
ts.plot(logprice)               
title('Log Transform of Stock Price (Disney)')
ts.plot(logrtn)
title('Log Return of Stock Price (Disney)')


# Basic statistics 
# making statistics function  
mystatistics <- function(dat){
  dat.mean <- round(mean(dat),4)       # mean
  dat.stdev <- round(stdev(dat),4)     # standard deviation
  dat.skewness <- round(skewness(dat),4)   # skewness
  dat.excesskurtosis <- round(kurtosis(dat)-3,4)  # excess kurtosis 
  dat.min <- round(min(dat),4)   # minimum
  dat.max <- round(max(dat),4)   # maximum
  dat.median <- round(median(dat), 4)
  result <- cbind(dat.mean, dat.stdev,dat.min, dat.max, dat.skewness, dat.excesskurtosis, dat.median )
  
  return(result)
} 

mystatistics(logrtn)
mystatistics(price)

## zero mean test 
# mean test
t.test(logrtn)        # exist mean

## Check skewness 
# number of observation , length 
logrtn.len <- length(logrtn)
# statistics of skewness 
logrtn.skew <- skewness(logrtn)/sqrt(6/logrtn.len)
logrtn.skew
# p-value
pv.logrtnskew <- 2*(1 - pnorm(abs(logrtn.skew)))
pv.logrtnskew # result : p-value is 0.004011 , reject H0 , so skewness != 0 

## Check kurtosis
# statistics of kurtosis test
logrtn.kur <- (kurtosis(logrtn)-3)/sqrt(24/logrtn.len)
logrtn.kur    # lectokurtosis : tick tails 
pv.logrtnkur <- 2*(1- pnorm(abs(logrtn.kur)))
pv.logrtnkur  # result : p-value is 0.0001 , reject H0 , so excess kurtosis != 0 

## check normality 

# normality test
normalTest(logrtn, method='sw')   # reject the null , this series is not normal

## Plot Log return distribution
# probability density function of log return. 
d1 <- density(logrtn)  # obtain density estimate
plot(d1$x, d1$y, xlab = 'log return', ylab='density', type='l') #type :line
title('Possibility Density Function')
# plotting normal distribution 
range(logrtn)
x = seq(-0.20, 0.14, 0.001)
y1 = dnorm(x, mean(logrtn), stdev(logrtn))
lines(x, y1, lty=2)  # add plot , lty =2 : dot line

## 3. stationary test
# kpss test
kpss.test(logrtn)   # p-value 0.1 : not reject the null , stationary

# find lag to use adftest
madf <- ar(diff(logrtn), method='mle', order.max=20)
madf$order   # order : 14

# Augmented dickey fuller test
adfTest(logrtn, lags = 14, type=c('c'))   #p-value 0.01 : reject the null , stationary 

#### Find model 
## check the ACF & PACF
par(mfcol=c(2,1))           # multi plot 
acf(logrtn, lag = 20)     # lag 9 slightly significant --> MA(9)
pacf(logrtn, lag = 20)    # lag 8 slightly significant --> AR(8)

## Check Serial Correlation 
# Ljung box test
Box.test(logrtn, lag=12, type='Ljung') # result : not reject H0 , not exist serial correaltion
Box.test(logrtn, lag=24, type='Ljung') # result : not reject H0 , not exist serial correaltion

## So , potentiol models are WN, MA(9), AR(8) , ARMA(8,9)

## 4. fitting model 
## fitting ARIMA 
# White noise
m11 <- arima(logrtn, order=c(0,0,0))
m11   # aic : -2480.88
# MA(9) refined
m12 <- arima(logrtn, order=c(0,0,9), fixed = c(0,0,0,0,0,0,0,0,NA,NA))
m12   # aic : -2493.71
# AR(8) refined
m13 <- arima(logrtn, order=c(8,0,0), fixed=c(0,0,0,0,0,0,0,NA,NA))
m13   # aic : -2492.72    ,   0.0794/0.0405 = 1.96 , significant 
# ARMA(8,9)
m14 <- arima(logrtn, order=c(8,0,9), fixed = c(0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,NA,NA))
m14   # 0.0737/0.0406 = 1.81 , insignificant  but 90% significant ,AIC : -2495

## 5. check residual 
# diagnosis
tsdiag(m11, gof=20)   # bad 
tsdiag(m12, gof=20)
tsdiag(m13, gof=20)
tsdiag(m14, gof=20)   # look better  

# qqplot
par(mfcol=c(3,1)) 
qqnorm(m12$residuals, main= "Fitted MA(9) Normal Q-Q plot")
qqline(m12$residuals)
qqnorm(m13$residuals, main= "Fitted AR(8) Normal Q-Q plot")
qqline(m13$residuals)
qqnorm(m14$residuals, main= "Fitted ARMA(8,9) Normal Q-Q plot")
qqline(m14$residuals)   # look better , but tail is out of line

# check residual normality  
normalTest(m14$residuals, method = 'sw')   # reject the null , not normal

## 5. forecast,  insample test
fit.ma9 <- Arima(logrtn[1:611], order=c(0,0,9), fixed = c(0,0,0,0,0,0,0,0,NA,NA))
future.ma9 <- forecast(fit.ma9, h=10)
fit.arma89 <- Arima(logrtn[1:611],order=c(8,0,9), fixed = c(0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,NA,NA))
future.arma89 <- forecast(fit.arma89, h=10)
fit.ar8 <- Arima(logrtn[1:611], order=c(8,0,0), fixed = c(0,0,0,0,0,0,0,NA,NA))
future.ar8 <- forecast(fit.ar8, h=10)

# plot forecast 
par(mfcol=c(3,1)) 
plot(future.ma9)  # increase
plot(future.ar8)  # decrease 
plot(future.arma89)  # close mean  and random 

# RMSE  
actual <- logrtn[612:621]
pred.ma9 <- future.ma9$mean
pred.ar8 <- future.ar8$mean
pred.arma89 <- future.arma89$mean
# squared error
se.ma9 <- (pred.ma9-actual)^2
se.ar8 <- (pred.ar8-actual)^2
se.arma89 <- (pred.arma89-actual)^2
# calculate rmse
rmse.ma9 <- sqrt(sum(se.ma9)/10)
rmse.ar8 <- sqrt(sum(se.ar8)/10)
rmse.arma89 <- sqrt(sum(se.arma89)/10)

cbind(rmse.ma9, rmse.ar8, rmse.arma89)


# forecast 6 step 
fit.arma89 <- Arima(logrtn,order=c(8,0,9), fixed = c(0,0,0,0,0,0,0,NA,0,0,0,0,0,0,0,0,NA,NA))
future.arma89 <- forecast(fit.arma89, h=6)
plot(future.arma89) 
future.arma89$mean
future.arma89$lower
future.arma89$upper
