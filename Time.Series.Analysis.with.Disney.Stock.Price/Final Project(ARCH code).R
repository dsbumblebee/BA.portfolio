### GARCH disney

### 0. import library
library(quantmod)
library(fBasics)
library(tseries)
library(fUnitRoots)
library(forecast)
library(fGarch)
library(rugarch)
### 1. load data
# 2010-01-01 : Disney acquire Mavel
getSymbols('DIS', from = '2010-01-01', to='2021-11-30')
# change period ( daily -> weekly )
DIS <- to.weekly(DIS)

## change data   ( price --> log return)
# adjusted price data
price <- DIS$DIS.Adjusted
# log price
logprice <- log(as.numeric(price)) #as.numeric(): transform to numeric format
# log return
logrtn <- diff(logprice) 

### 2. ARCH test 
# check the correlation of volatility 
y = logrtn - mean(logrtn)
Box.test(y^2, lag=12, type='Ljung')   # reject the null : exist serial correlation

# checking the PACF
par(mfcol=c(2,1)) 
pacf(y^2, lag=24)     # until lag 3 exist 
# plot volatility
plot(y^2, type='l')   # some volatility is big

setwd('/Users/jin/Documents/Rstudy/QM')
source("archTest.R")  # R script available on the book web site.
archTest(y,12)  # lag 1~3, and 5 significant : exist arch effect 

### 3. Model Estimate 
## rugarch only can use xts 
library(xts)
dates <- seq(as.Date('2010-01-01'), length=621, by='weeks')
xtsdata <- xts(x=logrtn, order.by=dates)
# data resample code : apply.monthly(xtsdata, mean)

tail(xtsdata)
# ARMA(8,9)  + ARCH(3)
model389<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),
                     mean.model= list(include.mean=TRUE, armaOrder = c(8,9)),
                     fixed.pars = list(ar1=0, ar2=0, ar3=0, ar4=0, ar5=0, ar6=0, ar7=0,
                                       ma1=0, ma2=0, ma3=0, ma4=0, ma5=0, ma6=0, ma7=0,ma8=0),
                                       distribution.model = "norm")

m.389<-ugarchfit(spec=model389,data=xtsdata, out.sample = 0)
m.389    # ar8 , ma9 are insignificant 

# ARCH(3)
model30<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),mean.model= list(include.mean=TRUE),
                    fixed.pars = list(ar1 = 0, ma1=0), distribution.model = "norm")

m.30<-ugarchfit(spec=model30,data=xtsdata, out.sample = 0)
m.30     # alpha1 is insignificant 

# ARCH(3) , without alpha1
model20<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),
                    mean.model= list(include.mean=TRUE),
                    fixed.pars = list(ar1 = 0, ma1=0, alpha1=0), 
                    distribution.model = "norm")

m.20<-ugarchfit(spec=model20,data=xtsdata, out.sample = 0)
m.20     # all coefficient is significant  , aic -4.0854

# ARCH(3) without alpha1 + t-distribution
model20std<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),mean.model= list(include.mean=TRUE),
                       fixed.pars = list(ar1 = 0, ma1=0, alpha1=0), distribution.model = "std")

m.20std <-ugarchfit(spec=model20std,data=xtsdata, out.sample = 0)
m.20std  # aic : -4.1744

# ARCH(3) without alpha1 + skewed t-distribution
model20sstd<-ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(3, 0)),mean.model= list(include.mean=TRUE),
                        fixed.pars = list(ar1 = 0, ma1=0, alpha1=0), distribution.model = "sstd")

m.20sstd <-ugarchfit(spec=model20sstd,data=xtsdata, out.sample = 0)
m.20sstd # aic : -4.1716

# GARCH(1,1)
gm.11 <- garchFit(~garch(1,1), data=logrtn, trace=F)
summary(gm.11)  # aic : -4.100713

# GARCH(1,1)  t-distribution
gm.11.std <- garchFit(~garch(1,1), data=logrtn, trace=F, cond.dist='std')
summary(gm.11.std)  # aic : -4.196     # omega's p-value : 0.0567

# GARCH(1,1)  skewed t-distribution
gm.11.sstd <- garchFit(~garch(1,1), data=logrtn, trace=F, cond.dist='sstd')
summary(gm.11.sstd)  # aic : -4.193     # omega's p-value : 0.0556

### 4. Check Residual 
# qq plot   model comparison
par(mfcol=c(2,2))  
plot(m.20std)
plot(m.20sstd)
plot(gm.11.std)
plot(gm.11.sstd)

# residual check
plot(gm.11.std)  # 9,10,11,12

# check residual normality  
resid.gm.11.std <- residuals(gm.11.std)
res=ts(resid.gm.11.std,frequency=52,start=c(2010,1,8))
normalTest(res, method = 'sw')   # reject the null , not normal

### 5. Predictive interval
### plot of predictive intervals 95%
# volatility
v1<-volatility(gm.11.std)
resid.gm.11.std <- residuals(gm.11.std, standardize=T)
vol=ts(v1,frequency=52,start=c(2010,1,8))
res=ts(resid.gm.11.std,frequency=52,start=c(2010,1,8))

# Obtain plot of predictive intervals 95%
par(mfcol=c(1,1))
upp=0.00290+1.96*v1  # mean + 2 * volatility (gach 11) 책은 2 사용 
low=0.00290-1.96*v1
tdx=c(1:621)/52+2010
# predictive intervals 95% CI : 1.96 or 2 (approx)
plot(tdx,logrtn,xlab='year',ylab='series',type='l',ylim=c(-0.2,0.2))
lines(tdx,upp,lty=2,col='red')  # line type
lines(tdx,low,lty=2,col='red')
abline(h=c(0.0029))  # draw mean value , # volatility 포함
title('Plot of Predictive interval 95%')

### 6. Forecast 
# Forecast  6 step
prediction <- predict(gm.11.std, n.ahead=6, trace=F, plot=T)

prediction$meanForecast
prediction$lowerInterval
prediction$upperInterval
## insample test
fit <- garchFit(~garch(1,1), data=logrtn[1:611], trace=F, cond.dist='std')
pred <- predict(fit, n.ahead=10, trace=F, plot=T)

# rmse
actual <- logrtn[612:621]
pred.g11 <- pred$meanForecast
se.g11 <- (pred.g11-actual)^2
rmse.g11 <- sqrt(sum(se.g11)/10)
rmse.g11

### Additional model : E garch
source("Egarch.R") # Compile R script
egm <- Egarch(logrtn) # Model fitting

stresi=egm$residuals/egm$volatility # Obtain standardized residuals
tdx=c(1:621)/52+2011 # Compute time index
par(mfcol=c(2,1)) # Plotting
plot(tdx,logrtn,xlab='year',ylab='logrtn',type='l')
plot(tdx,stresi,xlab='year',ylab='stresi',type='l')
Box.test(stresi,lag=10,type='Ljung')  # Model checking
Box.test(stresi,lag=20,type='Ljung')
Box.test(stresi^2,lag=10,type='Ljung')
Box.test(stresi^2,lag=20,type='Ljung')

acf(stresi)
acf(stresi^2)
