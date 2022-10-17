### VAR (Netflix & Disney)
library(vars)

### 1. Import data  (from starting Disney OTT service)
getSymbols('NFLX', from = '2019-11-12', to='2021-11-30')
getSymbols('DIS', from = '2019-11-12', to='2021-11-30')
nf <- NFLX$NFLX.Adjusted
nf <- as.numeric(nf)
dp <- DIS$DIS.Adjusted
dp <- as.numeric(dp)

### 2. Plotting Data
par(mfcol=c(2,1)) 
ts.plot(nf)
title('Netflix')
ts.plot(dp)
title('Disney')

# log
log.nf <- log(nf)
log.dp <- log(dp)
# plot log price data
par(mfcol=c(2,1)) 
ts.plot(log.nf)
title('Log transformed Netflix')
ts.plot(log.dp)
title('Log transformed Disney')
# log return

nf.logrtn <- diff(log.nf) 
dp.logrtn <- diff(log.dp) 

# plot log return
par(mfcol=c(2,1)) 
plot(nf.logrtn, type='l')
title('Log return of Netflix')
plot(dp.logrtn, type='l')
title('Log return of Disney')

### 3. Model Estimate
# ACF
acf(nf.logrtn)
acf(dp.logrtn)

# group data and choose lag length
dat.bv <- cbind(nf.logrtn, dp.logrtn)
colnames(dat.bv) <- c('netflix','disney')
# Select lag
info.bv <- VARselect(dat.bv, lag.max = 15, type = 'none')
info.bv$selection

# Build VAR model 
bv.est <- VAR(dat.bv, p=7, type = 'const', season = NULL, exog = NULL)
summary(bv.est)

### 4. Residual check
bv.serial <- serial.test(bv.est, lags.pt=12, type='PT.asymptotic')
bv.serial

bv.norm <- normality.test(bv.est, multivariate.only = TRUE)
bv.norm

bv.arch <- arch.test(bv.est, lags.multi = 12, multivariate.only = TRUE)
bv.arch

plot(bv.serial, names = 'disney')
plot(bv.serial, names = 'netflix')

### 5. Impulse Response
irf.netflix <- irf(bv.est, response = 'netflix', n.ahead=20, boot=TRUE)
plot(irf.netflix)

irf.disney <- irf(bv.est, response = 'disney', n.ahead=20, boot=TRUE)
plot(irf.disney)

### 6. Causality 
bv.cause.netflix <- causality(bv.est, cause = 'netflix')
bv.cause.netflix

bv.cause.disney <- causality(bv.est, cause = 'disney')
bv.cause.disney

