library(fpp)
#laod fortify.ts function
source(url("https://www.dropbox.com/s/f8a6ngld1ej7zrc/fortify-ts.r?dl=1"))

# === Problem 1 EDA === #

#moving average filter
v = filter(usmelec,rep(1/12, 12), sides = 1)
plot(v, lwd=3, col = 2)


#convert to data frame
usmelec.df = fortify.ts(usmelec)
#calculate year and month
usmelec.df$year = floor(usmelec.df$time)
usmelec.df$month = round(12*(usmelec.df$time - floor(usmelec.df$time)))+1

#using loess calculate seasonal pattern
seas_fit <- loess(x ~ month, data = usmelec.df, na.action = na.exclude)
usmelec.df$seas_fit <- predict(seas_fit, newdata = usmelec.df)
usmelec.df$deseas <- residuals(seas_fit)

#check seasonal pattern
plot(usmelec.df$time, usmelec.df$seas_fit ,type='l')
# and residuals (deseasonalised data)
plot(usmelec.df$time, usmelec.df$deseas ,type='l')

#fit linear trend to the data
trend = lm(deseas~time, data = usmelec.df)
# Instead of linear fit, try piecewise polinomial
# trend_lo = loess(deseas~time, data = usmelec.df, span = 0.2)

plot(usmelec.df$time, usmelec.df$deseas, )
abline(trend,col=2, lwd=3)
usmelec.df$trend <-  fitted(trend)
usmelec.df$trend <-  predict(trend,newdata = usmelec.df)
usmelec.df$residual <- residuals(trend)
plot(usmelec.df$time,usmelec.df$residual, col=2, type='l')
plot(usmelec.df$time,usmelec.df$trend, col=2, type='l')

# Compare to the automated process
plot(stl(usmelec, s.window = "periodic"))


# === Problem 2 === #
plot(usmelec)
acf(usmelec, lag.max = 48)
pacf(usmelec, lag.max = 48)
diff_D1 = diff(usmelec,12)
plot(diff_D1)
# The acf2 command asks for information about 48 lags
acf(diff_D1, 48)
pacf(diff_D1, 48)

# estimate your candidate models using 
# fit <- Arima(usmelec, order=c(p,d,q),seasonal=c(P,D,Q))

#for example
fit <- Arima(usmelec, order=c(1,0,2),seasonal=c(1,1,0))

#examine residuals
tsdisplay(residuals(fit))
Box.test(residuals(fit), lag=36, fitdf=6, type="Ljung")
#examine AICC
print(fit)



#use this function to perform cross-validation
cv_ts <- function(model, test_data)
{
  h_ = length(test_data)
  fcast = forecast(model,h = h_)
  return(abs(fcast[['mean']]-test_data)/test_data)
}

# Compare out of sample performance of four forecasitng methods
n = length(usmelec)
h = 10*12  # length of the test data set (forecast horison in months)
k = n-h  # length of the training data set in months
training_end <- tsp(usmelec)[1]+(k-1)/12 #in years
training_data <- window(usmelec, end=training_end ) # first k

test_data <- window(usmelec, start=training_end+1/12)
fit1 <- Arima(training_data, order=c(1,0,1),seasonal=c(0,1,2), include.drift = T) #your best ARIMA model
fit2 <- tslm(training_data ~ trend + season, lambda=0)
fit3 <- ets(training_data,model="MMM",damped=TRUE)
fit4 <- auto.arima(training_data)
mre1 = cv_ts(fit1, test_data)
mre2 = cv_ts(fit2, test_data)
mre3 = cv_ts(fit3, test_data)
mre4 = cv_ts(fit4, test_data)
plot(mre1, col=2)
lines(mre2, col=3)
lines(mre3, col=4)
lines(mre4, col=5)
legend("topleft",legend=c("ARIMA", "LM","ETS", "auto.arima"),col=2:5,lty=1)

# === Problem 3 === #
plot(condmilk)
#get help for BoxCox function
?BoxCox.lambda
?BoxCox

# use BoxCox and BoxCox.lambda to tranform the data if needed


# use diff function if needed to make data staitonary

# estimate your candidate models using 
# fit <- Arima(usmelec, order=c(p,d,q),seasonal=c(P,D,Q))

#for example
fit <- Arima(condmilk, order=c(1,0,2),seasonal=c(1,1,0))

#examine AICC
print(fit)

# forrecats
fcast = forecast(fit,h = 24)

stlf.data  = stlf(condmilk)

