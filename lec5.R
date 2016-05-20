source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))

#==== problems with statistical tests ====#
# source: http://stats.stackexchange.com/questions/86269/what-is-the-effect-of-having-correlated-predictors-in-a-multiple-regression-mode
set.seed(4314)   # makes this example exactly replicable

# generate sets of 2 correlated variables w/ means=0 & SDs=1
X0 = mvrnorm(n=20,   mu=c(0,0), Sigma=rbind(c(1.00, 0.70),    # r=.70
                                            c(0.70, 1.00)) )
X1 = mvrnorm(n=100,  mu=c(0,0), Sigma=rbind(c(1.00, 0.87),    # r=.87
                                            c(0.87, 1.00)) )
X2 = mvrnorm(n=1000, mu=c(0,0), Sigma=rbind(c(1.00, 0.95),    # r=.95
                                            c(0.95, 1.00)) )
y0 = 5 + 0.6*X0[,1] + 0.4*X0[,2] + rnorm(20)    # y is a function of both
y1 = 5 + 0.6*X1[,1] + 0.4*X1[,2] + rnorm(100)   #  but is more strongly
y2 = 5 + 0.6*X2[,1] + 0.4*X2[,2] + rnorm(1000)  #  related to the 1st

# results of fitted models (skipping a lot of output, including the intercepts)
summary(lm(y0~X0[,1]+X0[,2]))
#             Estimate Std. Error t value Pr(>|t|)    
# X0[, 1]       0.6614     0.3612   1.831   0.0847 .     # neither variable
# X0[, 2]       0.4215     0.3217   1.310   0.2075       #  is significant
summary(lm(y1~X1[,1]+X1[,2]))
#             Estimate Std. Error t value Pr(>|t|)    
# X1[, 1]      0.57987    0.21074   2.752  0.00708 **    # only 1 variable
# X1[, 2]      0.25081    0.19806   1.266  0.20841       #  is significant
summary(lm(y2~X2[,1]+X2[,2]))
#             Estimate Std. Error t value Pr(>|t|)    
# X2[, 1]      0.60783    0.09841   6.177 9.52e-10 ***   # both variables
# X2[, 2]      0.39632    0.09781   4.052 5.47e-05 ***   #  are significant


# We can solve the issue by using cross-validation
x1 = X1[1:80,1]
x2 = X1[1:80,2]

m1 = lm(y1~x1)
m2 = lm(y1~x2)
m3 = lm(y1~x1+x2)

summary(m3)

# Call:
#   lm(formula = y1 ~ x1 + x2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.52986 -0.52079  0.06853  0.57157  2.45764 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   5.1393     0.1115  46.075  < 2e-16 ***
#   x1            0.3306     0.2423   1.364  0.17645    
# x2            0.6236     0.2232   2.794  0.00656 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.9785 on 77 degrees of freedom
# Multiple R-squared:  0.4486,  Adjusted R-squared:  0.4342 
# F-statistic: 31.32 on 2 and 77 DF,  p-value: 1.116e-10

#So significance test shows us that variable  x2 is insignificant

#let's do cross-validation
new = data.frame(x1 = X1[81:100,1], x2 = X1[81:100,1])
p1=predict(m1,new)
p2=predict(m2,new)
p3=predict(m3,new)

sum(abs(p1-y1[81:100])) # 12.7631
sum(abs(p2-y1[81:100])) # 12.55488
sum(abs(p3-y1[81:100])) # 11.93203 the lowest

# however cross-validation shows us that we have the best model when both x1 and x2 are included, which we know is the case, because that is how we generated the data!


#forward spepwise Regression picks wrong mode las well
null=lm(y~1, data=train)
full = lm(y~., data=train)
fwd = step(null, scope=formula(full), dir="forward")
summary(fwd)
# lm(formula = y ~ x1, data = train)

sz = 150
x = arima.sim(model=list(ma=c(0.2,0.3)),n=sz)
y = x[1:(sz-2)]
x1 = x[2:(sz-1)] # 1-lagged data
x2 = x[3:sz]  #2-lagged data
# m1 = lm(y~x1)
# m2 = lm(y~x2)
m3 = lm(y~x1+x2)
summary(m3)
# It is a typical situation when working with TS data
# Call:
#   lm(formula = y ~ x1 + x2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.57793 -0.72089 -0.00756  0.71302  2.60226 
# 
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.02578    0.10704   0.241    0.810    
# x1           0.05170    0.09467   0.546    0.586    
# x2           0.39496    0.09443   4.182 6.43e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.055 on 95 degrees of freedom
# Multiple R-squared:  0.1614,  Adjusted R-squared:  0.1437 
# F-statistic: 9.142 on 2 and 95 DF,  p-value: 0.0002338


x1 = rnorm(sz-2)
x2 = rnorm(sz-2)
y = 0.2*x1+0.2*x2 + rnorm(sz-2)
# m1 = lm(y~x1)
# m2 = lm(y~x2)
m3 = lm(y~x1+x2)
summary(m3)

# Call:
#   lm(formula = y ~ x1 + x2)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.82413 -0.54280 -0.08998  0.56914  2.94143 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  0.03035    0.08921   0.340   0.7342  
# x1           0.18668    0.09024   2.069   0.0403 *
#   x2           0.21252    0.08732   2.434   0.0162 *
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.085 on 145 degrees of freedom
# Multiple R-squared:  0.06498,  Adjusted R-squared:  0.05209 
# F-statistic: 5.039 on 2 and 145 DF,  p-value: 0.007664



#==== cross-validation ====#
# source: http://robjhyndman.com/hyndsight/tscvexample/

cv_ts <- function(model, test_data, p = 1)
{
  h_ = length(test_data)
  fcast = forecast(model,h = h_)
  return(abs(fcast[['mean']]-test_data))
}

plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales", col=3, type='l', lwd=2)
save_fig(pdf, "pharm-sales.pdf")
plot(log(a10), ylab="", xlab="Year", main="Log Antidiabetic drug sales",col=3, lwd=2)
save_fig(pdf, "pharm-sales-log.pdf")

k <- 60 # minimum data length for fitting in months
n <- length(a10)
mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12)
st <- tsp(a10)[1]+(k-2)/12 #in years


for(i in 1:(n-k))
{
  #if we want to have an expanding window
  xshort <- window(a10, end=st + i/12) # first 60 + i-1 months
  # if we want to have a window of fixed length
#   xshort <- window(a10, start=st+(i-k+1)/12, end=st+i/12) # always k observations
  xnext <- window(a10, start=st + (i+1)/12, end=st + (i+12)/12) #next 12 months
  fit1 <- tslm(xshort ~ trend + season, lambda=0)
  fit2 <- Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12), 
                include.drift=TRUE, lambda=0, method="ML")
  fit3 <- ets(xshort,model="MMM",damped=TRUE)
  mae1[i,1:length(xnext)] <- cv_ts(fit1, xnext)
  mae2[i,1:length(xnext)] <- cv_ts(fit2, xnext)
  mae3[i,1:length(xnext)] <- cv_ts(fit3, xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE",
     ylim=c(0.65,1.05))
lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3)
lines(1:12, colMeans(mae3,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)
save_fig(pdf, "cross-validation.pdf")




data(ausair, package = "fpp")
air <- window(ausair,start=1990,end=2004)
fit1 <- holt(air, alpha=0.8, beta=0.2, initial="simple", h=5) 
fit2 <- holt(air, alpha=0.8, beta=0.2, initial="simple", exponential=TRUE, h=5) 
# Results for first model:
fit1$model$state
fitted(fit1)
fit1$mean
fit3 <- holt(air, alpha=0.8, beta=0.2, damped=TRUE, initial="simple", h=5) 
plot(fit2, type="o", ylab="Air passengers in Australia (millions)", xlab="Year", 
     fcol="white", plot.conf=FALSE)
lines(fitted(fit1), col="blue") 
lines(fitted(fit2), col="red")
lines(fitted(fit3), col="green")
lines(fit1$mean, col="blue", type="o") 
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft", lty=1, col=c("black","blue","red","green"), 
       c("Data","Holt's linear trend","Exponential trend","Additive damped trend"), bty = "n")
save_fig(pdf, name = "air-smoothing.pdf")


%# Sheep in Asia
data(livestock, package = "fpp")
livestock2 <- window(livestock,start=1970,end=2000)
fit1 <- ses(livestock2)
fit2 <- holt(livestock2)
fit3 <- holt(livestock2,exponential=TRUE)
fit4 <- holt(livestock2,damped=TRUE)
fit5 <- holt(livestock2,exponential=TRUE,damped=TRUE)
# Results for first model:
fit1$model
accuracy(fit1) # training set
accuracy(fit1,livestock) # test set

fit2_df = fortify(fit2$model$state)
fit4_df = fortify(fit4$model$state)

par(mfrow=c(2,2), mar=c(3,4,0,0))
plot(fit2_df$time,fit2_df$l,type='o', ylab="Level", xlab="Year", col=3)
plot(fit2_df$time,fit2_df$b,type='o', ylab="Trend", xlab="Year", col=3, ylim=c(3,5.5))

plot(fit4_df$time,fit4_df$l,type='o', ylab="Level", xlab="Year", col=3)
plot(fit4_df$time,fit4_df$b,type='o', ylab="Trend", xlab="Year", col=3, ylim=c(3,5.5))
save_fig(pdf, name = "sheep-smoothing.pdf")
par(par.def)

plot(fit3, type="o", ylab="Livestock, sheep in Asia (millions)", 
     flwd=1, plot.conf=FALSE)
lines(window(livestock,start=2001),type="o")
lines(fit1$mean,col=2)
lines(fit2$mean,col=3)
lines(fit4$mean,col=5)
lines(fit5$mean,col=6)
legend("topleft", lty=1, pch=1, col=1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"), bty = "n")
save_fig(pdf, name = "sheep-smoothing1.pdf")

# International tourist visitor nights in Australia
data(austourists, package = "fpp")
aust <- window(austourists,start=2005)
fit1 <- hw(aust,seasonal="additive")
fit2 <- hw(aust,seasonal="multiplicative")

plot(fit2,ylab="International visitor night in Australia (millions)",
     plot.conf=FALSE, type="o", fcol="white", xlab="Year")
lines(fitted(fit1), col="red", lty=2)
lines(fitted(fit2), col="green", lty=2)
lines(fit1$mean, type="o", col="red")
lines(fit2$mean, type="o", col="green")
legend("topleft",lty=1, pch=1, col=1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"), bty = "n")

save_fig(pdf, name = "tourist-smoothing.pdf")
states <- cbind(fit1$model$states[,1:3],fit2$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab="Year")
fit1$model$state[,1:3]
fitted(fit1)
fit1$mean
save_fig(pdf, name = "tourist-smoothing-states.pdf")








x <- arima.sim(model = list(ar = 0.8), 500)
fit_ar1 <- arima(x, order = c(1, 0, 0 ))
predict(fit_ar1, n.ahead = 10)
predict(fit_ar1, n.ahead = 10)
pred.df <- as.data.frame(predict(fit_ar1,n.ahead = 10))

qplot(1:500, x, geom = "line") +
  geom_line(aes(x = 501:510, pred - 2*se), data = pred.df,
            linetype = "dashed") +
  geom_line(aes(x = 501:510, pred + 2*se), data = pred.df,
            linetype = "dashed") +
  geom_line(aes(x = 501:510, pred), data = pred.df, colour = "red")
save_fig(type = pdf, name = "ar1-forecast.pdf")

pred.df.100 <- as.data.frame(predict(fit_ar1, n.ahead = 100))
qplot(1:500, x, geom = "line") +
  geom_line(aes(x = 501:600, pred - 2*se), data = pred.df.100,
            linetype = "dashed") +
  geom_line(aes(x = 501:600, pred + 2*se), data = pred.df.100,
            linetype = "dashed") +
  geom_line(aes(x = 501:600, pred), data = pred.df.100, colour = "red")
save_fig(type = pdf, name = "ar1-forecast1.pdf")


# Global Temperature
temp <- fortify(gtemp)
temp <- rename(temp, c("x" = "temp"))

qplot(time, temp, data = temp, geom = "line") +
  big_font

temp$diff1 <- c(NA, diff(temp$temp))

qplot(time, diff1, data = temp, geom = "line") +
  geom_smooth() +
  big_font 

n <- nrow(temp)
(fit_ar3 <- arima(temp$temp, order = c(3, 1, 0), xreg = 1:n))
(fit_ma1 <- arima(temp$temp, order = c(0, 1, 1), xreg = 1:n))
(fit_arma1 <- arima(temp$temp, order = c(1, 1, 1), xreg = 1:n))
(fit_ma2 <- arima(temp$temp, order = c(0, 1, 2), xreg = 1:n))


pred.df <- as.data.frame(predict(fit_ar3, n.ahead = 20, newxreg = (n + 1):(n+20)))
pred.df$time <- max(temp$time) + (1:20)

qplot(time, temp, data = temp, geom = "line") + 
  geom_ribbon(aes(ymin = pred- 2*se, ymax = pred + 2*se, y = NULL), data = pred.df, alpha = 0.2) +
  geom_line(aes(y = pred), data = pred.df) +
  big_font
save_fig(type = pdf, name = "global-temp-forecast.pdf")
# implicity assuming a linear trend

pred.df_ma2 <- as.data.frame(predict(fit_ma2, n.ahead = 20, newxreg = (n + 1):(n+20)))
pred.df_ma2$time <- max(temp$time) + (1:20)

qplot(time, temp, data = temp, geom = "line") + 
  geom_ribbon(aes(ymin = pred- 2*se, ymax = pred + 2*se, y = NULL), data = pred.df_ma2, alpha = 0.2) +
  geom_line(aes(y = pred), data = pred.df_ma2) +
  big_font
# implicity assuming a linear trend

# compare to explicitly modelling linear trend
fit_temp <- lm(temp ~ time, data= temp, 
               na.action = na.exclude)
temp$lin_res <- residuals(fit_temp)
(fit_lin_ar1 <- arima(temp$temp, order = c(1, 0, 0), xreg = 1:n))
pred.df2 <- as.data.frame(predict(fit_lin_ar1, n.ahead = 20, newxreg = (n + 1):(n+20)))
pred.df2$time <- max(temp$time) + (1:20)

qplot(time, temp, data = temp, geom = "line") + 
  geom_ribbon(aes(ymin = pred- 2*se, ymax = pred + 2*se, y = NULL), data = pred.df2, alpha = 0.2) +
  geom_line(aes(y = pred), data = pred.df2) +
  big_font
save_fig(type = pdf, name = "global-temp-forecast1.pdf")



#==== Seasonal ARIMA ====#
data(co2, package = "TSA")
co2 <- fortify(co2)
co2 <- rename(co2, c("x" = "CO2"))
qplot(time, CO2, data = co2, geom = "line") +
  big_font
save_fig(type = pdf, name = "co2-canada.pdf")

# taking first differences removes the trend
co2$diff <- c(rep(NA, 1), diff(co2$CO2, lag = 1))
qplot(time, diff, data = co2, geom = "line") +
  big_font
save_fig(type = pdf, name = "co2-diff1.pdf")


# but there is still strong seasonality
# could also remove the seasonality by differencing
# 12 months per year, so difference at lag 12 in original data
# taking first differences removes the trend
co2$diff_12 <- c(rep(NA, 12), diff(co2$diff, lag = 12))
qplot(time, diff_12, data = co2, geom = "line") +
  big_font 
save_fig(type = pdf, name = "co2-diff12.pdf")
# now we have something that looks stationary


# raw acf
examine_corr(co2$CO2, lag.max = 40)
# acf of 1st diff
examine_corr(co2$diff, na.action = na.pass, lag.max = 40)
# acf of 1st and seasonsal diff
examine_corr(co2$diff_12, na.action = na.pass, lag.max = 40)
save_fig(type = pdf, name = "co2-diff12-acf.pdf")


acf(co2$diff_12, na.action = na.pass)
pacf(co2$diff_12, na.action = na.pass)



(fit_ma <- arima(co2$CO2, order = c(0, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)))
(fit_ar <- arima(co2$CO2, order = c(1, 1, 0), seasonal = list(order = c(0, 1, 1), period = 12)))
(fit_sarma <- arima(co2$CO2, order = c(1, 1, 1), seasonal = list(order = c(0, 1, 1), period = 12)))
# choose ARIMA (0, 1, 1) x (0, 1, 1)_12

# what does the automatic procedure say
library(forecast)
# need to make it a ts to get seasonality
# freq = 12 measurements per cycle
auto.arima(ts(co2$CO2, freq =  12))
# picks SARIMA(1, 0, 1)x(0, 1, 1)[12] but AIC is within 
# 3 of our model.

# diagnostics
examine_corr(residuals(fit_ma)) 
last_plot()+ big_font
save_fig(type = pdf, name = "co2-sarma-res1.pdf")

qqnorm(residuals(fit_ma))
qqline(residuals(fit_ma))
save_fig(type = pdf, name = "co2-sarma-res2.pdf")

# forecast
pred.df <- as.data.frame(predict(fit_ma, n.ahead = 4*12))
pred.df$time <- max(co2$time) + (1:(4*12))/12

qplot(time, CO2, data = co2, geom = "line") + 
  geom_ribbon(aes(ymin = pred- 2*se, ymax = pred + 2*se, y = NULL), data = pred.df, alpha = 0.2) +
  geom_line(aes(y = pred), data = pred.df) +
  big_font
save_fig(type = pdf, name = "co2-sarma-forecast.pdf")



plot(euretail)
save_fig(type = pdf, name = "euretail.pdf")

tsdisplay(diff(euretail,4))
save_fig(type = pdf, name = "euretail-s4.pdf")

tsdisplay(diff(diff(euretail,4)))
save_fig(type = pdf, name = "euretail-s4d1.pdf")


fit <- Arima(euretail, order=c(0,1,1),
             seasonal=c(0,1,1))
tsdisplay(residuals(fit))
save_fig(type = pdf, name = "euretail-res-fit.pdf")

fit <- Arima(euretail, order=c(0,1,3),
             seasonal=c(0,1,1))
tsdisplay(residuals(fit))
save_fig(type = pdf, name = "euretail-res-fit-1.pdf")


plot(h02)
save_fig(type = pdf, name = "h02.pdf")

plot(log(h02))
save_fig(type = pdf, name = "h02-log.pdf")

acf(h02,lag.max=25)
save_fig(type = pdf, name = "h02-acf.pdf")

pacf(h02, lag.max=25)
save_fig(type = pdf, name = "h02-pacf.pdf")

tsdisplay(diff(h02,12))
save_fig(type = pdf, name = "h02-s12.pdf")

fit <- Arima(h02, order=c(3,0,1), seasonal=c(0,1,2), lambda=0)
tsdisplay(residuals(fit))
save_fig(type = pdf, name = "h02-res-fit.pdf")

getrmse <- function(x,h,...)
{
  train.end <- time(x)[length(x)-h]
  test.start <- time(x)[length(x)-h+1]
  train <- window(x,end=train.end)
  test <- window(x,start=test.start)
  fit <- Arima(train,...)
  fc <- forecast(fit,h=h)
  return(accuracy(fc,test)[2,"RMSE"])
}
