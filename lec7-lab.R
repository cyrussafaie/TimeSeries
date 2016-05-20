# install.packages("orcutt")
library(fpp)
library(orcutt)
source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))
library(forecast)

# n=50
# z = fortify(arima.sim(n=n, list(ar =c(0.8,-0.4) ), sd = sqrt(0.2)))
# z = z$x
# x = fortify(200+arima.sim(n=n, list(ar =c(0.2) ), , sd = sqrt(2))) # the predictor
# x = x$x
# y = 36000 + 1.6*x + z # variable we are studying 
# retail= data.frame(x,y,z)
# retail = saveRDS(object = retail,file = "~/retail.rds")
# retail = readRDS("~/retail.rds")
ls()
getwd()

econ = readRDS("econ.rds")
str(econ)
attach(econ)
plot(x,y) #there is an obvious correation
regmodel=lm(y~x) # etimate regular regression
summary(regmodel)
examine_corr(residuals(regmodel))

#choose the best model
p = 1
d = 0
q = 0
arres = arima (residuals (regmodel), order = c(p,d,q), include.mean = FALSE) #fit the model
summary(arres)

arres =auto.arima(residuals(regmodel))
#apply AR model to the x, y data.
# For examle for AR(1) model:

# Cyrus: basically substracting the variables to remove the correlation, if more x's were available you should more 
n=50
xnew = x[2:n] - arres$coef[1]*x[1:(n-1)]
ynew = y[2:n] - arres$coef[1]*y[1:(n-1)]

# Fit the new model
armodel=lm(ynew~xnew) 
summary(armodel)
examine_corr(residuals(armodel))
# Compare your fit against Cochrane-Orcutt fit. 
cochrane.orcutt(regmodel)
# Cyrus:for prediction you need to caluclate x_new again 


# Turnover ;  New South Wales ;  Supermarket and grocery stores ;
# Turnover ;  New South Wales ;  Liquor retailing ;
# Unit: $ Millions
t = read.table(url("http://www.dropbox.com/s/d7fjrya4xr19euo/retail.csv?dl=1"), sep=',', header=T)

retail <- ts(t[,-1], frequency=12, start=c(1982,4))
x <- retail[,2] 
plot(x)
plot(log(x))

#find the best model
K = 1#what value of K is the best
L = 0#what value of L is the best

(fit <- auto.arima(x, xreg=fourier(x,K), lambda=L))
summary(fit)
#check the residuals
tsdisplay(residuals(fit))

fc <- forecast(fit, xreg=fourierf(x,K,h=24))
plot(fc)

#compare with simple linear model
x.df = fortify(x)
min_t = min(x.df$time)
max_t = max(x.df$time)
lm = lm(log(x)~time, data=x.df)
nd <- data.frame(time=seq(max_t,max_t+2,1/12))
# plot(x~time, data=x.df, xlim = c(min_t,max_t+2), type='l')
lines(nd$time,exp(predict(object = lm,newdata = nd)), col=2, lwd=4)

#spurious correlations
one_sim <- function(){
  beta =  0
  phi_x = 0.8
  phi_z = 0
  x <- arima.sim(list(ar=phi_x), n = 100)
  y <- 1 + beta*x + arima.sim(list(ar=phi_z), n = 100)
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
         sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject <- replicate(1000, one_sim())
table(reject)


