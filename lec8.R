source(url("https://www.dropbox.com/s/leckggnen53sd7a/utils.R?dl=1"))


# GARCH -------------------------------------------------------------------
library(astsa)
plot(nyse, ylab="NYSE Returns")
utils$save_pdf(path = '')


# Unit Root ---------------------------------------------------------------


n = 100
phi = 0.8
x = rnorm(100)
for (i in 2:n)
{
  x[i] = phi*x[i-1] + x[i]
}


lags=0
z=diff(x)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=x[(lags+1):n]
summary(lm(z.diff~0+z.lag.1 ))

#compare t value with t-distibution quantlies
qt(c(.01,.05,.1),n-3)


#other libraries to do the same thing
library(urca)
df=ur.df(x,type="none",lags=0)
df
summary(df)


library(tseries)
adf.test(x,k=1)

summary(ur.kpss(x,type="mu"))


#compare

n=100
AR=seq(1,.7,by=-.01)
P=matrix(NA,3,31)
M1=matrix(NA,1000,length(AR))
M2=matrix(NA,1000,length(AR))
M3=matrix(NA,1000,length(AR))

for(i in 1:(length(AR)+1))
{
  for(s in 1:1000)
  {
    if(i==1) X=cumsum(rnorm(n))
    if(i!=1) X=arima.sim(n=n,list(ar=AR[i]))
    library(urca)
    M2[s,i]=as.numeric(pp.test(X)$p.value)
    M1[s,i]=as.numeric(kpss.test(X)$p.value)
    M3[s,i]=as.numeric(adf.test(X)$p.value)
  }
}

# Here, we would like to count how many times the p-value of our tests exceed 5%,
prop05=function(x) mean(x>.05)
P[1,]=1-apply(M1,2,prop05)
P[2,]=apply(M2,2,prop05)
P[3,]=apply(M3,2,prop05)

plot(AR,P[1,],type="l",col="red",ylim=c(0,1),ylab="proportion of non-stationnary 
    series",xlab="autocorrelation coefficient")
lines(AR,P[2,],type="l",col="blue")
lines(AR,P[3,],type="l",col="green")
legend(.7,1,c("ADF","KPSS","PP"),col=c("green","red","blue"),lty=1,lwd=1)
save_pdf("unit-root-comparison")



# for higher orders
lags=1
z=diff(x)
n=length(z)
z.diff=embed(z, lags+1)[,1]
z.lag.1=x[(lags+1):n]
k=lags+1
z.diff.lag = embed(z, lags+1)[, 2:k]
summary(lm(z.diff~0+z.lag.1+z.diff.lag ))

Call:
  lm(formula = z.diff ~ 0 + z.lag.1 + z.diff.lag)


library(fpp)
ar(usconsumption,order=3)

library(vars)
VARselect(usconsumption, lag.max=8, type="const")$selection
var <- VAR(usconsumption, p=3, type="const")
serial.test(var, lags.pt=10, type="PT.asymptotic")
summary(var)

fcst <- forecast(var)
plot(fcst, xlab="Year")
save_pdf("usconsumption-var")


fit <- nnetar(sunspotarea)
plot(forecast(fit,h=20))
# To restrict to positive values:
fit <- nnetar(sunspotarea,lambda=0)
plot(forecast(fit,h=20))
save_pdf("sunspots-forecast")


taylor.fit <- tbats(taylor)
plot(forecast(taylor.fit))


###Frequency Domain
x1 = 2*cos(2*pi*1:100*6/100) + 3*sin(2*pi*1:100*6/100)
x2 = 4*cos(2*pi*1:100*10/100) + 5*sin(2*pi*1:100*10/100)
x3 = 6*cos(2*pi*1:100*40/100) + 7*sin(2*pi*1:100*40/100)
x = x1 + x2 + x3
P = abs(2*fft(x)/100)^2; Fr = 0:99/100
plot(Fr, P, type="o", xlab="frequency", ylab="periodogram")
sa
save_pdf("periodogram")
