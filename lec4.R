library(ggplot2)
library(fpp)
source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))
# simulate MA(1) process
ma.sim<-arima.sim(model=list(ma=c(-.7)),n=100)
plot(ma.sim, lwd=3, type='o', pch='*', col=3)
save_fig(type = pdf, name = "ma-1.pdf")

par(mfrow=c(2,1), mar=c(2,1,0,1))
ma.sim = arima.sim(model=list(ma=c(0.99)),n=100)
ma.sim1 = rnorm(100,0,1)
plot(ma.sim, xlab="", ylab="")
plot(ma.sim1,type='l', xlab="", ylab="")
par(par.def)
save_fig(type = pdf, name = "ma-1-vs-white-nose.pdf")

# simulate AR(2) process
plot(arima.sim(list(ar=c(1, -0.5)), 200),lwd=0.5, ylab="")
for (i in 1:50)
{
  lines(arima.sim(list(ar=c(1, -0.5)), 200),lwd=0.5)
}
save_fig(type = pdf, name = "ar-2-stationary.pdf", lheight = pdf_h/2)

T = 200
p = 2
dat=seri=rnorm(T) #white noise
for (t in ((p+1):T))
  seri[t]= 1.5*seri[t-1] - 0.5*seri[t-2] + dat[t]
plot(seri,lwd=0.5, ylab="", type='l', ylim=c(-60,60))
for (i in 1:50)
{
  dat=seri=rnorm(T) #white nois
  for (t in ((p+1):T))
    seri[t]= 1.5*seri[t-1] - 0.5*seri[t-2] + dat[t]
  lines(seri,lwd=0.5)
}
save_fig(type = pdf, name = "ar-2-non-stationary.pdf",lheight = pdf_h/2)


ar_convege <- function(phi,n)
{
  s = 0
  d = rep(0.0,n)
  for (i in 1:n)
  {
    w = rnorm(1,0,sd = 1);
    s = s + (phi^i)*w
    d[i] = s
  }
  return(d)
}


par(mfrow=c(4,1), mar = c(0,0,0,0))
plot(ar_convege(0.9, 50), type='o', lwd=3, col = 3)
plot(ar_convege(0.9, 50), type='o', lwd=3, col = 3)
plot(ar_convege(0.9, 50), type='o', lwd=3, col = 3)
plot(ar_convege(0.9, 50), type='o', lwd=3, col = 3)
save_fig(type = pdf, name = "ar-converge.pdf")


par(mfrow=c(4,1), mar = c(0,0,0,0))
plot(ar_convege(1, 100), type='o', lwd=3, col = 3)
plot(ar_convege(1, 100), type='o', lwd=3, col = 3)
plot(ar_convege(1, 100), type='o', lwd=3, col = 3)
plot(ar_convege(1, 100), type='o', lwd=3, col = 3)
save_fig(type = pdf, name = "ar-converge1.pdf")

par(mfrow=c(4,1), mar = c(1,2,0,0))
plot(ar_convege(1.1, 100), type='o', lwd=3, col = 3)
plot(ar_convege(1.1, 100), type='o', lwd=3, col = 3)
plot(ar_convege(1.1, 100), type='o', lwd=3, col = 3)
plot(ar_convege(1.1, 100), type='o', lwd=3, col = 3)
save_fig(type = pdf, name = "ar-converge2.pdf")


y =  ARMAacf(ar=c(0,-0.25), ma=0.2,lag.max = 5)
qplot(x = 0:5, ymin = 0, ymax = y, geom = "linerange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.3, 0.3))
save_fig(type = pdf, name = "acf-arma11-ARMAacf.pdf")


# check by hands:
psi=c(1,1/5,-1/4,-1/20,1/16,1/80,-1/64)
g0 = sum(psi^2)
g1 = sum(psi[1:6]*psi[2:7])
g2 = sum(psi[1:5]*psi[3:7])
abs(g1/g0 - y[2])
abs(g2/g0 - y[3])

#ACF of MA(q)
y =  ARMAacf(ma = 0.5,lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ma1.pdf")


y =  ARMAacf(ma = c(1/6,1/2),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ma2.pdf")

y =  ARMAacf(ma = c(1/2,1/2,-1/4,-1/4,-1/4),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ma5.pdf")

y =  ARMAacf(ma = rep(1/2,10),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ma10.pdf")

#ACF of AR(P)
y =  ARMAacf(ar = 0.5,lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
dev.copy(pdf,paste(path,"acf-ar1.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

y =  ARMAacf(ar = c(1/6,1/2),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ar2.pdf")

y =  ARMAacf(ar = -c(1/2,1/2,-1/4,-1/4,-1/4),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.5, 1))
save_fig(type = pdf, name = "acf-ar5.pdf")

y =  ARMAacf(ar = rep(1/9,8),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ar8.pdf")


#ACF ARMA(p,q)
y =  ARMAacf(ar = 0.5,ma=0.5,lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-arma11.pdf")

y =  ARMAacf(ar = c(1/6,1/2),ma=1/2,lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-arma21.pdf")

y =  ARMAacf(ar = c(-1/2,+1/4),ma=c(-1/4,1/2),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-1, 1))
save_fig(type = pdf, name = "acf-arma22.pdf")

y =  ARMAacf(ar = c(1/9,1/9),ma=c(1/4,-1/2),lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-arma22-1.pdf")


#ACF which is which
y =  ARMAacf(ar = 0.6,lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ar1-which.pdf")

y =  ARMAacf(ar = 0.5,ma=0.5,lag.max = 15)
qplot(x = 0:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-arma11-which.pdf")

y =  ARMAacf(ar = 0.6,lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "pacf-ar1-which.pdf")

y =  ARMAacf(ar = 0.5,ma=0.5,lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.5, 1))
save_fig(type = pdf, name = "pacf-arma11-which.pdf")

#PACF of MA(q)
y =  ARMAacf(ma = 0.5,lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red"))+
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.5, 1))
save_fig(type = pdf, name = "pacf-ma1.pdf")

y =  ARMAacf(ma = c(1/6,1/2),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.3, 1))
save_fig(type = pdf, name = "pacf-ma2.pdf")

y =  ARMAacf(ma = -c(1/2,1/2,-1/4,-1/4,-1/4),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.5, 1))
save_fig(type = pdf, name = "pacf-ma5.pdf")

y =  ARMAacf(ma = rep(1/2,10),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.3, 1))
save_fig(type = pdf, name = "pacf-ma10.pdf")

#PACF of AR(P)
y =  ARMAacf(ar = 0.5,lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "pacf-ar1.pdf")

y =  ARMAacf(ar = c(1/6,1/2),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "pacf-ar2.pdf")

y =  ARMAacf(ar = -c(1/2,1/2,-1/4,-1/4,-1/4),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.6, 1))
save_fig(type = pdf, name = "pacf-ar5.pdf")

y =  ARMAacf(ar = rep(1/9,8),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "pacf-ar8.pdf")

#PACF ARMA(p,q)
y =  ARMAacf(ar = 0.5,ma=0.5,lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.4, 1))
save_fig(type = pdf, name = "pacf-arma11.pdf")

y =  ARMAacf(ar = c(1/6,1/2),ma=1/2,lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-0.1, 1))
save_fig(type = pdf, name = "pacf-arma21.pdf")


y =  ARMAacf(ar = c(-1/2,+1/4),ma=c(-1/4,1/2),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-1, 1))
save_fig(type = pdf, name = "pacf-arma22.pdf")

y =  ARMAacf(ar = c(1/9,1/9),ma=c(1/4,-1/2),lag.max = 15, pacf=TRUE)
qplot(x = 1:15, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="", color=I("red")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(-1, 1))
save_fig(type = pdf, name = "pacf-arma22-1.pdf")



#ACF vs PACF for order 1 processes
y =  ARMAacf(ar = 0.8,lag.max = 5, pacf=TRUE)
qplot(x = 1:5, ymin = 0, ymax = y, geom = "linerange", size=I(2),xlab="") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
save_fig(type = pdf, name = "acf-ma1.pdf")



plot(elec)
save_fig(pdf, "elec.pdf")
plot(log(elec), ylab="Transformed electricity demand",
     xlab="Year", main="Transformed monthly electricity demand")
title(main="Log",line=-1)
save_fig(pdf, "elec-log.pdf")


plot(BoxCox(elec,lambda))
save_fig(pdf, "elec-box-cox.pdf")

##### Constant #######
n=100
x = rep(1,n)
w = rnorm(n)
c = 0
phi = 0.6
for (i in 2:n)
{
  x[i] = c + phi*x[i-1] + w[i]
}
plot(x, type='l')
plot(diffinv(x,lag = 2), type='l')
y = diffinv(x,lag = 2)
aa = auto.arima(y)
print(aa)
