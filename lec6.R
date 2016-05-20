source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))
library(fpp)
data(oil, package = "fpp")
oildata <- window(oil,start=1996,end=2007)
plot(oildata, ylab="Oil (millions of tonnes)",xlab="Year")
save_fig(pdf, name = "oil-prices.pdf")

al = 0.8
plot(c(al,al^2,al^3,al^4,al^5,al^6, al^7, al^8), type='l', ylab=expression((1-alpha)^j),xlab="j",col=3,lwd=3)
save_fig(pdf, name = "smoothing-parameter.pdf")

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)
fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)
fit3 <- ses(oildata, h=3)
plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")
lines(fitted(fit1), col="blue", type="o")
lines(fitted(fit2), col="red", type="o")
lines(fitted(fit3), col="green", type="o")
lines(fit1$mean, col="blue", type="o")
lines(fit2$mean, col="red", type="o")
lines(fit3$mean, col="green", type="o")
legend("topleft",lty=1, col=c(1,"blue","red","green"), 
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1,bty = "n")
save_fig(pdf, name = "oil-smoothing.pdf")

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