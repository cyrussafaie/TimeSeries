library(astsa)
library(ggplot2)
library(plyr)
library(reshape)
library(fpp)
source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))
big_font <- theme_grey(base_size =  24)


cmort.df <- fortify.ts(cmort)
cmort.df <- rename(cmort.df, c("x"= "mortality"))
tempr.df <- fortify.ts(tempr)
tempr.df <- rename(tempr.df, c("x" =  "temp"))
part.df <- fortify.ts(part)
part.df <- rename(part.df, c("x"= "part"))

mort <- join(join(cmort.df, tempr.df), part.df)

qplot(time, value, data = melt(mort, id.vars = "time"), geom = "line") +
  facet_grid(variable ~ ., scale = "free")
save_fig(pdf,"mort.pdf")

mort <- mutate(mort, temp_sc = temp - mean(temp),
               temp_2 = temp_sc^2,
               time0 = time - min(time))


p1=qplot(temp, mortality, data = mort)
p2=qplot(part, mortality, data = mort)
p3=qplot(temp, part, data = mort)
multiplot(p1, p2, cols=2)
save_fig(pdf,"mort-scatter.pdf")

p1=qplot(temp, mortality, data = mort) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, colour = "red") +
  geom_smooth(se = FALSE)  +big_font

p2=qplot(part, mortality, data = mort) + geom_smooth(se = FALSE) +big_font
multiplot(p1, p2, cols=2)
save_fig(pdf,"mort-scatter-fit.pdf")


#split the data
k = 400
mort_train = mort[1:k,]
mort_test = mort[(k+1):508,]

# fit the modesl
lm_fit    = lm(mortality ~ time0 + temp_sc + temp_2 + part, data = mort_train)
arima_fit = arima(mort_train$mortality, order=c(2,0,0), xreg = mort_train[,c("time0","temp_sc", "temp_2", "part")])
library(nlme)
gls_fit <- gls(mortality ~ time0 + temp_sc + temp_2 + part, data = mort_train,                correlation = corARMA(p = 2), method = "ML")

screenreg(list(lm_fit, arima_fit))

#calculate out-of-sample forecst/prediction
p1=predict(lm_fit,mort_test)
e1=abs(p1-mort_test$mortality)
# p2=predict(gls_fit,mort_test)
# e2 = abs(p2-mort_test$mortality)
p3=forecast(arima_fit,xreg = mort_test[,c("time0","temp_sc", "temp_2", "part")])
e3=abs(as.double(p3[['mean']])-mort_test$mortality)

#compare results
plot(mort_test$time, e1, ylab = "MAE", xlab="Time", type='l', ylim=c(min(e3),max(e3)))
# lines(mort_test$time, e2, col=2)
lines(mort_test$time, e3, col='green')
save_fig(pdf,"mort-lm-arima-cv.pdf")
sum(e1)
# sum(e2)
sum(e3)



plot(abs(fitted(lm_fit)-mort_train$mortality), col=1, type='l')
lines(abs(fitted(arima_fit)-mort_train$mortality), col=3)


summary(lm_fit)





# assumptions
# residuals versus covariates
mort_lm <- fortify(lm_fit)
qplot(time0, .resid, data = mort_lm, geom = "line")
save_fig(pdf,"mort-res-time.pdf")
qplot(temp_sc, .resid, data = mort_lm)
qplot(part, .resid, data = mort_lm)

# residuals versus fitted
qplot(.fitted, .resid, data = mort_lm)
save_fig(pdf,"mort-res-fit.pdf")

# normality of residuals
qqnorm(mort_lm$.resid)
qqline(mort_lm$.resid)
save_fig(pdf,"mort-res-qq.pdf")

# correlation of residuals
examine_corr(residuals(lm_fit)) 
last_plot() + big_font
save_fig(pdf,"mort-res-corr.pdf")
# AR (2)? violates regression assumptions


arima_fit

p1=predict(lm_fit,mort_test)
e1=abs(p1-mort_test$mortality)
# p2=predict(gls_fit,mort_test)
# e2 = abs(p2-mort_test$mortality)
p3=forecast(arima_fit,h=58, xreg = mort_test[,c("time0","temp_sc", "temp_2", "part")])
e3=abs(as.double(p3[['mean']])-mort_test$mortality)

plot(mort_test$time, e1, ylab = "MAE", type='l', ylim=c(min(e3),max(e3)))
# lines(mort_test$time, e2, col=2)
lines(mort_test$time, e3, col=3)
sum(e1)
# sum(e2)
sum(e3)

# diagnostics
mort_train$residuals <- residuals(gls_fit, type = "normalized")
mort_train$fitted <- fitted(gls_fit)
examine_corr(mort_train$residuals)
last_plot() + big_font
save_fig(pdf,"mort-res-corr1.pdf")

qplot(fitted, residuals, data = mort_train) + geom_hline(yintercept = 0)+ big_font
save_fig(pdf,"mort-res-fit1.pdf")
qplot(time, residuals, data = mort_train) + geom_hline(yintercept = 0)+ big_font
save_fig(pdf,"mort-res-time1.pdf")
qplot(temp, residuals, data = mort_train) + geom_hline(yintercept = 0)+ big_font
save_fig(pdf,"mort-res-temp1.pdf")
qplot(part, residuals, data = mort_train) + geom_hline(yintercept = 0)+ big_font
save_fig(pdf,"mort-res-part1.pdf")

qqnorm(mort_train$residuals) 
qqline(mort_train$residuals)
save_fig(pdf,"mort-res-qq1.pdf")

confint(arima_fit)
confint(lm_fit)
# intervals(gls_fit)



plot(mort_test$time, mort_test$mortality, type="l", xlab="Time", ylab="Mortality")
lines(mort_test$time, p1, col=2)
lines(mort_test$time,  p3$mean, col=3)
legend("topright", legend=c("Data","LM","ARIMA"), col=1:3, lty=1)
save_fig(pdf,"mort-cv.pdf")
mean(abs(residuals(lm_fit)))
mean(abs(residuals(arima_fit)))


# A plot of the residuals shows that the LM+ARMA model performs better on the training set because it captures the variation in the residuals of the pure LM model there.  However, these forecasts of noise vanish very quickly in the test set.
num.test <- 508 - k
lm_fit.e <- residuals(lm_fit)
plot(mort$time, c(lm_fit.e, rep(NA, num.test)), type="l")
abline(0, 0, col="darkgray")
ar2_fit <- Arima(lm_fit.e, order=c(2,0,0))
lines(mort$time, c(fitted(ar2_fit), forecast(ar2_fit, num.test)$mean), col="orange")



# Consumpiton Data Example
usconsumption.df = fortify(usconsumption)
par(mar = c(1,1,1,1))
plot(usconsumption.df$consumption)
save_fig(pdf,"usconsumption.pdf")
par(par.def)
plot(usconsumption.df$consumption, usconsumption.df$income,xlab="Consumption", ylab="Income")
save_fig(pdf,"usconsumption-scatter.pdf")
fit <- arima(usconsumption[,1],
            xreg=usconsumption[,2],
            order=c(2,0,0))
tsdisplay(arima.errors(fit),
          main="ARIMA errors")
save_fig(pdf,"usconsumption-ar2-error.pdf")
fit2 <- Arima(usconsumption[,1], xreg=usconsumption[,2], order=c(1,0,2))
tsdisplay(arima.errors(fit2), main="ARIMA errors")
Box.test(residuals(fit2), fitdf=5,
         lag=10, type="Ljung")

fit_lm = lm(usconsumption[,1] ~ usconsumption[,2])
screenreg(list(fit, fit2, fit_lm))

Box.test(residuals(fit2), fitdf=5,lag=10, type="Ljung")
fcast <- forecast(fit2,
                  xreg=rep(mean(usconsumption[,2]),8), h=8)
plot(fcast,
     main="Forecasts from regression with
    ARIMA(1,0,2) errors")
save_fig(pdf, "usconsumption-forecast.pdf")

fcast <- forecast(fit2,xreg=rep(mean(usconsumption[,2]),8), h=8)

k=360
st <- tsp(usconsumption)[1]+(k-2)/12 #in years
usconsumption_train <- window(usconsumption, end=st + 1/12)
usconsumption_test <- window(usconsumption, start=st + 2/12)
fit2 <- Arima(usconsumption_train[,1], xreg=usconsumption_train[,2], order=c(1,0,2))
fcast2 <- forecast(fit2,xreg=usconsumption_train[,2])

usconsumption_train.df = usconsumption.df[1:120,]
usconsumption_test.df = usconsumption.df[121:164,]
fit3 = lm(consumption ~ income, data = usconsumption_train.df)
fcast3=predict(fit3,usconsumption_test.df)

e1 = abs(fcast2[["mean"]] - usconsumption_test[,1])
e2 = abs(usconsumption_test.df$consumption-fcast3)
plot(e1)
lines(usconsumption_test.df$time, e2, type='l', col=2)
sum(e1)
sum(e2)

# International vistors to Australia
plot(austa)
save_pdf("austa")

auto.arima(austa,d=0,xreg=1:length(austa))
# ARIMA(2,0,0) with non-zero mean
# Coefficients:
#   ar1      ar2  intercept  1:length(austa)
# 1.0371  -0.3379     0.4173           0.1715
# s.e.  0.1675   0.1797     0.1866           0.0102
# sigma^2 estimated as 0.02486:  log likelihood=12.7
# AIC=-15.4   AICc=-13   BIC=-8.23


auto.arima(austa,d=1)

fit1 <- Arima(austa, order=c(0,1,0), include.drift=TRUE)
fit2 <- Arima(austa, order=c(2,0,0), include.drift=TRUE)
par(mfrow=c(2,1))
plot(forecast(fit2), main="Forecasts from linear trend + AR(2) error",
     ylim=c(1,8))
plot(forecast(fit1), ylim=c(1,8))
save_pdf("austa-forecast")


#fourier seasonality
par(par.def)
fit <- auto.arima(USAccDeaths,
                  xreg=fourier(USAccDeaths, 5),
                  seasonal=FALSE)
fc <- forecast(fit,
               xreg=fourierf(USAccDeaths, 5, 24))
plot(fc)
save_pdf("usa-death")

# tv advertisements and insurance quotations
plot(insurance, main="Insurance advertising and quotations", xlab="Year")
save_pdf("insurance")

# Lagged predictors. Test 0, 1, 2 or 3 lags.
Advert <- cbind(insurance[,2],
                c(NA,insurance[1:39,2]),
                c(NA,NA,insurance[1:38,2]),
                c(NA,NA,NA,insurance[1:37,2]))
colnames(Advert) <- paste("AdLag",0:3,sep="")

# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1], d=0)
fit2 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:2], d=0)
fit3 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:3], d=0)
fit4 <- auto.arima(insurance[4:40,1], xreg=Advert[4:40,1:4], d=0)

# Best model fitted to all data (based on AICc)
# Refit using all data
fit <- auto.arima(insurance[,1], xreg=Advert[,1:2], d=0)
fit

fc <-function(advert)
{ 
  fc <- forecast(fit, xreg=cbind(rep(advert,20),c(Advert[40,1],rep(advert,19))), h=20)
  plot(fc, main=paste("Forecast quotes with advertising set to", advert), ylab="Quotes")
  save_pdf(paste("insurance-forecast",advert, sep='-'))
}
fc(6)
fc(8)
fc(10)
fc(12)



#####Spurious correlation\
n = 100
t = seq(0,n-1)
w1 = rnorm(n)
w2 = rnorm(n)
x1 = 5*cos(2*pi*t/10) + w1
x2 = 10*cos(2*pi*t/10) + w2
par(mfrow=c(1,2))
plot(t,x1, type='l')
lines(t,x2, col=2)
plot(x1,x2)
save_pdf("spurious1")

x2 = 10*cos(2*pi*(t+2)/10) + w2
plot(t,x1, type='l')
lines(t,x2, col=2)
plot(x1,x2)
save_pdf("spurious2")


x1 = rnorm(100)
x2 = rnorm(100)
for (i in 2:n)
{
  x1[i] = 0.5+x1[i-1] + x1[i]
  x2[i] = 1+x2[i-1] + x2[i]
}
plot(t,x1, type='l', ylim=c(0,80))
lines(t,x2, col=2)
plot(x1,x2)
save_pdf("spurious3")


detach(package:TSA)
one_sim <- function(){
  beta =  0.5
  phi_x = 0.8
  phi_z = 0.9
  x <- arima.sim(list(ar=phi_x), n = 100)
  y <- 1 + beta*x + arima.sim(list(ar=phi_z), n = 100)
  fit <- arima(y, order = c(0, 0, 0), xreg = x)
  
  abs( fit$coef["x"] / 
         sqrt(diag(fit$var.coef)["x"]) ) > 1.96
}

reject <- replicate(1000, one_sim())
table(reject)

