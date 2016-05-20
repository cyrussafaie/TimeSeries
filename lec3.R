source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))


######################################################
#####################Estimation#######################
######################################################
load(url("https://www.dropbox.com/s/67mdli6vera39w8/chi.rda?dl=1"))
# fit the seasonal model in preparation for subtraction
lo_fit <- loess(temp ~ yday, data = chi, na.action = na.exclude)
chi$seasonal_smooth <- fitted(lo_fit)
# subtract off pattern
chi$deseasonalised <- chi$temp - chi$seasonal_smooth

# fit the trend model in preparation for subtraction
lo_fit_trend <- loess(deseasonalised ~ as.numeric(date), data = chi, na.action = na.exclude,
                      span = 0.4)
chi$trend_smooth <- fitted(lo_fit_trend)
chi$residual <- chi$deseasonalised - chi$trend_smooth

d = acf(chi$residual, na.action  =na.pass, lag.max=10, plot = F)
plot(d,type='h', xlab="Lag", ylab="ACF",lwd=3)
lines(d$lag,d$acf,type='p')
save_fig(type = pdf, name = "acf-chi-residual.pdf")

d = pacf(chi$residual, na.action  =na.pass, lag.max=10, plot = F)
plot(d,type='h', xlab="Lag", ylab="PACF",lwd=3)
lines(d$lag,d$acf,type='p')
save_fig(type = pdf, name = "pacf-chi-residual.pdf")

