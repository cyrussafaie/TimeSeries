library(fpp)
source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))

plot(usconsumption[,1], main="Us Consumption", ylab="Quartalry Change [%]")
save_fig(pdf, "us-consumption.pdf")

fit <- auto.arima(usconsumption[,1],
                  max.P=0,max.Q=0,D=0)

y =  ARMAacf(ar = 0.9, ma = 0.5,lag.max = 25)
qplot(x = 0:25, ymin = 0, ymax = y, geom = "linerange") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylim(c(0, 1))
