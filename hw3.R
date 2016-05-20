source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))

yt <- ts(numeric(100))
e <- rnorm(100)
for(i in 3:100)
  yt[i] <- 0.8 * yt[i-1] + 0.3 * yt[i-2] + e[i]


ar4 <- function(i) {
  x1 <- arima.sim(list(order=c(2,0,0), ar=c(.8,-.2)), n = 1000)
  x2 <- acf(x1,plot=FALSE)
  x2$acf[2]
}

result1k <- sapply(1:1000,ar4)

ACF <- ARMAacf(ar=c(.8,-.2), ma=0, 10)
t.test(result1k,mu=ACF[2])


d = read.csv(url("https://www.dropbox.com/s/4rof7jkakflmvoo/dat4a.txt?dl=1")); examine_corr(d); auto.arima(d)
d = read.csv(url("https://www.dropbox.com/s/nqbk9mopq6o1nxa/dat4b.txt?dl=1")); examine_corr(d); auto.arima(d)
d = read.csv(url("https://www.dropbox.com/s/wsae9mlanpb9zsx/dat4c.txt?dl=1")); examine_corr(d); auto.arima(d)
d = read.csv(url("https://www.dropbox.com/s/ryrsm0eaxodmjz5/dat4d.txt?dl=1")); examine_corr(d); auto.arima(d)
d = read.csv(url("https://www.dropbox.com/s/4zr4c780ttms4ui/dat4e.txt?dl=1")); examine_corr(d); auto.arima(d)


library(fpp)
examine_corr(wmurders)
examine_corr(diff(wmurders))
examine_corr(diff(wmurders,differences = 2))
examine_corr(austourists)
examine_corr(diff(austourists, lag = 4))
auto.arima(wmurders)

auto.arima(austourists)


auto.arima(usnetelec)
examine_corr(diff(usnetelec,differences = 2))
auto.arima(usgdp)
plot(diff(usgdp,differences = 2))
lambda  =  BoxCox.lambda(usgdp)
new.usgdp = BoxCox(usgdp,lambda)
auto.arima(new.usgdp)

auto.arima(mcopper)
auto.arima(enplanements)
plot(enplanements)
plot(log(enplanements))
lambda_d <- BoxCox.lambda(enplanements)
bc_d <- BoxCox(enplanements,lambda_d)
plot(bc_d)
auto.arima(visitors)

