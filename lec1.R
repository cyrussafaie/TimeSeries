# if (.Platform$OS.type == "windows") setwd("~/../Dropbox/teaching/time-series/src")
# source('./utils.R')
source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))

# big labels for lecture slides only
theme_set(theme_grey(base_size=18))

options(stringsAsFactors = FALSE)

# === Temperature in Chicago === #
# a function to get a years worth of daily weather data for ORD (O'Hare airport)
get_year <- function(year){       
  read.csv(url(paste("http://www.wunderground.com/history/airport/ORD/", 
    year, "/1/1/CustomHistory.html?dayend=31&monthend=12&yearend=", year,
    "&format=1", sep = "")), stringsAsFactors = FALSE)
}
get_day <- function(day, month, year){
  read.csv(url(paste("http://www.wunderground.com/history/airport/ORD",
    year, month, day, "DailyHistory.html?format=1", sep = "/")))
}

# get 14 years of data
chi <- ldply(2000:2013, get_year, .progress = "text")
# safest way to rename a column in  data.frame
chi <- rename(chi, c("Mean.TemperatureF" = "temp", "PrecipitationIn" = "precip"))
# save to put on the dropbox so it can be used later
# saveRDS(chi,'~/chicago.rds')
# write.csv(chi, "~/chicago.csv", row.names = FALSE)

# clean up our data so that is has all the dates and is in order.
# keep only those columns that will be used for analysis
chi <- chi[ , c("CST", "temp", "precip")]
# add date column
chi$date <- ymd(chi$CST)

# ASIDE: some missing days? which ones?
all_days <- seq(ymd("2000/01/01"), ymd("2013/12/31"),  by = "day")
all_days[!(all_days %in% chi$date)]

# clean up our data so that is has all the dates and is in order.
all_days <- data.frame(date = seq(ymd("2000/01/01"), ymd("2013/12/31"),  by = "day"))
# merge them together
chi <- join(chi, all_days, type = "full")

# another way to see which days are missing
chi[is.na(chi$temp),]

all_days <- seq(ymd("2000/01/01"), ymd("2011/12/31"),  by = "day")


# add years, months and ydays
chi$year <- year(chi$date)
chi$month <- month(chi$date)
chi$yday <- yday(chi$date)

# force it to be in order
chi <- chi[order(chi$date), ]
head(chi)

# save to put on the dropbox so it can be used later
# saveRDS(chi,'~/chi.rds')

# some strings to practice converting 
A <- "1-7-14"
B <- "Jan 7 14"
C <- "1:15 AM 2014-01-07"
D <- "3:25 PM"
E <- "Tue Jan 7 2014"
# parse_date_time(C,"hmp ymd")
# parse_date_time(B,"bdy")
# parse_date_time("Tue Jan 7 2014","bdy")

# === Google stock price (using quantmod) === #
getSymbols("AAPL",src='google')
ticker = AAPL
# calculate log-return time series
aapl=log(OpOp(ticker)+1)

# === Load Google Trend Data === #
gt=read.csv(url("https://www.dropbox.com/s/6ihlh3sq8vvg127/google-trend.csv?dl=1"),skip=4, nrows=599, stringsAsFactors=F)
a = strsplit(gt$Week, split = ' - ')
a = unlist(a)


qplot(date, temp, data = chi, geom = "jitter") +
  xlab("Year") +
  ylab("Mean Temperature F")
save_fig(pdf, name = "chicago.pdf")


plot(gtemp, lwd=3, type='o',pch='*', col=3, ylab="Temperature Anomaly")
save_fig(pdf, name = "gtemp.pdf")

# a peice of code to take a ts object and make it a data.frame
gtemp.df <- fortify(gtemp)
qplot(time, x, data = gtemp.df, geom ="line") + 
  ylab("Temperature Anomoly") +
  xlab("Year")
  
  # === Johnson & Johnson quarterly earnings per share === #
# jj from S&S 1.?
jj.df <- fortify(jj)
qplot(time, x, data = jj.df, geom ="line") + 
  ylab("Earnings per share")  +
  xlab("Year")
plot(jj,lwd=3, type='o',pch='*', col=3, ylab="Earning per Share")
save_fig(pdf, name = "jj.pdf")

  # === Google earnings per share === #
plot(goog,main="Log return for GOOGL", ylab="Log(r) [$]", cex.lab=1.5, cex.main=1.5)
save_fig(pdf, name = "googl_log_return.pdf")

y = as.double(goog)
dates = index(goog)

goog = Op(ticker)
y = as.double(goog)
ndays = length(goog)
plot(y[1:(ndays-1)], y[2:ndays])
plot(as.double(goog)[0:100],main="", ylab="Price",  type="o")
save_fig(pdf, name = "googl_open100.pdf")

plot(233 + cumsum(rnorm(sd=7.64,mean = 0,n=100)),main="", ylab="Price",  type="o")
save_fig(pdf, name = "googl_open_fake100.pdf")

plot(goog,main="GOOGL", ylab="Open Price [$]", cex.lab=1.5, cex.main=1.5)
save_fig(pdf, name = "googl_open.pdf")

hist(goog, breaks = 50, xlab="Open Price [$]", cex.lab=1.5, cex.main=1.5, main="GOOGL")
save_fig(pdf, name = "googl_open_hist.pdf")


# === an explosion and an earthquake === #
EQ5.df <- fortify(EQ5)
EQ5.df$type <- "earthquake"
EXP6.df <- fortify(EXP6)
EXP6.df$type <- "explosion"
qplot(time, x, data = rbind(EQ5.df, EXP6.df), geom = "line") + 
  facet_grid( type ~ .) +
  ylab("Displacement") +
  xlab("Time (secs?)")
save_fig(pdf, name = "explosion-earthquake.pdf")

# === combination of dumped sinus components === #
t = seq(from = 1,to = 200, by=0.2)
d1 = exp(-t/100)*sin(2*t)
d2 = exp(-t/200)*sin(1*t)
d3 = exp(-t/100)*sin(0.5*t)
d4 = exp(-t/500)*sin(0.2*t)
d5 = exp(-t/600)*sin(0.3*t)
d = d1 + d2 + d3 + d4 + d5
par.default = par()
par(mfrow=c(5,1), mar = c(0,0,0,0))
plot(t,d1, type='l')
plot(t,d2, type='l')
plot(t,d3, type='l')
plot(t,d4, type='l')
plot(t,d5, type='l')
save_fig(pdf, name = "earthquake-components.pdf")
par(par.default)
par(mar = c(4,4,0,0), mai = c(1,1,0.5,0.5))
plot(t,d, type='l', xlab='t', ylab='data', lwd=2, col=3)
save_fig(pdf, name = "earthquake.pdf")
plot(t[1:250],d[1:250], type='l', xlab='t', ylab='data', lwd=2, col=3)
save_fig(pdf, name = "earthquake-short.pdf")

# === random walk === #
n=100
y1 <- cumsum(sample(c(-1, 1), n, TRUE))
y2 <- cumsum(sample(c(-1, 1), n, TRUE))
y3 <- cumsum(sample(c(-1, 1), n, TRUE))
y4 <- cumsum(sample(c(-1, 1), n, TRUE))
ymax = max(c(y1,y2,y3,y4))
ymin=  min(c(y1,y2,y3,y4))
plot(y1,type='o', main="Random Walk", ylab = "y", xlab="time", col=1, ylim=c(ymin,ymax))
lines(y2,type="o", col=2)
lines(y3,type="o", col=3)
lines(y4,type="o", col=4)
save_fig(pdf, name = "random-walk4.pdf")


# === seasonal model === #
t = 1:100
s = sin(2*t)
plot(2 + 0.1*t + s + rnorm(n = 100,0,0.5),type='o', xlab='time', ylab='data', col=3)
save_fig(pdf, name = "trend-seasonal.pdf")

plot(2 + 0.1*t  + rnorm(n = 100,0,1),type='p', xlab='time', ylab='data', col=3)
abline(a = 2,b = 0.1,col=2,lwd=2)
save_fig(pdf, name = "trend.pdf")

plot( rnorm(n = 100,0,1),type='o', xlab='time', ylab='data', col=3)
save_fig(pdf, name = "trend-residual.pdf")

# === ACF theoretical vs sample === #
#ma
ma.sim = arima.sim(model = list(ma=c(0.7)),n=200)
plot(ma.sim,col=2,type='o')
save_fig(pdf, name = "ma-ts.pdf")
d = c(1,0.7,0,0,0,0)
plot(x=0:5,y=d,type='h', xlab="Lag", ylab="ACF",lwd=3, ylim=c(-0.2,1.1))
lines(x=0:5,d,type='p')
acf<-acf(ma.sim ,type="correlation",lag.max = 60, plot = F)
lines(acf$lag,acf$acf,type='h')
lines(acf$lag,acf$acf,type='p', col=2, cex=2)
legend("topright", legend=c("Theoreticsl", "Sample"), pch = c(1,1), col=c(1,2))
save_fig(pdf, name = "ma-acf.pdf")

#ar
ar.sim = arima.sim(model = list(ar=c(0.7)),n=100)
plot(ma.sim,col=2,type='o')
save_fig(pdf, name = "ar-ts.pdf")
dev.copy(pdf,paste(path,"ar-ts.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
d = c(1,0.7,0.7^2,0.7^3,0.7^4,0.7^5)
plot(x=0:5,y=d,type='h', xlab="Lag", ylab="ACF",lwd=3, ylim = c(-0.2,1))
lines(x=0:5,d,type='p')
acf<-acf(ar.sim ,type="correlation",lag.max = 60, plot = F)
lines(acf$lag,acf$acf,type='h')
lines(acf$lag,acf$acf,type='p', col=2, cex=2)
legend("topright", legend=c("Theoreticsl", "Sample"), pch = c(1,1), col=c(1,2))
save_fig(pdf, name = "ar-acf.pdf")
