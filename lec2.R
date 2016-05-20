# if (.Platform$OS.type == "windows") setwd("~/../Dropbox/teaching/time-series/src")
# source('./utils.R')
source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plyr)) install.packages("plyr")
if (!require(lubridate)) install.packages("lubridate")
if (!require(astsa)) install.packages("astsa")

big_font <- theme_grey(base_size =  24)

# read in the daily weather data
chi = readRDS(gzcon(url("https://www.dropbox.com/s/ijwxrbbgg49w30o/chi.rds?dl=1")))
head(chi)
tail(chi)

# clean up our data so that is has all the dates and is in order.
all_days <- data.frame(date = seq(ymd("2000/01/01"), ymd("2013/12/31"),  by = "day"))
# merge them together
chi <- join(chi, all_days, type = "full")

qplot(date, temp, data = chi, geom = "line")
# save_fig(pdf, name = "chicago.pdf")
# gaps come from missing values "NA"
# straight lines come from ommitted records
qplot(date, temp, data = chi, ,color=month)


#==== aggregation with plyr ====#
last_year <- subset(chi, year == 2013)

a = mutate(last_year, 
       avg_temp = mean(temp, na.rm = TRUE),
       n_temp = sum(!is.na(temp))
);

b = summarise(last_year, avg_temp = mean(temp, na.rm = TRUE), n_temp = sum(!is.na(temp)))

ddply(chi, "year", summarise, avg_temp = mean(temp, na.rm = TRUE))

c = ddply(chi, "year", mutate,avg_temp = mean(temp, na.rm = TRUE))

# summarise to annual level data
ann_temp <- ddply(chi, "year", summarise,
                  avg_temp = mean(temp, na.rm = TRUE),
                  year = year[1],
                  n = sum(!is.na(temp)))

qplot(year, avg_temp, 
      data = ann_temp, geom = "line")
# but we lose all sense of scale

# An alternative, rather than summarise, mutate
chi <- ddply(chi, "year", mutate,
             avg_temp = mean(temp, na.rm = TRUE),
             year = year[1],
             n = sum(!is.na(temp)))

chi <- ddply(chi, "month", mutate,
             avg_temp_month = mean(temp, na.rm = TRUE),
             n = sum(!is.na(temp)))

qplot(date, avg_temp_month, 
      data = chi, geom = "line", 
      size = I(2), colour = I("blue")) +
  geom_line(aes(y = temp))

# adding some labels etc.
qplot(date, avg_temp, 
      data = chi, geom = "line", 
      size = I(2), colour = I("blue")) +
  geom_line(aes(y = temp)) +
  xlab("Date") + ylab("Temperature (F)") +
  ggtitle("Chicago (ORD) Daily Average Temperature") 
# dev.copy(pdf,paste(path,"chi-avg-temp.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()  

# == Smooth == #

# the ma function expects a time series object so we first need to 
chi_ts <- ts(chi$temp, start = 2000, freq = 365.25)

# n is number of time points to average
n <- 7 # we have daily data so this is a week
chi$weekly_ma <- filter(chi_ts, filter = rep(1, n)/n)
n <- 30 # approximately a month
chi$monthly_ma <- filter(chi_ts, filter = c(1/2, rep(1, n-1), 1/2)/n)
n <- 365 # approximately a year
chi$annual_ma <- filter(chi_ts, filter = rep(1, n)/n)

qplot(date, temp, data = chi, geom = "line", alpha = I(0.5)) +
  geom_line(aes(y = weekly_ma, colour = "weekly_ma"), size = 1) +
  geom_line(aes(y = monthly_ma, colour = "monthly_ma"), size = 1) +
  geom_line(aes(y = annual_ma, colour = "annual_ma"), size = 1) +
  scale_colour_brewer("Moving average", type = "qual", palette = 2) +
  big_font
# dev.copy(pdf,paste(path,"filter.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
# missing data propagates

last_plot() + xlim(ymd("2011-01-01", "2011-12-31"))
# dev.copy(pdf,paste(path,"filter_zoom.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
# add a straight line
qplot(date, temp, data = chi, geom = "line") +
  geom_smooth(method = "lm")
# save_fig(pdf, name = "smooth-lm.pdf")

# add a locally weighted regression line (loess) 
qplot(date, temp, data = chi, geom = "line") +
  geom_smooth(method = "loess")
# dev.copy(pdf,paste(path,"smooth-lm.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off() 

# change span of loess line
qplot(date, temp, data = chi, geom = "line") +
  geom_smooth(method = "loess")
# dev.copy(pdf,paste(path,"smooth-loess.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off() 

qplot(date, temp, data = chi, geom = "line") +
  geom_smooth(method = "loess", span = .1)
# dev.copy(pdf,paste(path,"smooth-loess1.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()


# == Subtract == #
# calculate monthly means
month_fit <- lm(temp ~ factor(month), data = chi, na.action = na.exclude)
chi$month_avg <- predict(month_fit)
chi$res <- residuals(month_fit)

qplot(date, month_avg, data = chi, geom = "line", group = year)
qplot(date, res, data = chi, geom = "line") +
  geom_smooth(method = "loess", span = 0.2)
# dev.copy(pdf,paste(path,"substract.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

# === Removing seasonal pattern ex. 2 === #
qplot(yday, temp, data = chi, geom = "line", group = year, alpha = I(.3)) +
  geom_smooth(method = "loess", aes(group = 1), size = 1) +
  big_font
# dev.copy(pdf,paste(path,"chi-seasonal.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
seas_fit <- loess(temp ~ yday, data = chi, na.action = na.exclude)
chi$seas_fit <- predict(seas_fit, newdata = chi)
chi$deseas <- residuals(seas_fit)

# what did the fit look like
qplot(yday, seas_fit, data = chi, geom = "line")
# dev.copy(pdf,paste(path,"seasonal-fit.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

qplot(date, deseas, data = chi, geom = "line") +
  geom_smooth(method = "loess", span = 0.2) +
  big_font
# dev.copy(pdf,paste(path,"seasonal-fit1.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
# === automatic approaches === #
# just for demonstration fill in missings with seasonal smooth
temp_nomiss <- chi$temp
temp_nomiss[is.na(temp_nomiss)] <- chi$seas_fit[is.na(temp_nomiss)]
chi_ts2 <- ts(temp_nomiss, start = 2000, freq = 365.25)

plot(stl(chi_ts2, 365.25))
# dev.copy(pdf,paste(path,"stl-chi.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
plot(decompose(chi_ts2))
# dev.copy(pdf,paste(path,"decompose-chi.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()


# === JJ Return === #
jj.df <- fortify(jj)
qplot(time, x, data = jj.df, geom ="line") 
qplot(time, log(x), data = jj.df, geom ="line") +
  ylab("log (Earnings per share)")

# a quadratic trend
qplot(time, x, data = jj.df, geom ="line") +
  ylab("Earnings per share")  +
  xlab("Year") + 
  geom_smooth(method = "lm", formula = y ~ x + I(x^2))

jj_trend <- loess(x ~ time, data = jj.df, na.action = na.exclude)

jj.df$trend <- predict(jj_trend, newdata = jj.df)
jj.df$de_trend <- residuals(jj_trend)


# === Linear Trend === #
plot(gtemp,type='o', ylab="Global Temperature Deviations", lwd=2, col=3)
save_fig(pdf, name = "global-temp.pdf")
fit = lm(gtemp~time(gtemp))
abline(fit, col=2, lwd=2)
save_fig(pdf, name = "global-temp-trend.pdf")
qqnorm(fit$residuals, main="")
qqline(fit$residuals)
save_fig(pdf, name = "global-temp-qq.pdf")
plot(fit$residuals, main="")
abline(0,0)
save_fig(pdf, name = "global-temp-res.pdf")
plot(density(fit$residuals), main="Empirical Density for Residuals")
save_fig(pdf, name = "global-temp-res-dens.pdf")
n = length(fit$residuals)
plot(fit$residuals[1:129],fit$residuals[2:130], main="")
# save_fig(pdf, name = "global-temp-res-corr.pdf")


# other plots 
# simple histogram
qplot(temp, data = chi)
# change the binwidth
qplot(temp, data = chi, binwidth = 1)
qplot(temp, data = chi, breaks=seq(10, 100, by=1))
# density plot instead
qplot(temp, data = chi, geom = "density")
# smaller bandwidth
qplot(temp, data = chi, geom = "density", adjust = 0.25, fill = I("grey20"))

#save(chi,file="~/chi.rda")