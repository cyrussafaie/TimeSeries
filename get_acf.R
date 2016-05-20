fortify.acf <- function(acf){
  type <- switch(acf$type, correlation = "ACF", covariance = "ACF (cov)", 
         partial = "Partial ACF")
  data.frame(acf = acf$acf, lag =  acf$lag, type = type, n.used = acf$n.used)
}

acf_plot <- function(df, ...){
  clim <- qnorm((1 + 0.975)/2)/sqrt(df$n.used)
  qplot(lag, ymin = 0, ymax = acf, data = df, geom = "linerange") + 
    facet_grid(type ~ .) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5) +
    geom_hline(yintercept = c(-clim, clim), linetype = "dashed", colour = "grey50", size = 0.5)
}

examine_corr <- function(x, ...){
  acfs <- rbind(fortify(acf(x, plot = FALSE,...)), fortify(pacf(x, plot = FALSE,...)))
  print(acf_plot(acfs))
  invisible(acfs)
}
