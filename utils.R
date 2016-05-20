library(ggplot2)
#global parameters for saving slide figures
if (.Platform$OS.type == "windows") path="~/../Dropbox/teaching/time-series/slides/"
if (.Platform$OS.type == "unix") path="~/Dropbox/teaching/time-series/slides/"
img_w = 500
img_h = 750/2
pdf_w = 10.02
pdf_h = 7.27
par.def = par()
# Set to true if need plots to be saved inot files (used for slides preparation)
gen_slide_plot = T

# other global varibles
# big_font <- theme_grey(base_size =  24)

# saves current  contents of the current device to a file if gen_slide_plot is ste to TRUE
save_fig = function(type=pdf, name, lwidth = pdf_w, lheight = pdf_h, lgen_slide_plot = gen_slide_plot)
{
  if (lgen_slide_plot) {
    dev.copy(pdf,paste(path,"fig/", name,sep=""), width = lwidth, height = lheight); 
    dev.off();
  }
}

save_pdf = function(name, lwidth = pdf_w, lheight = pdf_h, lgen_slide_plot =gen_slide_plot)
{
  save_fig(pdf, paste(name,".pdf", sep=""), lwidth, lheight, lgen_slide_plot)
}

fortify.ts <- function(x){
  time <- as.numeric(time(x))
  if(is.matrix(x)){
    df <- as.data.frame(x)
  }
  else {
    x <- as.numeric(x)
    df <- as.data.frame(x)
  }
  df$time <- time
  return(df)
}

fortify.acf <- function(acf){
  n = length(acf$acf)
  range = 1:n
  if (acf$type=="correlation")
    range=2:n
  type <- switch(acf$type, correlation = "ACF", covariance = "ACF (cov)", 
                 partial = "Partial ACF")
  data.frame(acf = acf$acf[range], lag =  acf$lag[range], type = type, n.used = acf$n.used)
}

acf_plot <- function(df, ...){
  clim <- qnorm((1 + 0.975)/2)/sqrt(df$n.used)
  qplot(lag, ymin = 0, ymax = acf, data = df, geom = "linerange") + 
    facet_grid(type ~ .) +
    geom_hline(yintercept = 0, colour = "grey50", size = 0.5) +
    geom_hline(yintercept = c(-clim, clim), linetype = "dashed", colour = "grey50", size = 0.5)
}

examine_corr <- function(x, ...){
  acfs <- rbind(fortify.acf(acf(x, plot = FALSE,...)), fortify.acf(pacf(x, plot = FALSE,...)))
  print(acf_plot(acfs))
  invisible(acfs)
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
