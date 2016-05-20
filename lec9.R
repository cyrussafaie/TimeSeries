source(url("https://www.dropbox.com/s/hjfrdv8ljnn6sc4/utils.R?dl=1"))
DD <- 1:10
PP <- c(.01,.12,.13,.14,.2,.2,.1,.05,.04,.01)
plot(DD,PP,type="h",col=2,main="Pmf from user list",xlab="x",ylab="p(x)")
save_pdf("pmf")
r1 = rnorm(mean=0,    n=200)
r2 = rnorm(mean=10,   n=200)
plot(density(r1+r2))
