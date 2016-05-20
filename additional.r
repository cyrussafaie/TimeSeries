acf<-acf(soi,type="correlation",lag.max = 60)
plot(acf,type='h', xlab="Lag", ylab="ACF",lwd=3)
lines(acf$lag,acf$acf,type='p')
dev.copy(pdf,paste(path,"soi-acf.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()


pacf<-pacf(soi)
plot(pacf,type='h', xlab="Lag", ylab="ACF",lwd=3)
lines(pacf$lag,pacf$acf,type='p')
dev.copy(pdf,paste(path,"soi-pacf.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

plot(c(1,2,3),type='o', xlab="", ylab="", xlim=c(1,24), ylim=c(0.6, 3.4), lwd=2, col=2)
lines(c(5,6,7),c(1,3,2), type='o', lwd=2, col=2)
lines(c(9,10,11),c(2,1,3), type='o', lwd=2, col=2)
lines(c(13,14,15),c(2,3,1), type='o', lwd=2, col=2)
lines(c(17,18,19),c(3,1,2), type='o', lwd=2, col=2)
lines(c(21,22,23),c(3,2,1), type='o', lwd=2, col=2)
dev.copy(pdf,paste(path,"turn-point.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

par(par.def)
par(mar=c(4,4.5,1,1))
nu = seq(from = 0,to = 0.5,by = 0.01)
s2 = 1; phi = 0.9
y = s2/(1-2*phi*cos(2*pi*nu) + phi^2)
plot(nu,y, type='l',xlab=expression(nu),ylab=expression(f(nu)), lwd=3, col=2)
dev.copy(pdf,paste(path,"density-ar.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

s2 = 1; phi = -0.9
y = s2/(1-2*phi*cos(2*pi*nu) + phi^2)
plot(nu,y, type='l',xlab=expression(nu),ylab=expression(f(nu)), lwd=3, col=2)
dev.copy(pdf,paste(path,"density-ar1.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

s2 = 2; th = 0.9;
y = s2*(1+ th^2 + 2*th*cos(2*pi*nu))
plot(nu,y, type='l',xlab=expression(nu),ylab=expression(f(nu)), lwd=3, col=2)
dev.copy(pdf,paste(path,"density-ma.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()

s2 = 2; th = -0.9;
y = s2*(1+ th^2 + 2*th*cos(2*pi*nu))
plot(nu,y, type='l',xlab=expression(nu),ylab=expression(f(nu)), lwd=3, col=2)
dev.copy(pdf,paste(path,"density-ma1.pdf",sep=""), width = pdf_w, height = pdf_h); dev.off()
