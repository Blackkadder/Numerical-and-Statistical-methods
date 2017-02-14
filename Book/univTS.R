###########  R script for Chapter 12   ###################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

library(Ecdat)

##################################################
############  Code for Figure 12.1  ##############
##################################################

data(Mishkin,package="Ecdat")
head(Mishkin) # pai1 = one-month inflation rate (in percent, annual rate) 
y = as.ts(Mishkin[,1], start=1950, frequency=12) 
y = ts(as.vector(Mishkin[,1]), start=1950, frequency=12)  

pdf("inflation.pdf", width=7, height=6)
#
par(mfrow=c(2,1))
plot(y,ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(a)")
plot(diff(y),ylab="Change in Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.2,main="(b)")
#
graphics.off()


##################################################
############  Code for Figure 12.2  ##############
##################################################

data(AirPassengers) # monthly total international airline passengers
z = as.ts(AirPassengers, start=1949, frequency=12) 

pdf("airpass.pdf", width=7, height=6)
#
plot(z,type="b",ylab="Passengers",cex.axis=1.5,cex.lab=1.5,cex=1.5,lwd=2)
#
graphics.off()


##################################################
############  Code for Figure 12.3  ##############
##################################################

y = as.vector(Mishkin[,1]) 

pdf("inflation_acf.pdf", width=8, height=4)
#
par(mfrow=c(1,2))
acf(y,cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="(a)")
acf(diff(y),cex.axis=1.5,cex.lab=1.5,cex.main=1.2,main="(b)")
#
graphics.off()

Box.test(diff(y), lag=10, type="Ljung-Box")


##################################################
############  Code for Example 12.3  #############
##################################################

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 
par(mfrow=c(1,2))
acf(y)
acf(diff(y))
Box.test(diff(y), lag = 10, type = "Ljung-Box")


##################################################
############  Code for Figure 12.4  ##############
##################################################

phi = c(0.95, 0.75, 0.2, -0.9)

pdf("AR1Corr.pdf", width=7, height=7)
#
par(mfrow=c(2,2))
for(i in 1:4){
  y = phi[i]^(0:15)
  plot(0:15, y, xlab="h", ylab=expression(rho(h)), ylim=c(-1,1), type = 'l')
  points(0:15, y, pch = 8)
  text(10, -0.85, eval(substitute(expression(paste(phi," = ",j)), list(j = as.character(phi[i])))), cex = 1.1)
  abline(h=0)     
}
#
graphics.off()


##################################################
############  Code for Figure 12.5  ##############
##################################################

set.seed(8716)
e = rnorm(200)
x1 = x2 = x3 = x4 = e
for (t in 2:200){
  x1[t] = 0.98*x1[t-1]+e[t]
  x2[t] = -0.6*x2[t-1]+e[t]
  x3[t] = 1.00*x3[t-1]+e[t]
  x4[t] = 1.01*x4[t-1]+e[t]
}

pdf("ar_sim200.pdf", width=7, height=7)
#
par(mfrow=c(2,2),cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(x1,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(phi," = 0.98")))
plot(x2,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(phi == - 0.6)))
plot(x3,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(phi," = 1")))
plot(x4,type="l",xlab="Time (t)",ylab=expression(Y[t]),
     main=expression(paste(phi," = 1.01")))
#
graphics.off()


##################################################
############  Code for Figure 12.6  ##############
##################################################

library(xts)
data(bmw,package="evir")
BMW = xts(bmw, attr(bmw,"times"))

pdf("BMW_acf.pdf",width=7,height=7)
#
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(BMW,main="(a)", minor.ticks = FALSE)
acf(bmw,lag.max=20,main="(b)")
qqnorm(bmw,main="(c)") ; qqline(bmw)
#
graphics.off()

##################################################
############  Code for Example 12.4  #############
##################################################

options(digits=9)

data(bmw, package = "evir")
Box.test(bmw, lag = 5, type = "Ljung-Box")

fitAR1 = arima(bmw, order = c(1,0,0))
print(fitAR1)

Box.test(residuals(fitAR1), lag = 5, type = "Ljung-Box", fitdf = 1)

Box.test(residuals(fitAR1), lag = 10, type = "Ljung-Box", fitdf = 1)
Box.test(residuals(fitAR1), lag = 15, type = "Ljung-Box", fitdf = 1)
Box.test(residuals(fitAR1), lag = 20, type = "Ljung-Box", fitdf = 1)


##################################################
############  Code for Figure 12.7  ##############
##################################################

pdf("BMW_resid_acf.pdf",width=7,height=7)
#
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
plot(xts(residuals(fitAR1), attr(bmw,"times")),main="(a)", minor.ticks = FALSE)
acf(residuals(fitAR1),lag.max=20,main="(b)")
qqnorm(residuals(fitAR1),main="(c)") ; qqline(residuals(fitAR1))
#
graphics.off()


##################################################
############  Code for Example 12.5  #############
##################################################

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 
fit = arima(y, order = c(1,0,0))
Box.test(fit$resid, type = "Ljung", lag = 24, fitdf = 1)


##################################################
############  Code for Figure 12.8  ##############
##################################################

pdf("inflation_AR1_acf.pdf",width=9,height=5)
#
par(mfrow=c(1,2))
acf(y,main="Inflation rate")
acf(fit$resid,main="Residuals from AR(1)")
#
graphics.off()


##################################################
############  Code for Figure 12.9  ##############
##################################################

x1 = as.vector(ARMAacf(ar=c(0.5,-0.3), lag.max=10))
x2 = as.vector(ARMAacf(ar=c(0.5,0.15), lag.max=10))
x3 = as.vector(ARMAacf(ar=c(0.15,0.8), lag.max=10))

pdf("ar2acf.pdf",width=8,height=6)
#
par(mfrow=c(1,1))
plot(0:10,x1,xlab="lag",ylab="ACF", main= "ACF of three AR(2) processes",cex.axis=1.5,
     cex.lab=1.5,cex=2,cex.main=1.5,pch="*",type="b",ylim=c(-.5,1))
lines(0:10,x2,cex.axis=1.5, cex.lab=1.5,cex=2,pch="o",type="b")
lines(0:10,x3,cex.axis=1.5, cex.lab=1.5,cex=2,pch="x",type="b")

abline(h=0)
legend(6,-.1,c("(0.5, -0.3)", "(0.5, 0.15)","(0.15, 0.8)"), pch=c("*","o","x"), cex=1.5, box.lty=0)
#
graphics.off()


##################################################
############  Code for Figure 12.10  #############
##################################################

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 
x = diff(y)
logn = log(length(x))

#####  Fitting AR(p) models  #####  
resultsdiff = matrix(0,nrow=20,ncol=3)
for (i in 1:20){
  fit = arima(x,order=c(i,0,0))
  resultsdiff[i,1] = i  
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
}

pdf("inflation_diff_arfits.pdf",width=8,height=6)
#
plot(resultsdiff[,1],resultsdiff[,2],xlab="p",ylab="criterion",cex.lab=1.35,cex.axis=1.35,
     main="AIC and BIC for AR fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2440,2560),type='b')
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2,type='b')
legend(12,2565,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
#
graphics.off()


##################################################
############  Code for Example 12.6  #############
##################################################

options(digits=7)

library(forecast)
auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "aic")

auto.arima(diff(y), max.p = 20, max.q = 0, d = 0, ic = "bic")


##################################################
############  Code for Example 12.7  #############
##################################################

options(digits=5)

auto.arima(y, max.p = 20, max.q = 0,  d = 0, ic = "aic")
auto.arima(y, max.p = 20, max.q = 0,  d = 0, ic = "bic")


##################################################
############  Code for Figure 12.11  #############
##################################################

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 

pdf("inflationAR7_res_acf.pdf", width=7, height=6)
#
par(mfrow=c(2,2))
plot(ts(y, start=1950, frequency=12),ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(a)")
acf(y,main="(b)")
fitAR7 = arima(y,c(7,0,0))
plot(ts(fitAR7$resid, start=c(1950,2), frequency=12),ylab="Inflation Rate",type="l",xlab="Year",cex.lab=1.5,
     cex.axis=1.5,cex.main=1.3,main="(c)")
acf(fit$resid,main="(d)")
#
graphics.off()


##################################################
############  Code for Figure 12.12  #############
##################################################

data(Mishkin, package = "Ecdat")
y = as.vector(Mishkin[,1]) 
x = diff(y)
logn = log(length(x))

#####  Fitting MA(q) models  #####  
resultsdiff = matrix(0,nrow=9,ncol=3)
for (i in 1:9){
  fit = arima(x,order=c(0,0,i))
  resultsdiff[i,1] = i  
  resultsdiff[i,2] = fit$aic
  resultsdiff[i,3] = resultsdiff[i,2] + (logn-2)*i
}

pdf("inflation_diff_mafits.pdf",width=8,height=6)
#
par(mfrow=c(1,1))
plot(resultsdiff[,1],resultsdiff[,2],xlab="q",ylab="criterion",cex.lab=1.35,cex.axis=1.35,
     main="AIC and BIC for MA fits to changes in inflation rate",
     cex.main=1.35,cex=2,pch="*",ylim=c(2445,2500),type='b')
points(resultsdiff[,1],resultsdiff[,3],pch="o",cex=2,type='b')
legend(1.2,2498,c("AIC","BIC"),pch=c("*","o"),cex=2,box.lty=0)
#
graphics.off()


##################################################
############  Code for Example 12.8  #############
##################################################

fitMA3 = arima(diff(y), order = c(0,0,3))
fitMA3
Box.test(fitMA3$residual, lag = 5, type="Ljung", fitdf = 3)
Box.test(fitMA3$residual, lag = 10, type="Ljung", fitdf = 3)
Box.test(fitMA3$residual, lag = 15, type="Ljung", fitdf = 3)

fitMA2 = arima(diff(y), order = c(0,0,2))
fitMA2
Box.test(fitMA2$residual, lag = 5, type="Ljung", fitdf = 2)
Box.test(fitMA2$residual, lag = 10, type="Ljung", fitdf = 2)
Box.test(fitMA2$residual, lag = 15, type="Ljung", fitdf = 2)


##################################################
############  Code for Example 12.9  #############
############  Code for Table 12.1    #############
##################################################

library("forecast")
data(Capm,package="Ecdat")  
rf=Capm$rf
diffrf=diff(rf)
acf(diffrf)
arima(rf,c(2,1,0))

res = matrix(0,nrow=9,ncol=4)
i = 1
for (p in 0:2)
{
  for (q in 0:2)
  {
    res[i,1] = p
    res[i,2] = q
    fit = arima(diffrf,c(p,0,q))
    res[i,4] = AIC(fit,k=logn) +1290
    res[i,3] = AIC(fit) +1290
    i=i+1
  }
}
options(digits=3)
res 


##################################################
############  Code for Figure 12.13  #############
##################################################

par(mfrow=c(2,1))
acf(diffrf)
bestfit =  arima(diffrf,c(1,0,1))
acf(bestfit$residual)

pdf("diffrf_arma11_residual.pdf",width=7,height=7)
#
layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
acf(bestfit$residual,lag.max=20,main="(a)")
qqnorm(bestfit$resid,datax=T,main="(b)") ; qqline(bestfit$resid,datax=T)
plot(ts(bestfit$resid, start = c(1960,2), frequency = 12),ylab="Residual",main="(c)")
#
graphics.off()


##################################################
############  Code for Figure 12.14  #############
##################################################

set.seed(4631)
y1 = arima.sim(n = 500, list(ar = c(0.4)))
y2 = cumsum(y1)
y3 = cumsum(y2)

pdf("integratedSim.pdf",width=5,height=6)
#
par(mfrow=c(3,1))
plot(y1,type="l",ylab=expression(y[1]),lwd=1,main="(a)")
plot(y2,type="l",xlab="Time",ylab=expression(y[2]),lwd=1,main="(b)")
plot(y3,type="l",xlab="Time",ylab=expression(y[3]),lwd=1,main="(c)")
#
graphics.off()


##################################################
############  Code for Example 12.10  ############
##################################################

CPI.dat = read.csv("CPI.dat.csv")
CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
CPI_diff1 = as.matrix(diff(log(CPI),diff=1))
CPI_diff2 = as.matrix(diff(log(CPI),diff=2))

fit_ma = arima(CPI_diff2,order=c(0,0,2))
Box.test(fit_ma$resid,lag=20,type="Ljung",fitdf=2)


##################################################
############  Code for Figure 12.15  #############
##################################################

pdf("CPI_differences.pdf",height=7,width=8)
#
par(mfrow=c(3,1),cex.axis=1.3,cex.lab=1.1,cex.main=1.35, mgp=c(3,1,0))
plot(ts(log(CPI),start = c(1977,1), frequency = 12),xlab="year",ylab="log(CPI)",type="b",main="(a)")
plot(ts(as.vector(CPI_diff1),start = c(1977,2), frequency = 12),xlab="year",
     ylab=expression(paste(Delta," log(CPI)")),type="b",main="(b)")
plot(ts(as.vector(CPI_diff2),start = c(1977,3), frequency = 12),xlab="year",
     ylab=expression(paste(Delta^2," log(CPI)")),type="b",main="(c)")
#
graphics.off()


##################################################
############  Code for Figure 12.16  #############
##################################################

pdf("CPI_differences_acf.pdf",height=7,width=8)
#
par(mfrow=c(2,2),cex.axis=1.35,cex.lab=1.35,cex.main=1.35)
acf(log(CPI),main="(a) log(CPI)")
acf(CPI_diff1,main=expression(paste("(b) ",Delta," log(CPI}")))
acf(CPI_diff2,main=expression(paste("(c) ",Delta^2," log(CPI}")))
acf(fit_ma$resid,main="(d) residuals, ARIMA(0,2,2)")
#
graphics.off()


##################################################
############  Code for Example 12.11  ############
##################################################

IP.dat = read.csv("IP.dat.csv")

logIP = log(as.matrix(IP.dat$IP)[697:828,])  # 1977-01-31 to 1987-12-31
logIP_diff1 = as.vector(diff(logIP))

auto.arima(logIP_diff1, max.p = 2, max.q = 2,  d = 0, ic = "bic", trace = TRUE)
fitARMA10 = arima(IP_diff1,order=c(1,0,0))


##################################################
############  Code for Figure 12.17  #############
##################################################

pdf("IP_differences.pdf",height=5.5,width=6)
#
par(mfrow=c(2,2))
plot(logIP,main="(a)",type="b")
plot(diff(logIP),main="(b)",type="b")
acf(diff(logIP),main="(c)")
acf(fitARMA10$resid,main="(d)")
#
graphics.off()


##################################################
############  Code for Example 12.12  ############
##################################################

data(Mishkin,package="Ecdat")
y = as.vector(Mishkin[,1])
auto.arima(y, max.p = 2, max.q = 2,  d = 0, ic = "bic", trace = TRUE)
polyroot(c(1,-1.229, +0.233))


##################################################
############  Code for Example 12.13  ############
##################################################

library(tseries)
adf.test(y)
pp.test(y)

kpss.test(y)


##################################################
############  Code for Example 12.14  ############
##################################################

auto.arima(y, max.p = 5, max.q = 5, ic = "aic", trace = FALSE)
auto.arima(y, max.p = 5, max.q = 5, ic = "bic", trace = FALSE)

fitARIMA111 = arima(y,c(1,1,1))
par(mfrow=c(1,1))
acf(fitARIMA111$resid)
Box.test(fitARIMA111$resid,lag=15,fitdf=2)


##################################################
############  Code for Example 12.15  ############
############  Code for Figure 12.18   ############
############  Code for Figure 12.19   ############
##################################################

data(Mishkin,package="Ecdat")
y = as.vector(Mishkin[,1]) 

 year = seq(1950 + 1/12,1990+11/12,1/12)
 n=length(year)
 logn=log(n)

fit=arima(y,c(0,1,3))

pred.infl = predict(fit, n.ahead = 100, se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)
year = seq(1950 + 1/12,2001+61/12,1/12)


pdf("Inflation_predict.pdf",width=7,height=6)
#
plot(year[t1],y[t1],ylim=c(-10,18),type="b",xlim=c(1975,1999),
     xlab="year",ylab="Inflation rate",cex.axis=1.15,cex.lab=1.15)
points(year[t2], pred.infl$pred,type="p",pch="*")
lines(year[t2], pred.infl$pred - 2*pred.infl$se)
lines(year[t2], pred.infl$pred + 2*pred.infl$se)
legend(1975,-3,c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))
#
graphics.off()

fit_diff=arima(diff(y),c(0,0,3))

pred.infl_diff =predict(fit_diff, n.ahead = 100, newxreg = NULL,
                        se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)

pdf("Diff_Inflation_predict.pdf",width=7,height=6)
#
plot(year[t1],diff(y)[t1],xlim=c(1975,1999),ylim=c(-9,15),type="b",
     xlab="year",ylab="Change in inflation rate",cex.axis=1.5,cex.lab=1.5)
points(year[t2], pred.infl_diff$pred,type="p",pch="*")
lines(year[t2], pred.infl_diff$pred - 2*pred.infl_diff$se)
lines(year[t2], pred.infl_diff$pred + 2*pred.infl_diff$se)
legend(1975,14,c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=0,pch=c("o","*",NA,NA),lty=c(NA,NA,1,1))
#
graphics.off()


##################################################
############  Code for Example 12.16  ############
############  Code for Figure 12.20   ############
############  Code for Figure 12.21   ############
############  Code for Figure 12.22   ############
##################################################

data(Mishkin,package="Ecdat")
infl = as.vector(Mishkin[,1]) 

year = seq(1950 + 1/12,1990+11/12,1/12)
n=length(year)
logn=log(n)

fit_diff=arima(diff(infl),c(0,0,3))

pred.infl_diff =predict(fit_diff, n.ahead = 100, newxreg = NULL, se.fit = TRUE)
t1 = 300:491
t2 = 492:(492+49+50)

resid = fit_diff$resid[488:490]
coeff = as.vector(fit_diff$coef[1:3])
mu = as.vector(fit_diff$coef[4])
niter = 50000
n.ahead = 30
futureobs = matrix(0,nrow=niter,ncol=n.ahead)
future_int = futureobs

set.seed(1234576)
for (i in 1:niter)
{
  errors = sample(fit_diff$resid, n.ahead, replace = TRUE)
  errors = c(resid,errors)
  for (j in 1:n.ahead)
  {
    futureobs[i,j] = mu + errors[j+3] + errors[j+2]*coeff[1]+ errors[j+1]*coeff[2] + errors[j]*coeff[3]
    if (j > 1)
    {
      future_int[i,j] = future_int[i,j-1] + futureobs[i,j]
    }
    if (j==1){future_int[i,j] = futureobs[i,j]
    }
  }
}
future_mean = apply(futureobs,2,mean)
ul = 0*(1:n.ahead)
ll =ul
for (k in 1:n.ahead)
{
  ul[k] = quantile(futureobs[,k],.975)
  ll[k] = quantile(futureobs[,k],.025)
}

pdf("inflation_forecasts_sim.pdf")
#
plot(1:n.ahead,ul,ylim=c(-10,10),type="b",lwd=2,xlab="month ahead",ylab="rate",cex.axis=1.5,cex.lab=1.5)
lines(ll,type="b",lwd=2)
lines(1:n.ahead, pred.infl_diff$pred[1:n.ahead] - 1.96*pred.infl_diff$se[1:n.ahead],type="b",lty=3)
lines(1:n.ahead, pred.infl_diff$pred[1:n.ahead] + 1.96*pred.infl_diff$se[1:n.ahead],type="b",lty=3)
lines(1:n.ahead, future_mean,lwd=2,lty=2)
#
graphics.off()

pdf("inflation_forecasts_sim_random.pdf")
#
plot(1:n.ahead,futureobs[1,],ylim=c(-12,12),
     type="b",xlab="month ahead",ylab="rate",cex.axis=1.5,cex.lab=1.5,lwd=2)
for (i in 2:5)
{
  lines(1:n.ahead,futureobs[i,],type="b",lty=i,lwd=2)
}
#
graphics.off()

plot(1:n.ahead,future_int[1,],ylim=c(-8,18),
     type="b",xlab="month ahead",ylab="rate",cex.axis=1.5,cex.lab=1.5,lwd=2)
for (i in 2:5)
{
  lines(1:n.ahead,future_int[i,],type="b",lty=i,lwd=2)
}

ul_int = 0*(1:n.ahead)
ll_int =ul_int
for (k in 1:n.ahead)
{
  ul_int[k] = quantile(future_int[,k],.975)
  ll_int[k] = quantile(future_int[,k],.025)
}
future_mean_int = apply(future_int,2,mean)

pdf("inflation_forecasts_sim_integrated.pdf")
#
plot(1:n.ahead,ul_int,ylim=c(-5,15),type="b",lwd=2,xlab="month ahead",ylab="rate",cex.axis=1.5,cex.lab=1.5)
lines(ll_int,type="b",lwd=2)
lines(future_mean_int)
#
graphics.off()


##################################################
############  Code for Example 12.17  ############
############  Code for Figure 12.23   ############
##################################################

data(bmw,package="evir")

pdf("BMW_pacf.pdf",width=6,height=5)
#
pacf(bmw,main="Sample PACF for daily BMW stock log returns")
#
graphics.off()


##################################################
############  Code for Example 12.18  ############
############  Code for Figure 12.24   ############
##################################################

data(Mishkin,package="Ecdat")
infl = as.vector(Mishkin[,1])  

pdf("Inflation_pacf.pdf",height=5,width=6)
#
pacf(diff(infl), main = "Change in inflation rate")
#
graphics.off()



####################################################
############  R lab - 12.16.1  #####################
############  T-bill Rates   #######################
####################################################

# # Data download 1/7/2015
# library(Quandl)
# 
# # US 3 Month Treasury Bill Rate, Percent, Not Seasonally Adjusted, 
# # Discount Basis, Source: US Federal Reserve Board
# R = Quandl("FRED/DTB3", start_date = "1955-01-01", end_date = "2013-12-31", 
#            collapse = "quarterly", sort = "asc")
# r = R$Value
# 
# # Billions of Chained 2005 Dollars Seasonally Adjusted Annual Rate, 
# # Source: US Federal Reserve Board
# Y = Quandl("FRED/GDPC96", start_date = "1955-01-01", end_date = "2013-12-31", 
#            collapse = "quarterly", sort = "asc")
# y = log(Y$Value)
# 
# # Consumer Price Index for All Urban Consumers: All Items (USA Inflation)
# # Index 1982-84=100 Not Seasonally Adjusted, Source: US Federal Reserve Board
# PI = Quandl("FRED/CPIAUCNS", start_date = "1954-10-01", end_date = "2013-12-31", 
#             collapse = "quarterly", sort = "asc")
# pi = ((PI$Value[-1] / PI$Value[-nrow(PI)]) - 1)*100
# 
# TbGdpPi = ts(cbind(r,y,pi), start = 1955, freq = 4) 
#
# write.csv(TbGdpPi, file ="TbGdpPi.csv", row.names = FALSE)


TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
#  r = the 91-day treasury bill rate
#  y = the log of real GDP
#  pi = the inflation rate
TbGdpPi = ts(TbGdpPi, start = 1955, freq = 4)
library(tseries)
plot(TbGdpPi)
acf(TbGdpPi)
adf.test(TbGdpPi[,1])
adf.test(TbGdpPi[,2])
adf.test(TbGdpPi[,3])


diff_rate = diff(TbGdpPi)
adf.test(diff_rate[,1])
adf.test(diff_rate[,2])
adf.test(diff_rate[,3])
pairs(diff_rate)           #  scatterplot matrix
plot(diff_rate)            #  time series plots

# attr(diff_rate,"class")  # check the class

acf(diff_rate)           # auto- and cross-correlations

par(mfrow=c(1,1))
boxplot(diff_rate[,1] ~ cycle(diff_rate))

library(forecast)
auto.arima(TbGdpPi[,1],max.P=0,max.Q=0,ic="aic")


fit1 = arima(TbGdpPi[,1],order=c(?,?,?))
acf(residuals(fit1))
Box.test(residuals(fit1), lag = 12, type="Ljung", fitdf=?)


resid2 = (residuals(fit1) - mean(residuals(fit1)))^2
acf(resid2)
Box.test(resid2, lag = 12, type="Ljung")


####################################################
############  R lab - 12.16.1  #####################
############  Forecasting    #######################
####################################################

TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
attach(TbGdpPi)
#  r = the 91-day treasury bill rate
#  y = the log of real GDP
#  pi = the inflation rate
#  fit the non-seasonal ARIMA model found by auto.arima()
#  quarterly observations from 1955-1 to 2013-4 
year = seq(1955,2013.75, by=0.25)
library(forecast)
auto.arima(pi, max.P=0, max.Q=0, ic="bic")
fit = arima(pi, order=c(?,?,?))
forecasts = predict(fit, 36)
plot(year,pi,xlim=c(1980,2023), ylim=c(-7,12), type="b")
lines(seq(from=2014, by=.25, length=36), forecasts$pred, col="red")
lines(seq(from=2014, by=.25, length=36),
      forecasts$pred + 1.96*forecasts$se, col="blue")
lines(seq(from=2014, by=.25, length=36),
      forecasts$pred - 1.96*forecasts$se, col="blue")



##################################################
############  Code for Figure   ##################
############  in Exercise 13    ##################
##################################################

set.seed(2015)
e = rnorm(200)
y = cumsum(e)

pdf("stationary.pdf",height=6,width=6)
#
par(mfrow=c(3,2))
plot(y,type="l")
acf(y)
plot(diff(y),type="l")
acf(diff(y))
plot(diff(y,d=2),type="l")
acf(diff(y,d=2))
#
graphics.off()





