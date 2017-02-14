###########  R script for Chapter 13   ###################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

##################################################
############  Code for Figure 13.1  ##############
##################################################

data(Hstarts, package="Ecdat")
x = ts(Hstarts[,1], start=1960, frequency=4) 

pdf("Hstart.pdf", width=5.5, height=2.5)
#
par(mfrow=c(1,3))
plot(x,ylab="log(starts)",type="l",xlab="year",main="(a)")
acf(x,main="(b)",xlab="lag")
quart = rep(1,42) %x% (1:4)
boxplot(x~quart,xlab="quarter",ylab="log(starts)",main="(c)")
#
graphics.off()


##################################################
############  Code for Figure 13.2  ##############
##################################################

pdf("Hstart_diff_plots.pdf", width=6, height=6)
#
par(mfrow=c(3,2))
plot(diff(x),xlab="year",type="l",main="(a) nonseasonal differencing")
acf(diff(x),main="(b) nonseasonal differencing",xlab="lag")

plot(diff(x,4),type="l",xlab="year",main="(c) seasonal differencing")
acf(diff(x,4),main="(d) seasonal differencing",xlab="lag")

plot(diff(diff(x,1),4),type="l",xlab="year",
     main="(e) seasonal & nonseasonal differencing")
acf( diff(diff(x,1),4),main="(f) seasonal & nonseasonal differencing",xlab="lag")
#
graphics.off()


##################################################
############  Code for Example 13.1  #############
##################################################

data(Hstarts, package="Ecdat")
x = ts(Hstarts[,1], start=1960, frequency=4) 
fit1 = arima(x, c(1,1,1), seasonal = list(order = c(1,1,1), period = 4))
fit1

fit2 = arima(x, c(1,1,1), seasonal = list(order = c(0,1,1), period = 4))
fit2

##################################################
############  Code for Figure 13.3  ##############
##################################################

pred = predict(fit2, n.ahead = 16, newxreg = NULL, se.fit = TRUE)
year = seq(1960.25,2006,0.25)
t1 = 130:168
t2 = 169:(169+15)

pdf("Hstarts_predict.pdf")
#
par(mfrow=c(1,1))
plot(year[t1],x[t1],ylim=c(8.25,10.3),xlim=c(1992.50,2006.00),type="b",lty=2,
     xlab="year",ylab="log(starts)",cex.axis=1.5,cex.lab=1.5)
points(year[t2], as.matrix(pred$pred),type="b",pch="*",lty=3)
lines(year[t2], pred$pred - 2*pred$se)
lines(year[t2], pred$pred + 2*pred$se)
legend("topleft",c("data","predictions","lower CL","upper CL"),cex=1.2,
       box.lty=1,pch=c("o","*",NA,NA),lty=c(2,3,1,1))
#
graphics.off()


##################################################
############  Code for Figure 13.4  ##############
##################################################

data(AirPassengers) # monthly total international airline passengers
z = as.ts(log(AirPassengers), start=1949, frequency=12) # log transformation

pdf("airpass_logs.pdf", width=7, height=6)
#
plot(z,type="b",ylab="passengers",cex.axis=1.5,cex.lab=1.5,cex=1.5,lwd=2)
#
graphics.off()


##################################################
############  Code for Example 13.2  #############
############  Code for Figure 13.5  ##############
##################################################

data(Hstarts, package="Ecdat")
library("forecast")
library("FitAR")
y = exp(Hstarts[,1])
plot(y)

Hstart.arima = arima(y, c(1,1,1), seasonal = list(order = c(1,1,1), period = 4))

pdf("Hstart_BoxCox.pdf", width=6, height=5)
#
BoxCox.Arima(Hstart.arima)
#
graphics.off()



##################################################
############  Code for Example 13.3  #############
############  Code for Figure 13.6  ##############
##################################################

dat = read.table(file="WeekInt.txt",header=T)
library("car")
library("lmtest")
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
fit = lm(aaa_dif~cm10_dif+cm30_dif)
durbinWatsonTest(fit,max.lag=1,rep=1000)
durbinWatsonTest(fit,max.lag=1,rep=10000)
dwtest(fit,alternative="two.sided",exact=F)
dwtest(fit,alternative="two.sided",exact=T)
resid = residuals(fit)
n = length(resid)
tt = (1:n)/(n+1)

pdf("WeekInt_2var_residuals.pdf",height=6,width=6)
#
par(mfrow=c(2,2),lwd=1)
qqnorm(resid,datax=T,main="(a) Normal plot",xlab="theoretical quantiles",
       ylab="sample quantiles")
qqline(resid,datax=T)
qqplot(resid, (-0.00035 +0.04058 * qt(tt,df=3)) ,main="(b) t-plot",
       ylab="theoretical Quantiles")
abline(0,1)
acf(resid,main="(c)",xlab="lag")
plot(fitted(fit),resid,main="(d)",ylab="Residuals",xlab="fitted values")
#
graphics.off()

auto.arima(resid,ic="bic")
auto.arima(resid, ic="aic")
length(resid)

##################################################
############  Code for Example 13.4  #############
############  Code for Figure 13.7  ##############
##################################################

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
fit1 = lm(aaa_dif~cm10_dif+cm30_dif)
fit2 = lm(aaa~cm10+cm30)

pdf("WeekIntNoDifResid.pdf",height=3.5,width=8)
#
par(mfrow=c(1,2),cex.axis=1.05,cex.lab=1.05,lwd=1,cex.main=1.05)
dates = as.Date(paste(dat[,2],dat[,1],dat[,3], sep="/"), format = "%d/%m/%y")
library(xts)
plot(xts(residuals(fit2), dates),ylab="Residuals",main="(a)", type = 'l',  minor.ticks = FALSE)
#plot(residuals(fit2),ylab="Residuals",main="(a)", type = 'l')
acf(residuals(fit2),main="(b) Residuals")
#
graphics.off()

library(tseries)
adf.test(residuals(fit2))
kpss.test(residuals(fit2))

summary(fit2)
summary(fit1)


##################################################
############  Code for Example 13.5  #############
##################################################

set.seed(997711)
n = 200
x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit1 = lm(y~x)
fit5 = lm(diff(y)~diff(x))

x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit2= lm(y~x)
fit6 = lm(diff(y)~diff(x))

x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit3= lm(y~x)
fit7 = lm(diff(y)~diff(x))

x = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
y = arima.sim(list(order=c(1,0,0),ar=.99),n=n)
fit4= lm(y~x)
fit8 = lm(diff(y)~diff(x))

summary(fit1)$coefficients
summary(fit2)$coefficients
summary(fit3)$coefficients
summary(fit4)$coefficients

summary(fit5)$coefficients
summary(fit6)$coefficients
summary(fit7)$coefficients
summary(fit8)$coefficients


##################################################
############  Code for Example 13.6  #############
############  Code for Figure 13.8  ##############
##################################################

phi = c(-.75,-.5,-.25,0,.25,.5,.75)
x = cbind(rep(1,21),seq(-10,10,1))
se = as.matrix(cbind(phi,rep(0,7),rep(0,7)))
for (i in 1:7)
{
  xx = (t(x)%*%x)
  xxinv = solve(xx)
  
  sig = toeplitz(phi[i]^(0:20))
  
  cov = xxinv %*% t(x) %*%sig %*% x %*% xxinv
  se[i,2:3] = t(sqrt(diag(cov)))
}

pdf("example_arerrors.pdf")
#
plot(se[,1],se[4,2]/se[,2],type="b",xlab=expression(phi),
     ylab= "SE ratio",cex.axis=1.5,cex.lab=1.5,cex=1.5,lwd=2)
lines(se[,1],se[4,3]/se[,3],type="b",lty="dashed",pch="*",cex=1.5,lwd=2,col="red")
legend("topright",c("intercept","slope"),lty=c(1,2),pch=c("o","*"),
       lwd=2,cex=1.5,col=c("black","red"))
#
graphics.off()


##################################################
############  Code for Example 13.7  #############
############  Code for Table 13.1    #############
##################################################

dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff(cm10)
aaa_dif = diff(aaa)
cm30_dif = diff(cm30)
fit = lm(aaa_dif ~ cm10_dif + cm30_dif)
round(summary(fit)$coef, 4)

library(sandwich)
options(digits=2)

NeweyWest(fit, lag = 0, prewhite = F)

NeweyWest(fit, lag = 3, prewhite = F)


sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))
sqrt(diag(NeweyWest(fit, lag = 3, prewhite = F)))

coef(fit)/sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 3, prewhite = F)))


##################################################
############  Code for Example 13.8  #############
############  Code for Figure 13.9   #############
############  Code for Figure 13.10  #############
##################################################

library("Ecdat")
data(Icecream)
attach(Icecream)
fit_ic_lm = lm(cons~income+price+temp)
summary(fit_ic_lm)


options(digits=3)
library("car")
durbinWatsonTest(fit_ic_lm)


fit_ic_ar = arima(cons,order=c(1,0,0),xreg=cbind(income,price,temp))
print(fit_ic_ar,digits=3)

fit_ic_ma = arima(cons,order=c(0,0,1),xreg=cbind(income,price,temp))
print(fit_ic_ma,digits=3)


pdf("ice_cream_acf_resid.pdf",width=6.5,height=2.75)  
#
par(mfrow=c(1,3))
acf(fit_ic_lm$resid,main="linear model/indep. noise",xlab="lag")
acf(fit_ic_ar$resid,main="linear model/AR(1) noise",xlab="lag")
acf(fit_ic_ma$resid,main="linear model/MA(1) noise",xlab="lag")
#
graphics.off()

pdf("ice_cream_tsplots.pdf",height=5,width=6)  ##  figure 11.
#
par(mfrow=c(2,2))
plot(cons,type="b",main="",xlab="index")
plot(temp,type="b",main="",xlab="index")
plot(income,type="b",main="",xlab="index")
plot(price,type="b",main="",xlab="index")
#
graphics.off()

detach(Icecream)


##################################################
############  Code for Example 13.9  #############
############  Code for Figure 13.11  #############
############  Code for Figure 13.12  #############
##################################################

CPI.dat = read.csv("CPI.dat.csv")
IP.dat = read.csv("IP.dat.csv")
CPI = as.matrix(CPI.dat$CPI)[769:900,] # 1977-01-31 to 1987-12-31
IP = as.matrix(IP.dat$IP)[697:828,] # 1977-01-31 to 1987-12-31
cpi = log(CPI)
ip = log(IP)
cpi_diff1 = diff(cpi)
ip_diff1 = diff(ip)


pdf("CPI_IP_ts.pdf", height=6, width=8)
#
par(mfrow=c(2,2),cex.axis=1.3,cex.lab=1.1,cex.main=1.35, mgp=c(3,1,0))
plot(ts(cpi,start = c(1977,1), frequency = 12),xlab="year",ylab="log(CPI)",main="(a)")
plot(ts(ip,start = c(1977,1), frequency = 12),xlab="year",ylab="log(IP)",main="(b)")
plot(ts(cpi_diff1,start = c(1977,2), frequency = 12),xlab="year",ylab=expression(paste(Delta," log(CPI)")),main="(c)")
plot(ts(ip_diff1,start = c(1977,2), frequency = 12),xlab="year",ylab=expression(paste(Delta," log(IP)")),main="(d)")
#
graphics.off()



pdf("CPI_IP_ccf.pdf", height=6, width=8)
#
par(cex.axis=1.35,cex.lab=1.35,cex.main=1.35)
ccf(cpi_diff1,ip_diff1,lwd=3,  ylab="CCF",
    main=expression(paste("corr{",Delta,"cpi(t),",Delta,"ip(t-lag)}" )))
abline(v=0,lty=5)
#
graphics.off()


##################################################
############  Code for Figure 13.13  #############
##################################################

CPI_IP = cbind(cpi_diff1, ip_diff1)

pdf("CPI_IP_acf.pdf", height=6, width=8)
#
acf(CPI_IP)
#
graphics.off()


source("SDAFE2.R")
mLjungBox(CPI_IP, lag = 10)


##################################################
############  Code for Example 13.10  ############
############  Code for Figure 13.14  #############
##################################################

CPI_IP = cbind(cpi_diff1,ip_diff1)
arFit = ar(CPI_IP,order.max=10)
options(digits=2)
arFit$aic

arFit1 = ar(CPI_IP,order.max=1)
arFit1

colMeans(CPI_IP)

bPhi = arFit1$ar[,,] ; bPhi
bPhi2 = bPhi %*% bPhi ; bPhi2
bPhi3 = bPhi2 %*% bPhi ; bPhi3
bPhi4 = bPhi3 %*% bPhi ; bPhi4
bPhi5 = bPhi4 %*% bPhi ; bPhi5

epsilon.hat = as.matrix(na.omit(arFit1$resid))
colnames(epsilon.hat) = c("Resid Series 1", "Resid Series 2")

pdf("CPI_IP_ar1_resid_acf.pdf",height=6,width=8)
#
par(cex.axis=1.15,cex.lab=1.15,cex.main=1.15)
acf(epsilon.hat)
#
graphics.off()


##################################################
############  Code for Example 13.11  ############
############  Code for Figure 13.15  #############
############  Code for Figure 13.16  #############
##################################################

x.n = CPI_IP[131,]
forecasts =  x.n
means = colMeans(CPI_IP)
offset = x.n - means
for (i in 1:10){
  offset = bPhi %*% offset
  forecasts = cbind(forecasts, means + offset)
}

pdf("CPI_IP_AR_forecasts.pdf", height=6, width=8)
#
plot(0:10,forecasts[2,],type="b",lty=2,ylim=c(0,.006),pch="*",cex=3,
     xlab="h",ylab="forecast",cex.axis=1.15,cex.lab=1.15,lwd=3)
lines(0:10,forecasts[1,],type="b",pch="o",cex=2,lwd=3)
abline(h=means[1])
abline(h=means[2],lty=2)
legend(2,.0015,c(expression(paste(Delta,"cpi")),expression(paste(Delta,"ip"))),
       lty=c(1,2),pch=c("o","*"),cex=1.6,lwd=3, pt.cex=c(2,3))
#
graphics.off()



library("MASS")
arFit1 = ar(CPI_IP,order.max=1)

res = arFit1$resid[7:126,]
bPhi = arFit1$ar[,,]
bSigma = arFit1$var.pred
means = (apply(CPI_IP,2,mean))

set.seed(2015)
niter = 50000
fore_CPI = matrix(0,nrow=niter,ncol=11)
fore_IP = fore_CPI

for (k in 1:niter)
{
  forecasts =  t(CPI_IP[131,])
  for (i in 1:11)
  {
    fore_CPI[k,i] = forecasts[1]
    fore_IP[k,i] = forecasts[2]
    fore_means = means + bPhi %*% t(forecasts-means)
    forecasts = t(mvrnorm(n=1,mu=fore_means,Sigma=bSigma))
  }
}

fore_CPI_xbar=apply(fore_CPI,2,mean)
fore_IP_xbar=apply(fore_IP,2,mean)
plot(0:10,fore_IP_xbar,type="b",lty=2,ylim=c(0,.006),pch="*",cex=3,
     xlab="k",ylab="forecast",cex.axis=1.5,cex.lab=1.5,lwd=3)

lines(0:10,fore_CPI_xbar,type="b",pch="o",cex=2,lwd=3)
abline(h=means[1])
abline(h=means[2],lty=2)
legend(2,.0015,c("IP","CPI"),lty=c(2,1),pch=c("*","o"),cex=1.6,lwd=3,
       pt.cex=c(3,2))

ul_CPI = 0*(1:11)
ll_CPI =ul_CPI
mean_CPI=ul_CPI
for (k in 1:11)
{
  ul_CPI[k] = quantile(fore_CPI[,k],.975)
  ll_CPI[k] = quantile(fore_CPI[,k],.025)
  mean_CPI[k] = mean(fore_CPI[,k])
}

ul_IP = 0*(1:11)
ll_IP =ul_IP
mean_IP=ul_IP
for (k in 1:11)
{
  ul_IP[k] = quantile(fore_IP[,k],.975)
  ll_IP[k] = quantile(fore_IP[,k],.025)
  mean_IP[k] = mean(fore_IP[,k])
}

pdf("CPI_IP_forecasts_sim.pdf")
#
par(mfrow=c(2,1))
plot(0:10,ul_CPI,type="b",lty=2,ylim=c(-0.003,.013),lwd=2,
     xlab="h",ylab="forecast",cex.axis=1.5,cex.lab=1.5,
     main=expression(paste(Delta,"cpi")),cex=1.5)
lines(0:10,ll_CPI,type="b",lty=2,lwd=2,cex=1.5)
lines(0:10,mean_CPI,type="b",lwd=2,pch="*",cex=1.5)

plot(0:10,ul_IP,,type="b",lty=2,ylim=c(-0.02,.025),lwd=2,
     xlab="h",ylab="forecast",cex.axis=1.5,cex.lab=1.5,
     main=expression(paste(Delta,"ip")),cex=1.5)

lines(0:10,ll_IP,type="b",lty=2,lwd=2,pch="o",cex=1.5)
lines(0:10,mean_IP,type="b",lwd=2,pch="*",cex=1.5)
#
graphics.off()


##################################################
############  Code for Figure 13.17  #############
############  Code for Figure 13.18  #############
##################################################

library("fracdiff")
library("longmemo")

D = c(-.35, .35)

pdf("FARIMA_sim.pdf",height=6,width=5)  
#
set.seed("09201948")
par(mfrow=c(length(D)+1,2))
for (i in 1:length(D))
{
  H = D[i] + 1/2
  x = simARMA0(2500,H)
  plot(x,main=toString(paste("d =", D[i]))  )
  acf(x,main=toString(paste("d =", D[i]))  )
}

d= .7-1
H = d + 1/2
y = simARMA0(2500,H)
x = cumsum(y)
plot(x,main="d = 0.7",type="l")
acf(x,main="d = 0.7")
#
graphics.off()

pdf("FARIMA_sim_diff_ACF.pdf",width=6,height=3)  
#
par(mfrow=c(1,2))
acf(diffseries(x,.7),main = expression(paste(Delta^0.7, Y)))
acf(diff(x),main = expression(paste(Delta, Y)))
#
graphics.off()


##################################################
############  Code for Example 13.19  ############
############  Code for Figure 13.12   ############
##################################################

data(Mishkin,package="Ecdat")
library("fracdiff")
library("forecast")
y = as.vector(Mishkin[,1]) 

fit.frac = fracdiff(y,nar=0,nma=0)
summary(fit.frac)

fdiffy = diffseries(y,0.4)

pdf("inflation_fractionalDiff_acf.pdf")
#
par(mfrow=c(3,2))
plot(ts(y, start=1950, frequency=12), ylab = "y", main="d = 0") 
acf(y,main="d = 0")
plot(ts(fdiffy, start=1950, frequency=12), ylab = "fdiffy", main="d = 0.4") 
acf(fdiffy,main="d = 0.4")
plot(ts(diff(y), start=1950, frequency=12), ylab = "diffy", main="d = 1") 
acf(diff(y),main="d = 1")
#
graphics.off()

options(digits=5)
auto.arima(diffseries(y, 0.378),ic="bic", trace=TRUE)



####################################################
############  R lab - 13.8.1   #####################
############  Seasonal ARIMA Models   ##############
####################################################

library("Ecdat")
library("forecast")
data(IncomeUK)
consumption = IncomeUK[,2]
plot(consumption)

?predict.Arima

logConsumption = log(consumption)
fitAutoArima = auto.arima(logConsumption, ic="bic") 
foreAutoArima = forecast(fitAutoArima, h=8) 
plot(foreAutoArima, xlim=c(1985.5,1987.5), ylim=c(10.7,11.2))


####################################################
############  R lab - 13.8.2   #####################
############  Regression with HAC Standard Errors ##
####################################################
data(Mishkin, package="Ecdat")
tb1_dif = diff(as.vector(Mishkin[,3]))
tb3_dif = diff(as.vector(Mishkin[,4]))
fit = lm(tb1_dif ~ tb3_dif )
round(summary(fit)$coef, 4)
acf(fit$resid)

library(sandwich)
sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))
coef(fit)/sqrt(diag(NeweyWest(fit, lag = 0, prewhite = F)))

####################################################
############  R lab - 13.8.3   #####################
############  Regression with ARMA Noise   #########
####################################################

library(AER)
data("USMacroG")
MacroDiff = as.data.frame(apply(USMacroG, 2, diff))
attach(MacroDiff)
fit1 = arima(unemp, order=c(1,0,0), xreg=cbind(invest, government))


####################################################
############  R lab - 13.8.4   #####################
############  VAR Models    ########################
####################################################

TbGdpPi = read.csv("TbGdpPi.csv", header=TRUE)
#  r = the 91-day treasury bill rate
#  y = the log of real GDP
#  pi = the inflation rate
TbGdpPi = ts(TbGdpPi, start = 1955, freq = 4)
del_dat = diff(TbGdpPi)
var1 = ar(del_dat, order.max=4, aic=T)
var1
acf(na.omit(var1$resid))

tail(TbGdpPi, n = 4)

var1 = ar(del_dat, order.max=1)

yn = var1$x.mean * 1.1 ; yn

Phi_hat = var1$ar[1,,] ; Phi_hat

eigen.values = eigen(Phi_hat)$values
abs(eigen.values)

MacroVars = read.csv("MacroVars.csv", head=TRUE)

####################################################
############  R lab - 13.8.5   #####################
############  Long-Memory Processes   ##############
####################################################

data(Mishkin,package="Ecdat")
cpi = as.vector(Mishkin[,5])  # cpi = one-month inflation rate 
DiffSqrtCpi = diff(sqrt(cpi))

library("fracdiff")
fit.frac = fracdiff(DiffSqrtCpi,nar=0,nma=0)
fit.frac$d
fdiff = diffseries(DiffSqrtCpi,fit.frac$d)
acf(fdiff)


####################################################
############  R lab - 13.8.6   #####################
############  Model-Based Bootstrapping   ########## 
############   an ARIMA Process       ##############
####################################################


library(AER)
library(forecast)
data("FrozenJuice")
price = FrozenJuice[,1]
plot(price)
auto.arima(price, ic="bic")

n = length(price)
sink("priceBootstrap.txt")
set.seed(1998852)
for (iter in 1:10){
  eps = rnorm(n+20)
  y = rep(0,n+20)
  for (t in 3:(n+20)){
    y[t] = 0.2825*y[t-1] + 0.0570*y[t-2] + eps[t] 
  }
  y = y[101:n+20]
  y = cumsum(y)
  y = ts(y, frequency=12)
  fit = auto.arima(y, d=1, D=0, ic="bic")
  print(fit)
}
sink()


##########  second bootstrap  ##########  
set.seed(1998852)
niter = 1000
estimates=matrix(0, nrow=niter, ncol=2)
for (iter in 1:niter){
  eps = rnorm(n+20)
  y = rep(0, n+20)
  for (t in 3:(n+20)){
    y[t] = .2825 *y[t-1] + 0.0570*y[t-2] + eps[t] 
  }
  y = y[101:n+20]
  y = cumsum(y)
  y = ts(y, frequency=12)
  fit=arima(y, order=c(2,1,0))
  estimates[iter,] = fit$coef
}


##################################################
############  Code for Figure   ##################
############  in Exercise 1-3    #################
##################################################

n=180
set.seed(998877)
y = rnorm(n)
e = rnorm(n)
e2 = rnorm(n)
for (t in 9:n)
{
  y[t] = .6*y[t-1] + e[t] + .3*e[t-1]
}
y = cumsum(y)
for (t in 5:n)
{
  y[t] = y[t] + y[t-4]
}
y = y[21:n]
y=as.ts(y,f=4)

pdf("exercise01.pdf",width=6,height=2.25)
#
par(mfrow=c(1,4))
acf(y,main="(a) none")
acf(diff(y),main="(b) non-seasonal")
acf(diff(y,lag=4),main="(c) seasonal")
acf(diff(diff(y),lag=4),main="(d) both")
#
graphics.off()

######################## seasonal only

n=180
set.seed(998877)
y = rnorm(n)
e = rnorm(n)
e2 = rnorm(n)
for (t in 9:n)
{
  y[t] = .6*y[t-1] + e[t] + .3*e[t-1]
}
for (t in 5:n)
{
  y[t] = y[t] + y[t-4]
}
y = y[21:n]
y=as.ts(y,f=4)

pdf("exercise02.pdf",width=6,height=2.25)
#
par(mfrow=c(1,4))
acf(y,main="(a) none")
acf(diff(y),main="(b) non-seasonal")
acf(diff(y,lag=4),main="(c) seasonal")
acf(diff(diff(y),lag=4),main="(d) both")
#
graphics.off()

######################## non-seasonal only

n=180
set.seed(998877)
y = rnorm(n)
e = rnorm(n)
e2 = rnorm(n)
for (t in 9:n)
{
  y[t] = .8*y[t-1] + e[t] - .3*e[t-1]
}
y=cumsum(y)
y = y[21:n]
y=as.ts(y,f=4)

pdf("exercise03.pdf",width=6,height=2.25)
#
par(mfrow=c(1,4))
acf(y,main="(a) none")
acf(diff(y),main="(b) non-seasonal")
acf(diff(y,lag=4),main="(c) seasonal")
acf(diff(diff(y),lag=4),main="(d) both")
#
graphics.off()




set.seed(998877)
n = 200
mu = 1
x1 = rnorm(n, 0, 1)
x2 = rnorm(n, mu, 1)
y = c(x1,x2)
par(mfrow=c(3,2))
ts.plot(x1) ; acf(x1)
ts.plot(x2) ; acf(x2)
ts.plot(y) ; acf(y)





