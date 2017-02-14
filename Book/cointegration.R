###########  R script for Chapter 15   ###################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

##################################################
############  Code for Example 15.1  #############
############  Code for Figure 15.1  ##############
##################################################

library(forecast)
library(tseries)
library(urca)
library(xts)

yieldDat = read.table("treasury_yields.txt",header=T)
date = as.Date(yieldDat[,1], format = "%m/%d/%y")
dat = as.xts(yieldDat[,3:7], date)

res = residuals(lm(dat[,3]~dat[,1]+dat[,2]+dat[,4]+dat[,5]))

pdf("yieldsCointegration.pdf",width=7.25,height=4.5)
#
par(mfrow=c(2,4))
plot(dat[,1],type="l",ylab="",main="3-month", minor.tick=FALSE)
plot(dat[,2],type="l",ylab="",main="6-month", minor.tick=FALSE)
plot(dat[,3],type="l",ylab="",main="1-year", minor.tick=FALSE)
plot(dat[,4],type="l",ylab="",main="2-year", minor.tick=FALSE)
plot(dat[,5],type="l",ylab="",main="3-year", minor.tick=FALSE)
plot(res,type="l",ylab="",main="residuals")
acf(res,main="ACF of residuals",xlab="lag")
#
graphics.off()

po.test(dat[,c(3,1,2,4,5)])


##################################################
############  Code for Example 15.2  #############
############  Code for Figure 15.2  ##############
##################################################

n = 5000
set.seed(12345)
a1 = 0.5
a2 = 0.55
lambda  = 1
y1 = rep(0,n)
y2 = y1
e1 = rnorm(n)
e2 = rnorm(n)
for (i in 2:n){
  y1[i] = y1[i-1] + a1 * (y1[i-1] - lambda*y2[i-1]) + e1[i]
  y2[i] = y2[i-1] + a2 * (y1[i-1] - lambda*y2[i-1]) + e2[i]
}
a1 - lambda*a2
ind = 10*(1:500)

pdf("cointSimEx1.pdf",width=6,height=2.5)
#
par(mfrow=c(1,3))
plot(y1[ind],type="l",main=expression(Y[1]),ylab="")
plot(y2[ind],type="l",main=expression(Y[2]),ylab="")
plot(y1[ind]-lambda*y2[ind],type="l",
     main=expression(paste(Y[1],-lambda,Y[2])),ylab="")
#
graphics.off()


##################################################
############  Code for Example 15.3  #############
##################################################

options(digits=3)
summary(ca.jo(dat))



####################################################
############  R lab - 15.6.1   #####################
############  Cointegration Analysis    ############
############  of Midcap Prices        ##############
####################################################

library(urca)
midcapD.ts = read.csv("midcapD.csv")
x = midcapD.ts[,2:11]
prices= exp(apply(x,2,cumsum))
options(digits=3)
summary(ca.jo(prices))


####################################################
############  R lab - 15.6.2   #####################
############  Cointegration Analysis of Yields  ####
####################################################

library(urca)
mk.maturity = read.csv("mk.zero2.csv", header=T)
summary(ca.jo(mk.maturity[,2:11]))


####################################################
############  R lab - 15.6.3   #####################
############  Cointegration Analysis ###############
############  of Daily Stock Prices  ###############
####################################################

CokePepsi = read.table("CokePepsi.csv", header=T)
ts.plot(CokePepsi)

ts.plot(CokePepsi[,2] - CokePepsi[,1])

library(urca)
summary(ca.jo(CokePepsi))


Stock_FX_Bond = read.csv("Stock_FX_Bond.csv", header=T)
adjClose = Stock_FX_Bond[,seq(from=3, to=21, by=2)]
ts.plot(adjClose)
summary(ca.jo(adjClose))

summary(ca.jo(adjClose, K=8))





