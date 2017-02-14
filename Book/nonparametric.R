##########################################################################################
###########  R script for Chapter 21   ###################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################
##########################################################################################

##################################################
############  Code for Figure 21.1  ##############
##################################################
library(Ecdat)
library(KernSmooth)
data(Capm)
attach(Capm)
n = length(rf)
year = seq(1960.125,2003,length=n)
diffrf=diff(Capm$rf)
rf_lag = rf[1:(n-1)]
ll_mu <- locpoly(rf_lag,diffrf, bandwidth = dpill(rf_lag,diffrf) )
muhat = spline(ll_mu$x,ll_mu$y,xout=rf_lag)$y
epsilon_sqr = (diffrf-muhat)^2
ll_sig <- locpoly(rf_lag,epsilon_sqr, 
   bandwidth = dpill(rf_lag,epsilon_sqr) )
pdf("riskfree01.pdf",width=6,height=5)   ##  Figure 21.1  ##
par(mfrow=c(2,2))
plot(year,rf,ylab="return",main="(a)",type="l" )
plot(year[2:n],diffrf,ylab="change in  return",main="(b)",type="l",xlab="year")
plot(rf_lag,diffrf,ylab="change in return",xlab="return",main="(c)",type="p",cex=.7)
lines(ll_mu$x,ll_mu$y,lwd=4,col="red")
plot(rf_lag,(diffrf-muhat)^2,xlab="return",ylab="squared residual",main="(d)",cex=.7)
lines(ll_sig$x,ll_sig$y,lwd=4,col="red")
graphics.off()

##################################################
############  Code for Figure 21.2  ##############
##################################################
set.seed("9988")
library("KernSmooth")
scale=.25
x = seq(0,1,length=75)
true = .1*x+ sin(5* x^1.5) + 3.6
y = true + rnorm(75,sd=.2)
h = dpill(x,y)
w1 = scale*dnorm(x,mean=x[25],sd=h)
fit1 = lm(y~x,weights=w1)
w2 = scale*dnorm(x,mean=x[55],sd=h)
fit2 = lm(y~x,weights=w2)
fit3 <- locpoly(x,y, bandwidth = h)
pdf("loclin.pdf",width=6,height=4.5)   ##  Figure 21.2  ##
par(mfrow = c(1,1))
plot(x,y,ylim=c(0,6),cex=1.15,pch="*")
lines(x,w1,lty="dashed",lwd=4,col="blue")
lines(x,w2,lty="dashed",lwd=4,col="blue")
lines(x[12:38],fit1$fitted[12:38],lwd=4,col="red")
lines(x[42:68],fit2$fitted[42:68],lwd=4,col="red")
points(c(x[25],x[55]),c(fit1$fit[25],fit2$fit[55]),pch="+",cex=4)
lines(fit3$x,fit3$y,lwd=3,lty=1)
abline(h=0,lwd=2)
graphics.off()


##################################################
############  Code for Figure 21.3  ##############
##################################################
library("KernSmooth")
set.seed("9988")
scale=.25
x = seq(0,1,length=75)
true = .1*x+ sin(5* x^1.5) + 3.6
y = true + rnorm(75,sd=.25)
h1 = dpill(x,y)
fit1 <- locpoly(x,y, bandwidth = h1)
fit2 <- locpoly(x,y, bandwidth = 3*h1)
fit3 <- locpoly(x,y, bandwidth = h1/3)
options(digits=4)
muhat1 = spline(fit1$x,fit1$y,xout=x)$y
mean((true-muhat1)^2)
muhat2 = spline(fit2$x,fit2$y,xout=x)$y
mean((true-muhat2)^2) / mean((true-muhat1)^2)
muhat3 = spline(fit3$x,fit3$y,xout=x)$y
mean((true-muhat3)^2) / mean((true-muhat1)^2)
pdf("loclinVaryBand.pdf",width=6,height=5)   ##  Figure 21.3  ##
par(mfrow = c(1,1))
plot(x,y,ylim=c(2.4,5),cex=.5,pch="o")
lines(fit1$x,fit1$y,lwd=4)
lines(fit2$x,fit2$y,lty=2,lwd=4,col="red")
lines(fit3$x,fit3$y,lty=4,lwd=4,col="blue")
legend("bottomleft",c("dpi","3*dpi","dpi/3"),lty=c(1,2,1),lwd=4,
   col=c("black", "red", "blue"))
graphics.off()

################################################################
########## Code for figure 21.4  ###############################
################################################################

dat = read.table("strips_dec95.txt", header = T)
T = dat$T
n = length(T)
ord = order(T)
T = T[ord]
price = dat$price[ord]
Int_F = - log(price) + log(100)

library(locfit)
fit_loc_Int_F = locfit(Int_F ~ T, deriv = 1, deg = 3) 

NelsonSiegel = function(theta){ 
                                ####  f = forward rate and int_f = intergrated forward rate  ####
                                f = theta[1] + (theta[2] + theta[3] * T) * exp(-theta[4] * T)
                                int_f = theta[1] * T - theta[2] / theta[4] *
                                (exp(-theta[4] * T) - 1) -
                                  theta[3] * (T * exp(-theta[4] * T) / theta[4] +
                                                (exp(-theta[4] * T) - 1) / theta[4]^2)
                                list("f" = f, "inf_t" = int_f)
                                }
fit_NS = optim(c(0.05, 0.001, 0.001, 0.08),
               fn = function(theta){
                 Yhat = NelsonSiegel(theta)[[2]]
                 sum((Int_F - Yhat)^2)}, 
               control = list(maxit=30000, reltol = 1e-10))
NS_yhat = NelsonSiegel(fit_NS$par)[[1]]  

pdf("strips02.pdf",width=6,height=5)   ##  Figure 21.4  ##
par(mfrow=c(1,1))
plot(fit_loc_Int_F, ylim = c(.025,.075), ylab = "Forward rate", 
     col = "red", lwd = 3)
lines(fit_loc_Int_F, col = "red", lwd=3) # to widen to linewidth = 3
lines(T, NS_yhat, col = "blue", lwd = 3, lty = 4)
points(T[2:n], diff(Int_F) / diff(T))
legend("bottomleft", c("local cubic",
                       "Nelson-Siegel", "Empirical"),
       lty=c(1, 4,NA),pch=c(NA, NA, "o"),
       col=c("red", "blue", "black"), lwd = c(3, 3, NA))
graphics.off()

##################################################
############  Code for Figure 21.5  ##############
##################################################
x= seq(0,4,length=401)
plus = (x>2)*(x-2)
f = .5 + .2*x + .5*plus
f2 = (x>1)*(x-1)
pdf("splinePlots.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot(x,f,type="l",lwd=2,main="(a) Linear spline",ylab="")
plot(x,f2,type="l",lwd=2,main="(b) Linear plus function",ylab="")
lines(x,(x>1),lty=2,lwd=2,col="red")
legend("topleft",c("plus fn.","derivative"),lty=c(1,2),col=c("black", "red")
       lwd=2)
graphics.off()

##################################################
############  Code for Figure 21.6  ##############
##################################################
x=seq(0,3,length=401)
f1 = (x>1)
pdf("quadSplines.pdf",width=8,height=4)
par(mfrow=c(1,2))
plot(x, x + 2*(x-1)^2-3*f1*(x-1)^2,type="l",main="(a)",ylab="",lwd=2)
abline(v=1,lty=4,col="red")
plot(x,2*f1,type="l",ylim=c(0,4),lwd=3,lty=2,ylab="",main="(b)",col="blue")
lines(x,f1*(x-1)^2,lwd=3,lty=1,col="black")
lines(x,2*f1*(x-1),lwd=3,lty=5,col="red")
legend("topleft",c("plus fn.","derivative","2nd derivative"),
       lty=c(1,5,2),lwd=c(3,2,3),col=c("black", "red", "blue"))
graphics.off()

############################################################################
############  Code for Example 21.2 and Figure 21.7 and 21.8  ##############
############################################################################
library(Ecdat)
library(KernSmooth)
library(locfit)
library(mgcv)
data(Capm)
attach(Capm)
n = length(rf)
year = seq(1960.125,2003,length=n)
diffrf=diff(Capm$rf)
rf_lag = rf[1:(n-1)]
log_rf_lag = log(rf_lag)
ll_mu <- locpoly(rf_lag,diffrf, bandwidth = dpill(rf_lag,diffrf) )
muhat = spline(ll_mu$x,ll_mu$y,xout=rf_lag)$y
epsilon_sqr = (diffrf-muhat)^2
ll_sig <- locpoly(rf_lag,epsilon_sqr, 
                  bandwidth = dpill(rf_lag,epsilon_sqr) )
gam_mu = gam(diffrf~s(rf_lag,bs="cr"),method="REML")
epsilon_sqr = (diffrf-gam_mu$fit)^2
gam_sig = gam(epsilon_sqr~s(rf_lag,bs="cr"),method="REML")
locfit_mu = locfit(diffrf~rf_lag)
epsilon_sqr = (diffrf - fitted(locfit_mu))^2
locfit_sig = locfit(epsilon_sqr~rf_lag)
std_res = (diffrf - fitted(locfit_mu)) / sqrt(fitted(locfit_sig))
min(rf_lag[(gam_mu$fit < 0)])
orrf = order(rf_lag)
pdf("riskfree02.pdf",width=8,height=4)    ##  Figure 21.7  ##
par(mfrow=c(1,2))
plot(rf_lag[orrf],gam_mu$fit[orrf],type="l",lwd=3,lty=1,
     xlab="lagged rate",ylab="change in rate",main="(a)") 
lines(ll_mu$x,ll_mu$y,lwd=3,lty=2,col="red")
lines(locfit_mu,lwd=3,lty=3,col="blue")
legend(.1,-.05,c("spline","local linear","local quadratic"),
       lty=c(1,2,3),cex=.85,lwd=3,col=c("black","red","blue"))
rug(rf_lag)
abline(h=0,lwd=2)
plot(rf_lag[orrf],gam_sig$fit[orrf],type="l",lwd=3,lty=1,ylim=c(0,.03),  
     xlab="lagged rate",ylab="squared residual",main="(b)")
lines(ll_sig$x,ll_sig$y,lwd=3,lty=2,col="red")
lines(locfit_sig,lwd=3,lty=3,col="blue")
abline(h=0,lwd=2)
legend("topleft",c("spline","local linear","local quadratic"),
       lty=c(1,2,3),cex=.85,lwd=3,col=c("black", "red", "blue"))
rug(rf_lag)
graphics.off()

pdf("riskfree04.pdf",width=7,height=3)    ##  Figure 21.8  ##
par(mfrow=c(1,3))
plot(year[2:n],std_res,type="l",xlab="year",ylab="",main="(a) standardized residuals")
acf(std_res,main="(b) standardized residuals")
acf(std_res^2,main="(c) squared standardized residuals")
graphics.off()


################################################################
########## Code for Example 21.3   #############################
################################################################

dat = read.table("strips_dec95.txt", header = T)
T = dat$T
n = length(T)
ord = order(T)
T = T[ord]
price = dat$price[ord]
Int_F = - log(price) + log(100)
emp_forward = diff(Int_F)/diff(T)
library(mgcv)
X = T[-1]
fit_gam = gam(emp_forward ~ s(X, bs = "cr"))
pred_gam = predict(fit_gam, as.data.frame(X) )
pdf("forward_spline.pdf", width = 6, height = 5)    ##  Figure 21.9  ##
par(mfrow = c(1,1))
plot(X, emp_forward, xlab = "maturity", ylab = "forward rate")
lines(X, pred_gam, col = "red", lwd = 2)
graphics.off()

##################################################
############  Code for R lab       ###############
##################################################
library(AER)
library(mgcv)
data(CPS1988)
attach(CPS1988)
fitGam = gam(log(wage)~s(education)+s(experience)+ethnicity)
summary(fitGam)
par(mfrow=c(1,2))
plot(fitGam)

# #### CKLS, extended  #####
library(Ecdat)
data(Irates)
r1 = Irates[,1]
n = length(r1)
lag_r1 = lag(r1)[-n]
delta_r1 = diff(r1)
n = length(lag_r1)
knots = seq(from=1950,to=1985,length=10)
t = seq(from=1946,to =1991+2/12,length=n)
X1 = outer(t,knots,FUN="-")
X2 = X1 * (X1>0)
X3 = cbind(rep(1,n), (t - 1946),X2)
m2 = dim(X3)[2]
m = m2 - 1

#####  Time-varying drift  #####
nlmod_CKLS_ext = nls(delta_r1 ~ X3[,1:2]%*%a *
                       (X3%*%theta-lag_r1),
                     start=list(theta = c(10,rep(0,m)),
                                a=c(.01,0)),control=list(maxiter=200))
AIC(nlmod_CKLS_ext)
param4 = summary(nlmod_CKLS_ext)$parameters[,1]
par(mfrow=c(1,3))
plot(t,X3%*%param4[1:m2],ylim=c(0,16),ylab="rate",
     main="(a)",col="red",type="l",lwd=2)
lines(t,lag_r1)
legend("topleft",c("theta(t)","lagged rate"),lwd=c(2,1),
       col=c("red","black"))

plot(t,X3[,1:2]%*%param4[(m2+1):(m2+2)],ylab="a(t)",
     col="red",type="l",lwd=2,main="(b)")

res_sq = residuals(nlmod_CKLS_ext)^2
nlmod_CKLS_ext_res <- nls(res_sq ~  A*lag_r1^B,
                          start=list(A=.2,B=1/2) )

plot(lag_r1,sqrt(res_sq),pch=5,ylim=c(0,6),ylab="",main="(c)")
lines(lag_r1,sqrt(fitted(nlmod_CKLS_ext_res)),
      lw=3,col="red",type="l")
legend("topleft",c("abs res","volatility fn"),lty=c(NA,1),
       pch=c(5,NA),col=c("black","red"),lwd=1:2)





