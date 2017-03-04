###########  R script for Chapter 7   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

############  Example 7.1  ################
data(CRSPday,package="Ecdat")
ge = CRSPday[,4]
ibm = CRSPday[,5]
options(digits = 3)
cov(CRSPday[,4:7])
cor(CRSPday[,4:7])


##################################################
############  Code for Figure 7.1  ###############
##################################################

pdf("CRSPday_scatterplot.pdf",width=7,height=7)  ##  Figure 7.1  ##
data(CRSPday,package="Ecdat")
plot(as.data.frame(CRSPday[,4:7]))
graphics.off()

##################################################
############  Code for Figure 7.2  ###############
##################################################
pdf("bivariate_normal_contours.pdf",width=8,height=4.5)   ##  Figure 7.2  ##
library("mnormt")
mu = rep(0,2)
cov1 = matrix(c(1,.5,.5,1),nrow=2)
cov2 = matrix(c(1,-.95,-.95,1),nrow=2)
t = seq(-2.5,2.5,.025)
ff1 <- function(x,x2){dmnorm(cbind(x,x2),mu,cov1)}
ff2 <- function(x,x2){dmnorm(cbind(x,x2),mu,cov2)}
par(mfrow=c(1,2))
contour(t,t,outer(t,t,ff1),xlab=expression(Y[1]),ylab=expression(Y[2]),
        cex.lab=1,cex.axis=1,labcex=.8,main="(a) corr = 0.5" )
contour(t,t,outer(t,t,ff2),xlab=expression(Y[1]),ylab=expression(Y[2]),
        cex.lab=1,cex.axis=1,labcex=1,main="(b) corr = -0.95" ,
        drawlabels=F)
graphics.off()

##################################################
############  Code for Figure 7.3  ###############
##################################################
pdf("MultiT_IndT.pdf",width=8,height=4.5)  ##  Figure 7.3  ##
library(sn)
n = 2500
df= 3
set.seed(5600)
x = rmt(n,mean=rep(0,2),S=matrix(c(1,0,0,1),nrow=2),df=df)
y1 = rt(n,df=df)
y2 = rt(n,df=df)
par(mfrow=c(1,2))
plot(x[,1],x[,2],main="(a) Multivariate-t",
     xlab=expression(Y[1]),ylab=expression(Y[2]))
abline(h=0)
abline(v=0)
plot(y1,y2,main="(b) Independent t",
     xlab=expression(Y[1]),ylab=expression(Y[2]))
abline(h=0)
abline(v=0)
graphics.off()

##################################################
############  Code for Figure 7.4  ###############
##################################################
pdf("midcapDPairs.pdf",width=6,height=6)  ##  Figure 7.4  ##
midcapD.ts = read.csv("midcapD.ts.csv")
x = midcapD.ts 
market = 100*as.matrix(x[,22])
x = as.matrix(x[,-c(1,22)])
pairs(x[,1:6])
graphics.off()

#####################################################################
############  Code for Example 7.4 and Figure 7.5     ###############
#####################################################################
pdf("CRSPday_MultiT_profile.pdf")        #####  Figure 7.5  #####
library(mnormt)
library(MASS)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]
#  Fitting symmetric t by profile likelihood
df = seq(5.25,6.75,.01)
n = length(df)
loglik = rep(0,n)
for(i in 1:n){
  fit = cov.trob(dat,nu=df)
  loglik[i] = sum(log(dmt(dat,mean=fit$center,S=fit$cov,df=df[i])))
}
options(digits=7)
aic_t = -max(2*loglik)+ 2*(4 + 10 + 1) + 64000
z1 = (2*loglik > 2*max(loglik) - qchisq(.95,1))
par(mfrow = c(1,1))
plot(df,2*loglik-64000,type="l",cex.axis=1.5,cex.lab=1.5,
     ylab="2*loglikelihood - 64,000",lwd=2)
abline(h = 2*max(loglik) - qchisq(.95,1)-64000)
abline(h = 2*max(loglik)-64000 )
abline(v=(df[16]+df[17])/2)
abline(v=(df[130]+df[131])/2)
graphics.off()
options(digits=4)
cov.trob(dat,nu=6,cor=TRUE)
options(digits=4)
cor(dat)

###########################################################################
############  Code for Section 7.8 including Figure 7.6     ###############
###########################################################################
pdf("bivariate_t.pdf",width=5,height=5.65)
par(mfrow=c(1,1))
t = seq(-3,3,.025)
df=4
Omega = matrix(c(2,1.1,1.1,1),nrow=2)
ei = eigen(Omega)
O = ei$vectors
ff3 <- function(x,x2){ dmt(cbind(x,x2),mean=c(0,0),
                           df=df,S=Omega ) }

contour(t,t,outer(t,t,ff3),xlab=expression(X[1]),ylab=expression(X[2]),
        cex.lab=1,cex.axis=1,labcex=1,main="multivariate t",
        drawlabels=T,cex.lab=1,cex.axis=1,
        pin=c(3,5))
kk =5
lines(c(0,kk*O[1,1]),c(0,kk*O[2,1]),lwd=2)
kk=-5
lines(c(0,kk*O[1,1]),c(0,kk*O[2,1]),lwd=2)
kk =5
lines(c(0,kk*O[1,2]),c(0,kk*O[2,2]),lty=2,lwd=2)
kk=-5
lines(c(0,kk*O[1,2]),c(0,kk*O[2,2]),lty=2,lwd=2)
graphics.off()
options(digits=3)
eigen(Omega)

#####################################################
############  Code for Figure 7.7     ###############
#####################################################
pdf("bivariate_skew_t.pdf",width=5,height=5)  ##  Figure 7.7
library(sn)
t = seq(-2.5,2.5,.025)
dp = list("xi"=c(0,0),"Omega"=diag(c(1,1)),"alpha"=c(-1,.25),"nu"=4)
ff3 <- function(x,x2){dmst(cbind(x,x2),dp=dp )}
contour(t,t,outer(t,t,ff3),xlab=expression(X[1]),ylab=expression(X[2]),
        cex.lab=1,cex.axis=1,labcex=1,main=expression( paste("multivariate skewed t:  ",
        alpha[1]," = -1, ", alpha[2]," = 0.25") ),
        drawlabels=T,cex.lab=1,cex.axis=1)
abline(h=-2,lty=2)
abline(h=0,lty=2)
abline(h=2,lty=2)
abline(v=-2,lty=2)
abline(v=0,lty=2)
abline(v=2,lty=2)
graphics.off()

##################################################
############  Code for Example 7.5   ###############
##################################################
library(sn)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]
fit = mst.mple(y=dat,penalty=NULL, control=list(iter.max=500, eval.max=750))
aic_skewt = -2*fit$logL + 64000 + 2*(4 + 10 + 4 + 1)
options(digits = 5)
dp2cp(fit$dp,"st")
aic_skewt
fit$dp

##################################################
############  Code for Figure 7.8  ###############
##################################################
pdf("CRSPNormalPlots.pdf",width=6,height=5)  ##  Figure 7.8  ##
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]
par(mfrow=c(2,2))
qqnorm(dat[,1],datax=T,main="GE")
qqline(dat[,1],datax=T,col="red",lwd=2)
qqnorm(dat[,2],datax=T,main="IBM")
qqline(dat[,2],datax=T,col="red", lwd=2)
qqnorm(dat[,3],datax=T,main="CRSP")
qqline(dat[,3],datax=T,col="red", lwd=2)
qqnorm(dat[,4],datax=T,main="CRSP")
qqline(dat[,4],datax=T,col="red",lwd=2)
graphics.off()



########################################################################
############  Code for Example 7.7 including Figure 7.9  ###############
########################################################################
####  Warning: resampling can take a while.
####  This example took 3 minutes on a MacBook Pro with a 2.9 GHz Intel Core i7 processor #####
pdf("CRSPSkewTBootAlpha.pdf",width=6,height=5)
data(CRSPday,package="Ecdat")
dat =CRSPday[,4:7]
n = dim(dat)[1]
nboot = 200
results=matrix(1,nrow=nboot,ncol=4)
set.seed(5640)
library(sn)
t1=proc.time()
for (iboot in 1:nboot)
{
  yboot = dat[sample((1:n),n,replace =T),]
  fitboot = mst.mple(y=yboot,penalty=NULL)
  results[iboot,] = as.numeric(fitboot$dp$alpha)
}
t2=proc.time()
(t2-t1)/60
par(mfrow=c(2,2))
hist(results[,1],main="GE",xlab=expression(alpha))
hist(results[,2],main="IBM",xlab=expression(alpha))
hist(results[,3],main="Mobil",xlab=expression(alpha))
hist(results[,4],main="CRSP",xlab=expression(alpha))
graphics.off()

library(e1071) #### necessary for functions skewness and kurtosis ####
for (i in 1:4){ print(quantile(results[,i],c(.025,.975))) }
for (i in 1:4){
  print(skewness(results[,i]))
}
for (i in 1:4){
  print(kurtosis(results[,i]))
}

####################################################
############ R lab - Equity Returns ################
####################################################
berndtInvest = read.csv("berndtInvest.csv")
Berndt = as.matrix(berndtInvest[,2:5])
cov(Berndt)
cor(Berndt)
pairs(Berndt)
library(MASS)  # needed for cov.trob
library(mnormt)  # needed for dmt
df = seq(2.5,8,.01)
n = length(df)
loglik_profile = rep(0,n)
for(i in 1:n)
{
  fit = cov.trob(Berndt,nu=df[i])
  mu = as.vector(fit$center)
  sigma =matrix(fit$cov,nrow=4)
  loglik_profile[i] = sum(log(dmt(Berndt,mean=fit$center,
      S = fit$cov,df=df[i])))
}

############  R lab - simulating multivariate t-distributions  ################

library(MASS)  # need for mvrnorm
par(mfrow=c(1,4))
N =  2500
nu = 3

set.seed(5640)
cov=matrix(c(1,.8,.8,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w,w)
plot(x,main="(a)")

set.seed(5640)
cov=matrix(c(1,.8,.8,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N, df=nu))
w2 = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w1,w2)
plot(x,main="(b)")

set.seed(5640)
cov=matrix(c(1,0,0,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w1 = sqrt(nu/rchisq(N, df=nu))
w2 = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w1,w2)
plot(x,main="(c)")

set.seed(5640)
cov=matrix(c(1,0,0,1),nrow=2)
x= mvrnorm(N, mu = c(0,0), Sigma=cov)
w = sqrt(nu/rchisq(N, df=nu))
x = x * cbind(w,w)
plot(x,main="(d)")


############  R lab - Fitting a bivariate t-distribution  ################

library(mnormt)
data(CRSPday,package="Ecdat")
Y = CRSPday[,c(5,7)]
loglik = function(par)
{
  mu = par[1:2]
  A = matrix(c(par[3],par[4],0,par[5]),nrow=2,byrow=T)
  scale_matrix = t(A)%*%A
  df = par[6]
  f = -sum(log(dmt(Y, mean=mu,S=scale_matrix,df=df)))
  f
}
A=chol(cov(Y))
start=as.vector(c(apply(Y,2,mean),A[1,1],A[1,2],A[2,2],4))
fit_mvt = optim(start,loglik,method="L-BFGS-B",
   lower=c(-.02,-.02,-.1,-.1,-.1,2),upper=c(.02,.02,.1,.1,.1,15),hessian=T)


