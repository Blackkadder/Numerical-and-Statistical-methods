###########  R script for Chapter 5   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

###################  Code for Figure 5.1  ############################################

x = seq(from=0,to=1,by = .01)
pdf("skewed_densities.pdf")                                          #  Figure 5.1
par(cex.axis=1.5,cex.lab=1.5,mfrow=c(2,2))
plot(x-4/14,dbeta(x,4,10),type="l",ylab="Density(y)",main="(a): Right-skewed",
     xlab="y")
abline(v=0)
plot(x-10/14,dbeta(x,10,4),type="l",ylab="Density(y)",main="(b): Left-skewed",
     xlab="y")
abline(v=0)
plot(x-1/2,dbeta(x,7,7),type="l",ylab="Density(y)",main="(c): Symmetric",
     xlab="y")
abline(v=0)
graphics.off()

##################  Code for Figures 5.2, 5.3, and 5.4 were created by MATLAB  ###########


################################################
#####  Code for figure 5.5  ####################
################################################
pdf("NormMix01.pdf",height=7,width=6)                           #  Figure 5.5
par(mfrow=c(2,2))
x=seq(-6,15,by=.01)
plot(x,dnorm(x,sd=sqrt(3.4)),type="l",lwd=2,ylim=c(0,.4),
     xlab="x",ylab="density",main="(a) densities")
lines(x,.9*dnorm(x)+.1*dnorm(x,sd=5),lwd=2,lty=5,col="red")
legend("topright",c("normal","mixture"),lwd=2,lty=c(1,5),col=c("black","red"))

plot(x,dnorm(x,sd=sqrt(3.4)),type="l",lwd=2,ylim=c(0,.025),xlim=c(4,15),
     xlab="x",ylab="density",main="(b) densities")
lines(x,.9*dnorm(x)+.1*dnorm(x,sd=5),lwd=2,lty=5,col="red")
legend("topright",c("normal","mixture"),lwd=2,lty=c(1,5),col=c("black","red"))
set.seed("7953")
y1 = rnorm(200,sd=sqrt(3.4))
qqnorm(y1,datax=T,main="(c) QQ plot, normal",xlab="theoretical quantiles",
       ylab="sample quantiles")
qqline(y1,datax=T,col="red")
n2=rbinom(1,200,.9)
y2 = c(rnorm(n2),rnorm(200-n2,sd=5))
qqnorm(y2,datax=T,main="(d) QQ plot, mixture",xlab="theoretical quantiles",
       ylab="sample quantiles")
qqline(y2,datax=T,col="red")
graphics.off()

################################################
#####  Code for figure 5.6  ####################
################################################
pdf("tailweights.pdf",height=6,width=6)               #  Figure 5.6
par(mfrow=c(1,1))
library(fGarch)
x = seq(from=1,to=100,by=.01)
plot(x,dged(x,nu=1.5),log="xy",type="l",lwd=2,
     ylab="density",lty=1,ylim=c(1e-20,1e-1),col="green")
lines(x,dged(x,nu=1),type="l",lwd=2,lty=1,col="red")
lines(x,dged(x,nu=1/2),type="l",lwd=2,lty=1,col="blue")
lines(x,dstd(x,nu=4),type="l",lwd=2,lty=2,col="black")
lines(x,dstd(x,nu=15),type="l",lwd=2,lty=2,col="purple")
legend(1,1.7e-12,cex=1.5,
       c(expression(paste("ged, ", nu, "=1.5")),
         expression(paste("ged, ", nu,  "=1")),
         expression(paste("ged, ", nu,  "=1/2")),
         expression(paste("t, ",   nu,  "=4")),
         expression(paste("t, ",   nu,  "=15"))),
       lty=c(1,1,1,2,2),box.lty=0,lwd=2,col=c("green","red","blue","black","purple"))
graphics.off()


################################################
#####  Code for figure 5.7  ####################
################################################   
pdf("GedStPeaks.pdf",height=5,width=6)                    #  Figure 5.7
x = seq(from=-1,to=1,by=.01)
par(mfrow=c(1,1))
plot(x,dged(x,nu=1.5),type="l",lwd=2,
     ylab="density",lty=1,,ylim=c(0,2),col="green")
lines(x,dged(x,nu=1),type="l",lwd=2,lty=1,col="red")
lines(x,dged(x,nu=1/2),type="l",lwd=2,lty=1,col="blue")
lines(x,dstd(x,nu=4),type="l",lwd=2,lty=2,col="black")
lines(x,dstd(x,nu=15),type="l",lwd=2,lty=2,col="purple")
legend(-1,1.7,
       c(expression(paste("ged, ", nu, "=1.5")),
         expression(paste("ged, ", nu,  "=1")),
         expression(paste("ged, ", nu,  "=1/2")),
         expression(paste("t, ",   nu,  "=4")),
         expression(paste("t, ",   nu,  "=15"))),
       lty=c(1,1,1,2,2),box.lty=0,cex=1.2,lwd=2,col=c("green", "red", "blue", "black", "purple"))
graphics.off()


################################################
#####  Code for figure 5.8  ####################
################################################  
pdf("skewT.pdf",height=5,width=6)   
library(fGarch)
xi = 2
x=seq(-5,5,.01)
x2 = seq(0,5,.01)
x3=seq(-5,0,.01)
plot(x,dstd(x,nu=10),type="l",lwd=3,ylim=c(0,.6),ylab="density")
lines(x,dsstd(x,nu=10,xi=xi),lty=5,lwd=3,col="red")
legend("topleft",c("symmetric t","skew t"),
       lty=c(1,5),lwd=c(3,3),box.lty=0,col=c("black", "red"))
graphics.off()

################################################
#####  Code for Example 5.2  ###################
################################################  
midcapD.ts = read.csv("midcapD.ts.csv")
x = 100*as.matrix(midcapD.ts[,-c(1,22)])
train = x[1:250,]
valid = x[-(1:250),]
meansTrain = apply(train,2,mean)
commonmeanTrain = mean(meansTrain)
meansValid = apply(valid,2,mean)
SSseparate = sum((meansTrain-meansValid)^2)
SScommon = sum((commonmeanTrain - meansValid)^2)
SScommonTrain = sum((commonmeanTrain - meansTrain)^2)
SSseparate
SScommon
SScommonTrain

################################################
#####  Code for Example 5.3  ###################
################################################  
data(Capm,package="Ecdat")  
x = diff(Capm$rf)
library(MASS)
fitdistr(x,"t")
library(fGarch)
n = length(x)
start = c(mean(x),sd(x),5)
loglik_t = function(beta) sum( - dt((x-beta[1])/beta[2],
                                    beta[3],log=TRUE) + log(beta[2]) )    
fit_t = optim(start,loglik_t,hessian=T,method="L-BFGS-B",
              lower = c(-1,.001,1))                                 
AIC_t = 2*fit_t$value+2*3
BIC_t = 2*fit_t$value+log(n)*3
sd_t = sqrt(diag(solve(fit_t$hessian)))
options(digits=3)
fit_t$par
sd_t
options(digits=5)
AIC_t
BIC_t
loglik_std = function(beta) sum( - dstd(x,mean=beta[1],
                                        sd=beta[2],nu=beta[3],log=TRUE) )
fit_std = optim(start,loglik_std,hessian=T,method="L-BFGS-B",
                lower = c(-.1,.01,2.1))                                   
AIC_std = 2*fit_std$value+2*3
BIC_std = 2*fit_std$value+log(n)*3
sd_std = sqrt(diag(solve(fit_std$hessian)))
fit_std$par
sd_std
AIC_std
BIC_std
0.04585 * sqrt(3.332/(3.332-2))


################################################
#####  Code for Example 5.4  ###################
################################################  
loglik_sstd = function(beta) sum( - dsstd(x,mean=beta[1],
   sd=beta[2],nu=beta[3],xi=beta[4],log=TRUE) )
start = c(mean(x),sd(x),5,1)
fit_sstd = optim(start,loglik_sstd,hessian=T,method="L-BFGS-B",
   lower = c(-.1,.01,2.1,-2))                                   
AIC_sstd = 2*fit_sstd$value+2*4
BIC_sstd = 2*fit_sstd$value+log(n)*4
sd_sstd = sqrt(diag(solve(fit_sstd$hessian)))
fit_sstd$par
sd_sstd
AIC_sstd
BIC_sstd


################################################
#####  Code for Example 5.5  ###################
################################################ 
loglik_ged = function(beta) sum( - dged(x,mean=beta[1],
  sd=beta[2],nu=beta[3],log=TRUE) )
start = c(mean(x),sd(x),1)
fit_ged = optim(start,loglik_ged,hessian=T,method="L-BFGS-B",
   lower = c(-.1,.01,1))                                   
AIC_ged = 2*fit_ged$value+2*4
BIC_ged = 2*fit_ged$value+log(n)*4
sd_ged = sqrt(diag(solve(fit_ged$hessian)))
fit_ged$par
sd_ged
AIC_ged
BIC_ged
################################################
#####  Code for Example 5.9  ###################
################################################ 
pdf("diffrf_kde.pdf",width=6,height=4)        
library(Ecdat)
data(Capm)
diffrf=diff(Capm$rf)
par(mfrow=c(1,1))
plot(density(diffrf,kernel="gaussian",adjust=1),xlim=c(-.3,.3),
     main="",lwd=2,cex.lab=1.5,cex.axis=1.5,cex.main=1.5,
     ylim=c(0,7.6),xlab="change in return")
graphics.off()

################################################
#####  Code for Figure 5.10  ###################
################################################ 

## The normal mixture model in panel (c) was fit in the first edition.
## It was omitted in the second edition since, by BIC,  it did not fit better than the t model.

pdf("QQ_t_ged_nm_riskfree.pdf",height=2.5,width=6)    
library(Ecdat)
data(Capm)
diffrf=diff(Capm$rf) 
x = diffrf
n = length(x)
options(digits=4)
loglik_std = function(x) {
  f = -sum(log(dstd(diffrf, x[1], x[2], x[3])))
  f
}

loglik_ged = function(x) {
  f = -sum(log(dged(diffrf, x[1], x[2], x[3])))
  f
}
start=c(mean(diffrf),sd(diffrf),4)
fit_std = optim(start,loglik_std,method="L-BFGS-B",
                lower=c(-.1,.001,2.1),
                upper=c(.1,1,10),
                control=list(maxit=1000))
start=c(mean(diffrf),sd(diffrf),1)
fit_ged = optim(start,loglik_ged,method="L-BFGS-B",
                lower=c(-.1,.001,.1),
                upper=c(.1,1,10),
                control=list(maxit=1000))
grid = (1:n)/(n+1)

par(mfrow=c(1,3))
qqplot(sort(x), qstd(grid,mean=fit_std$par[1],
                     sd=fit_std$par[2],
                     nu=fit_std$par[3]),
       xlab="Data",ylab="t-quantiles",
       main="(a) t model")
abline(0,1,col="red",lwd=2)
qqplot(sort(x), qged(grid,mean=fit_ged$par[1],
                     sd=fit_ged$par[2],
                     nu=fit_ged$par[3]),
       xlab="Data",ylab="ged-quantiles",  main="(b) ged model")
abline(0,1,col="red",lwd=2)
sig2 = c(0.047558^2, 0.047558^2 +(0.10819 )^2) 
library(nor1mix)
obj = norMix(mu = c(0.0018950,0.0018950),
             sigma = sqrt(sig2), w = c(0.878, 1-0.878))
t=(1:n)/(n+1)
quants = qnorMix(t,obj)
qqplot(sort(x),quants,xlab="Data",
       ylab="Normal-mixture quantiles",   main="(c) Normal mixture")
abline(0,1,col="red",lwd=2)
graphics.off()

################################################
#####  Code for Example 5.6  ###################
################################################ 

pdf("FlowSkewTAzzalini.pdf",width=6,height=6)   ###  Figure 5.11
library(sn)
dat=read.csv("FlowData.csv")
dat = dat/10000
par(mfrow=c(3,2),cex.lab=1,cex.axis=1,cex.main=1)
x=dat$Flow1
x1 = sort(x)
fit1 = sn.mple(y=x1,x = as.matrix(rep(1,length(x1)) ))
est1 = cp2dp(fit1$cp,family="SN")
plot(x1,dsn(x1, dp=est1),
     type="l",lwd=2,xlab="flow",ylab="flow 1 density")
d = density(x1)
lines(d$x,d$y,lty=2,lwd=2,col="red")
legend(40,.034,c("t-model","KDE"),lty=c(1,2),lwd=c(2,2),cex=1,col=c("black", "red"))
n = length(x1)
u=(1:n)/(n+1)
plot(x1,qsn(u, dp=est1),
     xlab="data",ylab="skew-t quantiles",main="Flow 1")
lmfit = lm( qsn(c(.25,.75),dp=est1) ~ quantile(x1,c(.25,.75)) )
abline(lmfit,col="red",lwd=2)
x=dat$Flow2
x2 = sort(x)
fit2 = sn.mple(y=x2)
est2 = cp2dp(fit2$cp,family="SN")
plot(x2,dsn(x2, dp=est2),
     type="l",lwd=2,xlab="flow",ylab="flow 2 density",ylim=c(0,.055))
d = density(x2)
lines(d$x,d$y,lty=2,lwd=2,col="red")
legend(162,.051,c("t-model","KDE"),lty=c(1,2),lwd=c(2,2),cex=1,col=c("black", "red"))
plot(x2,qsn(u, dp=est2),
     xlab="data",ylab="skew-t quantiles",main="Flow 2")
lmfit = lm( qsn(c(.25,.75),dp=est2) ~ quantile(x2,c(.25,.75)) )
abline(lmfit,col="red", lwd=2)
x3=dat$Flow3
x3 = sort(x3)
fit3 = sn.mple(y=x3)
est3= cp2dp(fit3$cp,family="SN")
est3[3] = -29  ## The large negative value of this parameter was creating numerical problems
plot(x3,dsn(x3, dp=est3),
     type="l",lwd=2,xlab="flow",ylab="flow 3 density",ylim=c(0,.056))
d = density(x3)
lines(d$x,d$y,lty=2,lwd=2,col="red")
legend(3,.0575,c("t-model","KDE"),lty=c(1,2),lwd=c(2,2),cex=1,col=c("black", "red"))
plot(x3,qsn(u, dp=est3),
     xlab="data",ylab="skew-t quantiles",main="Flow 3")
lmfit = lm( qsn(c(.25,.75),dp=est3) ~ quantile(x3,c(.25,.75)) )
abline(lmfit,col="red", lwd=2)
graphics.off()


################################################
#####  Code for Example 5.7  ###################
################################################ 
dat=read.csv("FlowData.csv")
dat = dat/10000
library("MASS")
adj = 1.5
pdf("GasFlowsTransProfile.pdf", width = 5, height = 6) ## Figure 5.12
par(mfrow=c(3,3))
x = dat$Flow1
x1 = sort(x)
bcfit1 = boxcox(x1~1,lambda=seq(2.6, 4.5, 1/100),
                xlab=expression(alpha))
text(3,-1898.75,"Flow 1")
plot(density((x1^3.5-1)/3.5,adjust=adj),main="Flow 1")
qqnorm((x1^3.5-1)/3.5,datax=T,main="Flow 1")
x=dat$Flow2
x2 = sort(x)
bcfit2 = boxcox(x2~1,lambda=seq(8, 13.5, 1/100),xlab=expression(alpha))
text(8.5,-1776.5,"Flow 2")
plot(density( (x2^10.5-1)/10.5,adjust=adj),main="Flow 2")
qqnorm((x2^10.5-1)/10.5,datax=T,main="Flow 2")
x=dat$Flow3
x3 = sort(x)
bcfit3 = boxcox(x3~1,lambda=seq(.6, 2, 1/100),xlab=expression(alpha))
text(1.6,-1793,"Flow 3")
plot(density( (x3^1.235-1)/1.235,adjust=adj),main="Flow 3")
qqnorm((x3^1.235-1)/1.235,datax=T,main="Flow 3")
graphics.off()










#######################################################
#####  Code for Figure 5.13 - 5.16  ###################
#######################################################

pdf("diffrf_TKDE.pdf",width=6,height=5)       ##### Figure 5.13  #####            
data(Capm,package="Ecdat")
y=diff(Capm$rf)
diffrf=y
library(fGarch)
x1 = pstd(y,mean = 0.001, sd = .0725, nu = 3.34)
x = qnorm(x1)
par(mfrow=c(1,1))
d1 = density(diffrf)
plot(d1$x,d1$y,type="l",xlab="y",ylab="Density(y)",lwd=2)
d2 = density(x)
ginvx = qstd(pnorm(d2$x), mean = 0.001, sd = .0725, nu = 3.34)
gprime_num = dstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)
gprime_den = dnorm(qnorm(pstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)))
gprime = gprime_num/gprime_den
lines(ginvx,d2$y*gprime,type="l",lty=2,col="red",lwd=2)
legend("topleft",c("KDE","TKDE"),lty=c(1,2),lwd=2,cex=1.5,col=c("black", "red"))
graphics.off()

pdf("diffrf_TKDE_detail.pdf",width=6,height=5)                 ##### Figure 5.14  #####
d1 = density(diffrf)
plot(d1$x,d1$y,type="l",xlab="y",ylab="Density(y)",xlim=c(-.5,-.1),
     ylim=c(0,1),lwd=2)
d2 = density(x)
ginvx = qstd(pnorm(d2$x), mean = 0.001, sd = .0725, nu = 3.34)
gprime_num = dstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)
gprime_den = dnorm(qnorm(pstd(ginvx,mean = 0.001, sd = .0725, nu = 3.34)))
gprime = gprime_num/gprime_den
lines(ginvx,d2$y*gprime,type="l",lty=2,col="red",lwd=2)
legend("topleft",c("KDE","TKDE"),lty=c(1,2),lwd=2,cex=1.5,col=c("black","red"))
graphics.off()

pdf("diffrf_TKDE_trans.pdf",width=6,height=5)                  ##### Figure 5.15  #####
ygrid  = seq(min(y),max(y),.01)
plot(ygrid, qnorm(pstd(ygrid,mean = 0.001, sd = .0725, nu = 3.34)  ),
     type="l",lwd=2,xlab="y",ylab="g(y)")
graphics.off()

pdf("diffrf_TKDE_QQ.pdf",width=6,height=5)                      ##### Figure 5.16  #####
qqnorm( qnorm(pstd(jitter(y,factor=2),mean = 0.001, sd = .0725, nu = 3.34)),
        datax=T )
graphics.off()



##################  Start R lab  ##################

#################$  Earnings data  #############
library("Ecdat")
?CPSch3
data(CPSch3)
dimnames(CPSch3)[[2]]

male.earnings = CPSch3[CPSch3[,3]=="male",2]
sqrt.male.earnings = sqrt(male.earnings)
log.male.earnings = log(male.earnings)

par(mfrow=c(2,2))
qqnorm(male.earnings,datax=T,main="untransformed")
qqnorm(sqrt.male.earnings,datax=T,main="square-root transformed")
qqnorm(log.male.earnings,datax=T,main="log-transformed")

par(mfrow=c(2,2))
boxplot(male.earnings,main="untransformed")
boxplot(sqrt.male.earnings,main="square-root transformed")
boxplot(log.male.earnings,main="log-transformed")

par(mfrow=c(2,2))
plot(density(male.earnings),main="untransformed")
plot(density(sqrt.male.earnings),main="square-root transformed")
plot(density(log.male.earnings),main="log-transformed")


library("MASS")
par(mfrow=c(1,1))
boxcox(male.earnings~1)
boxcox(male.earnings~1,lambda = seq(.3, .45, 1/100))
bc = boxcox(male.earnings~1,lambda = seq(.3, .45, by=1/100),interp=F)
ind = (bc$y==max(bc$y))
ind2 = (bc$y > max(bc$y) - qchisq(.95,df=1)/2)
bc$x[ind]
bc$x[ind2]

library("fGarch")
fit = sstdFit(male.earnings,hessian=T)


################  DAX returns  ###########

data(Garch,package="Ecdat")
library("fGarch")
data(EuStockMarkets)
Y = diff(log(EuStockMarkets[,1]))  #  DAX

#####  std  #####
loglik_std = function(x) {
  f = -sum(dstd(Y, x[1], x[2], x[3],log=TRUE))
f}
start=c(mean(Y),sd(Y),4)
fit_std = optim(start,loglik_std,method="L-BFGS-B",
                lower=c(-.1,.001,2.1),
                upper=c(.1,1,20))
cat("MLE =", round(fit_std$par,digits = 5))
minus_logL_std = fit_std$value  # minus the log-likelihood
AIC_std = 2*minus_logL_std+2*length(fit_std$par)


############### R lab  ###################

###  Problem 8  ###

library("fGarch")
data(EuStockMarkets)
Y = diff(log(EuStockMarkets[,1]))  #  DAX
X = qnorm(pstd(Y,mean=fit_std$par[1],sd=fit_std$par[2],
               nu=fit_std$par[3]))  #  transformed data
d2 = density(X)  # KDE of transformed data
ginvx = qstd(pnorm(d2$x), mean = fit_std$par[1],
             sd =  fit_std$par[2], nu =  fit_std$par[3]) # inverse transformation
gprime_num = dstd(ginvx, mean = fit_std$par[1],
                  sd =  fit_std$par[2],
                  nu =  fit_std$par[3]) # numerator of derivative of transformation
gprime_den = dnorm(qnorm(pstd(ginvx, mean = fit_std$par[1],
                              sd =  fit_std$par[2],
                              nu =  fit_std$par[3]))) # denominator of derivative of transformation
gprime = gprime_num/gprime_den  # derivative of transformation
plot(ginvx,d2$y*gprime,type="l",lwd=2)  # plot KDE of untransformed data

###  Problem 9  ###

d1 = density(Y)  #  KDE of untransformed data
plot(d1$x,d1$y,type="l",xlab="y",ylab="Density(y)",xlim=c(.035,.06),
     ylim=c(0,.8),lwd=2)  # plot KDE of untransformed data, only right tail
x =seq(-.09,.09,by = 0.001)
lines(x,dstd(x,fit_std$par[1], fit_std$par[2],
             fit_std$par[3]),lty=3,lwd=2) # plot parametric fit
lines(ginvx,d2$y*gprime,type="l",lty=2,lwd=2) # plot TKDE
legend("topright",c("KDE","TKDE","PARAM"),lty=c(1,2,3),lwd=2)


###  Problem 10  ###

library("fGarch")
data(EuStockMarkets)
Y = diff(log(EuStockMarkets[,4]))  #  FTSE
loglik_sstd = function(x) {
  f = -sum(dsstd(Y,x[1],x[2],x[3],xi=x[4],log=TRUE))
  f}
start=c(mean(Y),sd(Y),4,1)
fit_sstd = optim(start,loglik_sstd,method="L-BFGS-B",
                lower=c(-.1,.001,2.1,.1),
                upper=c(.1,1,20,10),hessian=TRUE)
minus_logL_sstd = fit_sstd$value  # minus the log-likelihood
AIC_sstd = 2*minus_logL_sstd+2*length(fit_sstd$par)
se = sqrt(diag(solve(fit_sstd$hessian)))
print(c("MLE =",round(fit_sstd$par,digits=5)))
print(c("SE =",round(se,digits=5)))
print(c("AIC =",round(AIC_sstd,digits=5)))


################  McDonald's  #################

data = read.csv('MCD_PriceDaily.csv')
adjPrice = data[ ,7]
LogRet = diff(log(adjPrice))
library(MASS)
library(fGarch)
fit.T = fitdistr(LogRet, "t") 
params.T = fit.T$estimate
mean.T = params.T[1]
sd.T = params.T[2] * sqrt(params.T[3] / (params.T[3] - 2))
nu.T = params.T[3]
x = seq(-0.04, 0.04, by = 0.0001)
hist(LogRet, 80, freq = FALSE)
lines(x, dstd(x, mean = mean.T, sd = sd.T, nu = nu.T), 
      lwd = 2, lty = 2, col = 'red')

################################################
###############  Exercises  ####################
################################################


###########  Exercise 11  #####

set.seed(583)
x = runif(250,1,3)
y = rpois(250,lambda = exp(1+x/2))
start = c(1, 1)
loglik = function(theta) {-sum(log(dpois(y,lambda=exp(theta[1]+
   theta[2]*x))))}
mle= optim(start,loglik,hessian=T)
invFishInfo = solve(mle$hessian)
options(digits=4)
mle$par
mle$value
mle$convergence
sqrt(diag(invFishInfo))
