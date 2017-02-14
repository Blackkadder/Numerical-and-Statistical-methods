###########  R script for Chapter 11  ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################


###########################################################
#####  Code for example 11.1 and figure 11.1          #####
###########################################################
bondprices = read.table("bondprices.txt",header=T)
attach(bondprices)
fit = nls(price~1000*exp(-r*maturity),start=list(r=.04))
summary(fit)
pdf("nonl_regr_bonds.pdf",width=6,height=5)
par(mfrow=c(1,1))
plot(maturity,price,pch="*",cex = 2)
grid = seq(0, 20, length=201)
price_grid = 1000*exp(-0.0585*grid)
lines(grid,price_grid, lwd = 2, col = "red")
#legend("topright",c("price","predicted price"),pch=c("*",NA),
#       col = c("black","red"), lty=c(NA,1),pt.cex=c(2,1))
graphics.off()
detach(bondprices)

#############################################################
#####  Code for examples 11.2 and 11.4 and figure 11.2  #####
#############################################################
DefaultData = read.table("DefaultData.txt",header=T)
attach(DefaultData)
freq2=freq/100
y = log(freq2[freq2>0])
fit_bow = lm(y ~ rating[freq>0])
fit_nls = nls(freq2 ~ exp(b1+b2*rating), start=list(b1=-5,b2=.5))
fit_tbs = nls(sqrt(freq2) ~ exp(b1/2+b2*rating/2), start=list(b1=-6,b2=.5))
sum_nls = summary(fit_nls)
coef_nls = as.numeric(sum_nls$coef[1:2])
sum_tbs = summary(fit_tbs)
coef_tbs = as.numeric(sum_tbs$coef[1:2])
rate_grid = seq(1,16,by=.01)
coef_bow = fit_bow$coefficients

pdf("DefaultProb.pdf",width=8,height=4)  ##  figure 11.2
par(mfrow=c(1,2))
plot(rating,freq2,ylim=c(-.0001,.13),pch="*",ylab="frequency",cex=1.5)
lines(rate_grid,exp( coef_nls[1]+coef_nls[2]*rate_grid))
legend("topleft",c("exponential","data"),lty=c(1,NA),
       pch=c("","*"),pt.cex=c(1, 1.5))
plot(rate_grid, (coef_bow[1]+rate_grid*coef_bow[2]),
     type="l",ylim=c(-14.15,1),xlab="rating",ylab="log(default probability)")
lines(rate_grid,( coef_nls[1]+coef_nls[2]*rate_grid) ,lty=2,col="red")
lines(rate_grid,( coef_tbs[1]+coef_tbs[2]*rate_grid) ,lty=6,col="blue")
points(rating,log(freq2+1e-6))
legend("topleft",c("BOW","nonlinear","tbs","data"),lty=c(1,2,6,NA),
       pch=c("","","","o"),col=c("black","red","blue"))
graphics.off()

pdf("DefaultProbResid1.pdf",width=7,height=3.5)  ##  figure 11.3
par(mfrow=c(1,2))
fitted_nls = -sum_nls$resid+freq2
plot(fitted_nls,abs(sum_nls$resid),xlab="fitted values",
     ylab="absolute residual")
fit_loess  = loess(abs(sum_nls$resid)~ fitted_nls,span=1,deg=1)
ord_nls = order(fitted_nls)
lines(fitted_nls[ord_nls],fit_loess$fit[ord_nls])
qqnorm(sum_nls$resid,datax=T,main="",ylab="sample quantiles",
       xlab="theoretical quantiles")
qqline(sum_nls$resid,datax=T)
graphics.off()


#############################################################
#####  Code for figures 11.4 and 11.5 and example 11.3  #####
#############################################################
dat = read.table("strips_dec95.txt",header=TRUE)
dat = dat[order(dat$T),]
t = seq(0,30,length=100)
emp = -diff(log(dat$price))/diff(dat$T)

fitQuad = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 ),data=dat,
              start=list(beta0=.03,beta1=0,beta2=0))
coefQuad = summary(fitQuad)$coef[,1]
forwardQuad = coefQuad[1] + (coefQuad[2]*t) + (coefQuad[3]*t^2) 

fitCubic = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 - (beta3*T^4)/4 ),data=dat,
               start=list(beta0=.03,beta1=0,beta2=0,beta3=0))
coefCubic = summary(fitCubic)$coef[,1]
forwardCubic = coefCubic[1] + (coefCubic[2]*t) + (coefCubic[3]*t^2) +(coefCubic[4]*t^3)

fitSpline = nls(price~100*exp(-beta0*T - (beta1*T^2)/2 - (beta2*T^3)/3 
                              - (T>15)*(beta3*(T-15)^3)/3 ),data=dat,
                start=list(beta0=.047,beta1=0.0024,beta2=0,beta3=-0.00007) )
coefSpline = summary(fitSpline)$coef[,1]
forwardSpline = coefSpline[1] + (coefSpline[2]*t)  + (coefSpline[3]*t^2) +  
    (t>15)*(coefSpline[4]*(t-15)^2)

pdf("empirical.pdf", width = 8, height = 4.5)  ##  figure 11.4  ##
par(mfrow=c(1,2))
plot(dat$T,dat$price,xlab="maturity",ylab="price",main="(a)",cex=.75)
plot(dat$T[2:length(dat$T)],emp,,ylim=c(0.025,.075),lwd=2,xlab="maturity",
     ylab="empirical forward rate",type="b",cex=.75,
     main="(b)")
graphics.off()

pdf("stripsNL01.pdf", width = 6, height = 5)  ##  figure 11.5  ##
par(mfrow=c(1,1))
plot(t,forwardQuad,type="l",ylim=c(0.025,.075),lwd=2,xlab="maturity",ylab="forward rate")
lines(t,forwardCubic,lty=3,lwd=3,col="red")
lines(t,forwardSpline,lty=2,lwd=3,col="blue")
points(dat$T[2:length(dat$T)],emp,pch="*",cex=1.5)
legend("bottomleft",c("quadratic","cubic","spline","empirical"),
       lty=c(1,3,2,NA),lwd=c(2,3,3),pt.cex=1.5,pch=c(NA,NA,NA,"*"),
       cex=1.2,col=c("black","red","blue") )
graphics.off()

summary(fitSpline)

pdf("DefaultProbResid2.pdf",width=7,height=3.5)  ##  figure 11.6
par(mfrow=c(1,2))
fitted_tbs = -sum_tbs$resid+sqrt(freq2)
plot(fitted_tbs,abs(sum_tbs$resid),xlab="fitted values",ylab="absolute residual")
fit_loess_tbs  = loess( abs(sum_tbs$resid)~ fitted_tbs,span=1,deg=1)
ord_tbs = order(fitted_tbs)
lines(fitted_tbs[ord_tbs],fit_loess_tbs$fit[ord_tbs])
qqnorm(sum_tbs$resid,datax=T,main="")
qqline(sum_tbs$resid,datax=T)
graphics.off()




##############################################################
#####  Code for example 11.6 and figures 11.7 and 11.8   #####
##############################################################
n = 80
set.seed("781235")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)
y = (8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 
fit = lm(y~x1+x2)
rstudent = rstudent(fit)
pdf("simDataBoxCox.pdf",width=6,height=5)  ##  figure 11.7  ##
library(MASS)
par(mfrow=c(1,1))
boxcox(y~poly(x1,2)+x2,ylab="log-likelihood")
graphics.off()
yinv = -1/y
lm_bc = lm(yinv~poly(x1,2)+x2)
rstudent=rstudent(lm_bc)

pdf("simDataBoxCoxResid.pdf",width=6,height=6)  ##  figure 11.8
par(mfrow=c(2,2))
plot(lm_bc$fitted,rstudent,xlab="fitted values",main="(a)")
plot(x1,rstudent,main="(b)",xlab=expression(x[1]))
plot(x2,rstudent,main="(c)",xlab=expression(x[2]))
qqnorm(rstudent,datax=T,main="(d)",xlab="theoretical quantiles",
       ylab="sample quantiles")
qqline(rstudent,datax=T)
graphics.off()

###############################################################
#####  Code for example 11.7 and figures 11.9 and 11.10   #####
###############################################################
library("AER")
library("faraway")
library(MASS)
data("CreditCard") 
CreditCard_clean = CreditCard[CreditCard$age>18,]
attach(CreditCard_clean)
names(CreditCard)
fit1= glm(card~log(reports+1)+income+log(share)+age+owner+dependents+months,
          family="binomial",data=CreditCard_clean)
summary(fit1)
stepAIC(fit1)

pdf("CreditCardHist.pdf",width=5.5,height=5.5)  ###  Figure 11.9
par(mfrow=c(3,3)) 
hist(reports,main="reports")
hist(income, main="income")
hist(share, main="share")
hist(age, main="age")
owner2 = c( sum((owner=="yes")),sum((owner=="no")))
hist(as.numeric(owner), main="owner",
     breaks=2,xlab=" no   yes",axes=F,ylab="")
h=hist(dependents,main="dependents",breaks=(0:7)-.5)
hist(months,main="months")
hist(log(share),main="log(share)")
hist(log(reports+1),main="log(reports+1)")
graphics.off()

log_reports_c = log(reports+1)
log_reports_c = log_reports_c - mean(log_reports_c)
income_c = income - mean(income)
log_share_c = log(share) - mean(log(share))
dependents_c = dependents - mean(dependents)
glm_fit02 = glm(card~log_reports_c+income_c+log_share_c+dependents_c,
                family="binomial",data=CreditCard_clean)
summary(glm_fit02)
income_grid = seq(min(income),max(income),.01)
share_grid = exp(seq(min(log(share)),max(log(share)),.01))
share_grid2 = log(share_grid) - mean(log(share))
reports_grid = 0:14
reports_grid2 = log(reports_grid+1) - mean(log(reports+1))

pdf("CreditCards_effects.pdf",width=6,height=5)  ## figure 11.10  ##
par(mfrow=c(2,2))
plot(reports_grid, plogis(9.5238 -2.8953 * reports_grid2 ),type="b",
     lwd=2,xlab="reports",ylab="P(accept)",ylim=c(0,1))
plot(income_grid, plogis(9.5238 + 0.8717 *(income_grid-mean(income)) ),type="l",
     cex=2,lwd=2,xlab="income",ylab="P(accept)",ylim=c(0,1))
plot(log(share_grid), plogis(9.5238  + 3.3102  * share_grid2 ),type="l",
     cex=2,lwd=2,xlab="log(share)",ylab="P(accept)",ylim=c(0,1))
plot(0:6,plogis(9.5238 - .5506*((0:6)-mean(dependents)) ),type="b",
     lwd=2,xlab="dependents",ylab="P(accept)",ylim=c(0,1))
graphics.off()

pdf("CreditCards_share_card.pdf",width=6,height=5)  ##  figure 11.11  ##
par(mfrow=c(2,2))
plot(log(share),as.numeric(card)-1,ylab="card",main="(a)" )
plot(log(share),reports,main="(b)" )
plot(log(share),income,main="(c)" )
plot(log(share),majorcards,main="(d)" )
graphics.off()
detach(CreditCard_clean)

#####################################################
#####  Code for figure 11.12 and example 11.8   #####
#####################################################
x= seq(-1,2.5,.025)
n = length(x)
set.seed(3592)
y = exp(-x) + rnorm(n,sd=.06)
pdf("linearizing_sim.pdf",width=6,height=5)  ###  Figure 11.12
par(mfrow=c(2,2),cex.axis=1.05,cex.lab=1.05)
plot(x,y,main="(a)")
plot(x,log(y),main="(b)")
fit = lm(log(y)~x)
qqnorm(fit$res,datax=T,main="(c)",ylab="sample quantiles",xlab="theoretical
       quantiles")
qqline(fit$res,datax=T)
plot(fit$fit,fit$res,xlab="fitted value",ylab="residual",main="(d)")
graphics.off()
summary(fit)

###################################
#####  Code for figure 11.13  #####
###################################
set.seed("9847725")
x=seq(0,10,by=.4)
n = length(x)
y = 2 + 5*x + rnorm(n)
ind = c(3,24)
y[ind[1]] = y[ind[1]] + 35
y[ind[2]] = y[ind[2]] - 15
pdf("regression_outliers.pdf",width=6,height=5)
par(mfrow = c(1,1))
plot(x[-ind],y[-ind],xlab="X",ylab="Y")
points(x[ind],y[ind],pch="*",cex=2.5,col="blue")
abline(lm(y~x),lwd=2)
abline( lm(y[-ind]~x[-ind]),lty=5,col="red",lwd=2)
legend("bottomright",c("good data","residual outliers","with outliers",
                       "w/o outliers"),
       pch=c("o","*",NA,NA),lty=c(NA,NA,1,5),pt.cex=c(1,2.5,1,1),
       col=c("black","blue","black","red"),lwd=2)
graphics.off()



#####################################################
#####  Code for example 11.9 and figure 11.14   #####
#####################################################
library(robust)
library("faraway")
set.seed(99)
x = 1:11
x[11] = 50
y=1+x+rnorm(11)
y2 = y
y2[11] = y[11]-45
x2 = x
x2[11] = 5.5
cexx = c(rep(21,10),19)
start = c(1,1,1,1)
loglik1 = function(beta){
  -sum(dt( (y-beta[1]-beta[2]*x)/beta[3], df=beta[4],log=TRUE)) + log(beta[3])
}
fit1 = optim(start,loglik1)
fit1Line = fit1$par[1] + fit1$par[2]*x
loglik2 = function(beta){
  -sum(dt( (y2-beta[1]-beta[2]*x)/beta[3], df=beta[4],log=TRUE)) + log(beta[3])
}
fit2 = optim(start,loglik2)
fit2Line = fit2$par[1] + fit2$par[2]*x
loglik3 = function(beta){
  -sum(dt( (y-beta[1]-beta[2]*x2)/beta[3], df=beta[4],log=TRUE)) + log(beta[3])
}

fit3 = optim(start,loglik3)
fit3Line = fit3$par[1] + fit3$par[2]*x2

pdf("SimDataRobReg.pdf",width=7,height=7)  ##  figure 11.14  ##
par(mfrow=c(2,2),pch="*")
plot(x,y,ylim=c(0,60),cex=3,main="(a)")
abline(ltsReg(y~x),lwd=2)
abline(lm(y~x),lty=2,lwd=2,col="red")
lines(x,fit1Line,lty=3,lwd=6,col="blue")
legend("topleft",c("LTS","LS","t"),lty=c(1,2,3),lwd=c(2,2,6), col=c("black","red","blue"))
plot(x,y2,ylim=c(0,60),cex=3,ylab="y",main="(b)")
abline(ltsReg(y2~x),lwd=2)
abline(lm(y2~x),lty=2,lwd=2,col="red")
lines(x,fit2Line,lty=3,lwd=6,col="blue")
legend("topleft",c("LTS","LS","t"),lty=c(1,2,3),lwd=c(2,2,6), col=c("black","red","blue"))
plot(x2,y,ylim=c(0,60),cex=3,xlab="x",main="(c)")
abline(ltsReg(y~x2),lwd=2)
abline(lm(y~x2),lwd=2,lty=2,col="red")
lines(x2,fit3Line,lty=3,lwd=6,col="blue")
legend("topleft",c("LTS","LS","t"),lty=c(1,2,3),lwd=c(2,2,6), col=c("black","red","blue"))
graphics.off()


############################
#####  Code for R lab  #####
############################


library(Ecdat)
data(Irates)
r1 = Irates[,1]
n = length(r1)
lag_r1 = lag(r1)[-n]
delta_r1 = diff(r1)
n = length(lag_r1)
par(mfrow=c(3,2))
plot(r1,main="(a)")
plot(delta_r1,main="(b)")
plot(delta_r1^2,main="(c)")
plot(lag_r1,delta_r1,main="(d)")
plot(lag_r1,delta_r1^2,main="(e)")

#  CKLS (Chan, Karolyi, Longstaff, Sanders)

nlmod_CKLS = nls(delta_r1 ~ a * (theta-lag_r1),
                 start=list(theta = 5,   a=.01),
                 control=list(maxiter=200))
param = summary(nlmod_CKLS)$parameters[,1]
par(mfrow=c(2,2))
t = seq(from=1946,to =1991+2/12,length=n)
plot(lag_r1,ylim=c(0,16),ylab="rate and theta",
     main="(a)",type="l")
abline(h=param[1],lwd=2,col="red")

res_sq = residuals(nlmod_CKLS)^2
nlmod_CKLS_res <- nls(res_sq ~  A*lag_r1^B,
                      start=list(A=.2,B=1/2) )
param2 = summary(nlmod_CKLS_res)$parameters[,1]
plot(lag_r1,sqrt(res_sq),pch=5,ylim=c(0,6),
     main="(b)")
attach(as.list(param2))
curve(sqrt(A*x^B),add=T,col="red",lwd=3)

nlmod_CKLS_wt = nls(delta_r1 ~ a * (theta-lag_r1),
                    start=list(theta = 5,  a=.01),
                    control=list(maxiter=200),
                    weights=1/fitted(nlmod_CKLS_res))

plot(lag_r1,ylim=c(0,16),ylab="rate and theta",
     main="(c)",type="l")
param3 = summary(nlmod_CKLS_wt)$parameters[,1]
abline(h=param3[1],lwd=2,col="red")


##############  Response Transformation  ###########
library(AER)
data(HousePrices)
fit1 = lm(price~.,data=HousePrices)
summary(fit1)

library(MASS)
par(mfrow = c(1,1))
fit2=boxcox(fit1,xlab=expression(alpha))

library(car)
alphahat = 1/2
fit3=lm(box.cox(price,alphahat)~.,data=HousePrices)
summary(fit3)
AIC(fit1)
AIC(fit3)

#############  Who Owns an Air Conditioner?  ##########
library(AER)
data(HousePrices)
fit1 = glm(aircon~.,family="binomial",data=HousePrices)
summary(fit1)
library(MASS)
fit2 = stepAIC(fit1)
summary(fit2)
