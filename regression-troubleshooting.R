###########  R script for Chapter 10  ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

#####  Code for figure 10.1 to 10.4  #####
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

pdf("leverage_ex.pdf",width=6,height=5)  ##  Figure 10.1
par(mfrow=c(2,2),lwd=1)
plot(x,y,ylim=c(0,60),cex=c(rep(1.25,10),1.5),pch=cexx,main="(a)")
abline(lm(y~x),lwd=2)
plot(x,y2,ylim=c(0,60),cex=c(rep(1.25,10),1.5),ylab="y",pch=cexx,main="(b)")
abline(lm(y2~x),lwd=2)
plot(x2,y,ylim=c(0,60),cex=c(rep(1.15,10),1.5),xlab="x",pch=cexx,main="(C)")
abline(lm(y~x2),lwd=2)
graphics.off()

pdf("leverage_ex_lev.pdf",width=6,height=5)  ##  Figure 10.2
par(mfrow=c(2,2),lwd=1,pch=19)
plot(hatvalues(lm(y~x)),ylab="leverage",main="(a)",ylim=c(0,1))
plot(hatvalues(lm(y2~x)),ylab="leverage",main="(b)",ylim=c(0,1))
plot(hatvalues(lm(y~x2)),ylab="leverage",main="(c)",ylim=c(0,1))
plot(x2,hatvalues(lm(y~x2)),xlab="x",ylab="leverage",
     main="(d)",ylim=c(0,1))
graphics.off()

pdf("leverage_ex_residualsNEW.pdf",width=6,height=5)  ## Figure 10.3
par(mfrow=c(2,3),lwd=1,pch=19)
plot(rstudent(lm(y~x)),ylab="studentized residual",main="Dataset (a)")
plot(rstudent(lm(y2~x)),ylab="studentized residual",main="Dataset (b)")
plot(rstudent(lm(y~x2)),ylab="studentized residual",main="Dataset (c)")
plot(residuals(lm(y~x)),ylab="residual",main="Dataset (a)")
plot(residuals(lm(y2~x)),ylab="residual",main="Dataset (b)")
plot(residuals(lm(y~x2)),ylab="residual",main="Dataset (c)")
graphics.off()

pdf("leverage_ex_cookD.pdf",width=6,height=5)  ##  Figure 10.4
par(mfrow=c(2,3),cex.axis=1,cex.lab=1,lwd=1,pch=19)
plot(sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)",ylim=c(0,11))
plot(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)",ylim=c(0,11))
halfnorm( sqrt(cooks.distance(lm(y~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (a)",
          xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y2~x))),ylab=("square root Cook's D"),cex=1,main="Dataset (b)",
         xlim=c(0,1.85))
halfnorm(sqrt(cooks.distance(lm(y~x2))),ylab=("square root Cook's D"),cex=1,main="Dataset (c)",
         xlim=c(0,1.85))
graphics.off()

########################################################
#####  Code for figured 10.5 - 18.9  ###########################
########################################################
dat = read.table(file="WeekIntAllData.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
fit = lm(aaa_dif~cm10_dif+cm30_dif)
n=length(cm10)
pdf("WeeklyInterestCookD.pdf",width=6,height=5)  ##  figure 10.5  ##
par(mfrow=c(2,2))
plot(hatvalues(fit),ylab="Leverage",xlab="Index",
     main="(a)")
plot(2:n,rstudent(fit),ylab="rstudent",xlab="Index",
     main="(b)")
plot(2:n,cooks.distance(fit),ylab="Cook's D",xlab="Index", main="(c)")
plot(2:n,cooks.distance(fit),ylab="Cook's D",
     xlim=c(368,378),xlab="Index", main="(d)")
graphics.off()

#####  Code for figures 10.6 and 10.7  #####
n = 80
set.seed("781235")
e = matrix(runif(12*n),nrow=n) %*% rep(1,12)
e = abs(e)^4
e= e/mean(e) 
x1 = runif(n)
x1 = sort(x1) 
x2 = rbeta(n,6,.5)

y =( 8*x2 + x1 + 5*x1^3) + ( 4* x2 + x1 + 7*x1^3) * e 

pdf("ResidEx5.pdf",height=4,width=7)  ##  Figure 10.6
par(mfrow=c(1,2))
plot(x1,y,xlab=expression(x[1]))
plot(x2,y,xlab=expression(x[2]))
graphics.off()

fit = lm(y~x1+x2)
rstudent = rstudent(fit)
pdf("ResidEx1.pdf",height=4,width=7)  ##  Figure 10.7
par(mfrow=c(1,2))
qqnorm(rstudent,datax=T,main="Normal QQ Plot")
hist(rstudent,12)
graphics.off()


pdf("ResidEx3.pdf",height=2.2,width=5)  ##  Figure 10.8
par(mfrow=c(1,3))
plot(x1,rstudent,main="(a)",xlab=expression(x[1]))
fit2 = loess(rstudent~x1)
lines(x1,fit2$fitted,col="red",lwd=2)
plot(x2,rstudent,main="(b)",xlab=expression(x[1]))
fit3 = loess(rstudent~x2)
ordx2 = order(x2)
lines(x2[ordx2],fit3$fitted[ordx2],col="red",lwd=2)
fitquad = lm(y~poly(x1,2)+x2 )
rstudentquad = rstudent(fitquad)
plot(fitquad$fitted,abs(rstudentquad),xlab="fitted values",ylab="abs(rstudent)",main="(c) ")
fit4 = loess(abs(rstudentquad)~fitquad$fitted)
ord = order(fitquad$fitted)
lines(fitquad$fitted[ord],fit4$fitted[ord],col="red",lwd=2)
graphics.off()

transy = log(y)
fitquad2 = lm(transy~poly(x1,2)+x2 )
rstudentquad2 = rstudent(fitquad2)
pdf("ResidEx6.pdf",height=5.5,width=6)  ##  Figure 10.9
par(mfrow=c(2,2))
plot(x1,rstudentquad2,ylab="rstudent",main="(a)",xlab=expression(x[1]))
plot(x2,rstudentquad2,ylab="rstudent",main="(b)",xlab=expression(x[2]))
plot(fitquad$fitted,abs(rstudentquad2),xlab="fitted values",ylab="abs(rstudent)",main="(c) ")
fit5 = loess(abs(rstudentquad2)~fitquad2$fitted)
ord = order(fitquad2$fitted)
lines(fitquad2$fitted[ord],fit4$fitted[ord])
qqnorm(rstudentquad2,datax=T,main="(d) normal plot")
graphics.off()


############################
#####  Code for R lab  #####
############################
library(AER)
data(CPS1988)
attach(CPS1988)
fitLm1 = lm(wage ~ education + experience + ethnicity)

par(mfrow = c(3, 2))
resid1 = rstudent(fitLm1)
plot(fitLm1$fit, resid1,
     ylim = c(-1500, 1500), main = "(a)")
lines(lowess(fitLm1$fit, resid1, f = 0.2), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)

plot(fitLm1$fit, abs(resid1),
     ylim = c(0, 1500), main = "(b)")
lines(lowess(fitLm1$fit, abs(resid1), f = 0.2), 
      lwd = 5, col = "red")
abline(h = mean(abs(resid1)), col = "blue", lwd = 5)

qqnorm(resid1, datax = FALSE, main = "(c)")
qqline(resid1, datax = FALSE, lwd = 5, col = "blue")

plot(education, resid1, ylim = c(-1000, 1500), main = "(d)")
lines(lowess(education, resid1), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)

plot(experience, resid1, ylim = c(-1000, 1500), main = "(e)")
lines(lowess(experience, resid1), lwd = 5, col = "red")
abline(h = 0, col = "blue", lwd = 5)

library(faraway)  #  required for halfnorm
par(mfrow=c(1, 3))
plot(hatvalues(fitLm4))
plot(sqrt(cooks.distance(fitLm4)))
halfnorm(sqrt(cooks.distance(fitLm4)))


