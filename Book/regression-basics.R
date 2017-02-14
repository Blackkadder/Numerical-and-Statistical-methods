###########  R script for Chapter 9   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

################################################
#####  Code for example 9.1 and figure 9.2 #####
################################################
dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
par(mfrow=c(1,1))
pdf("cm10aaa.pdf",width=6,height=5)
plot(cm10_dif,aaa_dif,xlab="change in 10YR T rate",
     ylab="change in AAA rate")
graphics.off()
options(digits = 3)
summary(lm(aaa_dif ~ cm10_dif))

#################################################
#####  Code for example 9.2 and figure 9.3  #####
#################################################
data(Capm,package="Ecdat")
attach(Capm)
rfood2 = rfood/100
rmrf2 = rmrf/100
pdf("capm_regression_xyplot.pdf",width=6,height=5)
plot(rmrf2,rfood2,ylab="Food industry excess return",
     xlab="Market excess return")
graphics.off()
options(digits = 3)
summary(lm(rfood2~rmrf2))

#################################################
#####  Code for example 9.4 and figure 9.4  #####
#################################################
options(digits = 3)
summary(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
pdf("weekint_splotm.pdf",width=6,height=5)
plot(as.data.frame(cbind(aaa_dif,cm10_dif,cm30_dif,ff_dif)))
graphics.off()

####################################################
#####  Code for ANOVA tables in Section 9.4.1  #####
####################################################
options(digits = 3)
anova(lm(aaa_dif ~ cm10_dif + cm30_dif + ff_dif))
anova(lm(aaa_dif~ff_dif+cm30_dif+cm10_dif))

#############################################
#####  Code for examples 9.5 and 9.6  #####
#############################################
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
anova(fit1, fit3)
anova(fit2, fit3)

#################################################
#####  Code for figure 9.5 and example 9.7  #####
#################################################
library(leaps)
subsets = regsubsets(aaa_dif~.,
                     data=as.data.frame(cbind(cm10_dif,cm30_dif,ff_dif)),nbest=1)
b = summary(subsets)
b
pdf("WeekInt_model_selection.pdf",width=6,height=3)
par(mfrow=c(1,3),lab=c(2,5,3),pch=19)
plot(1:3,b$bic,type="b",xlab="number of variables",
     ylab="BIC",cex=2.5)
plot(1:3,b$cp,type="b",xlab="number of variables",
     ylab="Cp",cex=2.5)
plot(1:3,b$adjr2,type="b",xlab="number of variables",
     ylab="adjusted R2")
graphics.off()

#################################
##### Code for example 9.8  #####
#################################
dat = read.table(file="WeekInt.txt",header=T)
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
library(faraway)
options(digits=2)
vif(lm(aaa_dif~cm10_dif+cm30_dif+ff_dif))
detach(dat)

################################
##### Code for figure 9.6  #####
################################
nelsonplosser = read.csv("nelsonplosser.csv")
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)
n = length(gnp.r)
year = 1909 + (1970-1909)*(0:(n-2))/n
pdf("nelsonPlosser_differences.pdf",width=6,height=5)
par(mfrow=c(2,3),cex.lab=1.35)
plot(year,diff(gnp.r),type="b",ylab="differences",main="gnp.r")
plot(year,diff(log(gnp.r)),type="b",ylab="differences",main="log(gnp.r)")
plot(year,diff(sqrt(gnp.r)),type="b",ylab="differences",main="sqrt(gnp.r)")
plot(year,diff(ip),type="b",ylab="differences",main="ip")
plot(year,diff(log(ip)),type="b",ylab="differences",main="log(ip)")
plot(year,diff(sqrt(ip)),type="b",ylab="differences",main="sqrt(ip)")
graphics.off()

###################################
#####  Code for example 9.9  #####
###################################
nelsonplosser = read.csv("nelsonplosser.csv")
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)
fit_lm=lm(diff(log(sp))~diff(gnp.r)+diff(gnp.pc)
          +diff(log(ip))+diff(log(cpi))
          +diff(emp)+diff(bnd),data=new_np)
options(digits = 3)
summary(fit_lm)
print(vif(fit_lm),digits=2)
cor(diff(gnp.r),diff(gnp.pc))
library(MASS)
step_lm = stepAIC(fit_lm)
summary(lm(step_lm))
x1= as.matrix(cbind(diff(gnp.r),diff(gnp.pc),diff(log(ip)),
                        diff(log(cpi)),
                        diff(emp),diff(bnd)))
names_x1 = c("gnp.r", "gnp.pc","log(ip)","log(cpi)",
             "emp","bnd")
leaps.fit = leaps(y = diff(log(sp)),x=x1,names=names_x1,nbest=1)
options(digits=2)
cbind(leaps.fit$which,leaps.fit$Cp)
summary(lm(diff(log(sp)) ~ diff(log(ip)) + diff(bnd),data=new_np))

###################################################
#####  Code for  Example 9.10 and Figure 9.7  #####
###################################################
dat = read.table(file="WeekInt.txt",header=T)
library("car")
attach(dat)
cm10_dif = diff( cm10 )
aaa_dif = diff( aaa )
cm30_dif = diff( cm30 )
ff_dif = diff( ff )
fit1 = lm(aaa_dif ~ cm10_dif)
fit2 = lm(aaa_dif~cm10_dif+cm30_dif)
fit3 = lm(aaa_dif~cm10_dif+cm30_dif+ff_dif)
fit4 = lm(aaa_dif~cm30_dif)
pdf("weekint_partialresidual.pdf",width=6,height=5)  #  Figure 12.7
par(mfrow=c(2,2))
crPlot(fit2,var="cm10_dif",main="(a)",smooth=F,lty=1,lwd=2,col="black")
crPlot(fit2,var="cm30_dif",main="(b)",smooth=F,lty=1,lwd=2,col="black")
plot(cm10_dif,aaa_dif,main="(c)")
regLine(fit1,col="red",lwd=2,lty="dashed")
plot(cm30_dif,aaa_dif,main="(d)")
regLine(fit4,col="red",lwd=2,lty="dashed")
graphics.off()


#####  Code for Example 9.11 and Figure 9.8  #####

nelsonplosser = read.csv("nelsonplosser.csv")
names(nelsonplosser)
names(nelsonplosser)
new_np = na.omit(nelsonplosser)
n=dim(new_np)[1]
attach(new_np)
fit_lm=lm(diff(log(sp))~diff(gnp.r)+diff(gnp.pc)
          +diff(log(ip))+diff(log(cpi))
          +diff(emp)+diff(bnd),data=new_np)
step_lm = stepAIC(fit_lm)
fit_lm2=lm(step_lm)
pdf("nelsonPlosser_partResid.pdf",width=6,height=5) # Figure 12.8
par(mfrow=c(2,2))
library("car")
crPlot(fit_lm2,var="diff(gnp.r)",main="(a)",smooth=F,lty=1,lwd=2)
crPlot(fit_lm2,var="diff(gnp.pc)",main="(b)",smooth=F,lty=1,lwd=2)
crPlot(fit_lm2,var="diff(log(ip))",main="(c)",smooth=F,lty=1,lwd=2)
crPlot(fit_lm2,var="diff(bnd)",main="(c)",smooth=F,lty=1,lwd=2)
graphics.off()

#####  Code for R lab  #####
library(AER)
data("USMacroG")
MacroDiff=as.data.frame(apply(USMacroG,2,diff))
attach(MacroDiff)
pairs(cbind(consumption,dpi,cpi,government,unemp))
fitLm1 = lm(consumption~dpi+cpi+government+unemp)
summary(fitLm1)
confint(fitLm1)
anova(fitLm1)
library(MASS)
fitLm2 = stepAIC(fitLm1)
summary(fitLm2)
AIC(fitLm1)
AIC(fitLm2)
AIC(fitLm1)-AIC(fitLm2)
library(car)
vif(fitLm1)
vif(fitLm2)
par(mfrow=c(2,2))
sp = 0.8
crPlot(fitLm1,dpi,span=sp,col="black")
crPlot(fitLm1,cpi,span=sp,col="black")
crPlot(fitLm1,government,span=sp,col="black")
crPlot(fitLm1,unemp,span=sp,col="black")







