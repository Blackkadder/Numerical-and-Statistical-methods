###########  R script for Chapter 6   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

###########################################################
############  Code for Examples 6.1 and 6.2  ##############
###########################################################

library(bootstrap) 
library(MASS)    #  For fitdistr
set.seed("3857")
data(CRSPday,package="Ecdat")
ge = CRSPday[,4]
ge_250 = ge[1:250]
nboot = 1000
options(digits=3)
t_mle = function(x){as.vector(fitdistr(x,"t")$estimate)}
t1=proc.time()
results = bootstrap(ge,nboot,t_mle)
t2=proc.time()
t2-t1
results_250 = bootstrap(ge_250,nboot,t_mle)
rowMeans(results$thetastar[,]) ## For Table 6.1, row 2
apply(results$thetastar[,],1,sd)  ## For Table 6.1, row 4
fitdistr(ge,"t")  ## For table 6.1, rows 1 and 3
apply(results_250$thetastar,1,mean)  ## For Table 6.2, row 2
apply(results_250$thetastar,1,sd)   ## For Table 6.2, row 4
fitdistr(ge_250,"t") ## For table 6.2, rows 1 and 3
quantile( results_250$thetastar[3,] , c(.95,.98,.99,.999)) 
pdf("MLE_t_BS_250.pdf",width=7,height=3.5)  ## Figure 6.1
par(mfrow=c(1,2))
plot(density(results_250$thetastar[3,]),xlab="df",
     xlim=c(2,21),main="(a) n = 250")
plot(density(results$thetastar[3,]),xlab="df",
     xlim=c(2,21),main="(b) n = 2528")
graphics.off()




##################################################
############  Code for Example 6.3   #############
##################################################
bmw = read.csv("bmw.csv")
library("bootstrap")
quKurt = function(y,p1=0.025,p2=0.25)
{
  Q = quantile(y,c(p1,p2,1-p2,1-p1))
  (Q[4]-Q[1]) / (Q[3]-Q[2])
}
set.seed("5640")
t1 =  proc.time()
bca_kurt= bcanon(bmw$bmw,5000,quKurt)
t2 = proc.time()
t2-t1
bca_kurt$confpoints

##################################################################
############  Code for Figure 6.2 and example 6.4  ###############
##################################################################
pdf("LSCC_CSGSQQ.pdf",width=6,height=5)  ##  Figure 6.2
par(mfrow = c(1,1))
midcapD.ts = read.csv("midcapD.ts.csv")
attach(midcapD.ts)
qqplot(LSCC,CSGS)
lmfit = lm(quantile(CSGS,c(.25,.75)) ~ quantile(LSCC,c(.25,.75)) )
abline(lmfit,col="red", lwd=2)
graphics.off()

n = dim(midcapD.ts)[1]
quKurt = function(y,p1=0.025,p2=0.25)
{
  Q = quantile(y,c(p1,p2,1-p2,1-p1))
  as.numeric((Q[4]-Q[1]) / (Q[3]-Q[2]))
}
compareQuKurt = function(x,p1=0.025,p2=0.25,xdata)
{
  quKurt(xdata[x,1],p1,p2)/quKurt(xdata[x,2],p1,p2)
}
quKurt(LSCC)
quKurt(CSGS)
xdata=cbind(LSCC,CSGS)
compareQuKurt(1:n,xdata=xdata)
library("bootstrap")
set.seed("5640")
bca_kurt= bcanon((1:n),5000,compareQuKurt,xdata=xdata)
bca_kurt$confpoints


################## R lab  ####################

############  Problems 1 - 5    ##############

library("fGarch")      
bmwRet = read.csv("bmwRet.csv")
n = dim(bmwRet)[1]

kurt = kurtosis(bmwRet[,2],method="moment")
skew = skewness(bmwRet[,2],method="moment")
fit_skewt = sstdFit(bmwRet[,2])

q.grid = (1:n)/(n+1)
qqplot(bmwRet[,2], qsstd(q.grid,fit_skewt$estimate[1],
                         fit_skewt$estimate[2],
                         fit_skewt$estimate[3],fit_skewt$estimate[4]),
       ylab="skewed-t quantiles" )


quKurt = function(y, p1 = 0.025, p2 = 0.25)
{
  Q = quantile(y, c(p1, p2, 1 - p2, 1 - p1))
  k = (Q[4] - Q[1]) / (Q[3] - Q[2])
  k
}
nboot = 5000
ModelFree_kurt  = rep(0, nboot)
ModelBased_kurt = rep(0, nboot)
set.seed("5640")
for (i in 1:nboot)
{
  samp_ModelFree = sample(bmwRet[,2], n, replace = TRUE)
  samp_ModelBased = rsstd(n, fit_skewt$estimate[1],
                          fit_skewt$estimate[2],
                          fit_skewt$estimate[3], fit_skewt$estimate[4])
  ModelFree_kurt[i] = quKurt(samp_ModelFree)
  ModelBased_kurt[i] = quKurt(samp_ModelBased)
}

############  Problems 6 -  14  ##############

library(bootstrap)
Kurtosis = function(x) mean( ((x-mean(x))/sd(x))^4 )
set.seed(3751)
niter = 500
nboot = 400
n = 50
nu = 10 
trueKurtosis =  3 + 6/(nu-4)
correct = matrix(nrow=niter,ncol=5)
width   = matrix(nrow=niter,ncol=5)
error = matrix(nrow=niter,ncol=1)
t1 = proc.time()
for (i in 1:niter){
y = rt(n,nu)
int1 = boott(y,Kurtosis,nboott=nboot,nbootsd=50)$confpoints[c(3,9)]
width[i,1] = int1[2]-int1[1]
correct[i,1] = as.numeric((int1[1]<trueKurtosis)&(trueKurtosis<int1[2]))
int2 = bcanon(y,nboot,Kurtosis)$confpoints[c(1,8),2]
width[i,2] = int2[2]-int2[1]
correct[i,2] = as.numeric((int2[1]<trueKurtosis)&(trueKurtosis<int2[2]))
boot = bootstrap(y,nboot,Kurtosis)$thetastar
int3 = Kurtosis(y)+1.96*c(-1,1)*sd(boot)
width[i,3] = int3[2]-int3[1]
correct[i,3] = as.numeric((int3[1]<trueKurtosis)&(trueKurtosis<int3[2]))
int4 = quantile(boot,c(.025,.975))
width[i,4] = int4[2]-int4[1]
correct[i,4] = as.numeric((int4[1]<trueKurtosis)&(trueKurtosis<int4[2]))
int5 = 2*Kurtosis(y) - quantile(boot,c(.975,.025))
width[i,5] = int5[2]-int5[1]
correct[i,5] = as.numeric((int5[1]<trueKurtosis)&(trueKurtosis<int5[2]))
error[i] = mean(boot)-Kurtosis(y)
}
t2 = proc.time()
(t2-t1)/60
colMeans(width)
colMeans(correct)
options(digits=3)
mean(error)
mean(error^2)

0.874 + c(-1,1)* 1.96 *sqrt(0.874*(1-0.874)/500)


############  Problem 15  ####################
###  Warning: this will take a long time to run ###
### It took 42 minutes on an Intel core 2 quad processor running at 3 GHz ###
### Reduce niter and nboot if you are impatient  ###

library(bootstrap)
quKurt = function(y,p1=0.025,p2=0.25)
{
  Q = quantile(y,c(p1,p2,1-p2,1-p1))
  k = (Q[4]-Q[1]) / (Q[3]-Q[2])
  k
}
set.seed(3751)
niter = 500
nboot = 400 
n = 50
nu = 10 
p1 = 0.025
p2 = 0.25
trueQuantiles = qt(c(p1,p2,1-p2,1-p1),df=nu)
truequKurt =  (trueQuantiles[4]-trueQuantiles[1]) / (trueQuantiles[3]-trueQuantiles[2])
correct = matrix(nrow=niter,ncol=5)
width   = matrix(nrow=niter,ncol=5)
error = matrix(nrow=niter,ncol=1)
t1 = proc.time()
for (i in 1:niter){
  y = rt(n,nu)
  int1 = boott(y,quKurt,nboott=nboot,nbootsd=50)$confpoints[c(3,9)]
  width[i,1] = int1[2]-int1[1]
  correct[i,1] = as.numeric((int1[1]<truequKurt)&(truequKurt<int1[2]))
  int2 = bcanon(y,nboot,quKurt)$confpoints[c(1,8),2]
  width[i,2] = int2[2]-int2[1]
  correct[i,2] = as.numeric((int2[1]<truequKurt)&(truequKurt<int2[2]))
  boot = bootstrap(y,nboot,quKurt)$thetastar
  int3 = quKurt(y)+1.96*c(-1,1)*sd(boot)
  width[i,3] = int3[2]-int3[1]
  correct[i,3] = as.numeric((int3[1]<truequKurt)&(truequKurt<int3[2]))
  int4 = quantile(boot,c(.025,.975))
  width[i,4] = int4[2]-int4[1]
  correct[i,4] = as.numeric((int4[1]<truequKurt)&(truequKurt<int4[2]))
  int5 = 2*quKurt(y) - quantile(boot,c(.975,.025))
  width[i,5] = int5[2]-int5[1]
  correct[i,5] = as.numeric((int5[1]<truequKurt)&(truequKurt<int5[2]))
  error[i] = mean(boot)-quKurt(y)
}
t2 = proc.time()
(t2-t1)/60
colMeans(width)
colMeans(correct)
options(digits=3)
mean(error)
mean(error^2)

p = 0.926
p + c(-1,1)* 1.96 *sqrt(p*(1-p)/500)

p = 0.932
p + c(-1,1)* 1.96 *sqrt(p*(1-p)/500)


