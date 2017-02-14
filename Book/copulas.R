###########  R script for Chapter 8   ####################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################

library(copula)

##################################################
############  Code for Figure 8.1  ###############
##################################################

u = seq(0.000001, 1, length=500)
frank = iPsi(copula=archmCopula(family="frank", param=1), u)

pdf("frankGen.pdf", width=6, height=5)
#
plot(u, frank, type="l", lwd=3, ylab=expression(phi(u)))
abline(h=0) ; abline(v=0)
#
graphics.off()


##################################################
############  Code for Figure 8.2  ###############
##################################################

set.seed(5640)
theta = c(-100, -50, -10, -1, 0, 5, 20, 50, 500)

pdf("frank_copulas.pdf", width=7, height=7)
#
par(mfrow=c(3,3), cex.axis=1.2, cex.lab=1.2, cex.main=1.2)

for(i in 1:9){
  U = rCopula(n = 200, copula=archmCopula(family="frank", param=theta[i]))
  plot(U, xlab=expression(u[1]), ylab=expression(u[2]),
       main=eval(substitute(expression(paste(theta," = ",j)), list(j = as.character(theta[i])))))
}
#
graphics.off()


##################################################
############  Code for Figure 8.3  ###############
##################################################

set.seed(5640)
theta = c(-0.98, -0.7, -0.3, -0.1, 0.1, 1, 5, 15, 100)

pdf("clayton_copulas.pdf", width=7, height=7)
#
par(mfrow=c(3,3), cex.axis=1.2, cex.lab=1.2, cex.main=1.2)

for(i in 1:9){
  U = rCopula(n = 200, copula=archmCopula(family="clayton", param=theta[i]))
  plot(U, xlab=expression(u[1]), ylab=expression(u[2]),
       main=eval(substitute(expression(paste(theta," = ",j)), list(j = as.character(theta[i])))))
}
#
graphics.off()


##################################################
############  Code for Figure 8.4  ###############
##################################################

set.seed(5640)
theta = c(1.1, 1.5, 2, 4, 8, 50)

pdf("gumbel_copulas.pdf", width=6, height=5)
#
par(mfrow=c(2,3), cex.axis=1.2, cex.lab=1.2, cex.main=1.2)

for(i in 1:6){
  U = rCopula(n = 200, copula=archmCopula(family="gumbel", param=theta[i]))
  plot(U, xlab=expression(u[1]), ylab=expression(u[2]),
       main=eval(substitute(expression(paste(theta," = ",j)), list(j = as.character(theta[i])))))
}
#
graphics.off()


##################################################
############  Code for Figure 8.5  ###############
##################################################

set.seed(5640)
theta = c(1.1, 1.5, 2, 4, 8, 50)

pdf("joe_copulas.pdf", width=6, height=5)
#
par(mfrow=c(2,3), cex.axis=1.2, cex.lab=1.2, cex.main=1.2)

for(i in 1:6){
  U = rCopula(n = 200, copula=archmCopula(family="joe", param=theta[i]))
  plot(U, xlab=expression(u[1]), ylab=expression(u[2]),
       main=eval(substitute(expression(paste(theta," = ",j)), list(j = as.character(theta[i])))))
}
#
graphics.off()


##################################################
############  Code for Figure 8.6  ###############
##################################################

rho = seq(-1,1,by=.01)
df = c(1, 4, 25, 240)

x1 = - sqrt((df[1]+1)*(1-rho)/(1+rho))
lambda1 = 2*pt(x1,df[1]+1)

x4 = - sqrt((df[2]+1)*(1-rho)/(1+rho))
lambda4 = 2*pt(x4,df[2]+1)

x25 = - sqrt((df[3]+1)*(1-rho)/(1+rho))
lambda25 = 2*pt(x25,df[3]+1)

x250 = - sqrt((df[4]+1)*(1-rho)/(1+rho))
lambda250 = 2*pt(x250,df[4]+1)

pdf("TailDependenceT.pdf",width=6,height=5)
#
par(mfrow=c(1,1), lwd=2, cex.axis=1.2, cex.lab=1.2)
plot(rho, lambda1, type="l", lty=1, xlab=expression(rho),
     ylab=expression(lambda[l] == lambda[u]) )
lines(rho,lambda4,lty=2)
lines(rho,lambda25,lty=3)
lines(rho,lambda250,lty=4)
legend("topleft", c(expression(nu==1), expression(nu==4), expression(nu==25),
                    expression(nu==250)), lty=1:4 )
#
graphics.off()


##################################################
############  Code for Example 8.1  ##############
##################################################

library(copula)
library(sn)

dat = read.csv("FlowData.csv")
dat = dat/10000
n = nrow(dat)

x1 = dat$Flow1
fit1 = st.mple(matrix(1,n,1), y=x1, dp = c(mean(x1),sd(x1),0,10))
est1 = fit1$dp
u1 = pst(x1, dp=est1)

x2 = dat$Flow2
fit2 = st.mple(matrix(1,n,1), y=x2, dp = c(mean(x2),sd(x2),0,10))
est2 = fit2$dp
u2 = pst(x2, dp=est2)
U.hat = cbind(u1,u2)

z1 = qnorm(u1)
z2 = qnorm(u2)
Z.hat = cbind(z1,z2)


##################################################
############  Code for Figure 8.7  ###############
##################################################

library(ks)
fhatU = kde(x=U.hat, H=Hscv(x=U.hat))

pdf("unif_flows_hist_plot.pdf", width=7, height=7)
#
par(mfrow=c(2,2), cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
hist(u1, main="(a)", xlab=expression(hat(U)[1]), freq = FALSE)
hist(u2, main="(b)", xlab=expression(hat(U)[2]), freq = FALSE)
plot(u1, u2, main="(c)", xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]), mgp = c(2.5, 1, 0))
plot(fhatU, drawpoints=FALSE, drawlabels=FALSE, cont=seq(10, 80, 10), 
     main="(d)", xlab=expression(hat(U)[1]), ylab=expression(hat(U)[2]), mgp = c(2.5, 1, 0)) 
#
graphics.off()


##################################################
############  Code for Figure 8.8  ###############
##################################################

fhatZ = kde(x=Z.hat, H=Hscv(x=Z.hat))

pdf("norm_flows_hist_plot.pdf", width=7, height=7)
#
par(mfrow=c(2,2), cex.axis=1.2, cex.lab=1.2, cex.main=1.2)
qqnorm(z1, datax=T, main="(a)") ; qqline(z1)
qqnorm(z2, datax=T, main="(b)") ; qqline(z2)
plot(z1, z2, main="(c)", xlab = expression(hat(Z)[1]), ylab = expression(hat(Z)[2]), mgp = c(2.5, 1, 0))
plot(fhatZ, drawpoints=FALSE, drawlabels=FALSE, cont=seq(10, 90, 10), 
     main="(d)", xlab=expression(hat(Z)[1]), ylab=expression(hat(Z)[2]), mgp = c(2.5, 1, 0)) 
#
graphics.off()


##################################################
############  Code for Table 8.1  ################
##################################################

options(digits=3)
cor.test(u1, u2, method="spearman")
cor.test(u1, u2, method="kendall")
sin(-0.242*pi/2)
cor.test(u1, u2, method="pearson")
cor.test(z1, z2, method="pearson")

omega = -0.371

options(digits=4)

Ct = fitCopula(copula=tCopula(dim = 2), data=U.hat, method="ml", start=c(omega, 10)) 
Ct@estimate
loglikCopula(param=Ct@estimate, x=U.hat, copula=tCopula(dim = 2))
-2*.Last.value + 2*length(Ct@estimate)
#
Cgauss = fitCopula(copula=normalCopula(dim = 2), data=U.hat, method="ml", start=c(omega)) 
Cgauss@estimate
loglikCopula(param=Cgauss@estimate, x=U.hat, copula=normalCopula(dim = 2))
-2*.Last.value + 2*length(Cgauss@estimate)
# Not run
Cgu = fitCopula(copula=gumbelCopula(2, dim=2), data=U.hat, method="ml")
# Not run
Cjoe = fitCopula(copula=joeCopula(2, dim=2), data=U.hat, method="ml")
#
Cfr = fitCopula(copula=frankCopula(1, dim=2), data=U.hat, method="ml")
Cfr@estimate
loglikCopula(param=Cfr@estimate, x=U.hat, copula=frankCopula(dim = 2))
-2*.Last.value + 2*length(Cfr@estimate)
#
Ccl = fitCopula(copula=claytonCopula(1, dim=2), data=U.hat, method="ml")
Ccl@estimate
loglikCopula(param=Ccl@estimate, x=U.hat, copula=claytonCopula(dim = 2))
-2*.Last.value + 2*length(Ccl@estimate)


##################################################
############  Code for Figure 8.9  ###############
##################################################

pdf("unif_flows_contours_copulas.pdf",width=7,height=6)
#
par(mfrow=c(2,3), mgp = c(2.5, 1, 0))
plot(u1, u2, main="Uniform-Transformed Data",
     xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]))
Udex = (1:n)/(n+1)
Cn = C.n(u = cbind(rep(Udex, n), rep(Udex, each=n)) , U = U.hat, 
         offset=0, method="C")
EmpCop = expression(contour(Udex,Udex,matrix(Cn,n,n), col=2, add=T))
#
contour(normalCopula(param=0,dim=2), pCopula, main=expression(C[0]), 
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]))
eval(EmpCop)
#
contour(tCopula(param=Ct@estimate[1], dim=2, 
                df=round(Ct@estimate[2])), 
        pCopula, main = expression(hat(C)[t]), 
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]))
eval(EmpCop)
#
contour(normalCopula(param=Cgauss@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Gauss]),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]))
eval(EmpCop)
#
contour(frankCopula(param=Cfr@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Fr]),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]))
eval(EmpCop)
#
contour(claytonCopula(param=Ccl@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Cl]),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]))
eval(EmpCop)
#
graphics.off()


##################################################
############  Code for Example 8.1 (cont.)  ######
##################################################

#  Recompute the above results now using EDFs. Let

u1 = (rank(x1) - 0.5)/n
u2 = (rank(x2) - 0.5)/n

# and rerun Example 8.1 starting from "Code for Figure 8.7" 
# through "Code for Figure 8.9" 


####################################################
############  R lab - 8.11.1  ######################
############  Simulating from Copula Models  #######
####################################################

library(copula)
cop_t_dim3 = tCopula(dim = 3, param = c(-0.6,0.75,0), 
                     dispstr = "un", df = 1)
set.seed(5640)
rand_t_cop = rCopula(n = 500, copula = cop_t_dim3)
pairs(rand_t_cop)
cor(rand_t_cop)


cor.test(rand_t_cop[,1],rand_t_cop[,3])


cop_normal_dim3 = normalCopula(dim = 3, param = c(-0.6,0.75,0), 
                               dispstr = "un")
mvdc_normal = mvdc(copula = cop_normal_dim3, margins = rep("exp",3),
                   paramMargins = list(list(rate=2), list(rate=3), 
                                       list(rate=4)))
set.seed(5640)
rand_mvdc = rMvdc(n = 1000, mvdc = mvdc_normal)
pairs(rand_mvdc)
par(mfrow = c(2,2))
for(i in 1:3) plot(density(rand_mvdc[,i]))


####################################################
############  R lab - 8.11.2  ######################
############  Fitting Copula Models to  ############
############  Bivariate Return Data  ###############
####################################################

# Data download 9/9/2014
# library(quantmod)
# getSymbols(c("IBM", "^GSPC"), from="2004-06-01",to="2014-05-31") 
# IBM.SP500 = cbind(IBM[,6],GSPC[,6]) ; head(IBM.SP500)
# netReturns = ((diff(IBM.SP500)/lag(IBM.SP500)*100)[-1,]) ; tail(netReturns)
# colnames(netReturns) = c("IBM", "SP500") ; colnames(netReturns)
# head(netReturns) ; tail(netReturns)
# write.zoo(netReturns,"IBM_SP500_04_14_daily_netRtns.csv", index.name="Date", sep=",")


library(MASS)     #  for fitdistr() and kde2d() functions
library(copula)   #  for copula functions
library(fGarch)   #  for standardized t density
netRtns = read.csv("IBM_SP500_04_14_daily_netRtns.csv", header = T)
ibm = netRtns[,2]
sp500 = netRtns[,3]
est.ibm = as.numeric( fitdistr(ibm,"t")$estimate )
est.sp500 = as.numeric( fitdistr(sp500,"t")$estimate )
est.ibm[2] = est.ibm[2] * sqrt( est.ibm[3] / (est.ibm[3]-2) )
est.sp500[2] = est.sp500[2] * sqrt(est.sp500[3] / (est.sp500[3]-2) )


cor_tau = cor(ibm, sp500, method = "kendall")
omega = 0.5 ######### need to get correct value   


cop_t_dim2 = tCopula(omega, dim = 2, dispstr = "un", df = 4)


data1 = cbind(pstd(ibm, est.ibm[1], est.ibm[2], est.ibm[3]), 
              pstd(sp500, est.sp500[1], est.sp500[2], est.sp500[3]))
n = nrow(netRtns) ; n
data2 = cbind(rank(ibm)/(n+1), rank(sp500)/(n+1))
ft1 = fitCopula(cop_t_dim2, data1, method="ml", start=c(omega,4) ) 
ft2 = fitCopula(cop_t_dim2, data2, method="ml", start=c(omega,4) ) 


mvdc_t_t = mvdc( cop_t_dim2, c("std","std"), list(
           list(mean=est.ibm[1],sd=est.ibm[2],nu=est.ibm[3]),
           list(mean=est.sp500[1],sd=est.sp500[2],nu=est.sp500[3])))


# Not run
# fit_cop = fitMvdc(cbind(ibm,sp500),mvdc_t_t,start=c(ft1@estimate,est.ibm,est.sp500), hideWarnings=FALSE)


start = c(est.ibm, est.sp500, ft1@estimate)
objFn = function(param) -loglikMvdc(param,cbind(ibm,sp500),mvdc_t_t)
tic = proc.time()
ft = optim(start, objFn, method="L-BFGS-B",
           lower = c(-.1,0.001,2.2, -0.1,0.001,2.2,  0.2,2.5),
           upper = c( .1,   10, 15,  0.1,   10, 15,  0.9, 15) )
toc = proc.time()
total_time = toc - tic ; total_time[3]/60


fnorm = fitCopula(copula=normalCopula(dim=2),data=data1,method="ml") 
ffrank = fitCopula(copula = frankCopula(3, dim = 2),
                   data = data1, method = "ml")
fclayton = fitCopula(copula = claytonCopula(1, dim=2),
                     data = data1, method = "ml") 
fgumbel = fitCopula(copula = gumbelCopula(3, dim=2),
                    data = data1, method = "ml") 
fjoe = fitCopula(copula=joeCopula(2,dim=2),data=data1,method="ml") 


Udex = (1:n)/(n+1)
Cn = C.n(u=cbind(rep(Udex,n),rep(Udex,each=n)), U=data1, method="C")
EmpCop = expression(contour(Udex, Udex, matrix(Cn, n, n), 
                            col = 2, add = TRUE))
par(mfrow=c(2,3))
contour(tCopula(param=ft$par[7],dim=2,df=round(ft$par[8])), 
        pCopula, main = expression(hat(C)[t]), mgp = c(2.5,1,0), 
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(normalCopula(param=fnorm@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Gauss]), mgp = c(2.5,1,0),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(frankCopula(param=ffrank@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Fr]), mgp = c(2.5,1,0),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(claytonCopula(param=fclayton@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Cl]), mgp = c(2.5,1,0),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(gumbelCopula(param=fgumbel@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Gu]), mgp = c(2.5,1,0),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
contour(joeCopula(param=fjoe@estimate[1], dim = 2), 
        pCopula, main = expression(hat(C)[Joe]), mgp = c(2.5,1,0),
        xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)


par(mfrow=c(2,3))
contour(tCopula(param=ft$par[7],dim=2,df=round(ft$par[8])),
        dCopula, main = expression(hat(c)[t]), mgp = c(2.5,1,0), 
  nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(normalCopula(param=fnorm@estimate[1], dim = 2), 
        dCopula, main = expression(hat(c)[Gauss]), mgp = c(2.5,1,0),
  nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(frankCopula(param=ffrank@estimate[1], dim = 2), 
        dCopula, main = expression(hat(c)[Fr]), mgp = c(2.5,1,0),
  nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(claytonCopula(param=fclayton@estimate[1], dim = 2), 
        dCopula, main = expression(hat(c)[Cl]), mgp = c(2.5,1,0),
  nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(gumbelCopula(param=fgumbel@estimate[1], dim = 2), 
        dCopula, main = expression(hat(c)[Gu]), mgp = c(2.5,1,0),
  nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)
contour(joeCopula(param=fjoe@estimate[1], dim = 2), 
        dCopula, main = expression(hat(c)[Joe]), mgp = c(2.5,1,0),
  nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data1[,1],data1[,2]), col = 2, add = TRUE)



