##########################################################################################
###########  R script for Chapter 20   ###################################################
###########  of Statistics and Data Analysis for Financial Engineering, 2nd Edition ######
###########  by Ruppert and Matteson  ####################################################
##########################################################################################

##################################################
############  Code for Figure 20.1  ##############
##################################################
pdf("bayes_densities.pdf",width=6,height=5)
p1 = .6^5
p2 = .4^5
numer = p1
denom = p1 + p2
p1
p2
numer
denom
numer/denom
x=seq(0,1,.01)
plot(x,dbeta(x,7,2),type="l",lwd=2,
     ylab=expression(paste("density(",theta,")")),xlab=expression(theta))
lines(x,dbeta(x,2,2),lty=5,lwd=2,col="red")
abline(v=qbeta(.05,7,2),lty=5)
abline(v=qbeta(.95,7,2),lty=5)
abline(v=6/7,lty="dotted")
abline(h=0)
legend("topleft",c("prior","posterior"),lwd=2,lty=c(5,1),col=c("red", "black"))
graphics.off()

##################################################
############  Code for Figure 20.2  ##############
##################################################
pdf("beta_densities2.pdf",width=6,height=5)
x = seq(0,1,.01)
par(lwd=2)
plot(x,dbeta(x,500,500),type="l",lty=1,xlab="y",
     ylab="density",main="beta densities",lwd=2)
lines(x,dbeta(x,20,20),type="l",lty=2,col="red",lwd=2)
lines(x,dbeta(x,3,3),type="l",lty=3,col="blue",lwd=2)
legend("topleft",c(
  expression(paste(alpha," = ",beta," = 500")) ,
  expression(paste(alpha," = ",beta," = 20")),
  expression(paste(alpha," = ",beta," = 3"))),
  lty=c(1,2,3),col=c("black", "red", "blue"),lwd=2)
graphics.off()

##################################################
############  Code for Figure 20.3  ##############
##################################################
pdf("MSE.pdf",width=6,height=5)
delta = seq(0,1,by=.005)
MLEVar = 1/2
k = 1.5
plot(delta, (delta-1)^2*k^2 + delta^2*MLEVar,type="l",ylim=c(0,2.3),
     xlab=expression(delta),ylab="MSE",lwd=2)
k=1
lines(delta, (delta-1)^2*k^2 + delta^2*MLEVar,type="l",lty=2,lwd=2,col="red")
k = .5
lines(delta, (delta-1)^2*k^2 + delta^2*MLEVar,type="l",lty=3,lwd=2,col="blue")
legend("topright",c("prior bias = 1.5","prior bias =  1",
   "prior bias = 1/2"),lty=c(1,2,3),lwd=2,col=c("black","red", "blue") )
abline(h=MLEVar)
graphics.off()

##############################################################################
############  Code for Examples 20.9 and 20.11 and Figure 20.4  ##############
##############################################################################

library(rjags)
library("Ecdat")
data(SP500)
r = SP500$r500
N = length(r)
data = list(r = r, N = N)
inits = function(){list(mu = rnorm(1, mean = mean(r), sd = 2*sd(r)),
   tau = runif(1, 0.2/var(r), 2/var(r)), k = runif(1, 2.5, 10))}
t1 = proc.time()
univt.mcmc <- jags.model("univt.bug", data=data, inits = inits,
                      n.chains = 3, n.adapt=1000, quiet=FALSE)
nthin = 20
univt.coda = coda.samples(univt.mcmc, c("mu","k","sigma"), 100*nthin, thin = nthin)
summary(univt.coda,digits=2) 
t2 = proc.time()
(t2-t1)/60

pdf("basic_plot.pdf", width=5, height=6)   ##  Figure 20.4
par(mfrow = c(3, 2))
plot(univt.coda, auto.layout = F)
graphics.off()

gelman.diag(univt.coda)
effectiveSize(univt.coda)

pdf("gelman_plot.pdf",width=6,height=6)  ##  Figure 20.6
gelman.plot(univt.coda)
graphics.off()
dic.samples(univt.mcmc, 100*nthin, thin = nthin, type = "pD")



#####################################################################
############  Code for Figure 20.5 and Example 20.10  ###############
#####################################################################
library(rjags)
library(coda)
library(mvtnorm)
set.seed(90201)
N = 50
beta1 = 1
beta2 = 2
alpha = 1
x1 = rnorm(N,mean=3,sd=2)
x2 = x1 + rnorm(N,mean=3,sd=.2)
cor(x1,x2)
x=cbind(rep(1,N),x1,x2)
y = alpha + beta1*x1 + beta2*x2 + rnorm(N,mean=0,sd=.2)
data=list(y = y, x = x, N = N)
summ = summary(lm(y~x1+x2))
betahat = as.numeric( summ$coeff )[1:3]
covbetahat = summ$sigma^2 * solve(t(x)%*%x)
inits=function(){list(beta=as.numeric(rmvnorm(n = 1, mean = betahat, sigma=1.5*covbetahat)) ,
                      tau=runif(1,1/(4*summ$sigma^2),4/summ$sigma^2))}
regr <- jags.model("lin_reg_vect.bug", data=data, inits = inits,
                      n.chains = 3, n.adapt=1000, quiet=FALSE)
regr.coda = coda.samples(regr, c("beta","tau"), 1000, thin = 1)
regr.coda.largeN = coda.samples(regr, c("beta","tau"), 50000, thin = 100)
#  no collinearity
set.seed(90201)
N = 50
beta1 = 1
beta2=2
alpha = 1
x1 = rnorm(N,mean=3,sd=2)
x2 = rnorm(N,mean=3,sd=2) + rnorm(N,mean=3,sd=.2)
cor(x1,x2)
x=cbind(rep(1,N),x1,x2)
y = alpha + beta1*x1 + beta2*x2 + rnorm(N,mean=0,sd=.2)
data=list(y = y, x = x, N = N)
summ = summary(lm(y~x1+x2))
betahat = as.numeric( summ$coeff )[1:3]
covbetahat = summ$sigma^2 * solve(t(x)%*%x)
inits=function(){list(beta=as.numeric(rmvnorm(n = 1, mean = betahat, sigma=1.5*covbetahat)) ,
                      tau=runif(1,1/(4*summ$sigma^2),4/summ$sigma^2))}
regr.noco <- jags.model("lin_reg_vect.bug", data=data, inits = inits,
                   n.chains = 3, n.adapt=1000, quiet=FALSE)
regr.coda.noco = coda.samples(regr.noco, c("beta","tau"), 1000, thin = 1)

pdf("linRegMCMC.pdf",width=7,height=6)   ##  Figure 20.5  ##
par(mfrow=c(3,4))
traceplot(regr.coda)
traceplot(regr.coda.noco)
traceplot(regr.coda.largeN)
graphics.off()

gelman.diag(regr.coda.noco)
##########################################################################
#####  The Code for Example 20.11 is with the code for Example 20.9  #####
##########################################################################

##################################################
############  Code for Figure 20.7 ###############
##################################################

library(rjags)
dat = read.csv("midcapD.ts.csv") 
market = 100*as.matrix(dat[,22])
x = 100*as.matrix(dat[,-c(1,22)])
m=20
k=100
x1 = x[1:k,]
x2 = x[(k+1):500,]
mu1 = apply(x1,2,mean)
mu2 = apply(x2,2,mean)
means = apply(x1,2,mean)
sd2 = apply(x1,2,sd)
tau_mu = 1/mean(sd2^2)
tau_eps = 1/sd(means)^2
n=k
data=list(x1 = x1, n = n, m = m)
inits.midCap =function(){list(alpha = 0.001, mu=means,tau_eps=tau_eps,tau_mu=tau_mu)}
midCap <- jags.model("midCap.bug", data=data, inits = inits.midCap,
   n.chains = 3, n.adapt=1000, quiet=FALSE)
nthin = 20
midCap.coda = coda.samples(midCap, c("mu","tau_mu","tau_eps","alpha",
        "sigma_mu","sigma_eps"), 500*nthin, thin = nthin)
summ.midCap = summary(midCap.coda)
summ.midCap
post.means = summ.midCap[[1]][2:21,1]
delta = 5.4/(5.4+75.7)
print(delta,digits=2)
pdf("midcap.pdf",width=6,height=3.75)   ##  Figure 20.7  ##
par(mfrow=c(1,2))
plot(c(rep(1,m),rep(2,m)),c(mu1,mu2),
     xlab="estimate                         target",ylab="mean",
     main="sample means",
     ylim=c(-.3,.7),axes=F)
axis(2)
axis(1,labels=F,tick=T,lwd.tick=0)
for (i in 1:m){ lines(1:2,c(mu1[i],mu2[i]),col=i) }
plot(c(rep(1,m),rep(2,m)),c(post.means,mu2),
     xlab="estimate                         target",ylab="mean",
     main="Bayes",
     ylim=c(-.3,.7),axes=F)
axis(2)
axis(1,labels=F,tick=T,lwd.tick=0)
for (i in 1:m){ lines(1:2,c(post.means[i],mu2[i]),col=i) }
graphics.off()

options(digits=2)
sum((mu1-mu2)^2 )
sum((post.means-mu2)^2)
sum((mean(mu1)-mu2)^2)



##################################################
############  Code for Example 20.13   ###########
##################################################

library(R2WinBUGS)
library(MASS)  # need to mvrnorm
library(MCMCpack) # need for rwish
library(mnormt)
data(CRSPday,package="Ecdat")
y = CRSPday[,4:7]
N = dim(y)[1]
m = dim(y)[2]
mu0 = rep(0,m)
Prec_mu = diag(rep(1,m))/10000
Prec_tau =  diag(rep(1,m))/10000
df_wishart = 6
df_likelihood = 6
df_prior = 6
data = list(y = y, N = N, Prec_mu = Prec_mu, 
   Prec_tau = Prec_tau,
   mu0 = mu0, m = m, df_likelihood = df_likelihood, 
   df_prior = df_prior, df_wishart = df_wishart)
inits_t_CRSP = function(){list(mu = mvrnorm(1, mu0, diag(rep(1,m)/100)),
   tau = rwish(6,diag(rep(1,m))/100))}
library(R2WinBUGS)
multi_t.sim = bugs(data,inits_t_CRSP,model.file="mult_t_CRSP.bug",
                   parameters=c("mu","tau"),n.chains = 3,
                   n.iter=2200,n.burnin=200,n.thin=2,
                   program="WinBUGS",bugs.seed=13,codaPkg=FALSE)
print(multi_t.sim,digits=2)
print(multi_t.sim,digits=7)

tauhat = multi_t.sim$mean$tau
lambdahat = solve(tauhat)
sdinv = diag(1/sqrt(diag(lambdahat)))
cor = sdinv %*% lambdahat %*% sdinv
print(cor,digits=4)


########### mult_t_CRSP.bug  #####################
model{
  for(i in 1:N)
  {
    y[i,1:m] ~ dmt(mu[], tau[,], df_likelihood)
  }
  mu[1:m] ~ dmt(mu0[], Prec_mu[,], df_prior) 
  tau[1:m,1:m] ~ dwish(Prec_tau[,], df_wishart)
  lambda[1:m,1:m] <- inverse(tau[,])
}


###  The following code is not working  ###
library(rjags)
multi_t_CRSP <- jags.model("mult_t_CRSP.bug", data=data, inits = inits_t_CRSP,
   n.chains = 3, n.adapt=1000, quiet=FALSE)
nthin = 1
multi_t_CRSP.coda = coda.samples(multi_t_CRSP, c("mu","tau","lambda"), 100*nthin, thin = nthin)
summary(multi_t_CRSP.coda)



##########################################################
############  Code for Example 20.14       ###############
##########################################################
####  Warning: this examples takes a long time to run  ###
library(rjags)
dat = read.csv("S&P500_new.csv")
prices = dat$Adj.Close
y = diff(log(prices))
#####  get initial estimates  #####
N = length(y)
logy2 = log(y^2)
fitar = lm(logy2[2:N] ~ logy2[1:(N - 1)])
beta0Init = as.numeric(fitar$coef[1])
phiInit = as.numeric(fitar$coef[2])
sfitar = summary(fitar)
tauInit = 1/sfitar$sigma^2
#####  Set up for MCMC  #####
N = length(y)
data = list(y=y,N=N)
inits_stochVol_ARMA11 = function(){list(mu = rnorm(1, mean = mean(y), sd = sd(y) / sqrt(N)),
                                        logh = log(y^2), beta0 = runif(1, beta0Init * 1.5, beta0Init/1.5),
                                        phi = runif(1,phiInit / 1.5, phiInit * 1.5),
                                        tau_v = runif(1, tauInit / 1.5, tauInit * 1.5),
                                        theta = runif(1,-0.5,0.5))}
stochVol_ARMA11 <- jags.model("stochVol_ARMA11.bug", data=data, inits=inits_stochVol_ARMA11,
                              n.chains = 3, n.adapt=1000, quiet=FALSE)

nthin = 20
stochVol_ARMA.coda = coda.samples(stochVol_ARMA11, c("mu", "beta0", "phi", "theta", "tau_v", "nu", "tau"), 100*nthin, thin = nthin)
summ_stochVol_ARMA11 = summary(stochVol_ARMA.coda)
summ_stochVol_ARMA11
head(summ_stochVol_ARMA11[[1]], 8)
tail(summ_stochVol_ARMA11[[1]], 8)

dic.stochVol_ARMA11 = dic.samples(stochVol_ARMA11, 100*nthin, thin = nthin, type = "pD")
dic.stochVol_ARMA11

tau_stochVolARMA11 = summ_stochVol_ARMA11[[1]][5:968, 1]
h_stochVolARMA11 = 1/tau_stochVolARMA11
year = seq(2011, 2014 + 10/12, length = N)

##  need to run example 20.15 before creating Figure 20.8  ##
pdf("stochVolandGARChCondVar.pdf", width = 6, height = 5)    ##  Figure 20.8
par(mfrow=c(1,1))
plot(year, sqrt(h_stochVolARMA11), ylim = c(0,.035), ylab = "conditional std. dev.", 
     type = "l", lwd = 2)
lines(year, sqrt(h_garch11), col= "red", lwd = 1)
legend("topright", c("Stoch. vol", "GARCH"), lty = 1, lwd = c(2,1), col = c("black", "red"))
graphics.off()


##  need to run example 20.15 before creating Figure 20.9  ##
pdf("stochVolandGARCH_acf.pdf",width = 8, height = 3.25)    ##  Figure 20.9  ##
par(mfrow=c(1,3))
acf((y-mean(y))^2, main = "(a) Constant conditional variance")
acf( (y-mean(y))^2*tau_stochVolARMA11, main = "(b) Stochastic Volatility")
acf( (y-mean(y))^2*tau_garch11, main = "(c) GARCH")
graphics.off()

##########################################################
############  Code for Example 20.15       ###############
##########################################################

library(rjags)
dat = read.csv("S&P500_new.csv")
prices = dat$Adj.Close
y = diff(log(prices))
#####  get initial estimates  #####
N = length(y)

#####  Set up for MCMC  #####

data = list(y=y, N=N)
##  alpha0 initiation is different here from in the book.  Large values of alpha0
##  were causing problems.
inits_garch11 = function(){list(alpha0 = runif(1, 0.001, 0.01), 
                                beta0 = runif(1, 0.001, 0.25), mu = runif(1, 0.001, 0.25), 
                                alpha1 = runif(1, 0.001, 0.25), nu = runif(1, 2, 10))}
t1 = proc.time()
garch11 <- jags.model("garch11.bug", data=data, inits = inits_garch11,
                      n.chains = 3, n.adapt=1000, quiet=FALSE)
nthin = 20
garch11.coda = coda.samples(garch11,c("mu","beta0","alpha0", "alpha1", "nu", "tau"), 
        100*nthin, thin = nthin)
t2 = proc.time()
(t2 - t1)/60

dic.garch11 = dic.samples(garch11, 100*nthin, thin = nthin)
dic.garch11
diffdic(dic.garch11, dic.stochVol_ARMA11)


summ_garch11 = summary(garch11.coda)
head(summ_garch11[[1]])
tail(summ_garch11[[1]])
tau_garch11 = summ_garch11[[1]][-(1:5), 1]
h_garch11 = 1/tau_garch11

std_res = (y - mean(y)) * sqrt(tau_garch11)
plot(tau_garch11[]^(-1/2))
acf(std_res[-(1:3)])

##################################################
############  Code for Example 20.16  ############
##################################################

library(rjags)
dat = read.csv("Stock_Bond.csv")
y = dat[,c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 22)]
n = dim(y)[1]
m = dim(y)[2] - 1
r = y[-1,]/y[-n,] - 1
k1 = 250
k2 = k1 + 250
rtrain = r[1:k1, 1:m]
mkt_train = r[1:k1, 11]
rtest = r[(k1+1):k2, 1:m]
lambda = 10
data = list(R = rtrain, N = k1, mkt = mkt_train, m = m)
inits.Capm = function(){list(beta = rep(1,m))}
Capm.jags <- jags.model("BayesCapm.bug", data = data, inits = inits.Capm,
                        n.chains = 1, n.adapt = 1000, quiet = FALSE)
nthin = 10
N = 500
Capm.coda = coda.samples(Capm.jags, 
        c("beta", "tauepsilon", "taubeta"), N * nthin, thin = nthin)
MCMC_out = Capm.coda[[1]]
summ = as.matrix(summary(Capm.coda)[[1]][,1])
summ
beta = summ[1:10]
taubeta = summ[11]
tauepsilon = summ[12:21]
sigmaepsilon = tauepsilon^(-.5)

pdf("two_betas.pdf", width = 6, height = 5)    ##  Figure 20.11
lmFit = lm(as.matrix(rtrain) ~ mkt_train)
beta_lm = lmFit$coeff[2,]
par(mfrow =  c(1,1))
plot(beta_lm, beta, xlim = c(0.7, 1.55), ylim = c(0.7, 1.55), 
     xlab = "beta - least squares", ylab = "beta - Bayes")
grid(lwd=2)
abline(0,1)
graphics.off()

ExUtil = function(w){
  -1 + exp(-lambda * (1 + t(w) %*% mu) + lambda^2 * t(w) %*% Omega %*% w /2 )
}

lambda = 3
mu_model_free = colMeans(rtrain)
Omega_model_free = cov(rtrain)
mu_Capm = beta * mean(mkt_train)
Omega_Capm = beta %o% beta * var(mkt_train) + diag(sigmaepsilon^2)

mu = mu_model_free
Omega = Omega_model_free

library(quadprog)
opt1 = solve.QP(Dmat = as.matrix(lambda^2 * Omega), dvec = lambda * mu, 
                Amat = as.matrix(rep(1,10)), bvec = 1, meq = 1)
w_model_free = opt1$solution 

mu = mu_Capm
Omega = Omega_Capm
opt2 = solve.QP(Dmat = as.matrix(lambda^2 * Omega), dvec = lambda * mu, 
                Amat = as.matrix(rep(1,10)), bvec = 1, meq = 1)
w_Capm = opt2$solution

pdf("two_w_lambda3.pdf", width = 6, height = 5)    ##  Figure 20.10
plot(w_model_free, type = "b", col = "blue", main = paste("lambda = ", lambda), 
     ylab = "w", ylim = c(-2.25, 2.25), lwd = 2 )
points(w_Capm, type = "b", col = "red", pch = "*", lwd = 2)
legend("bottomleft", c("model-free", "CAPM"), lty = 1, 
       pch = c("o", "*"), col = c("blue", "red"), lwd = 2)
abline(h=0)
graphics.off()


return_model_free = as.matrix(rtest) %*% w_model_free
ExUt_model_free = mean(1 - exp(-lambda * return_model_free))

return_Capm = as.matrix(rtest) %*% w_Capm
ExUt_Capm = mean(1 - exp(-lambda * return_Capm))

print(c(ExUt_model_free, ExUt_Capm), digits = 2)
print(c(mean(return_model_free), mean(return_Capm)), digits = 2)
print(c(sd(return_model_free), sd(return_Capm)), digits = 2)






##################################################
############  Code for R lab       ###############
##################################################

##### Univariate t-distribution  #####

library(rjags)
data(CRSPmon,package="Ecdat")
ibm = CRSPmon[,2]
r = as.numeric(ibm)
r = ibm
N = length(r)
ibm_data = list(r = r, N = N)
inits = function(){list(mu = rnorm(1, 0, 0.3), tau = runif(1, 1, 10),
   k = runif(1, 1, 30))}
univ_t <- jags.model("univt.bug", data=ibm_data, inits = inits,
   n.chains = 3, n.adapt=1000, quiet=FALSE)
nthin = 2
univ_t.coda = coda.samples(univ_t, c("mu", "tau", "k", "sigma"), 
   n.iter = 500*nthin, thin = nthin)
summary(univ_t.coda)
effectiveSize(univ_t.coda)
gelman.diag(univ_t.coda)
gelman.plot(univ_t.coda)
par(mfrow = c(2, 2))
traceplot(univ_t.coda)
par(mfrow = c(2, 2))
autocorr.plot(univ_t.coda, auto.layout = FALSE)
library(lattice)
densityplot(univ_t.coda)

##### BUGS code - place in file univt.bug  #####
model{
  for (t in 1:N)
  {
    r[t] ~ dt(mu, tau, k)
  }
  mu ~ dnorm(0.0, 1.0E-6)
  tau ~ dgamma(0.1, 0.01)
  k ~ dunif(2, 50)
  sigma2 <- (k/(k-2))/tau
  sigma <- sqrt(sigma2)
}


#####  AR model  #####
library(rjags)
data(Tbrate, package = "Ecdat")
#  r = the 91-day treasury bill rate
#  y = the log of real GDP
#  pi = the inflation rate
del_dat = diff(Tbrate)
y = del_dat[,2]
N = length(y)
GDP_data=list(y = y, N = N)
inits=function(){ list(mu = rnorm(1,0, 2*sd(y)/sqrt(N)), 
   phi = rnorm(1, 0, 0.3), tau = runif(1, 1, 10)) }
ar1 <- jags.model("ar1.bug", data = GDP_data, inits = inits,
   n.chains = 3, n.adapt = 1000, quiet = FALSE)
nthin = 20
ar1.coda = coda.samples(ar1, c("mu", "phi", "sigma"), 
   n.iter = 500 * nthin, thin = nthin)
summary(ar1.coda, digits = 3)
arima(y, order = c(1, 0, 0))

#####  BUGS code - place in file ar1.bug  #####
model{
  for(i in 2:N){
    y[i] ~ dnorm(mu + phi * (y[i-1] - mu), tau) 
  }
  mu ~ dnorm(0, 0.00001)
  phi ~ dnorm(0, 0.00001)
  tau ~ dgamma(0.1 ,0.0001)
  sigma <- 1/sqrt(tau)
}





#####  MA(1) Model  #####
library(rjags)
set.seed(5640)
N=600
y = arima.sim(n = N, list(ma = -0.5), sd = 0.4)
y = as.numeric(y) + 3
q=5
ma.sim_data=list(y = y, N = N, q = q)
inits.ma=function(){list(mu = rnorm(1, mean(y), 2*sd(y)/sqrt(N)),
   theta = rnorm(1, -0.05, 0.1), tau = runif(1, 5, 8))}

ma1 <- jags.model("ma1.bug", data = ma.sim_data, inits = inits.ma,
                  n.chains = 3, n.adapt = 1000, quiet = FALSE)
nthin = 5
ma1.coda = coda.samples(ma1, c("mu", "theta", "sigma", "ypred"), 
   n.iter = 500 * nthin, thin = nthin)
summary(ma1.coda)

#####  BUGS code - place in file ma1.bug  #####
model{
  for (i in 2:N) 
  {
    w[i] <- y[i] - mu - theta*w[i-1]
  }
  w[1] ~ dnorm(0, 0.01)
  for (i in 2:N)
  {
    y[i] ~ dnorm(mu + theta * w[i-1], tau)
  }
  mu ~ dnorm(0, 0.0001)
  theta ~ dnorm(0, 0.0001)
  tau ~ dgamma(0.01, 0.0001)
  sigma <- 1/sqrt(tau)
  for (i in 1:q)
  {
    ypred[i] ~ dnorm(theta * w[N + i - 1], tau)
    w[i + N] <- ypred[i] - theta * w[N + i - 1]
  }
}


#####  ARMA(1,1) Model  #####

library(rjags)
set.seed(5640)
N=600
y = arima.sim(n = N, list(ar = .9, ma = -.5), sd = .4)
y = as.numeric(y)
arma11.sim_data=list(y = y, N = N)
inits.arma11 =function(){list(phi = rnorm(1, 0 , 0.3), theta=rnorm(1,-0.5, 0.1), tau=runif(1,5,8))}

arma11 <- jags.model("arma11.bug", data = arma11.sim_data, inits = inits.arma11,
   n.chains = 3, n.adapt = 1000, quiet = FALSE)
nthin = 5
arma11.coda = coda.samples(arma11, c("phi", "theta", "sigma"), 
   n.iter = 500 * nthin, thin = nthin)
summary(arma11.coda)


##################  Exercises  ################################

##  Exercise 5  ##
library(rjags)
dat = read.csv("Stock_Bond.csv")
y = dat[1:1501,c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 22)]
n = dim(y)[1]
m = dim(y)[2] - 1
r = y[-1,]/y[-n,] 
mkt = r[, 11]
GM = r[,1]
data = list(r = GM, mkt = mkt, n = n - 1)
inits = function(){list(beta = 1, theta = 0)}
tsReg.jags <- jags.model("tsReg.bug.R", data = data, inits = inits,
      n.chains = 1, n.adapt = 1000, quiet = FALSE)
nthin = 10
N = 500
tsReg.coda = coda.samples(tsReg.jags, 
      c("beta", "taueps", "theta"), N * nthin, thin = nthin)
summary(tsReg.coda)

