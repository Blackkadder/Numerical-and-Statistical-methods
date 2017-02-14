setwd("C:/Users/e115487/Box Sync/Financial Engineering/Numerical and statistical methods/Team project/Data/")


library(FitAR)
library(Ecdat)
library(moments)
library(MASS)
library(tseries)
library(MTS)
library(xts)
library(urca)
library(forecast)
library(Rcmdr)




## Import and transfer data to time series format

clean = read.csv("clean1.csv")

clean <- clean[,-1]

Total = ts(clean, start=c(1992,1), end=c(2015,12), frequency=12) 

## First glance of S&P-500 sector index

par(mfrow=c(2,5))
for(i in colnames(Total[,1:10]))
{
  plot(Total[,i],ylab="S&P-500 Sector Index",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i)
}


##Compute Sector Return

n=dim(Total)[1]
Total2<- Total[2:n,1:10]/Total[1:(n-1),1:10]-1

Total2 = ts(Total2, start=c(1992,2), end=c(2015,12), frequency=12) 


##Check Returns' stationary (Dependent Variable)

par(mfrow=c(2,5))
for(i in colnames(Total2[,1:10]))
{
  plot(Total2[,i],ylab="S&P-500 Sector Index",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i)
}


par(mfrow=c(5,4))
for(i in colnames(Total2[,1:10]))
{
  acf(Total2[,i],main=i)
  pacf(Total2[,i],main=i)
}


for(i in colnames(Total2[,1:10]))
{print(adf.test(Total2[,i])$p.value)
}
#All are stationary 

##Conclusion: Most of the sectors' returns are stationary, thus, 
##in this research, we only take first diffirence for dependent variables
##But Healthcare and Financial sector can be further investigated in ARIMA model


######################################################


##Check independent variables' stationary 


## First glance
par(mfrow=c(4,4))
for(i in colnames(Total[,11:26]))
{
  plot(Total[,i],ylab="Indicators",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i)
}

## Compute first difference for independent variables, except for recession 

X=diff(Total[,11:26])
X<- X[,-13]


par(mfrow=c(3,5))
for(i in colnames(X[,1:15]))
{
  plot(X[,i],ylab="Difference of Indicators",type="l",xlab="Year",cex.lab=1.5,
       cex.axis=1.5,cex.main=1.3,main=i)
}

par(mfrow=c(2,5))
for(i in colnames(X[,1:5]))
{
  acf(X[,i],main=i)
  pacf(X[,i],main=i)
}

par(mfrow=c(2,5))
for(i in colnames(X[,6:10]))
{
  acf(X[,i],main=i)
  pacf(X[,i],main=i)
}

par(mfrow=c(2,5))
for(i in colnames(X[,11:15]))
{
  acf(X[,i],main=i)
  pacf(X[,i],main=i)
}

for(i in colnames(X[,1:15]))
{print(adf.test(X[,i])$p.value)
}

Usrecession<- Total[-1,23]
Independent <- cbind(X, Usrecession)
colnames(Independent)[1:16]<- c(c(colnames(X)),"Usrecession")
View(Independent)

#Unemployment is not stationary under 5%, but stationary under 10%




#####Generate final dataset
#####dependent using return rate, independent using first difference

Final <- cbind(Total2, Independent)
colnames(Final)[1:26] <- c(c(colnames(Total2)),c(colnames(Independent)))
View(Final)


###########################################################################
###########################Regression, Test################################
###########################################################################

Final<- as.data.frame(Final)
attach(Final)
View(Final)
colnames(Final)


full.Healthcare <- lm(Healthcare ~ X30.YEAR + Federal.funds.rate + CPIAUCSL +
                        unemploymentRATE.regular + X5.year.treasury.rate + exchange.rate +real.disposable.personal.income
                      + PCE + industrail.production + import + export + M1 + M2 + Nominal.GDP +
                        Real.GDP + Usrecession) 
summary(full.Healthcare)

resstep<-stepwise(full.Healthcare)

full.Healthcare.Reduce <- lm(Healthcare ~ PCE)
summary(full.Healthcare.Reduce)


full.Financial <- lm(Financial ~ X30.YEAR + Federal.funds.rate + CPIAUCSL +
                       unemploymentRATE.regular + X5.year.treasury.rate + exchange.rate +real.disposable.personal.income
                     + PCE + industrail.production + import + export + M1 + M2 + Nominal.GDP +
                       Real.GDP + Usrecession)  
summary(full.Financial)

resstep<-stepwise(full.Financial)

full.Financial.Reduce <- lm(Financial ~ Usrecession + export)
summary(full.Financial.Reduce)




full.Energy <- lm(Energy ~ X30.YEAR + Federal.funds.rate + CPIAUCSL +
                    unemploymentRATE.regular + X5.year.treasury.rate + exchange.rate +real.disposable.personal.income
                  + PCE + industrail.production + import + export + M1 + M2 + Nominal.GDP +
                    Real.GDP + Usrecession) 
summary(full.Energy)

resstep<-stepwise(full.Energy)

full.Energy.Reduce <- lm(Energy ~ exchange.rate + import)
summary(full.Energy.Reduce)




full.Utilities <- lm(Final[,4] ~ Final[,11:26]) 
summary(full.Utilities)


full.IT <- lm(Final[,5] ~ Final[,11:26]) 
summary(full.IT)

full.Consumer.Discretionary <- lm(Final[,6] ~ Final[,11:26]) 
summary(full.Consumer.Discretionary)

full.Consumer.staples <- lm(Final[,7] ~ Final[,11:26]) 
summary(full.Consumer.staples)

full.Industrials <- lm(Final[,8] ~ Final[,11:26]) 
summary(full.Industrials)

full.Telecom <- lm(Final[,9] ~ Final[,11:26]) 
summary(full.Telecom)

full.Materials <- lm(Final[,10] ~ Final[,11:26]) 
summary(full.Materials)


###########################################################################
###############Create PCA using all macro factors##########################



PCA.Total = prcomp(Final[,11:25])
summary(PCA.Total)
PCA.Total$rotation

PCA.T <- predict(PCA.Total, newdata=Final)
View(PCA.T)

Total.PCA <- cbind(Final, PCA.T)
View(Total.PCA)

attach(Total.PCA)


full.Healthcare.PCA <- lm(Healthcare ~ PC1 + PC2) 
summary(full.Healthcare.PCA)

resstep<-stepwise(full.Energy)

full.Energy.Reduce <- lm(Energy ~ exchange.rate + import)
summary(full.Energy.Reduce)

a=auto.arima(Energy,xreg=cbind(exchange.rate,import))
summary(a)