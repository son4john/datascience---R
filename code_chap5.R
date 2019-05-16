#Chapter V
#test case of EURUSD-like prices

#This is S1-S6###########################
Y = c(30,29,28,28,30,32,31)
Ylogrets = diff(log(Y))
round(Ylogrets,4)
Yprices = c(Y[1],Y[1]*exp(cumsum(Ylogrets)))
Yprices

Y=c(1.3,1.2,1.3,1.4,1.5,1.4,1.3,1.4,1.5)

toPrices <- function(Y1,Ylogrets){
  Yprices = c(Y1,Y1*exp(cumsum(Ylogrets)))
  Yprices
}
Y
toPrices(Y[1],diff(log(Y))) #need starting price, and secquence of returns

##########################################
#assert
sum(Y-toPrices(Y[1],diff(log(Y)))<.00000001) == length(Y)

#RMIXTURE
#you use this to get a better fit heavy tails in alot of model you model that effect with two normal
#distributions combined

#test question how would you handle different mean. include more arguments update normal # distributor
rmixture <- function(N,sigma1,sigma2=0,thresh=.9) { #making sigma2 makes the code more general. if not using mixture leave sig2=0
  variates = vector(length=N)
  U = runif(N)
  for(i in 1:N) # Generate N # of values
    variates[i] = rnorm(1,0,sd=sigma1) #you edit here if you want to update mean 1 observation , 0 mean, sd = sigma
  if(sigma2 != 0) { #only mixture if sigma2 != 0
    for(i in 1:N)
      if(U[i] >= thresh) #when this uniform distribution value greater than the threshold will change
        #to the sigma 2 scenario
        #replace original variate with mixture variate
        variates[i] = rnorm(1,0,sd=sigma2)
  }
  variates
}


hist(rmixture(10000,sigma1=1,sigma2=5),breaks=50)

variates = rmixture(10000, sigma1=1, sigma2=5)
hist(variates,breaks=50,prob=T)
curve(dnorm(x,mean(variates), sd(variates)) ,add=T, col="red") # normal curve


#simulate price to do sensitivity analysis
simPricePath <- function(initPrice,N,seed,sigma1=.05,
                         sigma2=0,thresh=.9) {
  #Non mixture model
  set.seed(seed)
  Xlogrets = rmixture(N,sigma1,sigma2,thresh=thresh)  #calls function
  Xprices = toPrices(initPrice,Xlogrets)  #calls function
  list(Xprices,c(Xlogrets)) # returns a list
}
#unit test 
seed=26
sigma1=0.007157
N=365
par(mfrow=c(2,1)); maxy=10*.007
#not mixed distribution smooth up and down within in the mean returns
Y <- simPricePath(1.3,N=365,seed=seed,sigma1)
Yprices  <- Y[[1]]
Ylogrets <- Y[[2]] #log returns
plot(Yprices,type='l')
plot(Ylogrets,type='l',ylim=c(-maxy,maxy))
points(Ylogrets)

#this time we specifiy a sigma 2 show more extreme events
Z <- simPricePath(1.3,N=365,seed=seed,sigma1,sigma2=4*sigma1)
Zprices  <- Z[[1]]
Zlogrets <- Z[[2]]

plot(Zprices,type='l')
plot(Zlogrets,type='l',ylim=c(-maxy,maxy))
points(Zlogrets)
sd(Ylogrets)
sd(Zlogrets) # higher volatility
#non parametric density distribution only depends on data, black y, blue z mix
#blue part tail higher
#equity has heavy tails how do we capature mix distribution to capture it
par(mfrow=c(1,1))
plot(density(Ylogrets))
lines(density(Zlogrets),col=4)

library(moments)
#compute kurtosis for both values manually
KurtYlogrets = length(Ylogrets)^(-1)*sd(Ylogrets)^
  (-4)*sum((Ylogrets - mean(Ylogrets))^4)
KurtYlogrets

#kurtosis should tell you if the log returns distribution is normal or not
#if its not close to 3 then you need to use a mixed distribution, otherwise
#you under estimate the tail risk
#kurtosis for normal distribution 3
#############################################
# function for kurtosis need moments library
#############################################
kurtosis(Ylogrets) 
#measure Kurtosis of mixture
KurtZlogrets = length(Zlogrets)^(-1)*sd(Zlogrets)^
  (-4)*sum((Zlogrets - mean(Zlogrets))^4)
KurtZlogrets
kurtosis(Zlogrets)

#Multiple paths
library(moments)
par(mfrow=c(3,1))
mapToCol <- function(d)
  if(d==7) 1 else if(d==8)
    2 else if(d==15) 3 else if(d==23) 4 else d
allYlogrets = matrix(nrow=10,ncol=N)
for(path in 1:10) {
  Y <- simPricePath(1.3,N,seed=path,sigma1=.007157)
  Yprices <- Y[[1]]; Ylogrets <- Y[[2]]
  if(path == 1) plot(Yprices,type='l',ylim=c(.8,1.8))
  else lines(Yprices,col=mapToCol(path))
  allYlogrets[path,] = Ylogrets
}
for(path in 1:10) {
  if(path==1) plot(density(allYlogrets[path,]),main="")
  else lines(density(allYlogrets[path,]),
             col=mapToCol(path))
}
mean(Ylogrets)
sd(Ylogrets)
for(path in 1:10) {
  if(path==1) plot(allYlogrets[path,],ylab='Ylogrets')
  else points(allYlogrets[path,],col=mapToCol(path))
}
#mixture
allZlogrets = matrix(nrow=10,ncol=N)
for(path in 1:10) {
  Z <- simPricePath(1.3,N,seed=path,sigma1=.007157,
                    sigma2=4*.007157)
  Zprices <- Z[[1]]; Zlogrets <- Z[[2]]
  if(path == 1) plot(Zprices,type='l',ylim=c(.8,1.8))
  else lines(Zprices,col=mapToCol(path))
  allZlogrets[path,] = Zlogrets
}

for(path in 1:10) {
  if(path==1) plot(density(allZlogrets[path,]),main="")
  else lines(density(allZlogrets[path,]),
             col=mapToCol(path))
}
mean(Zlogrets)
sd(Zlogrets)
for(path in 1:10) {
  if(path==1) plot(allZlogrets[path,],ylab='Zlogrets')
  else points(allZlogrets[path,],col=mapToCol(path))
}
sd(Ylogrets)
sd(Zlogrets)
sd(Zlogrets)/sd(Ylogrets)

######################################
#IMPORTANT PART STARTING CODE 4-2-2019
######################################
#MARKET EVENT
#swiss BANK CHANGE ITS CURRENCEY
#this isn't working properly but the notes are good
library(tseries)
tmixture <- function(N,sigma1,sigma2=0,sigma3=0) #3 stages before the event, during the event, after the event
  #three level mixture with state changes
  #normal time
  #event window
  #
{
  variates = vector(length=N)
  mode = 1
  B = rbinom(365,1,1/365) #how many of these events happen 
  for(i in 1:N)
    variates[i] = rnorm(1,0,sd=sigma1)
  if(sigma2 != 0) { #only mixture if sigma2 != 0
    for(i in 1:N)
      if(B[i] == 1) {
        mode = 2
        #replace original variate with mixture variate
        variates[i] = rnorm(1,0,sd=sigma2)
        print(sigma2)
        print(variates[i])
      } else if (mode == 2) {
        variates[i] = rnorm(1,0,sd=sigma3)
      }
  }
  variates
}
#S<-get.hist.quote("CHF/EUR",provider="oanda",
#                 start="2014-01-30",end="2015-01-29")
setwd(paste(homeuser,"/FinAnalytics/ChapV",sep=""))
S<-rev(read.csv("CHFperEUR.csv",header=TRUE)[,2])


library(Quandl) #data repository
S<-1/rev(Quandl('ECB/EURCHF',
            start_date="2014-01-30",end_date="2015-01-29")[,2])
par(mfrow=c(2,2))
diffLogS <- diff(log(S))
plot(diffLogS,type='p',ylim=c(-.08,.08))
plot(S,type='l',col='blue',ylim=c(.60,1.05),
     xlab="One Year: early 2014 - early 2015",
     ylab="actual CHF per EUR")
S[351:359]
diffLogS351 <- diff(log(S[1:351])) #check before the policy change and then after 351 is before
diffLogS351mean <- mean(diffLogS351)
diffLogS351mean
diffLogS351dailyVol <- sd(diffLogS351)
diffLogS351dailyVol
diffLogSjumpMean = mean(diff(log(S[351:353])))
sd(diff(log(S[351:353])))/diffLogS351dailyVol
diffLogSlast <- diff(log(S[355:365]))
sd(diffLogSlast)/diffLogS351dailyVol

diffLogS351 <- diff(log(S[1:351]))
diffLogS351mean <- mean(diffLogS351)
diffLogS351mean
diffLogS351dailyVol <- sd(diffLogS351)
diffLogS351dailyVol
diffLogSjumpMean = mean(diff(log(S[351:353])))
sd(diff(log(S[351:353])))/diffLogS351dailyVol
diffLogSlast <- diff(log(S[355:365]))
sd(diffLogSlast)/diffLogS351dailyVol

b = 196 #simulation
for(path in b:205) {
  N=365
  set.seed(path)
  Y <- tmixture(N,diffLogS351dailyVol,
                73.00818*diffLogS351dailyVol,
                17.84*diffLogS351dailyVol)
  if(path == b)
    plot(Y,ylim=c(-.08,.08),xlab=path)
  Yprices = c(S[[1]],S[[1]]*exp(cumsum(Y)))
  if(path == b)
    plot(Yprices,col=14,type="l",ylim=c(.60,1.05),
         xlab="365 Days = 1 Simulated Year",
         ylab="simulated CHF per EUR")
  else
    lines(Yprices,col=mapToCol(path%%24))
  print(path)
  Sys.sleep(5)
}
