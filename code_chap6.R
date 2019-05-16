#Chapter VI
library(quantmod)
library(PerformanceAnalytics)
sym.vec <-c("^GSPC","^VIX","GOOG")
getSymbols(sym.vec, from = "2005-01-03", to = "2015-09-16")

#drop everything except adjusted close
GSPC <- GSPC[, "GSPC.Adjusted", drop=F]
VIX <- VIX[, "VIX.Adjusted", drop=F]

#takes the logs and then differences
GSPC.logret = CalculateReturns(GSPC, method="log")

#when we difference the first subscript becomes NA
GSPC.logret[1]
GSPC.logret[1] = 0.0

#plotting our information ts time series object
par(mfrow=c(3,1))
plot(GSPc)
plot(ts(GSPC))
plot(ts(GSPC.logret))
plot(ts(VIX))
hist(GSPC.logret, breaks = 100)
plot(density(GSPC))

library(TSA)
library(ggplot2)
data(google)

#google histogram and curve
hist(google, breaks=100)
curve(dnorm(x, mean=mean(google), sd=sd(google)), add=TRUE, col="blue")

#custom GSPC.logret histogram and curve
ggplot(NULL,aes(x=as.vector(GSPC.logret),y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size = 0.2) +
  geom_density(colour="blue")

dt4<-function(x) dt(x,df=4)

#plot the 3 density functions Blue standard normal, Green Cauchy, red t(4) degrees fredom = 4
ggplot(data.frame(x=c(-5,5)),aes(x=x)) +
  stat_function(fun=dnorm, colour="blue") +
  stat_function(fun=dcauchy, colour="green") +
  stat_function(fun=dt4, colour="red")

#Skewness is a measure of the symmetry in a distribution. ... It measures the amount of probability in the tails. 
#The value is often compared to the kurtosis of the normal distribution, which is equal to 3. 
#If the kurtosis is greater than 3, then the dataset has heavier tails than a normal distribution (more in the tails).
par(mfrow=c(2,2))
hist(rcauchy(n=10000), main="Cauchy",breaks=100)
hist(rt(n=10000,df=4), main="t(4)",breaks=100)
hist(rnorm(n=10000), main="Standard Normal",breaks=100)
hist(runif(n=10000), main="Uniform",breaks=100)
set.seed(255270)
kurtosis(rcauchy(n=10000))
kurtosis(rt(n=10000,df=4))
kurtosis(rnorm(n=10000))
kurtosis(GSPC.logret[c(-1)]) #remove 1st elem
kurtosis(runif(n=10000))

#time series analysis package
library(TSA)
library(tseries)
#average monthly tempature recorded in Dubuque, Iowa (1964 - 1975)
data(tempdub)
plot(tempdub,col='blue')

#test stationarity Augment Dickey-Fuller test
#null hypothesis non-stationarity
adf.test(tempdub)

#highly stationary time series
#seasonal mean model used 12 months in our case
#ysubt = musbutt + xsubt (mu cyclical) (x random)
month <- season(tempdub)
# - 1 in the regression suppress the coefficent 
model1 <- lm(tempdub ~ month - 1)
summary(model1)
#the model explains the data well


#now lets look at less non-stainonary data
#however this does not have time trends
data(hare)
plot(hare,col='blue')

#test for stationarity
adf.test(hare)

#it could be hetroskedasticity, or varation in the magnitude of the cycles
#box-cox to do power transformation
par(mfrow=c(2,2))

#lamada is the power and between 0 and 1 confidence interval works
#so a squareroot will work fine
#running agumented dick-fuller afterwards shows that it looks good and is stationary
BoxCox.ar(hare)
plot(sqrt(hare),col='blue')
acf(sqrt(hare))
pacf(sqrt(hare))
adf.test(sqrt(hare))

#structure of the arma process
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(armasubsets(y=hare,nar=7,nma=7))
acf(sqrt(hare))
pacf(sqrt(hare))

#3 from the PACF and zero from ACF(the ocelatiling but decaying behavior causes us to say 0)
#fit it to model
#then test the residuals
m1.hare <- arima(x=sqrt(hare),order=c(3,0,0))
runs(rstandard(m1.hare))

#should not expecting some repeating patterns for residuals
tsdiag(m1.hare)

#these are both single time series examples above


#from the shaprio test we can conclude that this is good enough model
par(mfrow=c(1,2))
hist(rstandard(m1.hare))
qqnorm(rstandard(m1.hare),col='blue')
qqline(rstandard(m1.hare))

shapiro.test(residuals(m1.hare))

#now we are using the model to perdict the future of hares
#the square is we tranformed the data with square root so when we make perdiciton 
#we have to transform back
square<-function(x) {y=x^2}
plot(m1.hare,n.ahead=25,xlab='Year',ylab='Hare Abundance',
          pch=19,transform=square,
          col='blue')

#examples of non-stationary with some time trends
#Earnings of Johnson and Johnson:
#thats what most financial is about
data(JJ)
plot(JJ,col='blue')
par(mfrow=c(2,1))
#increases exponetially time trend
plot(JJ,col='blue')

#take log
plot(log(JJ),ylab='log(Earnings)',type='l',col='blue')

#it is still increasing and not stationary so we take difference
par(mfrow=c(3,1))
plot(diff(log(JJ)),ylab='log differenced',type='l',col='blue')
#there is still a pattern so we take a lag difference
plot(diff(log(JJ),lag=4),ylab='seasonal diff',type='l',col='blue')
#optional information if there is still a large pattern you can combine both togther like below
#we will try to avoid this because after taking many difference
#it is hard to interpert the results
plot(diff(diff(log(JJ),lag=4)),ylab='diff differenced',type='l',
     col='blue')


series<-diff(diff(,lag=1))
adf.test(series)
par(mfrow=c(1,2))
acf(as.vector(series),ci.type='ma')
pacf(as.vector(series),ci.type='ma')
#seeing a seasonality affect so in the pac so we need to add another option for seasonal
#in our arima model
model<-arima(x=log(JJ),order=c(0,1,1),seasonal=
                 list(order=c(0,1,1),period=4))
model
shapiro.test(residuals(model))
tsdiag(model)
plot(model,n1=c(1975,1), n.ahead=8, pch=19, ylab='Earnings',
       transform=exp,col='blue')

#Monthly Airline Passenger Loadings:
data(airpass)
par(mfrow=c(3,1))
plot(airpass,ylab="Air Passengers",col="blue")
plot(log(airpass),ylab=" Log of Air Passengers",col="blue")
plot(diff(log(airpass)), ylab="Diff of Log Air Passengers",col="blue")
points(diff(log(airpass)),
       x=time(diff(log(airpass))),
       pch=as.vector(season(diff(log(airpass)))))
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
acf(as.vector(diff(log(airpass))),main="differenced")
acf(as.vector(diff(diff(log(airpass)),lag=12)),
    main="seasonal differenced")
plot(diff(diff(log(airpass)),lag=12),col="blue",
     ylab="seasonal differenced")
hist(diff(diff(log(airpass)),lag=12),main="histogram",
     xlab="difference")
#since we are using the log we need the 1 for d for difference
mod <- arima(log(airpass), order = c(0,1,1),seasonal=
               list(order=c(0,1,1),period=12))
mod
tsdiag(mod)
shapiro.test(residuals(mod))
plot(mod,n1=c(1970,1),n.ahead=36,pch=19,
  ylab="Predicted Air Passengers",transform=exp,col="blue")

#how changing variances overtime affect model giving incorrect result
#Electricity Production:
data(electricity)
plot(electricity,col='blue')
layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
BoxCox.ar(electricity)
acf(diff(log(as.vector(electricity))),main="differenced")
acf(diff(diff(log(as.vector(electricity))),lag=12),
        main="seasonal differenced")
hist(diff(diff(log(as.vector(electricity))),lag=12),
         main="histogram",xlab="difference")
mod2 <- arima(log(electricity), order = c(0,1,1),
          seasonal=list(order=c(0,1,1),period=12))
mod2
tsdiag(mod2)
shapiro.test(residuals(mod2))
plot(mod2,n1=c(2004,1),n.ahead=24,pch=19,
  ylab="Predicted Electricity Production",transform=exp,col="blue")

#Volatility of Google Stock
#changing volatility over time
data(google)
plot(google,col='blue')
price <- exp(cumsum(google)) * 50.12 #we get return convert them into price data
plot(price,type='l',col='blue') #price is random walk but return if it was rising would need to be trnasformed


#GAUGE
hist(google,breaks=100)
sum(abs(google)>0.06)
shapiro.test(google)

par(mfrow=c(2,2))
acf(google)
pacf(google)
acf(google^2) #when we square it we see that the conditional variance show some time dependence 
pacf(google^2)
mean(google) #need mean in your garch model see garch attributes, only focus on deviations
t.test(google, alternative='greater')

par(mfrow=c(1,2))
McLeod.Li.test(y=google)
McLeod.Li.test(y=rnorm(500))
eacf(google^2)

m1 <- garch(x=google-mean(google),order=c(1,1),reltol=1e-6)
summary(m1)

plot(residuals(m1),type='h',ylab='standard residuals',col='blue')
par(mfrow=c(3,1))
plot(price,type='l',col='blue',ylab='price')
plot(google,type='l',col='blue',ylab='log returns')
plot((fitted(m1)[,1])^2,type='l',
     ylab='conditional variance',xlab='time',col='blue') #estimated or perdicted conditional variance

par(mfrow=c(2,2))
plot(residuals(m1),col="blue",main="Residuals")
hist(residuals(m1))
McLeod.Li.test(y=residuals(m1),main="McLeod-Li")
qqnorm(residuals(m1),col='blue')
qqline(residuals(m1))
shapiro.test(residuals(m1))
var(google)
