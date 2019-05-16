#Group 2
#Jiemin
#Jessie
#Johnson
#Rutuja
#1. Compute the log returns and excess returns of each stock.
#2. Print the correlation and covariance matrix of stock returns.
#3. Print the summary stats of stock returns including location, scale, and shape measures.
#4. Call the function prunceBySharpe() to select half of stocks. 
#5. Plot the Sharpe Ratio of all stocks before and after sorting. 
#5. Print the correlation and covariance matrix of selected stock returns.
#6. Create a portfolio among the selected stocks. 
#7. Compute the portfolio's Sharpe Ratio.
#8. Examine the portfolio's performance and assess the risk of portfolio by Monte Carlo simulation. If the portfolio's returns demonstrate strong tail risk, use mixture models in simulation. Simulate the portfolio's performance for one month and three months separately. Assume the starting position of $10,000.
#9. From the simulation above, compute the 95% VaR of the portfolio over one month and three months separately.

library(quantmod)
library(PerformanceAnalytics)

tickers=c('F','BABA','CE','T','O','BAC','AAPL','CAT','JPM','EL',
          'ICE','KO','MMM','NVDA','NKE','MSFT','F','SAFT','UTX','AMZN','^GSPC','^IRX')
for (i in tickers){
  getSymbols(i, from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
}

sp500=GSPC[,6]
tbill=IRX[,6]
data1 <-cbind(F[,6],BABA[,6],CE[,6],T[,6],O[,6],BAC[,6],AAPL[,6],CAT[,6],JPM[,6],EL[,6],
              ICE[,6],KO[,6],MMM[,6],NVDA[,6],NKE[,6],MSFT[,6],F[,6],SAFT[,6],UTX[,6],AMZN[,6])
nameStock <-c('F','BABA','CE','T','O','BAC','AAPL','CAT','JPM','EL',
              'ICE','KO','MMM','NVDA','NKE','MSFT','F','SAFT','UTX','AMZN')
colnames(data1)=nameStock
dim(data1)
View(data1)


#1. Compute the log returns and excess returns of each stock.
diffLogs<-diff(log(data1))
logReturns=diffLogs
dim(diffLogs)
excessReturn <- logReturns - diff(log(tbill))
dim(excessReturn)

#2. #2 Print the correlation and covariance matrix of stock returns.
cor=cor(logReturns)
cor
cov=cov(logReturns)
cov
#3. Print the summary stats of stock returns including location, scale, and shape measures.
summR = summary(logReturns)
summR
skewness(logReturns)
kurtosis(logReturns)


#4. Call the function prunceBySharpe() to select half of stocks. 
#run the pruneBySharpe funciton first
pruneBySharpe <- function(prices,lab,meanv,sdevv,threshSR,mufree=0) {
  par(mar=c(4,4,1,1))
  par(mfrow=c(1,2))
  indepSharpes <- (meanv-mufree)/sdevv
  len = length(indepSharpes)
  plot(indepSharpes,ylab="SR",col=4,xlab="Before sorting")
  plot(sort(indepSharpes),ylab="SR",col=4,xlab="After sorting")
  lines(1:len,rep(threshSR,len),col="red") #lines(x value,y value)
  #abline(h=threshSR,col="red",lty=2)
  indHighSharpes <- (indepSharpes > threshSR) #indHighSharpes means indicator sharpe ratio
  #clean up NAs
  for(d in 1:length(indHighSharpes)) #clean up NAs
    if(is.na(indHighSharpes[d]))
      indHighSharpes[d] <- FALSE
  len = dim(prices)[1]
  wid = dim(prices)[2]
  smallerSz = sum(indHighSharpes) #smallerSz is the smaller size. It is the reduced size after selected
  newPrices <- matrix(rep(0,len*smallerSz),
                      nrow=len,ncol=smallerSz)
  newLab    <- vector(length=smallerSz)
  e <- 1
  for(d in 1:wid) {
    if(indHighSharpes[d]) {
      print(paste("e",e))
      newPrices[,e] <- prices[,d]
      newLab[e] <- lab[d]
      e <- e + 1
    }
  }
  print("completed Sharpe pruning")
  list(newPrices,newLab,indepSharpes)
}


means=apply(logReturns,2,mean)
sdevv=(diag(cov))^0.5 #cov stores the results of the covariance matrix above

#threshSR equal to 0.05 to select the top10 sharpe-ratio stocks (half of the total of 20 stocks)
data2 = pruneBySharpe(data1,nameStock,means,sdevv,threshSR=.05,mufree=mean(diff(log(tbill)))/52)
 

#5. Plot the Sharpe Ratio of all stocks before and after sorting. 
#5. Print the correlation and covariance matrix of selected stock returns.

selectStock=data2[[2]]
selectStock
length(selectStock)

selectStock.Price=data2[[1]]
colnames(selectStock.Price)=selectStock
head(selectStock.Price)
dim(selectStock.Price)

selectStock.Return=diff(log(selectStock.Price))
head(selectStock.Return)
dim(selectStock.Return)

new.cor=cor(selectStock.Return)
new.cov=cov(selectStock.Return)

new.cor
new.cov
dim(new.cor)
dim(new.cov)

#6. Create a portfolio among the selected stocks. 

weight=c(rep(1/10,10))
ourPortfolio.return=selectStock.Return %*% weight
colnames(ourPortfolio.return)=c("Return of Our Portfolio")
head(ourPortfolio.return)
dim(ourPortfolio.return)



#7. Compute the portfolio's Sharpe Ratio.
#compute expected return of our portfolio
expected.Port.Return=mean(ourPortfolio.return)
expected.Port.Return #This is weekly return 
Annualized.expected.Port.Return= expected.Port.Return*52
Annualized.expected.Port.Return # yearly return 
Daily.expected.Port.return=Annualized.expected.Port.Return/252
Daily.expected.Port.return #daily return

#compute standard deviation of our portfolio
Port.standard.deviation=sd(ourPortfolio.return)
Port.standard.deviation #weekly standard deviaton 
Annualized.Port.sd=Port.standard.deviation*52^0.5
Annualized.Port.sd
Daily.Port.sd=Annualized.Port.sd*(1/252)^0.5
Daily.Port.sd

weekly.riskfree.rate=mean(diff(log(tbill))/52)
weekly.riskfree.rate
sharpe.ratio= (expected.Port.Return-weekly.riskfree.rate)/Port.standard.deviation
sharpe.ratio #weekly sharpe ratio
Annualized.sharp.ratio=sharpe.ratio*52
Annualized.sharp.ratio

#8. Examine the portfolio's performance and assess the risk of portfolio by Monte Carlo simulation. If the portfolio's returns demonstrate strong tail risk, use mixture models in simulation. Simulate the portfolio's performance for one month and three months separately. Assume the starting position of $10,000.

#Our porfolio weekly return is 0.36% compared to SP500 weekly return of 0.09%.It shows 
#our portfolio outperformed the market benchmark return from 2017 to 2019
print(c(expected.Port.Return,mean(diff(log(sp500)))))

par(mfrow=c(1,1))
hist(ourPortfolio.return,density = T,breaks=15)
lines(density(ourPortfolio.return),col="red")
skewness(ourPortfolio.return) #portfolio return is left-skewed, indicating more negative returns
kurtosis(ourPortfolio.return) # kurtosis is less than 3, showing no extreme returns during the observing period 

rmixture <- function(N,sigma1,sigma2=0,thresh=.9) {
  variates = vector(length=N)
  U = runif(N)
  for(i in 1:N)
    variates[i] = rnorm(1,0,sd=sigma1)
  if(sigma2 != 0) { #only mixture if sigma2 != 0
    for(i in 1:N)
      if(U[i] >= thresh)
        #replace original variate with mixture variate
        variates[i] = rnorm(1,0,sd=sigma2)
  }
  variates
}

toPrices=function(Y1,Ylogrets){
  Yprices = c(Y1,Y1*exp(cumsum(Ylogrets)))
  Yprices
}

simPricePath <- function(initPrice,N,seed,sigma1=.05,
                         sigma2=0,thresh=.9) {
  #Non mixture model
  set.seed(seed)
  Xlogrets = rmixture(N,sigma1,sigma2,thresh=thresh)
  Xprices = toPrices(initPrice,Xlogrets)
  list(Xprices,c(Xlogrets))
}

# Perform one-month simulation for our portfolio using portfolio daily sigma (30 observations in a month generated)
oneMonth.sim=simPricePath(initPrice=10000,N=30,seed=1,sigma1=Daily.Port.sd,sigma2=0)
oneMonth.sim
# Perform three-month simulation for our portfolio using daily sigma (30*3 observations in a month generated)
threeMonth.sim=simPricePath(initPrice=10000,N=30*3,seed=1,sigma1=Daily.Port.sd,sigma2=0)
threeMonth.sim

#9. From the simulation above, compute the 95% VaR of the portfolio over one month and three months separately.
hist(oneMonth.sim[[2]],density = T,main="Simulation of Returns in One Month",xlab = "Simulated Return")
hist(threeMonth.sim[[2]],density= T,main="Simulation of Returns in Three Month",xlab = "Simulated Return")

# 95% VaR of the portfolio over one month and three months separately
VaR.OneMonth=quantile(oneMonth.sim[[2]],probs = 0.05)
VaR.OneMonth

VaR.ThreeMonth=quantile(threeMonth.sim[[2]],probs = 0.05)
VaR.ThreeMonth

#conclusions:
#The 95% VaR in the one month simulation is -1.29%, indicating that there is 95% probility that 
#our portfolio return would not exceed -1.29%.
#The 95% VaR in the three month simulation is -1.12%, suggesting that there is 95% probility that 
#our portfolio return would not exceed -1.12%.
