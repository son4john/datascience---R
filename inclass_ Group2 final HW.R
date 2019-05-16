#group 2
#Jingxi
#Johnson
#Rutuja
#Jiemin

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)
library(gridExtra)

tickers=c('F','BABA','CE','T','O','BAC','AAPL','CAT','JPM','EL',
          'ICE','KO','MMM','NVDA','NKE','MSFT','F','SAFT','UTX','AMZN','^GSPC','^IRX')
for (i in tickers){
  getSymbols(i, from="2017-01-01",to="2019-01-01",periodicity="daily",return.class="ts")
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


#1 compute daily log return 
diffLogs=diff(log(data1))
logReturns=diffLogs
dim(logReturns)


returns_transpose=matrix(nrow=20,ncol=501)
returns_transpose=t(logReturns)
returns_transpose[1:3,1:5]
dim(returns_transpose)

stock_mean = apply(returns_transpose,1,mean)
stock_standardDeviation = apply(returns_transpose,1,sd)
stock_skewness=apply(returns_transpose,1,skewness)
stock_kurtosis=apply(returns_transpose,1,kurtosis)
length(stock_mean)
length(stock_standardDeviation)
length(stock_skewness)
length(stock_kurtosis)

data2 = data.frame(symbol=tickers[1:20],mean=stock_mean, stddev=stock_standardDeviation, 
                   skew=stock_skewness,kurtosis=stock_kurtosis)
dim(data2)
View(data2)

#group data for 2 attributes, mean and sd
set.seed(12345)
grp_2attribs = kmeans(data2[,c("mean","stddev")], centers=3, nstart=10)
par(mfrow=c(1,1))
plot(data2$mean,data2$stddev,type="n",xlab="mean", ylab="stddev",
     main="Clustering by 2 attributes with k=3")
text(x=data2$mean,y=data2$stddev,labels=data2$symbol, col=rainbow(3)[grp_2attribs$cluster])


#3.
##group data for 4 attributes and choose different of K (centers)
bs = numeric()   
ws = numeric()   
for (i in 1:10){
  bs[i] <- kmeans(data2[,-1], centers=i, nstart=10)$betweenss
  ws[i] <- kmeans(data2[,-1], centers=i, nstart=10)$tot.withinss
}

plot.bs = qplot(1:10, bs, geom=c("point", "line"), 
                xlab="Number of clusters", ylab="Betweenss") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
plot.ws = qplot(1:10, ws, geom=c("point", "line"),
                 xlab="Number of clusters", ylab="Total withinss") +
  scale_x_continuous(breaks=seq(0, 10, 1))
# Subplot
grid.arrange(plot.bs, plot.ws, ncol=2) # Eyeballing the graph, we choose three cluster as the optimal cluster 
#because the betweenness at 3 cluster is have huge increase showing cluster of 3 is optimal at 
#distinguishing each clustered group. 

#cluster with 4 atttibutes with k=3
set.seed(12345)
grp_4attribs = kmeans(data2[,-1], centers=3, nstart=10) 
grp_4attribs
#plot the 3 clustering in graph of mean vs stddev
plot(data2$mean, data2$stddev, type="n", xlab="Mean", ylab="Standard Deviation",
     main="Clustering by 4 attributes with k=3")
text(x=data2$mean, y=data2$stddev, labels=data2$symbol,col=rainbow(3)[grp_4attribs$cluster])


#4.
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



#threshSR equal to 0.027 to select the top10 sharpe-ratio stocks (half of the total of 20 stocks)
data3 = pruneBySharpe(data1,nameStock,stock_mean,stock_standardDeviation,
                      threshSR=.027,mufree=mean(diff(log(tbill)))/52)

selectStock=data3[[2]]
selectStock
length(selectStock)

selectStock.Price=data3[[1]]
colnames(selectStock.Price)=selectStock
head(selectStock.Price)
dim(selectStock.Price)

selectStock.Return=diff(log(selectStock.Price))
head(selectStock.Return)
dim(selectStock.Return)

selectStock_mean = apply(t(selectStock.Return),1,mean)
selectStock_standardDeviation = apply(t(selectStock.Return),1,sd)
selectStock_skewness=apply(t(selectStock.Return),1,skewness)
selectStock_kurtosis=apply(t(selectStock.Return),1,kurtosis)
length(selectStock_mean)
length(selectStock_standardDeviation)
length(selectStock_skewness)
length(selectStock_kurtosis)

data4 = data.frame(symbol=selectStock,mean=selectStock_mean, 
                   stddev=selectStock_standardDeviation, 
                   skew=selectStock_skewness,kurtosis=selectStock_kurtosis)
dim(data4)
View(data4)

#choose the optimal clustering for these 10 stocks again
bs1 = numeric()   
ws1 = numeric()   
for (i in 1:5){
  bs1[i] <- kmeans(data4[,-1], centers=i, nstart=10)$betweenss
  ws1[i] <- kmeans(data4[,-1], centers=i, nstart=10)$tot.withinss
}

plot.bs1 = qplot(1:5, bs1, geom=c("point", "line"), 
                xlab="Number of clusters", ylab="Betweenss") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
plot.ws1 = qplot(1:5, ws1, geom=c("point", "line"),
                xlab="Number of clusters", ylab="Total withinss") +
  scale_x_continuous(breaks=seq(0, 10, 1))
# Subplot
grid.arrange(plot.bs1, plot.ws1, ncol=2) #we choose cluster of 2 for these 10 stocks

#cluster with 4 atttibutes with k=2
set.seed(12345)
grp = kmeans(data4[,-1], centers=3, nstart=10) 
grp
#plot the 2 clustering in graph of mean vs stddev
par(mfrow=c(1,1))
plot(data4$mean, data4$stddev, type="n", xlab="Mean", ylab="Standard Deviation",
     main="Clustering by 4 attributes with k=2 for selected 10 stocks")
text(x=data4$mean, y=data4$stddev, labels=data4$symbol,col=rainbow(3)[grp$cluster])


#comparing the previous graph together
par(mfrow=c(2,1))
plot(data2$mean, data2$stddev, type="n", xlab="Mean", ylab="Standard Deviation",
     main="Clustering by 4 attributes with k=3 for all 20 stocks")
text(x=data2$mean, y=data2$stddev, labels=data2$symbol,col=rainbow(3)[grp_4attribs$cluster])

plot(data4$mean, data4$stddev, type="n", xlab="Mean", ylab="Standard Deviation",
     main="Clustering by 4 attributes with k=2 for selected 10 stocks")
text(x=data4$mean, y=data4$stddev, labels=data4$symbol,col=rainbow(3)[grp$cluster])
