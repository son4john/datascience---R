library(quantmod)
library(PerformanceAnalytics)
getSymbols("F", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("BABA", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("CE", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("T", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("O", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("BAC", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("AAPL", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("CAT", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("JPM", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("EL", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("ICE", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("KO", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("MMM", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("NVDA", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("NKE", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("MSFT", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("F", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("SAFT", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("UTX", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("AMZN", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("^IRX", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")
getSymbols("^GSPC", from="2017-01-01",to="2019-01-01",periodicity="weekly",return.class="ts")

head(F)
data1 <-cbind(F[,6],BABA[,6],CE[,6],T[,6],O[,6],BAC[,6],AAPL[,6],CAT[,6],JPM[,6],EL[,6],ICE[,6],KO[,6],MMM[,6],NVDA[,6],NKE[,6],MSFT[,6],F[,6],SAFT[,6],UTX[,6],AMZN[,6],IRX[,6],GSPC[,6])
nameStock <-c('F','BABA','CE','T','O','BAC','AAPL','CAT','JPM','EL','ICE','KO','MMM','NVDA','NKE','MSFT','F','SAFT','UTX','AMZN','GSPC')

diffLogs<-diff(log(data1))
dim(diffLogs)
excessReturn <- diffLogs[,-21] - diffLogs[,21]
cor(data1[,-21])
cov=cov(data1[,-21])
sdevv=(diag(cov))^0.5

summR = summary(data1[,-21])
skewness(data1[,-21])
kurtosis(data1[,-21])
View(data1)
means=apply(data1[,-21],2,mean)



pruneBySharpe <- function(prices,lab,meanv,sdevv,threshSR,mufree=0) {
  par(mar=c(4,4,1,1))
  par(mfrow=c(1,2))
  indepSharpes <- (meanv-mufree)/sdevv
  len = length(indepSharpes)
  plot(indepSharpes,ylab="SR",col=4, type="p")
  
  plot(sort(indepSharpes),ylab="SR",col=4)
  lines(1:len,rep(threshSR,len)) #lines(x value,y value)
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

data2 = pruneBySharpe(data1[,-21],nameStock,means,sdevv,.8,mufree=data1[,21])
p <- data2[[1]]
l <- data2[[2]]
dim(l)

findCovMat <- function(R) {
  meanv <- apply(R,2,mean)
  cov_mat <- cov(R)
  diag_cov_mat <- diag(cov_mat)
  sdevv <- sqrt(diag(cov_mat))
  list(meanv,cov_mat,diag_cov_mat,sdevv)
}

findCovMat(p)
  