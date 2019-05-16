#Chapter IV

##################
# Bond Valuation #
##################
#fixed income
#price disconted cash flows (coupon payments + par(future values))
#coupon payment is annuity over a time period
#with the time change move on the value of bond change (out at maturity your recive lump sump)
#until that maturity you are reciving coupon payments
#bond value changes because of interest rate
#interest effect bond values inverse relationship



1000/(1.02)^30
1000/(1.03)^30

P<-1000  # par value
T<-20    # total investment years
r<-.06   # annual yield to maturity
C<-30    # coupon payment per anual


BV <- function(P,C,r,t,T,freq) {
  #Finds coupon Bond Value at time t mat T
  tmat <- T-t
  cp <- P*C/freq
  accrued <- P*C*t #already paid
  if(tmat != 0) { #include interim coupons
    i <- seq(1,freq*tmat)
    accrued + sum(cp/(1+r/freq)^i) +
      P/(1+r/freq)^(freq*tmat)
  } else #no coupons left
    accrued + P/(1+r/freq)^(freq*tmat)
}

#HOW WOULD YOU DO IT FOR 0 COUPON BOND SAME USE THE ABOVE
#DISCOUNT BOND?
#THIS IS FOR INVESTORS TODAY
#PREMIUM BOND?

BV(P,C,r=.06,t=0,T=20)
BV(P,C,r=.06,t=1/2,T=20)
BV(P,C,r=.06,t=1,T=20)
BV(P,C,r=.07,t=1/2,T=20)
BV(P,C,r=.06,t=20,T=20)
BV(P,C,r=0,t=0,T=20)

set.seed(437)

par(mfrow=c(1,3))
#Simulate rates market for r
# Assume the market rate will have a normal dist 
# with mean of 6% and sd of 0.5%
rvec <- round(c(r,r+
                  rnorm(T)*.0050),4)
plot(rvec,type="l",ylim=c(0,.07),
     xlab="Years",ylab="r",col=4,main="interest rates over time")
points(rvec,col=4)

#Simulate PV of Bond at time t w.r.t the change of market rate
simBV <- function(P,C,rvec,T) {
  BVvec = rep(0,T)
  for(t in 0:T) {
    i = t+1
    BVvec[i] <- BV(P,C,rvec[i],t,T)
  }
  plot(BVvec,type="l",col=4,ylim=c(0,2500),
       xlab="Years",ylab="Bond Value",main=paste("Bond value with coupon =",C))
  points(BVvec,col=4)
  BVvec
}

C <- 0
simBV(P,C,rvec,T)
C <- 30
simBV(P,C,rvec,T)

##################
# Stock Analysis #
##################


#Stock position:
par(mfrow=c(1,1))
#Simulate rates market for r
#S(0)=$10000
T <- 45 # trading days 
Svec <- round(c(1,1+1.1*
                  rnorm(T)*.0025),4)
SVvec <- 10000*Svec
plot(SVvec,type="l",col=4,ylim=c(9800,10200),
     xlab="Days",ylab="Stock Value")
points(SVvec,col=4)
text(c(1,T),c(10050,10050),c("S(0)","S(T)"))

# plot the 12 stock data in R dataset "stockdata"
library(huge)
data(stockdata)
dim(stockdata$data)
prices = stockdata$data
prices12 = prices[,1:12]
lab = stockdata$info[,1]
par(mfrow=c(3,4))
for(i in 1:12) {
  plot(prices12[,i],type="l",xlab=paste(lab[i]))
}

# Stock split adjustment
splitAdjust <- function(prices,symbol) {
  len = length(prices)
  origFinalPrice = prices[len]
  for(j in 2:len) {
    split = 0
    #print(paste(prices[j-1],prices[j]))
    if(prices[j-1] >= 1.4*prices[j]) {
      split = +1.5 # a 3 for 2
      if(prices[j-1] >= 1.8*prices[j])
        split = +2 #At least a 2 for 1
      if(prices[j-1] >= 2.9*prices[j])
        split = +3 #Ah a 3 for 1
      if(prices[j-1] >= 3.9*prices[j])
        split = +4 #Ah a 3 for 1
      if(prices[j-1] >= 4.9*prices[j])
        stop(paste(symbol,'detected more than 4:1 split'))
      print(paste("split adjusting",symbol,split,
                  j,prices[j-1],prices[j]))
    } #reverse splits: price increases so divide
    if(prices[j-1] <= prices[j]/1.4) {
      split = -1.5
      if(prices[j-1] <= prices[j]/1.9 &&
         prices[j-1] >= prices[j]/2.1)
        split = -2
      if(prices[j-1] <= prices[j]/2.9 &&
         prices[j-1] >= prices[j]/3.1)
        split = -3
      if(prices[j-1] <= prices[j]/5.8 &&
         prices[j-1] >= prices[j]/6.2)
        split = -6
      if((prices[j-1] <= prices[j]/7.7) &&
         (prices[j-1] >= prices[j]/8.3))
        split = -8
      if((prices[j-1] <= prices[j]/9.7) &&
         (prices[j-1] >= prices[j]/10.3))
        split = -10
      if((split == 0) && (prices[j-1] <= prices[j]/2.9))
        stop(paste(symbol,
                   'detected more than double reverse split'))
      print(paste("reverse split adjusting",j,symbol,j,
                  split,prices[j-1],prices[j]))
    }
    if(split != 0) {
      for(k in j:len) { #adjust all prices to right from j:len
        if(symbol=="C")
          prices[k] = prices[k]/10 #hard coded for Citi
        else if(split == +1.5)
          prices[k] = 1.5*prices[k] # 3 for 2
        else if(split == +2)
          prices[k] = 2*prices[k] # 2 to 1
        else if(split == +3)
          prices[k] = 3*prices[k] # 3 to 1
        else if(split == +4)
          prices[k] = 4*prices[k] # 4 to 1
        else if(split == -1.5)
          prices[k] = prices[k]/1.5 # 2 to 3 rev
        else if(split == -2)
          prices[k] = prices[k]/2 # 1 to 2 rev
        else if(split == -3)
          prices[k] = prices[k]/3 # 1 to 2 rev
        else if(split == -6)
          prices[k] = prices[k]/6 # 1 to 8 rev
        else if(split == -8)
          prices[k] = prices[k]/8 # 1 to 8 rev
        else if(split == -10)
          prices[k] = prices[k]/10 # 1 to 10 rev
        else stop('splitAdjust internal error')
      }
    }
  }
  finalPrice = prices[len]
  return(prices*origFinalPrice/finalPrice)
}
#unit test:
p <- c(3.0,3.0,2.0,11.88,5.9,1.95,3.90,3.90,
       1.5,.75,1.00,1.2,1.4,1.8,2.1,1.05,
       1.30,1.31,1.32,.44,.43,.11,.12,.13)
sap <- splitAdjust(p,"SYM")
paste(p, collapse=',')
paste(round(sap,3), collapse=',')
par(mfrow=c(1,2))
plot(p,type='l',ylim=c(0,15)); points(sap,col=4)

JDSUidx <- match('JDSU',lab); 
par(mfrow=c(1,2))
plot(prices[,JDSUidx],type='l',xlab='JDSU')
adjp<-splitAdjust(prices[,JDSUidx],c('JDSU'))
plot(adjp,type='l',xlab='JDSUadj')

findR <- function(prices,isSplitAdjusted=TRUE) {#Find R: logrets:
  len <- dim(prices)[1]
  D   <<- dim(prices)[2]
  R   <- matrix(nrow=(len-1),ncol=D)
  for(i in 1:D) {
    #print(i)
    if(!isSplitAdjusted) prices[,i] <<- splitAdjust(prices[,i],lab[i])
    R[,i] = 100*diff(log(prices[,i])) ###log rets
  }
  R
}

R <- findR(prices,isSplitAdjusted=FALSE)
D <- dim(prices)[2]
D

par(mfrow=c(1,1))
ts.plot(prices12,gpars=list(col=c(1:12)),ylab="price")
plot(ts(prices12),plot.type="s",col=c(1:12),ylab="price")



