#Chapter III
#review

#Page 26 in book
#A hand with a single pair
#13 card types
#with 4 copies
#pick 2
#remaing 12 card types chosing 3 
#4^3 distinct choices
13*choose(4,2)*choose(12,3)*4^3 / choose(52,5)

#page 26 in book
#A hand with two pairs
#n values = 13, r orders = 2
choose(13,2)*choose(4,2)^2*11*4 / choose(52,5)

#review
#For figure where 1/x = exp(-x)
x = seq(0,.5,.01) # is a vector starting at 0 going to .5 with a stepsize of .01
y1 = 1-x #y1 is 1 minus the value of x
y2 = exp(-x) #is the exponetal value of -x
plot(x,y1,type="l",ylab="1/x,exp(-x)",col=2)
lines(x,y2,type="l",col=8)


#anualize it *sqrt t value for longer time period t is total number of
#periods for the data monthly data t is 12 months daily t is 365 days
#continues compounding return
#covered class
S = c(1.3,1.2,1.3,1.4,1.5,1.4,1.3,1.4,1.5) #vector with 9 values
#lag value
diffLogS = diff(log(S)) #get log off all s then current minus previous, vector with 8 values
diffLogSmean = mean(diffLogS)
N = length(diffLogS) #total number of returns
histVol = sqrt(1/(N-1)*sum((diffLogS-diffLogSmean)^2))#historical volatility pg 35 equation 3.30
annHistVol = histVol*sqrt(length(S))#annualizing similar to the above comment pg 35 equation 3.32
histVol
annHistVol

install.packages("RSQLite")#everytime i closed the browser it would delete it
library(RSQLite)#sql libaray
library(foreign)#functions to make calls to compiled code that has been loaded into R
setwd("D:/DRIVES/OneDrive/EDU/FIN_6368/code")
funda <- read.dta("funda.dta") #data1
msf   <- read.dta("msf.dta") #data2
con   <- dbConnect(SQLite(),":memory:") #connection to sql
dbWriteTable(con,"funda",funda,overwrite=TRUE)
dbWriteTable(con,"msf",msf,overwrite=TRUE)
command <- "SELECT tsymbol,ret
            FROM msf
            WHERE date BETWEEN '2005-01-01' AND '2013-12-31'
                AND tsymbol IN ('AAPL','SPY')"
result<-dbGetQuery(con,command)
y<-result[result$tsymbol=='AAPL',]$ret
x<-result[result$tsymbol=='SPY',]$ret



###############
# STATSISTICS #
###############


cov(x,y)/var(x) #beta in capm but not in pharma french two other factors
#gives us alot of useful data
lmout <- summary(lm(y~x+1))#linear model caluclate beta = 1.219 intercept is alpa abnormal return = significance
#alpha is intercept - perform better than the market
#beta is second
shapiro.test(x)
shapiro.test(y)
plot(x,y)
#anova analysis of variance 
#R squared value
#F-test evaluate variance between two variance
#REGRESSION MODEL F TEST IS FOR is for goodness of fit, overal how the model fit the data how far or how close
# the linear line is close to the data
#model explained variantion vs unexpected varianation
#F-test comparison to variance

#just want specific data from the result
names(lmout)
lmout$coefficients

#Use the function getSymbols() in package "quantmod" to retrieve two stock historical weekly data from yahoo.finance. The sample period is from "2015-01-01" to "2017-01-01". 
#Use the sixth column (adjusted market close price) for each stock data to perform the analysis. 
#For example: 
install.packages("quantmod")
library(quantmod)
getSymbols("F", from="2015-01-01",to="2017-01-01",periodicity="weekly",return.class="ts")
getSymbols("BABA", from="2015-01-01",to="2017-01-01",periodicity="weekly",return.class="ts")
head(F)
head(BABA)
mydat<-cbind(F[,6], BABA[,6])
colnames(mydat)<-c("F","BABA")

#?? search help
#? check function


#parametric distribution there are some parameters
#that you are trying to fit


write.csv(d, file="d.txt", row.names=FALSE)
getwd()


my.mat<-matrix(1:50, nrow=10)
colnames(my.mat)<-c("min", "max","length", "median ", "mean ")
s=c(1.3,1.2,1.4,1.3,1.5,1.3,1.2,1.5,1.2)
#this didn't do log difference i just used the price
#libary(moments)
sum.stats <-function(sa){
  #summary statistics
  maxValue = min(sa)
  minValue = max(sa)
  N = length(sa)
  #quantile(x, .25), quantile(x,.75) skewness, kertoses
  val<-c(maxValue, minValue, N, median(sa), mean(sa))
  # print(paste("Min: " , minValue))
  # print(paste("Max: " , maxValue)
  # print(paste("Length: " + N))
  # print(paste("medain: " + median(sa)))
  # print(paste("mean: " + mean(sa)))
  return(val)
}
#sum.stats(s)

for(i in 1:10){
  fac <- sample(s,5, replace = FALSE)
  #sum.stats(fac)
  my.mat[i,] = sum.stats(fac)
}
my.mat
getwd()
date()
