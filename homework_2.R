#Johnson Oonnoonni
#HW-2 FIN 6368
#2-11-2019


#1. Choose any two stocks in the database "msf.dta". Change the code above to get the stock ret data and S&P index ret. 
#2. Estimate each company's abnormal return and beta by CAPM model for the full sample.
#3. For each company, randomly select 80% of obervations to estimate abnormal returns and beta in the step 2. Repeat the iteration 20 times. Print the abnormal return and beta estimations for 20 samples as a matrix. 
#4. Compute the average of the abnormal return and beta estimations over 20 samples.


library(RSQLite) #SQL Tools
library(foreign) #interact with data store in memory
setwd("C:/Users/OOGAME/OneDrive/EDU/FIN_6368")	# reset the working directory to the folder which stores the data sets
msf   <- read.dta("msf.dta") #read msf data into msf variable
con   <- dbConnect(SQLite(),":memory:") # create a connection variable
dbWriteTable(con,"msf",msf,overwrite=TRUE) #setup connection to write sql statements
command <- "SELECT tsymbol,ret
FROM msf
WHERE date BETWEEN '2005-01-01' AND '2013-12-31'
AND tsymbol IN ('GCI', 'ALK', 'SPY')"
result<-dbGetQuery(con,command) #storing results of the query in variable

y1<-result[result$tsymbol=='GCI',]$ret #storing GCI returns in variable
y2<-result[result$tsymbol=='ALK',]$ret #storing ALK returns in variable
x<-result[result$tsymbol=='SPY',]$ret  #storing SPY returns in variable

GCI.beta = cov(x,y1)/var(x)  #calculating Beta GCI
names(GCI.beta)<-c("GCI BETA")
ALK.beta = cov(x,y2)/var(x)  #calculating Beta ALK
names(ALK.beta)<-("ALK BETA")

GCI.ret.avg = mean(y1) # average return GCI
ALK.ret.avg = mean(y2) # average return ALK
SPY.ret.avg = mean(x) # average return SPY (market)
RiskFree.ret = .003 # risk free return (user defined)

GCI.alpha = (GCI.ret.avg - RiskFree.ret) - GCI.beta*(SPY.ret.avg - RiskFree.ret) #GCI alpha
names(GCI.alpha)<-c("GCI ALPHA")
ALK.alpha = (ALK.ret.avg - RiskFree.ret) - ALK.beta*(SPY.ret.avg - RiskFree.ret) #ALK alpha
names(ALK.alpha)<-c("ALK ALPHA")

s.percent = .8 # Teacher specification
s.count = ceiling(.8*length(x)) #how many values to sample

iterations = 20 # Teacher specification

#Setting up matrix for samples
s.mat.GCI = matrix(0,iterations,3)
s.mat.ALK = matrix(0,iterations,3)
s.mat.SPY = matrix(0,iterations)

for(i in 1:iterations){
  s.y1 = sample(y1, s.count, replace = FALSE) #get the sample GCI returns
  s.y2 = sample(y2, s.count, replace = FALSE) #get the sample ALK returns
  s.x = sample(x, s.count, replace = FALSE)   #get the sample SPY returns
  
  s.GCI.beta = cov(s.x,s.y1)/var(s.x) #calculate sample beta GCI
  s.ALK.beta = cov(s.x,s.y2)/var(s.x) #calculate sample beta ALK
  
  s.GCI.ret.avg = mean(s.y1) #average of sample Gci returns
  s.ALK.ret.avg = mean(s.y2) #average of sample ALK returns
  s.SPY.ret.avg = mean(s.x)  #average of sample SPY returns
  
  s.GCI.alpha = (s.GCI.ret.avg - RiskFree.ret) - s.GCI.beta*(s.SPY.ret.avg - RiskFree.ret) #sample GCI alpha
  s.ALK.alpha = (s.ALK.ret.avg - RiskFree.ret) - s.ALK.beta*(s.SPY.ret.avg - RiskFree.ret) #sample ALK alpha
  
  s.mat.GCI[i,] = c(s.GCI.beta, s.GCI.ret.avg, s.GCI.alpha)  # matrix containing results for sample GCI
  s.mat.ALK[i,] = c(s.ALK.beta, s.ALK.ret.avg, s.ALK.alpha)  # matrix containing results for sample ALK
  s.mat.SPY[i,] = c(s.SPY.ret.avg)                           # matrix containing results for sample SPY
  
}

#Getting Averages for Samples
s.results.GCI = apply(s.mat.GCI, 2, mean)
s.results.ALK = apply(s.mat.ALK, 2, mean)
names(s.results.GCI)<-c("GCI SAMPLE AVG BETA", "GCI SAMPLE AVG RET", "GCI SAMPLE AVG ALPHA")
names(s.results.ALK)<-c("ALK SAMPLE AVG BETA", "ALK SAMPLE AVG RET", "ALK SAMPLE AVG ALPHA")

colnames(s.mat.GCI) = c("GCI BETA SAMPLE", "GCI AVG SAMPLE", "GCI ALPHA SAMPLE")
colnames(s.mat.ALK) = c("ALK BETA SAMPLE", "ALK AVG SAMPLE", "ALK ALPHA SAMPLE")

#Printing Results
s.mat.GCI
s.mat.ALK
s.results.GCI
s.results.ALK
GCI.alpha
GCI.beta
ALK.alpha
ALK.beta

