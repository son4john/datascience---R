library(quantmod)

ticker = c('AA','AUY','CETV','F','GE','GPRO','KMI','KODK',
           'LMNX','NIO','NOK','NRZ','PER','SBRA','SNAP',
           'SPWR','TCEHY','TCS','ZNGA')

for(i in ticker){
  getSymbols(i, from='2017-01-01', to='2019-01-01',perodicity='weekly', return.class='ts')
}

stockdata = cbind(AA[,6],AUY[,6],CETV[,6],F[,6],GE[,6],GPRO[,6],KMI[,6],KODK[,6],
              LMNX[,6],NIO[,6],NOK[,6],NRZ[,6],PER[,6],SBRA[,6],SNAP[,6],
              SPWR[,6],TCEHY[,6],TCS[,6],ZNGA[,6])

means=apply(stockdata,2,mean)

#need to find standard deviation
sdevv=apply(stockdata,2,sd)
dim(stockdata)

#SSW has missing values use na.function to change
#work on this when i get everything else compelete
replaceNA<-function(){
  print("HEllo World")
}

#Run prune by sharp file in code_chap7
pruneBySharpe(stockdata,ticker,means,sdevv,threshSR = .05,.057)