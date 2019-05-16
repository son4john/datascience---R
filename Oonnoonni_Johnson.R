library(Ecdat)
library(tseries)

data(Irates)
summary(Irates)

dim(Irates)

#i know this isn't right forgor to read the part aout calculating differencce for all
#i know its loop but need to finish rest of problem
dataT = Irates


r1data <- Irates[, "r1", drop=F]

par(mfrow=c(2,1))
plot(r1data, ylab = "Time Sries")
plot(diff(r1data), ylab = "Change Over Time")

difr1 = diff(r1data)
par(mfrow=c(1,1))
hist(difr1, breaks = 100)
curve(dnorm(x, mean=mean(difr1), sd=sd(difr1)), add=TRUE, col="blue")

plot(dataT,type="p")
cordifr1 = cor(dataT)
cordifr1

lm.out = lm(r1~., data=dataT)
summary(lm.out)


#after reviewinb the AIC and BIC i need tod modify the code and select the data with smallest BIC
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))


#i need to modify this code to run the analysis for the data
for(i in 1:B){
  samTrainID = sample(c(1:38),25, replace=FALSE) #randomlly sample 38 index
  samTrainID
  train=FuelEff[samTrainID,]
  dim(train)
  test=FuelEff[-samTrainID,]#subtract train data to get the test data
  dim(test)
  lm.out=lm(GPM~NC+HP+ACC+ET,data=train)# estimate the model for the training data lm is linear model
  pred=predict(lm.out,newdat=test)# predict value from the model for the test data
  obs=test$GPM
  diff.error=obs-pred
  me[i]=mean(diff.error)
  rmse[i]=sqrt(mean(diff.error**2))
  mape[i]=100*(mean(abs(diff.error)/obs))
  me   # mean error 
  rmse # root mean square error
  mape # mean absolute percent error 
}