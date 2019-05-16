#$ is a very important key symbol - has to do with feilds in tables
##Fuel Efficiency

library(MASS)
library(corrplot)
library(Hmisc)

FuelEff <- read.csv("FuelEfficiency.csv")
attach(FuelEff)
summary(FuelEff)
plot(FuelEff,data=FuelEff)

#box-cox transformation    
bc.out = boxcox(MPG~WT+DIS+NC+HP+ACC+ET,lambda=seq(-2,2,1/20),data=FuelEff)
lambda = bc.out$x[which.max(bc.out$y)] #the interior is log likelyhood function
lambda #lamada is power transofrmation value

#correlation analysis
mat = cor(FuelEff)
print(mat)
corrplot(mat)

FuelEff = FuelEff[,-1]
lm.out = lm(GPM~., data=FuelEff)
summary(lm.out)

#Variance inflation factor
vif(lm.out)

#residual plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm.out,which=1:4)

#reduced models
library(leaps)  
X=FuelEff[,2:7]
y=FuelEff[,1]
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,rsq=out$rsq,adjr2=out$adjr2,cp=out$cp,bic=out$bic)
tab

## cross-validation (leave one out) for the model on all six regressors
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
dim(FuelEff) # 38 item

B=50
#boot strap 50 times
me=rep(0,B) #initallize vectors
rmse=rep(0,B)
mape=rep(0,B)
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
#summary of all the predictions errors
me.boot = mean(me)
rsme.boot=mean(rmse)
mape.boot= mean(mape)

detach(FuelEff)

library(MASS)
library(corrplot)
library(Hmisc)
library(leaps)

##in-class practice: ToyotaCorolla.csv

toyota = read.csv("D:/DRIVES/OneDrive/EDU/FIN_6368/code/ToyotaCorolla.csv")
head(toyota)
plot(toyota)

# next we create indicator variables for the categorical variable
# FuelType with its three nominal outcomes: CNG, Diesel, and Petrol

#turn a catagorical variable into dummy variables
#3 states/ level need two variables (0,0), (1,0), (0,1)
toyota$FuelType1=ifelse(toyota$FuelType=="CNG",1,0) #include new variable to data
toyota$FuelType2=ifelse(toyota$FuelType=="Diesel",1,0) #include new variable to data
auto=toyota[-4] #remove orignal FuelType from orignal data and store in new variable
head(auto)

table(toyota$FuelType) # check the disribution of FuelType
table(toyota$FuelType1,toyota$FuelType2) # check the distrubtion of the categorical variables

m1=lm(Price~., data=auto) #lookng at how all the other variables ~. are related to price
summary(m1) #PR(>|t|) is the p-values 


#creating regression subsets
x=auto[,2:11] #independent
y=auto[,1] #dependent
out=summary(regsubsets(x,y,nbest=1,nvmax=ncol(x))) #nbest select the n best model 1
tab=cbind(out$which,rsq=out$rsq,adjr2=out$adjr2,cp=out$cp,bic=out$bic) #evaluate the model using rsqared cp bic adjusted rsquare
tab # if using one figure use age, if using two figures use age KM bic smallest value 7th

#refit the model based on the indpendent criteria from the 7th model

#using a specific percentage of the data to buid the model
n=length(auto$Price)
n1=round(n*.6)
n2=n-n1
train=sample(1:n,n1)

m1=lm(Price~Age+KM+HP+CC+Weight+FuelType1+FuelType2,data=auto[train,])
summary(m1)
pred=predict(m1,newdata = auto[-train,])
obs=auto$Price[-train]
diff1 = obs - pred
percdiff1 = abs(diff1)/obs
length(obs)
length(pred)
length(train)
me1=mean(diff1)
rmse1=sqrt(mean(diff1**2))
mape1=100*(mean(percdiff1))

me1
rmse1
mape1

##cross validation leave one out
n=length(auto$Price)
diff=dim(n)
percdiff=dim(n)
for(k in 1:n){
  m1=lm(Price~Age+KM+HP+CC+Weight+FuelType1+FuelType2, data=auto[-k,])
  pred=predict(m1,newdat=auto[k,])
  obs=auto$Price[k]
  diff[k] = obs - pred
  percdiff[k] = abs(diff[k])/obs
  
}

me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me
rmse
mape

##adding the squares of age and KM to the model
auto$Age2

attach(auto)
detach(auto)











