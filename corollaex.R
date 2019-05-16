
# Toyota Used Car Prices
library(leaps) 

toyota <- read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/code/ToyotaCorolla.csv")
head(toyota)
summary(toyota)
hist(toyota$Price)
## next we create indicator variables for the categorical variable
## FuelType with its three nominal outcomes: CNG, Diesel, and Petrol

toyota$FuelType1=ifelse(toyota$FuelType=="CNG",1,0)
toyota$FuelType2=ifelse(toyota$FuelType=="Diesel",1,0)
auto=toyota[,-4]
auto[1:3,]
plot(auto)


## regression on all data
m1=lm(Price~.,data=auto)
summary(m1)

X=auto[,2:11]
y=auto[,1]
out=summary(regsubsets(X,y,nbest=1,nvmax=ncol(X)))
tab=cbind(out$which,rsq=out$rsq,adjr2=out$adjr2,cp=out$cp,bic=out$bic)
tab

set.seed(1)
## fixing the seed value for the random selection guarantees the 
## same results in repeated runs
n=length(auto$Price)
n1=round(n*0.6)
n2=n-n1
train=sample(1:n,n1)

## regression on training set
m1=lm(Price~Age+KM+HP+CC+Weight+FuelType1+FuelType2,data=auto[train,])
summary(m1)
pred=predict(m1,newdat=auto[-train,])
obs=auto$Price[-train]
diff=obs-pred
percdiff=abs(diff)/obs
me=mean(diff)
rmse=sqrt(sum(diff**2)/n2)
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 

## cross-validation (leave one out)
n=length(auto$Price)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  m1=lm(Price~Age+KM+HP+CC+Weight+FuelType1+FuelType2,data=auto[-k,])
  pred=predict(m1,newdat=auto[k,])
  obs=auto$Price[k]
  diff[k]=obs-pred
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 


## Adding the squares of Age and KM to the model
auto$Age2=auto$Age^2
auto$KM2=auto$KM^2
m11=lm(Price~Age+KM,data=auto)
summary(m11)
m12=lm(Price~Age+Age2+KM+KM2,data=auto)
summary(m12)
m13=lm(Price~Age+Age2+KM,data=auto)
summary(m13)
plot(m11$res~m11$fitted)
hist(m11$res)
plot(m12$res~m12$fitted)

