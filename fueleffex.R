
##Fuel Efficiency example

library(MASS)
library(corrplot)
library(Hmisc)

FuelEff <- read.csv("D:/DRIVES/OneDrive/EDU/FIN_6368/code/FuelEfficiency.csv")
attach(FuelEff)
summary(FuelEff)
plot(FuelEff,data=FuelEff) #box plot

#box-cox transformation
#x value is alpha
#y is likely hood function
#you can see that its a quadratic curve the shape
#boxcox is power transformation
#boxcox first argument formula - fit to a model, if no model just plug in all values an a intercept term
#we made mpg our or intercept term
bc.out = boxcox(MPG~WT+DIS+NC+HP+ACC+ET,lambda=seq(-2,2,1/20),data=FuelEff) #lamda power transformation plot
lambda = bc.out$x[which.max(bc.out$y)] #chosing the lamda that gives us the largest liekly hood function
lambda
#which power
which.max(bc.out$y)
#sometimes better to use an integer not a decmial use a number that make sense to you like -1

#correlation analysis
mat = cor(FuelEff)
print(mat)
corrplot(mat)

#when you don't know what the formula is you can just add tilda and a .
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
# then estimate the model - once we estimate the model - plug in the data you 
#dropped out and predict the dependent variable
#due this for each data point
#use index do this
n=length(FuelEff$GPM)
diff=dim(n)
percdiff=dim(n)
for (k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  ## the R expression "train1[train1!=k]" picks from train1 those 
  ## elements that are different from k and stores those elements in the
  ## object train. 
  ## For k=1, train consists of elements that are different from 1; that 
  ## is 2, 3, ., n.
  m1=lm(GPM~.,data=FuelEff[train,])
  pred=predict(m1,newdat=FuelEff[-train,])
  obs=FuelEff$GPM[-train]
  diff[k]=obs-pred #difference between the observed value and perdicted value
  percdiff[k]=abs(diff[k])/obs #percentage difference
}
#accuracy levels
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 
#we like to use percentage different data have different unit of measurement

detach(FuelEff)

##in-class practice: ToyotaCorolla.csv
toyota = read.csv("ToyotaCorolla.csv")
head(toyota)
plot(toyota)

# next we create indicator variables for the categorical variable
# FuelType with its three nominal outcomes: CNG, Diesel, and Petrol

toyota$FuelType1=ifelse(toyota$FuelType=="CNG",1,0)
toyota$FuelType2=ifelse(toyota$FuelType=="Diesel",1,0)
auto=toyota[-4]
head(auto)
attach(auto)
detach(auto)











