#Final Project
#house Price Model

library(MASS)
library(corrplot)
library(Hmisc)
library(leaps) 

hPrice <- read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/final_project/train.csv")

attach(hPrice)
summary(hPrice)
names(hPrice)
typeof(hPrice)

hPrice$zoneT1=ifelse(hPrice$MSZoning=="C (all)",1,0)
hPrice$zoneT2=ifelse(hPrice$MSZoning=="FV",1,0)
hPrice$zoneT3=ifelse(hPrice$MSZoning=="RH",1,0)
hPrice$zoneT4=ifelse(hPrice$MSZoning=="RL",1,0)
hPrice$zoneT5=ifelse(hPrice$MSZoning=="RM",1,0)
hPrice$LotFrontage[is.na(hPrice$LotFrontage)]<-0
hPrice$LandC1=ifelse(hPrice$LandContour=="Bnk",1,0)
hPrice$LandC2=ifelse(hPrice$LandContour=="HLS",1,0)
hPrice$LandC3=ifelse(hPrice$LandContour=="Low",1,0)
hPrice$LandC4=ifelse(hPrice$LandContour=="Lvl",1,0)

#contact group tomorrow let them know your doiing utilites land configuration, and landslpe
#once these completed those thre send over to the group for more discussion


select.me <-c('SalePrice','LotArea', 'GarageArea','MSSubClass','MSZoning','zoneT1','zoneT2','zoneT3','zoneT4','zoneT5',
              'LotFrontage','LandC1', 'LandC2', 'LandC3', 'LandC4')

testSet <- hPrice[,select.me]
names(testSet)
dim(testSet)


plot(testSet,data=testSet)

#correlation analysis
mat = cor(testSet)
print(mat)
corrplot(mat)

lm.out = lm(SalePrice~., data=testSet)
summary(lm.out)

#reduced models 
X=testSet[,2:3]
y=testSet[,1]
out=summary(regsubsets(X,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,rsq=out$rsq,adjr2=out$adjr2,cp=out$cp,bic=out$bic)
tab

