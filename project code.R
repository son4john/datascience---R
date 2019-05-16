library(ggplot2)
library(leaps) 
library(randomForest)

hPrice=read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/code/Original.Train.csv")
ori.test=read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/code/Original.Test.csv")
dim(hPrice)
train.data=read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/code/cleaned.train.csv")
dim(train.data)
test.data=read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/code/cleaned.test.csv")
dim(test.data)

variables=names(train.data)
variables



#plot some interesting grphs to have a better understanding of the data 
options(scipen=999)
theme_set(theme_bw()) 
gg <- ggplot(hPrice, aes(YearBuilt, SalePrice)) + 
  geom_point() + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Sale Price Vs Year Built", 
       y="Saleprice", 
       x="Year Built", 
       title="Scatterplot", 
       caption = "Source: Housing Price") 
plot(gg)  



options(scipen=999)
theme_set(theme_bw()) 
gg <- ggplot(hPrice, aes(YearBuilt, SalePrice)) + 
  geom_point(aes(col=MSZoning)) + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Sale Price Vs Year Built", 
       y="Saleprice", 
       x="Year Built", 
       title="Scatterplot", 
       caption = "Source: Housing Price")+
  xlim(c(1880,2000))
plot(gg)  


options(scipen=999)
theme_set(theme_bw()) 
gg <- ggplot(hPrice, aes(YearBuilt, SalePrice)) + 
  geom_point(aes(col=LotShape)) + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Sale Price Vs Year Built", 
       y="Saleprice", 
       x="Year Built", 
       title="Scatterplot", 
       caption = "Source: Housing Price")+
  xlim(c(1880,2000))
plot(gg) 

options(scipen=999)
theme_set(theme_bw()) 
gg <- ggplot(hPrice, aes(YearBuilt, SalePrice)) + 
  geom_point(aes(col=BldgType)) + 
  geom_smooth(method="lm", se=F) +
  labs(subtitle="Sale Price Vs Year Built", 
       y="Saleprice", 
       x="Year Built", 
       title="Scatterplot", 
       caption = "Source: Housing Price")+
  xlim(c(1880,2000))
plot(gg)

options(scipen=999)
theme_set(theme_bw()) 
gg <- ggplot(hPrice, aes(YrSold, SalePrice)) + 
  geom_point(aes()) + 
  geom_smooth(method="loess", se=F) +
  labs(subtitle="Sale Price Vs Year Sold", 
       y="Saleprice", 
       x="Year Sold", 
       title="Scatterplot", 
       caption = "Source: Housing Price")+
  ylim(c(0,600000))

plot(gg)


library(ggplot2)
theme_set(theme_classic())
g <- ggplot(hPrice, aes(as.factor(YrSold), SalePrice))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot",
       subtitle="House Sale Price in differnt year sold ",
       caption="Source: Housing Price",
       x="Year Sold",
       y="Sale Price") 
  

g <- ggplot(hPrice, aes(as.factor(MoSold), SalePrice))
g + geom_boxplot(varwidth=T, fill="plum") + 
  labs(title="Box plot", 
       subtitle="House Sale Price in differnt month sold ",
       caption="Source: Housing Price",
       x="Month Sold",
       y="Sale Price") 


library(ggcorrplot)
theme_set(theme_bw()) 
# Correlation matrix
choose.v=c("SalePrice","LotArea","OverallQual","ExterQual","Foundation",
    "TotalBsmtSF","GrLivArea","KitchenQual","GarageArea","PoolArea","HeatingQC")
corr = round(cor(train.data[,choose.v]), 2)
corr
# Plot
ggcorrplot(corr, hc.order = T, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlogram of Selected Variables", 
           ggtheme=theme_bw)

reduce.v=c("SalePrice","OverallQual","TotalBsmtSF","GrLivArea","GarageArea")
pairs(train.data[,reduce.v])



#linear regression model
lm_model=lm(SalePrice~.,data=train.data[,-1])
sum.lm_model=summary(lm_model)
sum.lm_model

all.coef.var=rownames(sum.lm_model[["coefficients"]]) #3 NA have been ignored
length(all.coef.var)
index=which(sum.lm_model[["coefficients"]][,4]<0.05)
index
length(index)
reduced.variables=all.coef.var[index]
reduced.variables
length(reduced.variables)



X=train.data[,reduced.variables]
y=train.data[,"SalePrice"]
out=summary(regsubsets(X,y,nbest=1,nvmax=ncol(X),intercept=T))
tab=cbind(out$which,rsq=out$rsq,adjr2=out$adjr2,cp=out$cp,bic=out$bic)
tab
View(tab)
tab.name=colnames(tab)
tab.name
min(tab[,"bic"])
max(tab[,"adjr2"])
which(tab[,"bic"]==min(tab[,"bic"]))
tab[which(tab[,"bic"]==min(tab[,"bic"])),"adjr2"]
which(tab[,"adjr2"]==max(tab[,"adjr2"]))
tab[which(tab[,"adjr2"]==max(tab[,"adjr2"])),"bic"]

selectedModel=tab[which(tab[,"bic"]==min(tab[,"bic"])),]
selectedModel
selectedModel.variables=tab.name[which(tab[which(tab[,"bic"]==min(tab[,"bic"])),]==1)]
selectedModel.variables
length(selectedModel.variables)
selected.lm_model=lm(SalePrice~.,data=train.data[,c(selectedModel.variables[-1],"SalePrice")])
summary(selected.lm_model)

#checking the regression model through residual plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(selected.lm_model,which=1:4)

#make a prediction based on the regression model
pred=predict(selected.lm_model,newdat=test.data)
length(pred)
obs=test.data$saleprice
length(obs)
diff=obs-pred
length(diff)
percdiff=abs(diff)/obs
length(percdiff)
me=mean(diff)
rmse=sqrt(sum(diff^2)/dim(test.data)[1])
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error 
hist(diff)

model2 <- randomForest(SalePrice~.,data=train.data, importance  = TRUE,ntree = 500, mtry = sqrt(ncol(train.data)))
#We can tune the random forest model by changing the number of trees (ntree) 
#and the number of variables randomly sampled at each stage (mtry)
model2



