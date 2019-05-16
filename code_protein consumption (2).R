

###########
# K-means #
###########



### *** European Protein Consumption, in grams/person-day *** ###
## read in the data
library(stats)
library(ggplot2)
library(gridExtra)

#food <- read.csv("C:/Users/johns/OneDrive/EDU/FIN_6368/code/protein.csv") # surface
food <- read.csv("D:/DRIVES/OneDrive/EDU/FIN_6368/code/protein.csv") #omen

head(food)

xdata = food[,c("WhiteMeat","RedMeat")]

## first, clustering on just Red and White meat (p=2) and k=3 clusters
set.seed(1) ## to fix the random starting clusters
#?kmeans
grpMeat <- kmeans(xdata, centers=3, nstart=10)
grpMeat
names(grpMeat)


## choose different of K 

bs = numeric()    # define bs to store the values of distance between clusters
ws = numeric()    # define ws to store the values of distance within clusters
for (i in 1:10){
  bs[i] <- kmeans(xdata, centers=i, nstart=10)$betweenss
  ws[i] <- kmeans(xdata, centers=i, nstart=10)$tot.withinss
}

plot.bs = qplot(1:10, bs, geom=c("point", "line"), 
            xlab="Number of clusters", ylab="Betweenss") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Total within-cluster sum of squares vs Choice of k
plot.ws <- qplot(1:10, ws, geom=c("point", "line"),
            xlab="Number of clusters", ylab="Total withinss") +
  scale_x_continuous(breaks=seq(0, 10, 1))

# Subplot
grid.arrange(plot.bs, plot.ws, ncol=2)

## list of cluster assignments
o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

## plotting cluster assignments on Red and White meat scatter plot
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat")
text(x=food$Red, y=food$White, labels=food$Country, col=grpMeat$cluster+1)

## same analysis, but now with clustering on all protein groups
## change the number of clusters to 7
set.seed(1)
grpProtein <- kmeans(food[,-1], centers=3, nstart=10) 
grpProtein
o=order(grpProtein$cluster)
data.frame(food$Country[o],grpProtein$cluster[o])

par(mfrow=c(1,2))
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat",
     main="Clustering by 2 attributes")
text(x=food$Red, y=food$White, labels=food$Country, col=grpMeat$cluster+1)
plot(food$Red, food$White, type="n", xlim=c(3,19), xlab="Red Meat", ylab="White Meat",
     main="Clustering by 9 attributes")
text(x=food$Red, y=food$White, labels=food$Country, col=rainbow(3)[grpProtein$cluster])


## Use principle component analysis with 9 attributes
## clustering by the first 2 PCs
library(stats)
pcs=princomp(food[,-1])
pcs.out=summary(pcs)
pcs.out

plot(pcs)
biplot(pcs)

names(pcs.out)
pcs.out$loadings
pcs.out$scores

grpMeat <- kmeans(pcs.out$scores[,1:2], centers=3, nstart=10)
grpMeat
names(grpMeat)

o=order(grpMeat$cluster)
data.frame(food$Country[o],grpMeat$cluster[o])

plot(pcs.out$scores[,1], pcs.out$scores[,2], type="n", xlab="PC1", ylab="PC2")
text(x=pcs.out$scores[,1], y=pcs.out$scores[,2], labels=food$Country, col=grpMeat$cluster+1)



###########
#   E-M   #
###########

install.packages("mixtools")
library(mixtools)

## Consider just Red and White meat clusters

xdata = food[,c("WhiteMeat","RedMeat")]
head(xdata)

set.seed(2017)
## mixtures of two normal distributions on the first 2 features
## we consider different variances
out2<-mvnormalmixEM(xdata,arbvar=TRUE,k=2,epsilon=1e-02)
out2

prob1 = round(out2$posterior[,1],digits=3)
prob2 = round(out2$posterior[,2],digits=3)
prob = round(out2$posterior[,1])
o = order(prob)
data.frame(food$Country[o],prob1[o],prob2[o],prob[o])
plot(food$Red, food$White, type="n",xlab="Red Meat", ylab="White Meat")
text(x=food$Red,y=food$White,labels=food$Country,col=prob+1)


## mixtures of two normal distributions on all 9 features
## we consider equal variances
xdata_all = food[,-1]
head(xdata_all)

set.seed(2017)
out2all<-mvnormalmixEM(xdata_all,arbvar=FALSE,k=2,epsilon=1e-02)
out2all

prob1 = round(out2all$posterior[,1],digits=3)
prob2 = round(out2all$posterior[,2],digits=3)
prob.all = round(out2all$posterior[,1])
data.frame(food$Country,prob1,prob2,prob)
o_all=order(prob.all)
data.frame(food$Country[o_all],prob[o_all])

par(mfrow=c(1,2))
plot(food$Red, food$White, type="n",xlab="Red Meat", ylab="White Meat",main="E-M with 2 features")
text(x=food$Red,y=food$White,labels=food$Country,col=prob+1)
plot(food$Red, food$White, type="n",xlab="Red Meat", ylab="White Meat",main="E-M with 9 features")
text(x=food$Red,y=food$White,labels=food$Country,col=prob.all+1)


####################
#   Hierarchical   #
####################

install.packages("cluster")
library(cluster)

## we use the program agnes in the package cluster 
## argument diss=FALSE indicates that we use the dissimilarity 
## matrix that is being calculated from raw data. 
## argument metric="euclidian" indicates that we use Euclidian distance
## no standardization is used as the default
## the default is "average" linkage 

## first we consider just Red and White meat clusters

food2agg=agnes(xdata,diss=FALSE,metric="euclidian")
food2agg
plot(food2agg,main="Dendrogram of Protein intake with 2 features")	## dendrogram
food2agg$merge	## describes the sequential merge steps

## identical result obtained by first computing the distance matrix
food2aggv=agnes(daisy(xdata),metric="euclidian")
plot(food2aggv)

## Using data on all nine variables (features)
## Euclidean distance and average linkage 
foodagg=agnes(xdata_all,diss=FALSE,metric="euclidian")
plot(foodagg,main="Dendrogram of Protein intake with 9 features")	## dendrogram
foodagg$merge	## describes the sequential merge steps

## Using data on all nine variables (features)
## Euclidean distance and single linkage
foodaggsin=agnes(xdata_all,diss=FALSE,metric="euclidian",method="single")
plot(foodaggsin)	## dendrogram
foodaggsin$merge	## describes the sequential merge steps

## Euclidean distance and complete linkage
foodaggcomp=agnes(xdata_all,diss=FALSE,metric="euclidian",method="complete")
plot(foodaggcomp)	## dendrogram
foodaggcomp$merge	## describes the sequential merge steps













