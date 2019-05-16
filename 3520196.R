library(TSA)
data(boardings)
str(boardings)
names(boardings)
boardings<-boardings[,1]
plot(boardings, col = "blue")
points(boardings, x=time(boardings), pch = as.vector(season(boardings)))
acf(as.vector(boardings))
pacf(as.vector(boardings))