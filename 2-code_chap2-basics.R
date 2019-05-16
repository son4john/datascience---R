#Chapter II
#sets working directory
#paste combines text
#homesuser is your default setup root directory
#paste function combines strings

#packages
#install.packages()
#update.packages()

setwd(paste(homeuser,"/FinAnalytics/ChapII",sep=""))

#plot function is a scatter plot
x = c(1.3,1.2,1.3,NA,1.4,1.5)
plot(x,ylab="EUR prices")
is.na(x)

#Filter prices:
x[x > 1.3]
#Keeps the NA intact:
y <- diff(log(x))
round(y,3)

#g(x,y) is a power function:
g <-function(x,y=5) { return(x^y) }
g(4)
g(y=4,6)
g(4,y=7)
g(y=8,x=4)
g

#3 assignment operators
x <- 1
assign("x",2)
x
x = 3
x
f <-function(x)
{
  x = 4
  x
}
#returns 4
f(x)
#returns 3
x

#The fourth type is "<<-" SUPER-ASSIGNMENT
# the <<- changes the value of x everywhere not just the function
x = 3
x
f <-function(x)
{
  x <<- 4
  x
}
f(x)
x
#R is dynamically typed variables don't have types but based on the usecase 
#they act like specific types
typeof(f)
typeof(x)

#Classic if-else:
call_type = 2
if(call_type == 1) {
  str = "f(2)"
} else {
  str = "g(2)" #power function from earlier
}
#parse the string then evaluate
eval(parse(text=str))

#Not so classic if-else function:
#different way of writing simple if else function
call_type = 2
ifelse(call_type == 1,
  eval(parse(text="f(2)")),
  eval(parse(text="g(2)")))

#Functional nature:
set.seed(1)
vec = c(1:3)
sapply(vec,rnorm) # set seed forces the randomly generated normal dis value stay the same all itterations

#Create two column matrix:
A = cbind(rep(x,length(y)),y)
A
B = rbind(rep(x,length(y)),y)
B
t(A) == B #transpose A and compares it to B. Looks at each individual item
sum(t(A) == B) #comes back NA because anytihng sum with NA is NA

#Subscripting: positive and negative
B[,4] # show me the fourth column
B[,-4] # show me everything but the fourth column
t(A)[,-4] == B[,-4] #remove the fourth columns, transpose A and Compare
sum(t(A)[-2,-4] == B[-2,-4])

#Ranges and looping:
n <- 12
z <- 1:n
z
z <- c(1:n)
z
z <- vector(length=n)
for(i in 1:n)
  z[i] <- i
z

#Matrices and arrays:
my.mat<-matrix(1:9, nrow=3)
mat2by4 <- matrix(1:8, nrow=2, ncol=4)
mat2by4
#3 dimensonal array
arr2by4by3 <- array(1:24, dim=c(2,4,3))
arr2by4by3

arr2by4by3[1,,]
arr2by4by3[1,-4,]
arr2by4by3[1,c(-3,-4),]

length(c(-3,-4))
dim(arr2by4by3[1,c(-3,-4),])

A <- arr2by4by3[1,c(-3,-4),]
t(A)
A <- arr2by4by3[1,c(-3,-4),]
A
t(A)
A%*%t(A)
1+9*9+17*17

#Exception handling:
fh <- 0
tryCatch({
  #main block
  fh <<- file("file1.txt", open="r")
}, warning = function(w) {
  #warning-handler-code
  print(w)
  fh <<- NA
}, error = function(e) {
  #error-handler-code
  print(e)
  fh <<- NA
}, finally = {
  #cleanup-code
})
if(!is.na(fh)) readLines(fh)

#Setting precision:
options(digits=10)
pi = 3.1415926535897932384626
pi

if(FALSE) {
  x = -1.5
  abs(x)  #absolute value
  sqrt(-x)	#square root
  ceiling(3.475) #is 4
  floor(3.475) #is 3
  trunc(5.99) #is 5
  round(3.475, digits=2) #is 3.48
  signif(3.475, digits=2) #is 3.5
  x = pi / 4
  x
  v = c(cos(x),sin(x), tan(x))
  v
  acos(v[1])
  log(x)	  #natural logarithm
  log10(x)	#common logarithm
  exp(x)	  #e^x
}


##########
#  DIST  #
##########

#Random sampling:
?rbinom
a <- rbinom(50,100,1/2) #(trials,range,probability)
plot(density(rbinom(50,50,1/2)))
#when you use adust you can smooth the curve larger is smoother
plot(density(rbinom(50,50,1/2)), adjust = 10) #rbinom(number of observations,number of trials ,probability)
plot(density(runif(50, max = 2, min =-2))) #uniform distribution
plot(hist(runif(50, max = 2, min =-2)))

options(digits=6)


#String concatenation:
print(paste("PCLN","UNP","IBM","MCD","PFE",sep=","))

#Date and string functions:
date <- as.Date("2014-02-01")
substr(date,9,11)

#String array:
tickers <- c("PCLN","UNP","IBM","MCD","PFE")
match('MCD',tickers)

#Data frame:
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
d <- data.frame(x = 1, y = 1:10, fac = fac)
d[1:4,]
d$fac
d
d$x

#To keep the same sequence of random values have to run them both togther
set.seed(99)
testSeed<-sample(10, replace = TRUE)
set.seed(5) #using the same number 5 on any computer gives you same random sample
L3 <- LETTERS[1:3]
fac <- sample(L3, 10, replace = TRUE)
d<- data.frame(x=1, y=1:10, fac =fac)
d[1:4,]
d[4,]
d$fac #call the fac column in the d dataframe fac is name of column
#review some of the distriubtions marked on in our notes


#Input-ouput:
write.csv(d,file="d.txt",row.names=FALSE)
e <- read.csv("d.txt",header=TRUE)
e[1:4,]
names(e)
names(e) <- c(names(e)[1:2],"factor")
e[-c(2:dim(e)[1]),]
typeof(e)

#Lists:
c(1,c(1,2),3,"A",c(4,5))
list(1,c(1,2),3,"A",list(4,5))
l <- list(1,c(1,2),3,"A",list(4,5))
l[2]
l[[2]]
l

e[[1]]
e[[2]]
e[[3]]

obtainPrices <- function() {
  A <- matrix(c("VRSN","UNP","HPQ","NSC"),nrow=1)
  B <- matrix(c(37.61,125.62,50.48,50.44),nrow=1)
  list(A,B) #even though there is no return this is what is returned
}
res <- obtainPrices()
res[[1]]
res[[2]]
