#loops
#normal distribution
#if statements
#vectors
#list
#naming a function
#apply functions
#missing values

######################
## write a function ##
######################

# give a nice name for the function
# the arguments are defined in the parathesis of function()

say.hello<-function(){
  print("Hello, World!")  
}  
say.hello()


# define one argument "name" in the function

say.hello<-function(name)
{
  sprintf("Hello %s", name)  
}  
say.hello("Bob")    #call the function with value for the argument variable

say.hello("Bob", extra= "Goodbye")    #assign more values for the arguements
say.hello("Bob","Jerry")


#use the special operator (.) for the extra arguments

say.hello<-function(name,...)
{
  sprintf("Hello %s", name)  
}  
say.hello("Bob","Jerry")    


# define two arguments "first" and "last" in the function

hello.person<-function(first,last)
{
  sprintf("Hello %s %s", first,last)  
}  
hello.person("Bob","Smith")    


#Write a function to compute the means and standard deviations for the returns of a stock. 
#Name the function by stats(). Do it!

ret<-c(0.05,0.10,0.08,-0.05,0.12,0.06)
stats<-function(dat){
  mean=mean(dat)
  std=sd(dat)
  return(c(mean=mean,std=std))#combine mulitple returns
                              #if no return std returns last output
}
stats(ret)
  
#recording 3

######################
## control statement##
######################


beta<-1.5
ifelse(beta>1, "high risk","low risk")

#Use if else statement to test a value 
#if it is positive, print "positive value", 
#else if it is negative value, print "negative value", 
#else print "zero". Do it!

x=0.20
if (x>0){
  print("positive")
}else if (x<0){
  print("negative")
}else{
  print("zero")
}
  

######################
##        Loops     ##
######################

for (i in (1:10))
{
  print(i)
}

#Build a vector with three names in the class
#save it to an object "classnames" and use for loop to count the lengths of each name. Do it!
  
classnames<-c("Kevin","Jennifer","Michael")
for (x in classnames){
  print(x)
  print(nchar(x))#function for character string length
}


n <-length(classnames)
namelength<-rep(0,n) #function rep - repeates 3 times 0 - stores 3 zero in name length, numerical vector
names(namelength)<-classnames #make the actual names the names for the namelength vector
for (i in 1:length(classnames))
{
  namelength[i]<-nchar(classnames[i])
}
namelength

#31m Recording 3

#assume that a stock has a geometric random walk process
#returns have normal distribution
#price = $100
#mean return = .05
#standard deviation .2
#simulate for the next year how the stock price will change 9 different simulation
#253 trading days


par(mfrow=c(3,3))#add rmultiple figure 3 row 3 columns
for (i in 1:9){
  logr=rnorm(253,0.05/253,0.2/sqrt(253)) #normalize mean and standard over 253 days deviation by dividing by 253
  #rnorm is one of the functions avialable for the random number generater for normal distribution
  #return based on the current price
  #253 items in
  price=c(100,100*exp(cumsum(logr)))#intial day 100 return also cumulate all the 253 days random normal values
  plot(price,type="l",main="OoTEST")
}  

#Use next to skip one iteration

for (i in (1:10))
{
  if (i==5) 
  {
    next
  }
  print(i)
}

#use skip to break the loop

for (i in (1:10))
{
  if (i==5) 
  {
    break
  }
  print(i)
}

x<-1
while (x<=10)
{
  print(x)
  x=x+1
} 


######################
##  Apply functions ##
######################

#apply() function - can be used on a matrix but not a dataframe
#smilar to a loop
#use instead of oop when beneficial

my.mat<-matrix(1:9, nrow=3)  #create a matrix
apply(my.mat, 1, sum)         #1 indicates to do algorithm by row
apply(my.mat, 2, sum)         #2 indicates to do algorithm by column
rowSums(my.mat)               # does what 1 does
colSums(my.mat)               # does what 2 does

#apply() function for data with missing values
#matrix
my.mat.na<-my.mat
my.mat.na[2,2]<-NA #missing value
apply(my.mat.na, 1, sum) #without accounting for the missing value it makes the whole row NA
apply(my.mat.na, 1, sum, na.rm=TRUE) # na.rm remove missing values from your calculation

#lapply () function will apply a function to each element of a list and return the result as a list
# for list
my.list<-list(A=matrix(1:9,3),B=1:5,C=matrix(1:4,2))
my.list
lapply(my.list,sum)

#sapply () function will return the result as a vector
#for a list returned as a vector
sapply(my.list,sum)

# we use this because loops are slow when it comes to large data



my.mat
my.mat[,-2] #every row minus column 2 rows
