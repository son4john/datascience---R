#Johnson Oonnoonni
#Assignment 1
#Due 01-28-2018
  
#1. Create a  vector with 9 numerical values of different degree of freedoms
#2. Use the function rt() to generate 100 random values which is from a t-distribution with each choice of degree of freedoms in step 1.
#3. Plot the histogram and density curve of the 9 sequences of the data using functions hist() and density(). Print 9 plots in one page. In the plots, give a title of the each plot and indicate the value of degree of freedom, such as "Histogram with df = 5". 
#4. Use the random values in step 2 to compute the density value of the t-distribution by function dt(). Plot the density for the 9 sequences of the data. 

#create vector with 9 numerical values of different degrees of freedom
def <- c(10,25,33,29,18,12,23,27,30) #degrees freedom Vector

par(mfrow=c(3,3)) # 9 graphs per page
for(i in 1:length(def)){ # loop through all 9 degrees freedom
  tdat=rt(100,def[i]) #generate 100 random value which is from a t-distribution
  hist(tdat, breaks=100, main = paste("Histogram df = ", def[i])) #histogram
  plot(density(tdat), main = paste("Density Curve df = ", def[i])) #density curve non - parametric
  plot(dt(tdat,def[i]),type = "l", main = paste("Density Function df = ", def[i])) # density value parametric fit to distribution
  
}
######################
# DENSITY FUNCTION   # 
######################

#density()
#non-parametric refers to a statistical method in which
#the data is not required to fit a normal distribution
#uses data that is often ordinal, meaning it does not rely on numbers, 
#but rather a ranking or order of sorts.
#lines add plots on the same plot as before

#density - no distrubtion 
#based on histograpm
#adjust - the curve of this argument larger more regular shape
#density function non-parmetric

value1 = sample(-4:4, 100, replace = T)
sort(value1)
hist(value1)
plot(density(value1))
(dt(value1, 3), type = "l")

#plotting two values togther
hist(value1, freq = F)
lines(density(value1))