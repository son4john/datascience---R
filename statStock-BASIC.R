ret<-c(0.05,0.10,0.08,-0.05,0.12,0.06)

stat<-function(stock){
  meanx = mean(stock)
  stdx = sd(stock)
  return(c(mean = meanx, std = stdx ))
}

stat(ret)