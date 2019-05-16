splitAdjust <- function(prices,symbol) {
  len = length(prices)
  origFinalPrice = prices[len]
  for(j in 2:len) {
    split = 0
    #print(paste(prices[j-1],prices[j]))
    if(prices[j-1] >= 1.4*prices[j]) {
      split = +1.5 # a 3 for 2
      if(prices[j-1] >= 1.8*prices[j])
        split = +2 #At least a 2 for 1
      if(prices[j-1] >= 2.9*prices[j])
        split = +3 #Ah a 3 for 1
      if(prices[j-1] >= 3.9*prices[j])
        split = +4 #Ah a 3 for 1
      if(prices[j-1] >= 4.9*prices[j])
        stop(paste(symbol,'detected more than 4:1 split'))
      print(paste("split adjusting",symbol,split,
                  j,prices[j-1],prices[j]))
    } #reverse splits: price increases so divide
    if(prices[j-1] <= prices[j]/1.4) {
      split = -1.5
      if(prices[j-1] <= prices[j]/1.9 &&
         prices[j-1] >= prices[j]/2.1)
        split = -2
      if(prices[j-1] <= prices[j]/2.9 &&
         prices[j-1] >= prices[j]/3.1)
        split = -3
      if(prices[j-1] <= prices[j]/5.8 &&
         prices[j-1] >= prices[j]/6.2)
        split = -6
      if((prices[j-1] <= prices[j]/7.7) &&
         (prices[j-1] >= prices[j]/8.3))
        split = -8
      if((prices[j-1] <= prices[j]/9.7) &&
         (prices[j-1] >= prices[j]/10.3))
        split = -10
      if((split == 0) && (prices[j-1] <= prices[j]/2.9))
        stop(paste(symbol,
                   'detected more than double reverse split'))
      print(paste("reverse split adjusting",j,symbol,j,
                  split,prices[j-1],prices[j]))
    }
    if(split != 0) {
      for(k in j:len) { #adjust all prices to right from j:len
        if(symbol=="C")
          prices[k] = prices[k]/10 #hard coded for Citi
        else if(split == +1.5)
          prices[k] = 1.5*prices[k] # 3 for 2
        else if(split == +2)
          prices[k] = 2*prices[k] # 2 to 1
        else if(split == +3)
          prices[k] = 3*prices[k] # 3 to 1
        else if(split == +4)
          prices[k] = 4*prices[k] # 4 to 1
        else if(split == -1.5)
          prices[k] = prices[k]/1.5 # 2 to 3 rev
        else if(split == -2)
          prices[k] = prices[k]/2 # 1 to 2 rev
        else if(split == -3)
          prices[k] = prices[k]/3 # 1 to 2 rev
        else if(split == -6)
          prices[k] = prices[k]/6 # 1 to 8 rev
        else if(split == -8)
          prices[k] = prices[k]/8 # 1 to 8 rev
        else if(split == -10)
          prices[k] = prices[k]/10 # 1 to 10 rev
        else stop('splitAdjust internal error')
      }
    }
  }
  finalPrice = prices[len]
  return(prices*origFinalPrice/finalPrice)
}



findR <- function(prices,isSplitAdjusted=TRUE) {#Find R: logrets:
  len <- dim(prices)[1]
  D   <<- dim(prices)[2]
  R   <- matrix(nrow=(len-1),ncol=D)
  for(i in 1:D) {
    #print(i)
    if(!isSplitAdjusted) prices[,i] <<- splitAdjust(prices[,i],lab[i])
    R[,i] = 100*diff(log(prices[,i])) ###log rets
  }
  R
}

findCovMat <- function(R) {#black box give returns
  meanv <- apply(R,2,mean)
  cov_mat <- cov(R)
  diag_cov_mat <- diag(cov_mat)
  sdevv <- sqrt(diag(cov_mat))
  list(meanv,cov_mat,diag_cov_mat,sdevv)#get mean variance covariance matrix other things 
}

readExchSymbols <- function(fileName) {
  frame <- read.csv(fileName,header=TRUE,sep="\t")
  return(as.character(frame[,1]))
}

readSubDirs <- function(dir,isSubDir=TRUE) {
  if(isSubDir) {
    #Case: 2 sub-dirs: NYSE and NASDAQ
    #Return 3 results, the last being a large vec
    setwd(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",dir,"/NYSE",sep=""))
    lab <- readExchSymbols("NYSEclean.txt")
    D1 <- length(lab)
    print(D1)
    setwd(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",dir,"/NASDAQ",sep=""))
    lab2 <- readExchSymbols("NASDAQclean.txt")
    lab <- append(lab,lab2)
    D2 <- length(lab2)
    print(D2)
    list(D1,D2,as.character(lab))
  } else {
    setwd(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",dir,sep=""))
    lab <- readExchSymbols(paste(dir,"clean.txt",sep=""))
    D <- length(lab)
    print(D)
    list(D,as.character(lab))
  }
}

acquirePrices <- function(prices,lab,len,D,D1,D2,dir,
                          start,end,isSubDir=TRUE,verbose=TRUE) {
  isSuccessfulQuote <- FALSE
  for(d in 1:D) {
    if(d == 1 || (isSubDir && d == (D1+1)))
      if(d == 1 && isSubDir) {
        setwd(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",dir,"/NYSE",sep=""))
        unlink('bad*')
        print(paste("NYSE=======:",d))
      } else if(d == (D1+1) && isSubDir) {
        setwd(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",dir,"/NASDAQ",sep=""))
        unlink('bad*')
        print(paste("NASDAQ=======:",d))
      } else {
        setwd(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",dir,sep=""))
        unlink('bad*')
        print(paste("ETF==========:",d))
      }
    if(verbose) print(paste(d,lab[d]))
    fileName = paste("cached",lab[d],".csv",sep="")
    usingCacheThisFileName <- FALSE
    if(file.exists(fileName)) {
      usingCacheThisFileName <- TRUE
      pricesForStock <- read.csv(fileName,header=TRUE,sep="")[,1]
      if(!is.na(pricesForStock[1]))
        isSuccessfulQuote <- TRUE
    }
    if(!usingCacheThisFileName ||
       (usingCacheThisFileName && length(pricesForStock) != len)) {
      usingCacheThisFileName <- FALSE
      tryCatch( {
        #print(start);print(end)
        Sys.sleep(1)
        pricesForStock <- get.hist.quote(lab[d],quote="Adj",
                                         start=start,end=end)
        if(!is.na(pricesForStock[1]))
          isSuccessfulQuote <- TRUE
      }, error = function(err) {
        print(err);cat(lab[d],file="badsyms.txt",
                       append=TRUE,sep="\n")
        isSuccessfulQuote <- FALSE
      } )
    }
    if(length(pricesForStock) == len) {
      prices[,d] <- pricesForStock
      if(sum(is.na(prices[,d])) > 0 || (sum(is.na(prices[,d-1])) == 0 &&
                                        d > 1 && prices[1,d] == prices[1,d-1])) {
        print(paste(lab[d],"has NA prices"))
        cat(lab[d],file="badsyms.txt",
            append=TRUE,sep="\n")
        isSuccessfulQuote <- FALSE
      }
    } else {
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
    }
    if(!isSuccessfulQuote)
      cat(lab[d],file="badsyms.txt",append=TRUE,sep="\n")
    if(isPlotInAdjCloses) {
      if(d == 1)
        plot(prices[,d]/prices[1,d],type="l",col="blue",ylim=c(.2,6))
      else
        lines(prices[,d]/prices[1,d],type="l",col="blue")
      text(len,(prices[len,d]/prices[1,d]),lab[d],cex=.6)
    }
    if(isCacheEnabled && !usingCacheThisFileName &&
       isSuccessfulQuote) {
      #save redundant re-write
      fileName = paste("cached",lab[d],".csv",sep="")
      print(fileName)
      write.csv(prices[,d],file=fileName,row.names = FALSE)
    }
    isSplitAdjusted = TRUE
  }
  prices
}

createDirs <- function(dir,isSubDir=TRUE) {
  #check for the two subdirs if isSubDir TRUE
  mainDir <- paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",sep="")
  destDir <- paste(mainDir,dir,sep="")
  if (!file.exists(destDir))
    dir.create(file.path(destDir))
  setwd(file.path(destDir))
  if(isSubDir) {
    f1 <- "NYSEclean.txt"
    f2 <- "NASDAQclean.txt"
    
    NYSEsubDir <- paste(destDir,"/NYSE",sep="")
    if (!file.exists(NYSEsubDir))
      dir.create(file.path(NYSEsubDir))
    if(!file.exists(paste(NYSEsubDir,"/NYSEclean.txt",sep="")))
      file.copy(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",f1,sep=""),
                NYSEsubDir)
    
    NASDAQsubDir <- paste(destDir,"/NASDAQ",sep="")
    if (!file.exists(NASDAQsubDir))
      dir.create(file.path(NASDAQsubDir))
    if(!file.exists(paste(NASDAQsubDir,"/NASDAQclean.txt",sep="")))
      file.copy(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",f2,sep=""),
                NASDAQsubDir)
  } else {
    f <- paste(dir,"clean.txt",sep="")
    if(!file.exists(paste(destDir,"/",f,sep="")))
      if(file.exists(paste(mainDir,"/",f,sep="")))
        file.copy(paste("~/Desktop/Financial Information and Analysis (S19)/FinAnalytics/",f),".")
  }
}
createDirs("jiemin")
