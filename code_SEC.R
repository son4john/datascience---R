

### Download financial data from SEC ###
# query data from SEC by package "finreportr"
# CompanyInfo(): returns basic information about a company
# AnnualReports(): returns a listing of annual reports filed by a company
# GetIncome(): returns the income statement for a given company
# GetBalanceSheet(): returns the balance sheet for a given company
# GetCashFlow(): returns the cash flow statement for a given company

install.packages("finreportr")
library(finreportr)
CompanyInfo("FB")
head( AnnualReports("FB",2017))
FB.IS = GetIncome("FB",2017)
head(FB.IS,3)	
FB.BS = GetBalanceSheet("FB",2017)
head(FB.BS,3)
FB.CF = GetCashFlow("FB",2017)
head(FB.CF,3)


getSEC <- function(ticker, year) {
  options(digits=4)
  require(finreportr)
  require(reshape2)
  require(xlsx)
  SEC.IS <- GetIncome(ticker, year)
  SEC.IS$Amount <- as.numeric(SEC.IS$Amount)
  SEC.BS <- GetBalanceSheet(ticker, year)
  SEC.BS$Amount <- as.numeric(SEC.BS$Amount)
  SEC.CF <- GetCashFlow(ticker, year)
  SEC.CF$Amount <- as.numeric(SEC.CF$Amount)
  SEC.IS.W <- dcast(SEC.IS, Metric + Units~endDate, value.var="Amount", fun.aggregate=mean)
  SEC.CF.W <- dcast(SEC.CF, Metric + Units~endDate, value.var="Amount", fun.aggregate=mean)
  SEC.BS.W <- dcast(SEC.BS, Metric + Units~endDate, value.var="Amount", fun.aggregate=mean)
  #write.xlsx(SEC.IS.W, file = paste(ticker,"-",year,".xlsx", sep=""), sheetName = "IS", row.names = FALSE)
  #write.xlsx(SEC.BS.W, file = paste(ticker,"-",year,".xlsx", sep=""), sheetName = "BS", row.names = FALSE, append=TRUE)
  #write.xlsx(SEC.CF.W, file = paste(ticker,"-",year,".xlsx", sep=""), sheetName = "CF", row.names = FALSE, append=TRUE)
  return(list(BS=SEC.BS.W,IS=SEC.IS.W,CF=SEC.CF.W))
}

alldata=getSEC('FB',year=2017)
alldata$BS
alldata$IS
alldata$CF
