library("quantmod")  #Load the quantmod Library
investAmntMonthly <-1000 #How much we want to invest each month
stockData <- new.env() #Make a new environment for quantmod to store data in
tickers <- c("VITPX","VIDMX","VBTLX") #Define the tickers we are interested in
exp<-c(0.015,0.01,0.07) #expense ratio adjusted (NY529-Fund Exp). Fund exp is implicit in Nav
#Model
allocation<-c(.8,0.2,0.2)

startDate<-as.Date("2002-01-01") #Specify period of time we are interested in
endDate<-as.Date("2012-12-31")

#Download the stock history (for all tickers)
getSymbols(tickers, env = stockData, src = "yahoo",from = startDate, to = endDate)

#Get CPI and calulate average
cpi<-read.csv('http://www.quandl.com/api/v1/datasets/IMF/PCPI_111.csv?&auth_token=1D5mLmJ5LGDV8yPyp1zA&trim_start=2001-01-01&trim_end=2012-12-31&collapse=annual&transformation=rdiff&sort_order=asc', colClasses=c('Year'='Date'))
cpits <- xts(cpi[,-1], order.by=cpi[,1])
avgcpi<-mean(cpits)

#Setup Initial Lists
TotalSharesMonthly<-list()
TotalSharesYearly<-list()
TotalSharesYearlyR<-list()
TotalValueMonthly<-list()
TotalValueYearly<-list()
TotalValueYearlyR<-list()

#Loop over ticker list
for(i in 1:length(tickers)) 
{
  #Get CLosing Prices
  lastCloseM<-(Cl(to.period((get(tickers[i],stockData)),'months')))
  AdCloseM<-(Ad(to.period((get(tickers[i],stockData)),'months')))
  lastCloseY<-(Cl(to.period((get(tickers[i],stockData)),'years')))
  AdCloseY<-(Ad(to.period((get(tickers[i],stockData)),'years')))
      
  #calculate total shares bought per ticker
  MonthlyShares<-investAmntMonthly*allocation[i]/lastCloseM
  YearlyShares<-investAmntMonthly*12*allocation[i]/lastCloseY
  TotalSharesMonthly[i]<-c(as.numeric(last(cumsum(MonthlyShares))))
  TotalSharesYearly[i]<-c(as.numeric(last(cumsum(YearlyShares))))

  #calulate total Value per ticker at end of time horizon
  TotalValueMonthly[i]<-c((1-(exp[i]/12))*(as.numeric(TotalSharesMonthly[i])*as.numeric(last(AdCloseM))))
  TotalValueYearly[i]<-c((1-exp[i])*(as.numeric(TotalSharesYearly[i])*as.numeric(last(AdCloseY))))  

}

#Total Value for entire portfolio
FinalMonthly<-sum(unlist(TotalValueMonthly))
FinalYearly<-sum(unlist(TotalValueYearly))
FinalYearlR<-sum(unlist(TotalValueYearlyR))

#Calulate total out of pocket both real and nominal
TotalYears<-round(as.numeric((endDate-startDate)/365))
TotalInvestment<-TotalYears*investAmntMonthly*12
TotalInvestmentR<-investAmntMonthly*12*(((1+avgcpi)^TotalYears-1)/avgcpi)

#calulate total returns
invretM<-100*(FinalMonthly/TotalInvestment-1)
invretY<-100*(FinalYearly/TotalInvestment-1)
invertR<-100*(FinalYearly/TotalInvestmentR-1)

#print out
FinalMonthly
FinalYearly
invretM
invretY
invertR