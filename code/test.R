install.packages("devtools")

# Option 1: download and install latest version from ‘GitHub’
devtools::install_github("axelperschmann/AbnormalReturns")
library(AbnormalReturns)
x <- abnormalReturn(prices_stock=d.VW, prices_market=d.DAX, model="marketmodel",
                    estimationWindowLength=10, c=10, attributeOfInterest="Close", showPlot=TRUE)
head(x)
summary(x$abnormalReturn)



library(quantmod)

# Define the ticker symbol for NLSAX
fund_ticker <- "NLSAX"

# Define the start and end dates for the historical data
start_date <- "2022-01-01"
end_date <- "2023-01-01"

# Download historical data from Yahoo Finance
getSymbols(fund_ticker, from = start_date, to = end_date)

# Extract adjusted closing prices
fund_data <- Cl(get(fund_ticker))

# Print the first few rows of the data
head(fund_data)

