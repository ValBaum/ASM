rm(list = ls())
# Load the RData file
load("Project_ASM.RData")
library(dplyr)
library(quantmod)
library(lubridate)
#dat$Year <- floor(dat$Date)
#dat$Month <- round((dat$Date - dat$Year) * 12)+1
dat$Date <- as.Date(dat$Date, format = "%b. %Y")
# Create a new Date column with the first day of each month

dat$Inception.Date <- as.Date(dat$Inception.Date)

# Calculate the age of the fund for each row
dat$Fund.Age <- dat$Date - dat$Inception.Date

# Create a new variable 'result' with columns Date, Ticker, and Fund.Age
result <- dat[, c("Date", "Ticker","Global.Broad.Category", "Fund.Age", "NAV (USD)","Management.Fee", "Dividend.Yield", "Average.Manager.Tenure", "Return")]

totals <- dat %>%
  group_by(Date, Global.Broad.Category) %>%
  summarise(Total_NAV = sum(`NAV (USD)`))

# Merge the totals back to the original dataset and calculate the difference
result <- result %>%
  left_join(totals, by = c("Date", "Global.Broad.Category")) %>%
  mutate(family_tna = Total_NAV - `NAV (USD)`) 

category_counts <- dat %>%
  group_by(Global.Broad.Category) %>%
  summarise(family_no = n_distinct(Ticker))

result <- result %>%
  left_join(category_counts, by = "Global.Broad.Category") 

max_age_per_category <- dat %>%
  group_by(Date, Global.Broad.Category) %>%
  summarise(family_age = max(Fund.Age))

# Merge the maximum age information back to the result
result <- result %>%
  left_join(max_age_per_category, by = c("Date", "Global.Broad.Category"))
# Print the result


# Rename columns
result <- result %>%
  rename(tna = `NAV (USD)`,
         age = Fund.Age,
         expense_ratio = Management.Fee, 
         dividend = Dividend.Yield, 
         avg_manager = Average.Manager.Tenure)

result <- result %>%
  select(-Total_NAV)

df <- read.csv("df.csv")
df <- df %>%
  rename(Date = DATE)
df$Date <- as.Date(df$Date, format = "%Y-%m-%d") 
# Assuming you have already calculated max_age_per_category and result using the previous code

# Merge the result variable with the new dataframe df based on the "Date" column
result <- left_join(result, df, by = "Date")
# Get the list of columns with .x and .y suffixes
conflict_columns <- grep("\\.x|\\.y", names(result), value = TRUE, perl = TRUE)

# Merge conflicting columns, prioritizing non-NA values
for (col in conflict_columns) {
  col_name <- sub("\\.x|\\.y", "", col)  # Remove .x or .y suffix
  result[[col_name]] <- ifelse(!is.na(result[[paste0(col_name, ".y")]]),
                                     result[[paste0(col_name, ".y")]],
                                     result[[paste0(col_name, ".x")]])
}

# Remove the original conflicting columns
result <- result %>%
  select(-ends_with(".x"), -ends_with(".y"))



#excess return (abnormal return)

#
# s&p500
#

# Define the start and end dates
start_date <- "2007-05-01"
end_date <- Sys.Date()  # Use today's date as the end date

# Define the symbol for S&P 500 (Yahoo Finance code: ^GSPC)
symbol <- "^GSPC"

# Download the historical data
getSymbols(symbol, from = start_date, to = end_date, adjust = TRUE)

# Extract the adjusted closing prices for the closing time of the first day of each month
sp500 <- Op(to.monthly(GSPC, indexAt = "firstof"))
#return for each month
monthly_returns <- (lead(sp500) / sp500 - 1)*100# Remove the first row (NA due to lag)

#
# s&p500
#
# Define the start and end dates
start_date <- "2007-05-01"
end_date <- Sys.Date()  # Use today's date as the end date

# Define the symbol for the 10-year US Treasury bond (Yahoo Finance code: ^IRX)
symbol_treasury <- "^IRX"

# Download the historical data
getSymbols(symbol_treasury, from = start_date, to = end_date, adjust = TRUE)

# Extract the adjusted closing prices
treasury_close <- Ad(IRX)
monthly_mean <- apply.monthly(treasury_close, FUN = mean, na.rm = TRUE)
index(monthly_mean) <- as.Date(format(index(monthly_mean), "%Y-%m-01"))
monthly_returns_df <- data.frame(Date = index(monthly_returns), Monthly_Return = coredata(monthly_returns))
result <- left_join(result, monthly_returns_df, by = "Date", suffix = c(".result", ".s&p500"))
monthly_mean_df <- data.frame(Date = index(monthly_mean), Mean_Rate = coredata(monthly_mean))
result <- left_join(result, monthly_mean_df, by = "Date", suffix = c(".result", ".treasury"))


beta_result <- read.csv("beta_results.csv")

# Merge beta results with the main data frame
result <- merge(result, beta_result, by = "Ticker")

result$Expected_Return <- with(result, IRX.Adjusted + Beta * (GSPC.Open - IRX.Adjusted))

result$abnormal_return <- result$Return - result$Expected_Return

result <- result[order(result$Ticker, result$Date), ]
# Function to calculate mean abnormal return over the past 12 months
calculate_momentum <- function(abnormal_returns) {
  # Calculate the cumulative sum of abnormal returns for the past 12 months
  cumsum_abnormal_returns <- cumsum(abnormal_returns)
  
  # Calculate the mean abnormal return from 12 months ago to 2 months ago
  momentum_values <- sapply(1:(length(cumsum_abnormal_returns) - 11), function(i) {
    mean(abnormal_returns[i:(i + 10)], na.rm = TRUE)
  })
  
  # Pad with NA for the first 11 months (no sufficient data for the calculation)
  c(rep(NA, 11), momentum_values)
}


# Calculate the momentum column
result$momentum <- ave(result$abnormal_return, result$Ticker, FUN = calculate_momentum)

fam_momentum <- result %>%
  group_by(Global.Broad.Category, Date) %>%
  summarise(family_momentum = mean(momentum, na.rm = TRUE))
result <- left_join(result, fam_momentum, by = c("Global.Broad.Category", "Date"))


result <- result %>%
  select(-Return, -GSPC.Open, -IRX.Adjusted, -Expected_Return)

result <- na.omit(result)
result$SENT <- as.numeric(gsub(",", ".", result$SENT))
result$expense_ratio <- as.numeric(gsub(",", ".", result$expense_ratio))
result$avg_manager <- as.numeric(gsub(",", ".", result$avg_manager))

names(result)[names(result) == 'Beta'] <- 'beta'
names(result)[names(result) == 'dgp_growth'] <- 'dgp'
names(result)[names(result) == 'SENT'] <- 'sentiment'
names(result)[names(result) == 'CFNAI'] <- 'cfnai'
names(result)[names(result) == 'VIXCLS'] <- 'vix'
names(result)[names(result) == 'FEDFUNDS'] <- 'fed'
names(result)[names(result) == 'Ticker'] <- 'ticker'
names(result)[names(result) == 'Date'] <- 'date'
names(result)[names(result) == 'Global.Broad.Category'] <- 'category'


write.csv(result, "learning_data.csv")
