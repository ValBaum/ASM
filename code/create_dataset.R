# Load the RData file
load("Project_ASM.RData")
library(dplyr)
dat$Year <- floor(dat$Date)
dat$Month <- round((dat$Date - dat$Year) * 12)+1

# Create a new Date column with the first day of each month
dat$Date <- as.Date(paste(dat$Year, dat$Month, "01", sep="-"), format="%Y-%m-%d")
dat$Inception.Date <- as.Date(dat$Inception.Date)

# Calculate the age of the fund for each row
dat$Fund.Age <- dat$Date - dat$Inception.Date

# Create a new variable 'result' with columns Date, Ticker, and Fund.Age
result <- dat[, c("Date", "Ticker","Global.Broad.Category", "Fund.Age", "NAV (USD)","Management.Fee", "Dividend.Yield", "Average.Manager.Tenure")]

totals <- dat %>%
  group_by(Date, Global.Broad.Category) %>%
  summarise(Total_NAV = sum(`NAV (USD)`))

# Merge the totals back to the original dataset and calculate the difference
result <- result %>%
  left_join(totals, by = c("Date", "Global.Broad.Category")) %>%
  mutate(family_tna = Total_NAV - `NAV (USD)`) 

category_counts <- dat %>%
  group_by(Global.Broad.Category) %>%
  summarise(Family_no = n_distinct(Ticker))

result <- result %>%
  left_join(category_counts, by = "Global.Broad.Category") 

max_age_per_category <- dat %>%
  group_by(Date, Global.Broad.Category) %>%
  summarise(Family_age = max(Fund.Age))

# Merge the maximum age information back to the result
result <- result %>%
  left_join(max_age_per_category, by = c("Date", "Global.Broad.Category"))
# Print the result


# Rename columns
result <- result %>%
  rename(tna = `NAV (USD)`,
         age = Fund.Age,
         exp_ratio = Management.Fee, 
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
print(result)
