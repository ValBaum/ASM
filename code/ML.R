rm(list = ls())
data <- read.csv("~/Documents/Cours/stat/project/code/learning_data.csv")


# Prepare the data

# Normalization between -0.5 and 0.5 
numeric_cols <- sapply(data, is.numeric)
numeric_cols["prediction"] <- FALSE

data[, numeric_cols] <- apply(data[, numeric_cols], 2, function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) - 0.5)


# generating training/testing dataset
set.seed(10)

data$isTrain <- rbinom(nrow(data), 1, 0.66)
train <- subset(data, data$isTrain == 1)
test <- subset(data, data$isTrain == 0)



# Fit a logistic regression model
lm_model <- lm(prediction ~  age + tna + expense_ratio + abnormal_return +
                 dividend + avg_manager + family_tna + family_no + family_age +
                 vix + sentiment + cfnai + fed + dgp + beta + momentum + family_momentum,
               data = train)


predictions <- predict(lm_model, newdata = test)

# Calculate Mean Absolute Error (MAE)
mae <- sum(abs(test$abnormal_return - predictions))
print(paste("Mean Absolute Error (MAE):", mae))
