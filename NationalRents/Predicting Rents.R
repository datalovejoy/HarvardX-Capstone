# Install missing packages

# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(modelsummary)) install.packages("modelsummary", repos = "http://cran.us.r-project.org")

# Load libraries
library(caret)
library(tidyverse)
library(data.table)
library(corrplot)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(GGally)
library(modelsummary)
library(cowplot)

# Import data
data <- read.csv("https://raw.githubusercontent.com/lovejoylink/HarvardX-Capstone-Project/master/NationalRents/us_census_data.csv", stringsAsFactors = FALSE)

# Data summary
data %>% as_tibble()

# check for missing values
sum(is.na(data))
# replace missing values
data[is.na(data)] <- 0
# check again for missing values
sum(is.na(data))

# Partition the data into a train set and a test set
set.seed(1)
test_index <- createDataPartition(y = data$gross_rent, times = 1, p = 0.1, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]

# Summarizing data in the target variable
summary(train$gross_rent)

sd(train$gross_rent)

# Distribution of rent values
train %>%
  ggplot(aes(gross_rent)) +
  geom_histogram(bins = 30, fill = "blue", col = "black") +
  xlab("US Dollars") +
  ylab("Number of Counties") +
  ggtitle("Distribution of Monthly Rent")

# Checking variable correlation with rents
correlation <- apply(train,2, function(col)cor(col, train$gross_rent))
cor_table <- data.frame(correlation)
cor_table

# Create new data frame with potential predictors
df <- data.frame(train$pct_asian,train$pct_adults_bachelors_or_higher,train$pct_adults_with_high_school_diploma,train$pct_physical_inactivity, train$death_rate_per_1k, train$gross_rent)

# rename columns
names(df)[names(df) == 'train.gross_rent'] <- 'Rent'
names(df)[names(df) == 'train.pct_physical_inactivity'] <- 'Inactivity'
names(df)[names(df) == 'train.pct_adults_with_high_school_diploma'] <- 'HS Diploma'
names(df)[names(df) == 'train.death_rate_per_1k'] <- 'Deaths'
names(df)[names(df) == 'train.pct_asian'] <- 'Asian Pop'
names(df)[names(df) == 'train.pct_adults_bachelors_or_higher'] <- 'Bachelors'

colnames(df)

# Scatterplot matrix exploring multi-faceted relationships
ggpairs(df, columns = 2:4, ggplot2::aes(colour= "id"))

# Plotting linear model
train %>% ggplot(aes(gross_rent, pct_adults_bachelors_or_higher)) +
  geom_point() +
  geom_smooth(method = "lm")

# First linear model
fit_1 <- lm(gross_rent ~ pct_adults_bachelors_or_higher, data = train)

# RMSE for first linear model
prediction <- predict(fit_1, test)
error <- prediction - test[["gross_rent"]]
sqrt(mean(error^2))

# Multiple regression model
fit_2 <- lm(gross_rent ~ pct_adults_bachelors_or_higher + pct_asian + pct_adults_with_high_school_diploma + death_rate_per_1k + pct_physical_inactivity, data = train)

# RMSE results for multiple regression model
prediction <- predict(fit_2, test)
error <- prediction - test[["gross_rent"]]
sqrt(mean(error^2))