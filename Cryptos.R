#################################################################################
#INTRODUCTION
#################################################################################
#The goal of the project is to determine if the price of Bitcoin (the oldest 
#cyrptocurrency whose market capitalization is the largest, influences the price
#of the next nine most important cryptocurrencies (in terms of market 
#capitalization).
#The dataset used consists of the data for the top 10 cryptocurrencies (in terms
#of market capitalization), which is available 
#on Yahoo Finance's website. Cryptocurrency data on Yahoo Finance includes the 
#following columns:
#symbol (the cryptocurrency's ticker symbo), date (the date to which prices 
#refer),
#open (the cryptocurrency's opening price), high (the cryptocurrency's high price 
#for the day), 
#low (the cryptocurrency's low price for the day), close (the cryptocurrency's 
#closing price for the day),
#volume (the cryptocurrency's trading volume for the day), adjusted (the 
#cryptocurrency's price for the day,
#which has been adjusted for any price splits the cryptocurrency's price may 
#have experienced).
#The starting date of the data is the date on which the youngest cryptocurrency
#of the top ten cryptocurrencies (in terms
#or market capitalization) was launched (Dec. 24, 2020)
#The ending date of the dataset is the date on which Bitcoin's price reached
#USD$100,000, that is, December 4, 2024 (Bitcoin's 
#reaching this milestone motivated the presenet analysis). 
#The analysis period goes thus from Dec. 24, 2020 through Dec. 4, 2024.

#The key steps performed are the following;
#Extract the data for all cryptocurrencies and combine them into one dataset
#Select the appropriate price column: the adjusted close price is chosen as 
#it accounts for any price spits
#the price of the cryptocurrency may have experienced; the "regular" closing 
#price  (column "close") is not used in this project;  therefore all references
#to cryptocurrecy price
#in this project refer to the adjusted close price.
#Conduct feature engineering by calculating the daily performance of each 
#cryptocurrency based on the adjusted closin price
#Analyse each cryptocurrency
#Determine the correlation between the daily performance of Bitcoin and
#the daily performance of each of the other nine cryptocurrencies
#Select the cryptocurrencies that have at least a moderate correlation with 
#Bitcoin (absolute value of correlation coefficient is greater than or equal
#to 0.50)
#Run simple linear regression models between Bitcoin and the currencies that
#have at leas a moderate correlation with Bitcoin
#Select the best simple linear regression model (the one with the lowest
#residual error)
#Perform feature engineering to create additional variables
#Run several multiple regression models using different variables as predictors
#Determine the best multiple regression model (the one with the lowest residual
#error)
#Use the predictors of the best multiple regression model and run an robust
#regression model
#Draw conclusions

#################################################################################
#METHODS / ANALYSIS
#################################################################################
#The process used consists of the following:
#Conduct feature engineering by calculating the daily performance of each
#cryptocurrency based on the adjusted closin price
#Analyse each cryptocurrency
#Determine the correlation between the daily performance of Bitcoin and the 
#daily performance of each of the other nine cryptocurrencies
#Select the cryptocurrencies that have at least a moderate correlation with
#Bitcoin (absolute value of correlation coefficient is greater than or equal 
#to 0.50)
#Run simple linear regression models between Bitcoin and the currencies that 
#have at leas a moderate correlation with Bitcoin
#Select the best simple linear regression model (the one with the lowest 
#residual standard error)
#Perform feature engineering to create additional variables
#Run several multiple regression models using different variables as predictors
#Determine the best multiple regression model (the one with the lowest residual 
#standard error)
#Use the predictors of the best multiple regression model and run an robust 
#regression model
#Perform cross validation using the robust regression model

#The techniques used include, mainly, correlation analysis, simple linear 
#regression, multiple linear regression and robust regression
#Techniques also include the use of the residual standard error, the Root 
#Mean Squared Error (RMSE) and the Mean Absolute Erro (MAE) to measure model
#performance
#Other techniques include feature enginering to calculate daily performance 
#and lagged variables based on daily performance
#And further techniques include include calculating summary statistics, 
#such as mean, standard deviation, minimum, and maximum of variables, as well 
#as data exploration, visualization and cleaning.

# Set working directory
setwd("C:/Users/client/Documents/Harvard/Capstone/Investments")

#Install necessary packages and load necessary libraries

install.packages("tidyverse")
library(tidyr)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("dplyr")
library(dplyr)

install.packages("tidyquant")
library(tidyquant)

install.packages("quantmod")
library(quantmod)

install.packages("conflicted")
library(conflicted)

install.packages("corrplot")
library(corrplot)

install.packages("e1071")
library(e1071)

install.packages("car")
library(car)

install.packages("lmtest")
library(lmtest)

install.packages("MASS")
library(MASS)

install.packages("caret")
library(caret)

#Get cryptocurrecy data from Yahoo Finance (first date: launch date of Bitcoin;
#last date: date at which Bitcoin reached USD$100,000)
#Note: the date ranges are to be narrowed below based on the earliest date for 
#which data is available on the youngest cryptocurrency
#such date is determined below

# Function to fetch cryptocurrency data from Yahoo Finance from date Bitcoin was
#launched to date Bitcoin reached USD$100,000
get_crypto_data <- function(ticker, start_date = "2009-01-03", end_date = "2024-12-04") {
  tryCatch({
    data <- tq_get(ticker, from = start_date, to = end_date)
    return(data)
  }, error = function(e) {
    message(paste("Error fetching data for", ticker, ":", e$message))
    return(NULL)
  })
}

#Select top 10 cryptocurrrencies in terms of capitalization (according to Yahoo Finance)
#and extract data from Yahoo Finanace:

# Define selected cryptocurrencies
selected_cryptos <- c("BTC-USD", "ETH-USD", "XRP-USD", "USDT-USD", "SOL-USD", 
                      "BNB-USD", "DOGE-USD", "ADA-USD", "USDC-USD", "STETH-USD")


#Install necessary packages if not already installed

install.packages("tidyquant")
library(tidyquant)

# Fetch data for each cryptocurrency
crypto_datasets <- lapply(selected_cryptos, get_crypto_data)
str(crypto_datasets)


#Perform feature engineering by Calculating the daily performance of cryptocurrrencies:

# Install the dplyr package 
install.packages("dplyr")

# Load the dplyr package if not already installed
library(dplyr)

# Function to calculate daily performance based on the adjusted closing price and

#ensure it's added correctly
calculate_daily_performance <- function(data) {
  if (!is.null(data)) {  # Check if data is not NULL before processing
    data <- data %>%
      mutate(
        daily_performance = (adjusted / dplyr::lag(adjusted) - 1) * 100  # Calculate 
        #daily percentage change
      ) %>%
      dplyr::filter(!is.na(daily_performance))  # Filter out NA daily performances
    
    return(data)
  } else {
    return(NULL)  # Return NULL if data is unavailable
  }
}

# Apply the daily performance calculation to each dataset
crypto_datasets_with_performance <- lapply(crypto_datasets, calculate_daily_performance)

# Verify the daily_performance column is added
head(crypto_datasets_with_performance[[1]])  # Bitcoin data
head(crypto_datasets_with_performance[[2]])  # Ethereum data
head(crypto_datasets_with_performance[[3]])  # XRP data

#Assign datasets to variables
Bitcoin_data <- crypto_datasets_with_performance[[1]] # Bitcoin data
head(Bitcoin_data)

Ethereum_data <- crypto_datasets_with_performance[[2]]
head(Ethereum_data)

XRP_data <- crypto_datasets_with_performance[[3]]
head(`XRP_data`)

Tether_data <- crypto_datasets_with_performance[[4]]
head(Tether_data)

Solana_data <- crypto_datasets_with_performance[[5]]
head(Solana_data)

BNB_data <- crypto_datasets_with_performance[[6]]
head(BNB_data)

Dogecoin_data <- crypto_datasets_with_performance[[7]]
Dogecoin_data 

Cardano_data <- crypto_datasets_with_performance[[8]]
head(Cardano_data)

USD_Coin_data <- crypto_datasets_with_performance[[9]]
head(USD_Coin_data)

Lido_Staked_ETH_data <- crypto_datasets_with_performance[[10]]
head(Lido_Staked_ETH_data)

#Determine starting date of analysis period as the earliest date for which there 
#is data on the youngest  of all 10 cryptocurrencies:

# Define the list of datasets
datasets <- list(
  Bitcoin_data, 
  Ethereum_data, 
  XRP_data, 
  Tether_data, 
  Solana_data, 
  BNB_data, 
  Dogecoin_data, 
  Cardano_data, 
  USD_Coin_data, 
  Lido_Staked_ETH_data
)


# Get the minimum date for each dataset
min_dates <- sapply(datasets, function(data) min(data$date, na.rm = TRUE))

# Convert the numeric date to Date format
min_dates <- as.Date(min_dates, origin = "1970-01-01")

# Define the list of datasets
datasets <- list(
  Bitcoin_data, 
  Ethereum_data, 
  XRP_data, 
  Tether_data, 
  Solana_data, 
  BNB_data, 
  Dogecoin_data, 
  Cardano_data, 
  USD_Coin_data, 
  Lido_Staked_ETH_data
)

# Create a vector of dataset names
dataset_names <- c(
  "Bitcoin", 
  "Ethereum", 
  "XRP", 
  "Tether", 
  "Solana", 
  "BNB", 
  "Dogecoin", 
  "Cardano", 
  "USD Coin", 
  "Lido Staked ETH"
)

# Get the minimum date for each dataset
min_dates <- sapply(datasets, function(data) min(data$date, na.rm = TRUE))

# Convert the numeric date to Date format
min_dates <- as.Date(min_dates, origin = "1970-01-01")

# Combine the results with dataset names
result <- data.frame(
  Dataset = dataset_names,
  Min_Date = min_dates
)

#Set the analysis period start date as the earliest date for which there is data 
#available on the youngest of all ten cryptocurrencies. 
analysis_period_start_date <- max(result$Min_Date, na.rm = TRUE)
print(analysis_period_start_date)
#Note: the start date of the analysis period is December 24, 2020.

#Verify data type of analysis period start date.
class(analysis_period_start_date)

#Set the end date of the analysis period as the date Bitcoin reached USD$100,000
analysis_period_end_date <- "2024-12-04"
analysis_period_end_date <- as.Date(analysis_period_end_date)
class(analysis_period_end_date)
analysis_period_end_date 
#The analysis period end date is 2024-12-04

#Therefore, the analysis period goes from 2020-12-24 to 2024-12-04

#Filter data so that it only contains dates in the analysis period:

# Define the date to filter against
filter_date <- analysis_period_start_date
filter_date

#Verify data type of filter date
class(filter_date)

#Filter all datasets so that they only include data of dates that are included 
#in the analysis period

Bitcoin_data <- Bitcoin_data %>%
  dplyr::filter(date >= filter_date)
Bitcoin_data

Ethereum_data <- Ethereum_data %>%
  dplyr::filter(date >= filter_date)
Ethereum_data

XRP_data <- XRP_data %>%
  dplyr::filter(date >= filter_date)
XRP_data

Tether_data <- Tether_data %>%
  dplyr::filter(date >= filter_date)
Tether_data

Solana_data <- Solana_data %>%
  dplyr::filter(date >= filter_date)
Solana_data

BNB_data <- BNB_data %>%
  dplyr::filter(date >= filter_date)
BNB_data

Dogecoin_data <- Dogecoin_data %>%
  dplyr::filter(date >= filter_date)
Dogecoin_data

Cardano_data <- Cardano_data %>%
  dplyr::filter(date >= filter_date)
Cardano_data

USD_Coin_data <- USD_Coin_data %>%
  dplyr::filter(date >= filter_date)
USD_Coin_data

Lido_Staked_ETH_data <- Lido_Staked_ETH_data %>%
  dplyr::filter(date >= filter_date)
Lido_Staked_ETH_data

#################################################################################

#Analyse cryptocurrencies 

#Note: analysis is based on cryptocurrencies' adjusted closing prices ("adjusted") 
#as opposed to
#the "regular" closing prices ("close") to take into account any price splits.  
#Analysis is also based
#on the daily performance, which is calculated based on the adjusted closing prices.

# Analyze Bitcoin:
# Print the summary statistics for Bitcoin_data
cat("\nSummary for Bitcoin:\n")
print(summary(Bitcoin_data))  # Bitcoin_data summary

# Extract min and max adjusted closing prices 
min_price_Bitcoin <- min(Bitcoin_data$adjusted, na.rm = TRUE)
max_price_Bitcoin <- max(Bitcoin_data$adjusted, na.rm = TRUE)
cat("Bitcoin price went from", min_price_Bitcoin, "to", max_price_Bitcoin, "during the analysis period.\n")
#Insights: 
#Bitcoin' price shows a very wide range of values.
#Bitcoin's price increased substantially during the analysis period

# Install ggplot2 if not already installed
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Create line chart showing evolution of adjusted price over time
ggplot(Bitcoin_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Bitcoin Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insights: 
#Bitcoin's adjusted price formed a distinct cup-with-handle pattern.
#The cup goes from late 2022 to early 2024.
#The handle goes from early 2024 to near the end of 2024.
#The breakout from the handle occurred near the end of 2024.
#Bitcoin's adjusted price is at the global high of the analysis period.

# Extract the initial and final adjusted prices
initial_price_Bitcoin <- Bitcoin_data$adjusted[1]  # First date (initial price)
final_price_Bitcoin <- Bitcoin_data$adjusted[nrow(Bitcoin_data)]  # Last date (final price)

# Calculate the total return
total_return_Bitcoin <- (final_price_Bitcoin - initial_price_Bitcoin) / initial_price_Bitcoin * 100

# Print the total return
cat("The total return for Bitcoin over the analysis period is", round(total_return_Bitcoin, 2), "%.\n")
#Insight: Bitcoin's total return for the analysis period exceeded 300%

# Get min and max values of daily performance
min_daily_performance_Bitcoin <- min(Bitcoin_data$daily_performance, na.rm = TRUE)
max_daily_performance_Bitcoin <- max(Bitcoin_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Bitcoin was", round(min_daily_performance_Bitcoin, 2), "%.\n")
cat("The maximum daily performance for Bitcoin was", round(max_daily_performance_Bitcoin, 2), "%.\n")
#Insight: Bitcoin's daily performance shows a wide range of over 30 percentage points

# Get the median value of daily performance
median_daily_performance_Bitcoin <- median(Bitcoin_data$daily_performance, na.rm = TRUE)
median_daily_performance_Bitcoin
#Insight: Bitcoin's median daily performance is around zero

# Get the average value of daily performance
average_daily_performance_Bitcoin <- mean(Bitcoin_data$daily_performance, na.rm = TRUE)
average_daily_performance_Bitcoin
#Insigth: Bitcoin's average performance is around zero

# Get the standard deviation value of daily performance
sd_daily_performance_Bitcoin <- sd(Bitcoin_data$daily_performance, na.rm = TRUE)
sd_daily_performance_Bitcoin
#Insight: the standard deviation of Bitcoin's daily performance is over 3 percentage points

# Create line chart showing daily performance over time
ggplot(Bitcoin_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Bitcoin Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights: 
#Bitcoin's daily performance can have substantial volatility
#Bitcoin's daily performance shows stationarity; that is, daily performance fluctuates 
#around zero during the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze Ethereum:

# Print the summary statistics for Ethereum_data
cat("\nSummary for Ethereum:\n")
print(summary(Ethereum_data))  # Ethereum_data summary

# Extract min and max adjusted closing prices
min_price_Ethereum <- min(Ethereum_data$adjusted, na.rm = TRUE)
max_price_Ethereum <- max(Ethereum_data$adjusted, na.rm = TRUE)
cat("Ethereum price went from", min_price_Ethereum, "to", max_price_Ethereum, "during the analysis period.\n")
#Insights:
#Ethereum's price shows a  side range of values.
#Ethereum's price increased impressively during the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(Ethereum_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Ethereum Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insight: Ethereum's price reached a peak in late 2021.  Even though the price has 
#recovered from its low of late 2022, it is still below its peak of late 2021.

# Extract the initial and final adjusted prices
initial_price_Ethereum <- Ethereum_data$adjusted[1]  # First date (initial price)
final_price_Ethereum <- Ethereum_data$adjusted[nrow(Ethereum_data)]  # Last date (final price)


# Calculate the total return
total_return_Ethereum <- (final_price_Ethereum - initial_price_Ethereum) / initial_price_Ethereum * 100

# Print the total return
cat("The total return for Ethereum over the analysis period is", round(total_return_Ethereum, 2), "%.\n")
#Insight: Ethereum's return over the analysis period exceedes an impressive 500%

# Get min and max values of daily performance
min_daily_performance_Ethereum <- min(Ethereum_data$daily_performance, na.rm = TRUE)
max_daily_performance_Ethereum <- max(Ethereum_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Ethereum was", round(min_daily_performance_Ethereum, 2), "%.\n")
cat("The maximum daily performance for Ethereum was", round(max_daily_performance_Ethereum, 2), "%.\n")
#Insight: Ethereum's daily performance shows a wide range of over 50 percentage points

# Get the median value of daily performance
median_daily_performance_Ethereum <- median(Ethereum_data$daily_performance, na.rm = TRUE)
median_daily_performance_Ethereum
#The median value of Ethereum's daily performance is around zero

#Get the average value of daily performance
average_daily_performance_Ethereum <- mean(Ethereum_data$daily_performance, na.rm = TRUE)
average_daily_performance_Ethereum
#The average value of Ethereum's daily performance is around zero

#Get the standard deviation value of daily performance
sd_daily_performance_Ethereum <- sd(Ethereum_data$daily_performance, na.rm = TRUE)
sd_daily_performance_Ethereum
#The standard deviation of Ethereum's daily performance exceedss 4 percentage points

# Create line chart showing daily performance over time
ggplot(Ethereum_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Ethereum Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights: 
#Ethereum's daily performance shows significant volatility
#Ethereum's daily performance shows stationarity; that is, daily performance fluctuates 
#around zero during the analysis period
#and there is no upward or downward trend of the daily performance.


# Analyze XRP:

# Print the summary statistics for XRP_data
cat("\nSummary for XRP:\n")
print(summary(XRP_data))  # XRP_data summary

# Extract min and max adjusted closing prices
min_price_XRP <- min(XRP_data$adjusted, na.rm = TRUE)
max_price_XRP <- max(XRP_data$adjusted, na.rm = TRUE)
cat("XRP price went from", min_price_XRP, "to", max_price_XRP, "during the analysis period.\n")
#Insights: 
#XRP's prices shows a relatively wide range of values in relative terms.
#XRP's price increased substantially during the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(XRP_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "XRP Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insights: 
#XRP's price reached a peak in 2021.  
#XRP's price also shows a very sharp increase in late 2024, which allowed it to exceed its 2021 peak.


# Extract the initial and final adjusted prices
initial_price_XRP <- XRP_data$adjusted[1]  # First date (initial price)
final_price_XRP <- XRP_data$adjusted[nrow(XRP_data)]  # Last date (final price)

# Calculate the total return
total_return_XRP <- (final_price_XRP - initial_price_XRP) / initial_price_XRP * 100

# Print the total return
cat("The total return for XRP over the analysis period is", round(total_return_XRP, 2), "%.\n")
#Insight: XRP's return over the analysis period exceeded 500%! 


# Get min and max values of daily performance
min_daily_performance_XRP <- min(XRP_data$daily_performance, na.rm = TRUE)
max_daily_performance_XRP <- max(XRP_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for XRP was", round(min_daily_performance_XRP, 2), "%.\n")
cat("The maximum daily performance for XRP was", round(max_daily_performance_XRP, 2), "%.\n")
#Insights: 
#XRP's daily performance shows a very wide range of over 100 percentage points
#XRP'x price went up by over 70% in a single day during the analysis period

# Get the median value of daily performance
median_daily_performance_XRP <- median(XRP_data$daily_performance, na.rm = TRUE)
median_daily_performance_XRP
#Insight: The median value of XRP's daily performance is around zero

# Get the average value of daily performance
average_daily_performance_XRP <- mean(XRP_data$daily_performance, na.rm = TRUE)
average_daily_performance_XRP
#Insight: The average value of XRP's daily performance is around zero

# Get the standard deviation value of daily performance
sd_daily_performance_XRP <- sd(XRP_data$daily_performance, na.rm = TRUE)
sd_daily_performance_XRP
#The standard deviation value of XRP exceeds 5.5 percentage points;
#thus XRP's daily performance is substantially volatile.

# Create line chart showing daily performance over time
ggplot(XRP_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "XRP Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights: 
#XRP's daily performance shows substantial volatility.
#XRP's daily performance shows stationarity; that is, daily performance fluctuates around
#zero during the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze Tether:

# Print the summary statistics for Tether_data
cat("\nSummary for Tether:\n")
print(summary(Tether_data))  # Tether_data summary

# Extract min and max adjusted closing prices
min_price_Tether <- min(Tether_data$adjusted, na.rm = TRUE)
max_price_Tether <- max(Tether_data$adjusted, na.rm = TRUE)
cat("Tether price went from", min_price_Tether, "to", max_price_Tether, "during the analysis period.\n")
#Insights: 
#Tether's price shows a narrow range
#Tether's price was essentially the same at the beginning and at the end of the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(Tether_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Tether Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insight: Although there appears to be sustantial volatility in Tether's price over the analysis period,
#such volatily is low in absolute terms. 

# Extract the initial and final adjusted prices
initial_price_Tether <- Tether_data$adjusted[1]  # First date (initial price)
final_price_Tether <- Tether_data$adjusted[nrow(Tether_data)]  # Last date (final price)

# Calculate the total return
total_return_Tether <- (final_price_Tether - initial_price_Tether) / initial_price_Tether * 100

# Print the total return
cat("The total return for Tether over the analysis period is", round(total_return_Tether, 2), "%.\n")
#Insight: Tether's total return was essentially nil over the analysis period

# Get min and max values of daily performance
min_daily_performance_Tether <- min(Tether_data$daily_performance, na.rm = TRUE)
max_daily_performance_Tether <- max(Tether_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Tether was", round(min_daily_performance_Tether, 2), "%.\n")
cat("The maximum daily performance for Tether was", round(max_daily_performance_Tether, 2), "%.\n")
#Insight: The range of Tether's daily performance is narrow.

# Get the median value of daily performance
median_daily_performance_Tether <- median(Tether_data$daily_performance, na.rm = TRUE)
median_daily_performance_Tether
#Insight: The median value of Tether's daily performance is around zero

# Get the average value of daily performance
average_daily_performance_Tether <- mean(Tether_data$daily_performance, na.rm = TRUE)
average_daily_performance_Tether
#Insight: The average value of Tether's daily performance is around zero

# Get the standard deviation value of daily performance
sd_daily_performance_Tether <- sd(Tether_data$daily_performance, na.rm = TRUE)
sd_daily_performance_Tether
#Insight: The standard deviation of Tether's daily performance is essentially zero.

# Create line chart showing daily performance over time
ggplot(Tether_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Tether Daily Performance", x = "Date", y = "Daily Performance (%)")
#Although there appears to be wide volatility in Tether's daily performance over the analysis period,
#such volatility is low in absolute terms.
#Insight: Tether's daily performance shows stationarity; that is, daily performance fluctuates 
#around zero during the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze Solana:

# Print the summary statistics for Solana_data
cat("\nSummary for Solana:\n")
print(summary(Solana_data))  # Solana_data summary

# Extract min and max adjusted closing prices
min_price_Solana <- min(Solana_data$adjusted, na.rm = TRUE)
max_price_Solana <- max(Solana_data$adjusted, na.rm = TRUE)
cat("Solana price went from", min_price_Solana, "to", max_price_Solana, "during the analysis period.\n")
#Insight: Solana's price shows a very wide range of values
#Insight: Solana's price increased very substantially during the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(Solana_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Solana Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Solana's price reached a peak in late 2022.
#Although the price has recovered from the low of late 2023, it is still below its late 2022 peak.

# Extract the initial and final adjusted prices
initial_price_Solana <- Solana_data$adjusted[1]  # First date (initial price)
final_price_Solana <- Solana_data$adjusted[nrow(Solana_data)]  # Last date (final price)

# Calculate the total return
total_return_Solana <- (final_price_Solana - initial_price_Solana) / initial_price_Solana * 100

# Print the total return
cat("The total return for Solana over the analysis period is", round(total_return_Solana, 2), "%.\n")
#Insight: Solana's return over the analysis period exceeded a most impressive 16,000 % !

# Get min and max values of daily performance
min_daily_performance_Solana <- min(Solana_data$daily_performance, na.rm = TRUE)
max_daily_performance_Solana <- max(Solana_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Solana was", round(min_daily_performance_Solana, 2), "%.\n")
cat("The maximum daily performance for Solana was", round(max_daily_performance_Solana, 2), "%.\n")
#Insights:
#Solana's daily performance shows a very wide range of over 75 percentage points
#Solana's stock price increase by over 35% in a single day during the analysis period
#Solana's stock price dropped by over 42% in a single day during the analysis period

# Get the median value of daily performance
median_daily_performance_Solana <- median(Solana_data$daily_performance, na.rm = TRUE)
median_daily_performance_Solana
#Insight: The median value of Solana's daily performance is close to zero

# Get the average value of daily performance
average_daily_performance_Solana <- mean(Solana_data$daily_performance, na.rm = TRUE)
average_daily_performance_Solana
#Insight: The average value of Solana's daily performance is around zero

# Get the standard deviation value of daily performance
sd_daily_performance_Solana <- sd(Solana_data$daily_performance, na.rm = TRUE)
sd_daily_performance_Solana
#The standard deviation of Solana's daily performance exceedes 6 percentage points;
#thus, Solana's daily performance is quite volatile.

# Create line chart showing daily performance over time
ggplot(Solana_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Solana Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights:
#Solana's daily performance shows substantial volatility
#Solana's daily performance shows stationarity; that is, daily performance fluctuates around zero
#during the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze BNB:

# Print the summary statistics for BNB_data
cat("\nSummary for BNB:\n")
print(summary(BNB_data))  # BNB_data summary

# Extract min and max adjusted closing prices
min_price_BNB <- min(BNB_data$adjusted, na.rm = TRUE)
max_price_BNB <- max(BNB_data$adjusted, na.rm = TRUE)
cat("BNB price went from", min_price_BNB, "to", max_price_BNB, "during the analysis period.\n")
#Insight: BNB's price shows a very wide range of values.
#Insight: BNB's price increased very substantially during the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(BNB_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "BNB Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#BNB's price increased substantially during the analysis period.
#BNB's price is at the global high of the analysis period.

# Extract the initial and final adjusted prices
initial_price_BNB <- BNB_data$adjusted[1]  # First date (initial price)
final_price_BNB <- BNB_data$adjusted[nrow(BNB_data)]  # Last date (final price)

# Calculate the total return
total_return_BNB <- (final_price_BNB - initial_price_BNB) / initial_price_BNB * 100

# Print the total return
cat("The total return for BNB over the analysis period is", round(total_return_BNB, 2), "%.\n")
#BNN's total return over the analysis period exceeded an impressive 2,000 % !

# Get min and max values of daily performance
min_daily_performance_BNB <- min(BNB_data$daily_performance, na.rm = TRUE)
max_daily_performance_BNB <- max(BNB_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for BNB was", round(min_daily_performance_BNB, 2), "%.\n")
cat("The maximum daily performance for BNB was", round(max_daily_performance_BNB, 2), "%.\n")
#Insight: BNB's price shows a wide range of over 100 percentage points.
#Insight: BNB's price increased more than 69% in a single day during the analysis period!

# Create line chart showing daily performance over time
ggplot(BNB_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "BNB Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insight:
# BNN's daily performance shows substantial volatility.
#BNN's daily performance shows sationarity; that is, daily performance fluctuates around zero during
#the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze Dogecoin:

# Print the summary statistics for Dogecoin_data
cat("\nSummary for Dogecoin:\n")
print(summary(Dogecoin_data))  # Dogecoin_data summary

# Extract min and max adjusted closing prices
min_price_Dogecoin <- min(Dogecoin_data$adjusted, na.rm = TRUE)
max_price_Dogecoin <- max(Dogecoin_data$adjusted, na.rm = TRUE)
cat("Dogecoin price went from", min_price_Dogecoin, "to", max_price_Dogecoin, "during the analysis period.\n")
#Dogecoin's price has a wide range of values in relative terms.
#Dogecoin's price increased substantially during the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(Dogecoin_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Dogecoin Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insight: Dogecoin's price reached a peak in 2021.  Even though, Dogecoin's price
#shows a sharp upward move in 2024, it is still below its 2021 peak.

# Extract the initial and final adjusted prices
initial_price_Dogecoin <- Dogecoin_data$adjusted[1]  # First date (initial price)
final_price_Dogecoin <- Dogecoin_data$adjusted[nrow(Dogecoin_data)]  # Last date (final price)


# Calculate the total return
total_return_Dogecoin <- (final_price_Dogecoin - initial_price_Dogecoin) / initial_price_Dogecoin * 100

# Print the total return
cat("The total return for Dogecoin over the analysis period is", round(total_return_Dogecoin, 2), "%.\n")
#Insight: Dogecoin's return for the analysis period exceeded a very impressive 9,000 %!  

# Get min and max values of daily performance
min_daily_performance_Dogecoin <- min(Dogecoin_data$daily_performance, na.rm = TRUE)
max_daily_performance_Dogecoin <- max(Dogecoin_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Dogecoin was", round(min_daily_performance_Dogecoin, 2), "%.\n")
cat("The maximum daily performance for Dogecoin was", round(max_daily_performance_Dogecoin, 2), "%.\n")
#Insights:
#Dogecoion's daily performance has a very wide range of values of over 395 percentage points.
#Dogecoion's price dropped by over 40% in a single day during the analysis period.
#Dogecoion's price increase a most impressive 355.55 % in a single day during the analysis period!

# Get the median value of daily performance
median_daily_performance_Dogecoin <- median(Dogecoin_data$daily_performance, na.rm = TRUE)
median_daily_performance_Dogecoin
#Insight: the median value of Dogecoin's daily performance is close zero.

# Get the average value of daily performance
average_daily_performance_Dogecoin <- mean(Dogecoin_data$daily_performance, na.rm = TRUE)
average_daily_performance_Dogecoin
#Insight: the average value of Dogecoin's daily performance is around zero.

# Get the standard deviation value of daily performance
sd_daily_performance_Dogecoin <- sd(Dogecoin_data$daily_performance, na.rm = TRUE)
sd_daily_performance_Dogecoin
#Insight:  The standard deviation of Dogecoin's daily performance exceeds 11 percentage poins;
#thus, Dogecoin's daily performance is highly volatile.

# Create line chart showing daily performance over time
ggplot(Dogecoin_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Dogecoin Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights:
#Dogecoin's daily performance shows sharp volatility.
#Dogecoin's daily performance shows stationarity; that is, daily performance fluctuates around 
#zero during the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze Cardano:

# Print the summary statistics for Cardano_data
cat("\nSummary for Cardano:\n")
print(summary(Cardano_data))  # Cardano_data summary

# Extract min and max adjusted closing prices
min_price_Cardano <- min(Cardano_data$adjusted, na.rm = TRUE)
max_price_Cardano <- max(Cardano_data$adjusted, na.rm = TRUE)
cat("Cardano price went from", min_price_Cardano, "to", max_price_Cardano, "during the analysis period.\n")
#Insights:
#Although Cardano's price shows narrow range of values in absolute terms, it shows a wide range 
#of values in relative terms.
#Cardano's price increased very substantially during the analysis period in relative terms.


# Create line chart showing evolution of adjusted price over time
ggplot(Cardano_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Cardano Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insigth: Cardano reached a peak in 2021.  Even though the price shows a sharp increae in 2024,
#it is still well below its 2021 peak.

# Extract the initial and final adjusted prices
initial_price_Cardano <- Cardano_data$adjusted[1]  # First date (initial price)
final_price_Cardano <- Cardano_data$adjusted[nrow(Cardano_data)]  # Last date (final price)

# Calculate the total return
total_return_Cardano <- (final_price_Cardano - initial_price_Cardano) / initial_price_Cardano * 100

# Print the total return
cat("The total return for Cardano over the analysis period is", round(total_return_Cardano, 2), "%.\n")
#Insight: Cardano's return over the analysis period exceeded an impressive 600%!

# Get min and max values of daily performance
min_daily_performance_Cardano <- min(Cardano_data$daily_performance, na.rm = TRUE)
max_daily_performance_Cardano <- max(Cardano_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Cardano was", round(min_daily_performance_Cardano, 2), "%.\n")
cat("The maximum daily performance for Cardano was", round(max_daily_performance_Cardano, 2), "%.\n")
#Cardano's daily performance shows a wide range of over 58%


# Create line chart showing daily performance over time
ggplot(Cardano_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Cardano Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights:
#Cardano's daily performance shows high volatility.
#Cardano's daily performance shows stationarity; that is, daily performance fluctuates around zero
#during the analysis period
#and there is no upward or downward trend of the daily performance.

# Analyze USD Coin:

# Print the summary statistics for USD_Coin_data
cat("\nSummary for USD Coin:\n")
print(summary(USD_Coin_data))  # USD_Coin_data summary

# Extract min and max adjusted closing prices
min_price_USD_Coin <- min(USD_Coin_data$adjusted, na.rm = TRUE)
max_price_USD_Coin <- max(USD_Coin_data$adjusted, na.rm = TRUE)
cat("USD Coin price went from", min_price_USD_Coin, "to", max_price_USD_Coin, "during the analysis period.\n")
#Insights:
#USD coin's price shows a narrow range of values.
#USD coin's price  was essentially the same at the beginning and at the end of the analysis period

# Create line chart showing evolution of adjusted price over time
ggplot(USD_Coin_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "USD Coin Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insight: Although USD Coin's price shows what appears to be sharp volatility, such volatility is
#small in absolute terms.

# Extract the initial and final adjusted prices
initial_price_USD_Coin <- USD_Coin_data$adjusted[1]  # First date (initial price)
final_price_USD_Coin <- USD_Coin_data$adjusted[nrow(USD_Coin_data)]  # Last date (final price)

# Calculate the total return
total_return_USD_Coin <- (final_price_USD_Coin - initial_price_USD_Coin) / initial_price_USD_Coin * 100

# Print the total return
cat("The total return for USD Coin over the analysis period is", round(total_return_USD_Coin, 2), "%.\n")
#Insight: USD Coin's total return for the analysis period was essentially nil.

# Get min and max values of daily performance
min_daily_performance_USD_Coin <- min(USD_Coin_data$daily_performance, na.rm = TRUE)
max_daily_performance_USD_Coin <- max(USD_Coin_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for USD Coin was", round(min_daily_performance_USD_Coin, 2), "%.\n")
cat("The maximum daily performance for USD Coin was", round(max_daily_performance_USD_Coin, 2), "%.\n")
#Insight: USD Coin's daily performance shows a relatively narrow range.

# Get the median value of daily performance
median_daily_performance_USD_Coin <- median(USD_Coin_data$daily_performance, na.rm = TRUE)
median_daily_performance_USD_Coin
#Insight: the median value of USD Coin's daily performance is essentially zero

# Get the average value of daily performance
average_daily_performance_USD_Coin <- mean(USD_Coin_data$daily_performance, na.rm = TRUE)
average_daily_performance_USD_Coin
#Insight: the average value of USD Coin's daily performance is essentially zero

# Get the standard deviation value of daily performance
sd_daily_performance_USD_Coin <- sd(USD_Coin_data$daily_performance, na.rm = TRUE)
sd_daily_performance_USD_Coin
#Insight: the standard deviation value of USD Coin's daily performance is close to zero.

# Create line chart showing daily performance over time
ggplot(USD_Coin_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "USD Coin Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights:
#Although USD Coin's daily performance shows relatively high volatility in isolated sub-periods
#of the analysis period, in general, USD Coin's daily performace shows little volatiliry over
#the analysis period.
#USD Coin's daily performance shows stationarity; that is, daily performance fluctuates around
#zero during the analysis period
#and there is no upward or downward trend of the daily performance.


# Analyze Lido Staked ETH:

# Print the summary statistics for Lido_Staked_ETH_data
cat("\nSummary for Lido Staked ETH:\n")
print(summary(Lido_Staked_ETH_data))  # Lido_Staked_ETH_data summary

# Extract min and max adjusted closing prices
min_price_Lido_Staked_ETH <- min(Lido_Staked_ETH_data$adjusted, na.rm = TRUE)
max_price_Lido_Staked_ETH <- max(Lido_Staked_ETH_data$adjusted, na.rm = TRUE)
cat("Lido Staked ETH price went from", min_price_Lido_Staked_ETH, "to", max_price_Lido_Staked_ETH, "during the analysis period.\n")
#Insights:
#Lido Staked ETH's price shows a wide range of values
#Lido Staked ETH's price increased substantially during the analysis period.

# Create line chart showing evolution of adjusted price over time
ggplot(Lido_Staked_ETH_data, aes(x = date, y = adjusted)) + 
  geom_line(color = "blue", size = 1) +  # Line with blue color and thickness of 1
  labs(title = "Lido Staked ETH Adjusted Close Price Over Time", 
       x = "Date", 
       y = "Adjusted Close Price (USD)") + 
  theme_minimal() +  # A clean theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability
#Insight: Lido Staked ETH's price peaked in late 2021 and bottomed in late 2022.
#Even though  Lido Staked ETH's price has recovered from its late 2022 bottom and shows a sharp increase
#in 2024,
#it is still below its 2021 peak.

# Extract the initial and final adjusted prices
initial_price_Lido_Staked_ETH <- Lido_Staked_ETH_data$adjusted[1]  # First date (initial price)
final_price_Lido_Staked_ETH <- Lido_Staked_ETH_data$adjusted[nrow(Lido_Staked_ETH_data)]  # Last date (final price)

# Calculate the total return
total_return_Lido_Staked_ETH <- (final_price_Lido_Staked_ETH - initial_price_Lido_Staked_ETH) / initial_price_Lido_Staked_ETH * 100

# Print the total return
cat("The total return for Lido Staked ETH over the analysis period is", round(total_return_Lido_Staked_ETH, 2), "%.\n")
#Insight:  Lido Staked ETH's return for the analysis period exceeded a quite impressive 500%.

# Get min and max values of daily performance
min_daily_performance_Lido_Staked_ETH <- min(Lido_Staked_ETH_data$daily_performance, na.rm = TRUE)
max_daily_performance_Lido_Staked_ETH <- max(Lido_Staked_ETH_data$daily_performance, na.rm = TRUE)

# Print min and max daily performance
cat("The minimum daily performance for Lido Staked ETH was", round(min_daily_performance_Lido_Staked_ETH, 2), "%.\n")
cat("The maximum daily performance for Lido Staked ETH was", round(max_daily_performance_Lido_Staked_ETH, 2), "%.\n")
#Insights:
#Lido Staked ETH's daily performance shows a wide range of over 50 percentage points.

# Get the median value of daily performance
median_daily_performance_Lido_Staked_ETH <- median(Lido_Staked_ETH_data$daily_performance, na.rm = TRUE)
median_daily_performance_Lido_Staked_ETH
#Insight: the median of Lido Staked ETH's daily performance is close to zero.

# Get the average value of daily performance
average_daily_performance_Lido_Staked_ETH <- mean(Lido_Staked_ETH_data$daily_performance, na.rm = TRUE)
average_daily_performance_Lido_Staked_ETH
#Insight: the average of Lido Staked ETH's daily performance is close to zero.

# Get the standard deviation value of daily performance
sd_daily_performance_Lido_Staked_ETH <- sd(Lido_Staked_ETH_data$daily_performance, na.rm = TRUE)
sd_daily_performance_Lido_Staked_ETH
#Insight: the standard deviation of Lido Staked ETH's daily performance exceeds 4 percentage points;
#thus, Lido Staked ETH's daily performance is quite volatile.

# Create line chart showing daily performance over time
ggplot(Lido_Staked_ETH_data, aes(x = date, y = daily_performance)) +
  geom_line(color = "blue") +
  labs(title = "Lido Staked ETH Daily Performance", x = "Date", y = "Daily Performance (%)")
#Insights: 
#Lido Staked ETH's daily performance shows substantial volatility.
#Lido Staked ETH's daily performance shows stationarity; that is, daily performance fluctuates 
#around zero during the analysis period
#and there is no upward or downward trend of the daily performance. 

############################################    
#Analyse correlation among cryptocurrencies
############################################

#Create correlation matrix: 

# Extract the daily_performance for each cryptocurrency and align them by date
Bitcoin_data <- Bitcoin_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Bitcoin")
Ethereum_data <- Ethereum_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Ethereum")
XRP_data <- XRP_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "XRP")
Tether_data <- Tether_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Tether")
Solana_data <- Solana_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Solana")
BNB_data <- BNB_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "BNB")
Dogecoin_data <- Dogecoin_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Dogecoin")
Cardano_data <- Cardano_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Cardano")
USD_Coin_data <- USD_Coin_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "USD Coin")
Lido_Staked_ETH_data <- Lido_Staked_ETH_data %>% dplyr::select(date, daily_performance) %>% mutate(crypto = "Lido Staked ETH")


#View data:
Bitcoin_data
Ethereum_data
XRP_data
Tether_data
Solana_data
BNB_data
Dogecoin_data
Cardano_data
USD_Coin_data
Lido_Staked_ETH_data

# Combine the datasets into one data frame
combined_data <- bind_rows(Bitcoin_data, Ethereum_data, XRP_data, Tether_data, Solana_data, 
                           BNB_data, Dogecoin_data, Cardano_data, USD_Coin_data, Lido_Staked_ETH_data)
#View data
combined_data

#Install necessary packages if not already installed
install.packages("tidyr") 
library(tidyr)

# Reshape the data to have one column per cryptocurrency's daily_performance
performance_wide <- combined_data %>%
  spread(key = crypto, value = daily_performance)

#view data
performance_wide

# Compute the correlation matrix
cor_matrix <- cor(performance_wide[,-1], use = "complete.obs")  # Exclude the date column

#View the correlation matrix
cor_matrix

# Install the corrplot package if not already installed
install.packages("corrplot")

# Load the corrplot package
library(corrplot)

# Visualize the correlation matrix using corrplot
corrplot(cor_matrix, method = "circle", type = "upper", 
         title = "Correlation of Daily Performance Across Cryptocurrencies",
         tl.cex = 0.8)  # Adjust text size if needed
#################################################################################

#Run regression models between Bitcoin's daily performance and the daily
#performance of cryptocurrencies for which
#the absolute value of the correlation coefficient with Bitcoin is geater than or 
#equal to 0.50 in order to determine
#if the daily performance of Bitcoin explains and/or predicts the daily 
##performance of the other cryptocurrencies.

#################################################################################
#Bitcoin's daily performance has a high positive correlation (0.80718990) with 
#Ethereum's daily performnace 

#Create scatter plot of Bitcoin's daily performance vs Ethereum's daily performnace:

# Filter the combined data to get only Bitcoin and Ethereum's daily performance
bitcoin_ethereum_data <- combined_data %>%
  dplyr::filter(crypto %in% c("Bitcoin", "Ethereum")) %>%
  dplyr::select(date, crypto, daily_performance)

# Reshape the data to wide format for scatter plot
bitcoin_ethereum_performance_wide <- bitcoin_ethereum_data %>%
  spread(key = crypto, value = daily_performance)

# Install ggplot2 if it's not already installed
install.packages("ggplot2")

# Load the ggplot2 package
library(ggplot2)

# Scatter plot of Bitcoin vs Ethereum's daily performance
ggplot(bitcoin_ethereum_performance_wide, aes(x = Bitcoin, y = Ethereum)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear regression line
  labs(title = "Scatter Plot: Bitcoin vs Ethereum Daily Performance",
       x = "Bitcoin Daily Performance",
       y = "Ethereum Daily Performance") +
  theme_minimal()  # Clean theme
#Insight: Bitcoin's daily performance and Ethereum's daily performance appear to be 
#linearly correlated.

# Visualizing Bitcoin and Ethereum with boxplots to detect outliers
ggplot(bitcoin_ethereum_performance_wide, aes(x = "Bitcoin", y = Bitcoin)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bitcoin's Daily Performance", x = "Bitcoin", y = "Daily Performance") +
  theme_minimal()

ggplot(bitcoin_ethereum_performance_wide, aes(x = "Ethereum", y = Ethereum)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Ethereum's Daily Performance", x = "Ethereum", y = "Daily Performance") +
  theme_minimal()

#Remove outliers:

# Calculate the Interquartile Range (IQR) for Bitcoin and Ethereum
iqr_bitcoin <- IQR(bitcoin_ethereum_performance_wide$Bitcoin)
iqr_ethereum <- IQR(bitcoin_ethereum_performance_wide$Ethereum)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_ethereum_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_ethereum_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_ethereum <- quantile(bitcoin_ethereum_performance_wide$Ethereum, 0.25) - 1.5 * iqr_ethereum
upper_ethereum <- quantile(bitcoin_ethereum_performance_wide$Ethereum, 0.75) + 1.5 * iqr_ethereum

# Install dplyr (if not already installed)
install.packages("dplyr")

# Load the dplyr package
library(dplyr)

# Remove outliers from Bitcoin and Ethereum columns
bitcoin_ethereum_performance_wide_no_outliers <- bitcoin_ethereum_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(Ethereum >= lower_ethereum & Ethereum <= upper_ethereum)

#View the data
bitcoin_ethereum_performance_wide_no_outliers

# Verify the number of rows before and after removing outliers
nrow(bitcoin_ethereum_performance_wide)  # Before removing outliers
nrow(bitcoin_ethereum_performance_wide_no_outliers)  # After removing outliers


# Run a simple linear regression: Bitcoin's performance (independent) vs. Ethereum's 
#performance (dependent)
lm_model_Bitcoin_Ethereum <- lm(Ethereum ~ Bitcoin, data = bitcoin_ethereum_performance_wide_no_outliers)

# View the summary of the linear regression model
summary(lm_model_Bitcoin_Ethereum)

#Insights:
#Bitcoin's daily performance is a statistically significant predictor of Ethereum's daily 
#performance, with a coefficient of 0.96090.
#This means that for every 1 percentage increase in Bitcoin’s performance, Ethereum’s 
#performance is expected to increase by about 0.96 percentage points. 
#The R-squared suggests that Bitcoin explains about 55% of the variance in Ethereum’s 
#performance, which is a moderate fit.
#The residual standard error is 1.909, indicating that the model's predictions on 
#average are off by about 1.91 percentage points.
#The model as a whole is highly significant, with an extremely low p-value for the
#F-statistic.

#####################################################

#Bitcoin's daily performance also has a high positive correlation (0.79221309)  with 
#the daily performance of Lido Staked ETH

# Filter the combined data to get Bitcoin and Lido Staked ETH daily performance
bitcoin_lido_data <- combined_data %>%
  dplyr::filter(crypto %in% c("Bitcoin", "Lido Staked ETH")) %>%
  dplyr::select(date, crypto, daily_performance)

# Load the tidyr package
library(tidyr)

# Reshape the data to wide format
bitcoin_lido_performance_wide <- bitcoin_lido_data %>%
  spread(key = crypto, value = daily_performance)

# Install and load the ggplot2 package if not already installed
install.packages("ggplot2")
library(ggplot2)

# Scatter plot with the regression line
ggplot(bitcoin_lido_performance_wide, aes(x = Bitcoin, y = `Lido Staked ETH`)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear regression line
  labs(title = "Scatter Plot: Bitcoin vs Lido Staked ETH Daily Performance",
       x = "Bitcoin Daily Performance",
       y = "Lido Staked ETH Daily Performance") +
  theme_minimal()  # Clean theme
#Insight: Bitcoin's daily performance and Ethereum's daily performance appear to be
#linearly correlated.

# Visualizing Bitcoin and Lido Staked ETH with boxplots to detect outliers
ggplot(bitcoin_lido_performance_wide, aes(x = "Bitcoin", y = Bitcoin)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bitcoin's Daily Performance", x = "Bitcoin", y = "Daily Performance") +
  theme_minimal()

ggplot(bitcoin_lido_performance_wide, aes(x = "Lido Staked ETH", y = `Lido Staked ETH`)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Lido Staked ETH's Daily Performance", x = "Lido Staked ETH", y = "Daily Performance") +
  theme_minimal()


# Calculate the IQR for Bitcoin and Lido Staked ETH
iqr_bitcoin <- IQR(bitcoin_lido_performance_wide$Bitcoin)
iqr_lido <- IQR(bitcoin_lido_performance_wide$`Lido Staked ETH`)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_lido_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_lido_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_lido <- quantile(bitcoin_lido_performance_wide$`Lido Staked ETH`, 0.25) - 1.5 * iqr_lido
upper_lido <- quantile(bitcoin_lido_performance_wide$`Lido Staked ETH`, 0.75) + 1.5 * iqr_lido

# Remove outliers from Bitcoin and Lido Staked ETH columns
bitcoin_lido_performance_wide_no_outliers <- bitcoin_lido_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(`Lido Staked ETH` >= lower_lido & `Lido Staked ETH` <= upper_lido)

# Show data after removing outliers
bitcoin_lido_performance_wide_no_outliers

# Verify the number of rows before and after removing outliers
nrow(bitcoin_lido_performance_wide)  # Before removing outliers
nrow(bitcoin_lido_performance_wide_no_outliers)  # After removing outliers

# Run a simple linear regression: Bitcoin's performance (independent) vs. Lido Staked
#ETH's performance (dependent)
lm_model_Bitcoin_Lido <- lm(`Lido Staked ETH` ~ Bitcoin, data = bitcoin_lido_performance_wide_no_outliers)

# View the summary of the linear regression model
summary(lm_model_Bitcoin_Lido)

#Insights:
#Bitcoin's performance is a strong predictor of Lido Staked ETH's performance.
#The coefficient for Bitcoin (0.90033) means that for each percentage point 
#increase in Bitcoin’s performance, Lido Staked ETH’s performance increases 
#by about 0.90 percentage points.
#The p-value for Bitcoin is very small (< 2e-16), which indicates that the 
#relationship between Bitcoin and Lido Staked ETH is statistically significant.
#The R-squared value of 0.5053 indicates that approximately 50.5% of the 
#variation in Lido Staked ETH's daily performance can be explained by 
#Bitcoin's daily performance.
#While not a perfect fit, this is still a moderate-to-strong relationship.


#################################################################################

#Bitcoin's daily performance has a moderate positive correlation (0.64662130)
#with Cardano's daily performnace

# Create scatter plot of Bitcoin's daily performance vs Cardano's daily performance

# Filter the combined data to get only Bitcoin and Cardano's daily performance
bitcoin_cardano_data <- combined_data %>%
  dplyr::filter(crypto %in% c("Bitcoin", "Cardano")) %>%
  dplyr::select(date, crypto, daily_performance)

# Reshape the data to wide format for scatter plot
bitcoin_cardano_performance_wide <- bitcoin_cardano_data %>%
  spread(key = crypto, value = daily_performance)

# Scatter plot of Bitcoin vs Cardano's daily performance
ggplot(bitcoin_cardano_performance_wide, aes(x = Bitcoin, y = Cardano)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear regression line
  labs(title = "Scatter Plot: Bitcoin vs Cardano Daily Performance",
       x = "Bitcoin Daily Performance",
       y = "Cardano Daily Performance") +
  theme_minimal()  # Clean theme
#Insight: Bitcoin's daily performance and Ethereum's daily performance appear 
#to be somewhat linearly correlated.
#But there is a significant number of outliers.

# Visualizing Bitcoin and Cardano with boxplots to detect outliers
ggplot(bitcoin_cardano_performance_wide, aes(x = "Bitcoin", y = Bitcoin)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bitcoin's Daily Performance", x = "Bitcoin", y = "Daily Performance") +
  theme_minimal()

ggplot(bitcoin_cardano_performance_wide, aes(x = "Cardano", y = Cardano)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Cardano's Daily Performance", x = "Cardano", y = "Daily Performance") +
  theme_minimal()

# Remove outliers:

# Calculate the IQR for Bitcoin and Cardano
iqr_bitcoin <- IQR(bitcoin_cardano_performance_wide$Bitcoin)
iqr_cardano <- IQR(bitcoin_cardano_performance_wide$Cardano)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_cardano_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_cardano_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_cardano <- quantile(bitcoin_cardano_performance_wide$Cardano, 0.25) - 1.5 * iqr_cardano
upper_cardano <- quantile(bitcoin_cardano_performance_wide$Cardano, 0.75) + 1.5 * iqr_cardano

# Remove outliers from Bitcoin and Cardano columns
bitcoin_cardano_performance_wide_no_outliers <- bitcoin_cardano_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(Cardano >= lower_cardano & Cardano <= upper_cardano)

# Verify the number of rows before and after removing outliers
nrow(bitcoin_cardano_performance_wide)  # Before removing outliers
nrow(bitcoin_cardano_performance_wide_no_outliers)  # After removing outliers

# Run a simple linear regression: Bitcoin's performance (independent) vs. 
#Cardano's performance (dependent)
lm_model_Bitcoin_Cardano <- lm(Cardano ~ Bitcoin, data = bitcoin_cardano_performance_wide_no_outliers)

# View the summary of the linear regression model
summary(lm_model_Bitcoin_Cardano)

#Insights:
#Bitcoin’s daily performance is a significant predictor of Cardano’s daily
#performance.
#The positive coefficient of 0.90267 suggests that when Bitcoin's daily 
#performance increases by one percent,  Cardano's daily performance 
#increases by 0.90267 percentage points.
#R-squared value of  0.3932 means the model explains about 39% of the 
#variation in Cardano’s daily performance, which indicates a moderate fit.
#The p-value for Bitcoin’s coefficient is less than 0.05, indicating that
#Bitcoin’s performance is statistically significant in predicting Cardano's
#performance.

###############################################

#Bitcoin's daily performance has a moderate positive correlation (0.62863586)
#with BNB's daily performnace


# Filter the combined data to get only Bitcoin and BNB's daily performance
bitcoin_bnb_data <- combined_data %>%
  dplyr::filter(crypto %in% c("Bitcoin", "BNB")) %>%  # Adjust to your exact BNN name if necessary
  dplyr::select(date, crypto, daily_performance)

# Reshape the data to wide format for scatter plot
bitcoin_bnb_performance_wide <- bitcoin_bnb_data %>%
  spread(key = crypto, value = daily_performance)


# Scatter plot of Bitcoin vs BNB's daily performance
ggplot(bitcoin_bnb_performance_wide, aes(x = Bitcoin, y = BNB)) + 
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear regression line
  labs(title = "Scatter Plot: Bitcoin vs BNB Daily Performance",
       x = "Bitcoin Daily Performance",
       y = "BNB Daily Performance") +
  theme_minimal()  # Clean theme
##Insight: Bitcoin's daily performance and BNB's daily performance appear to be 
#linearly correlated,
#alhough there are some outliers.


# Visualizing Bitcoin and BNB with boxplots to detect outliers
ggplot(bitcoin_bnb_performance_wide, aes(x = "Bitcoin", y = Bitcoin)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bitcoin's Daily Performance", x = "Bitcoin", y = "Daily Performance") +
  theme_minimal()

ggplot(bitcoin_bnb_performance_wide, aes(x = "BNB", y = `BNB`)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of BNB's Daily Performance", x = "BNB", y = "Daily Performance") +
  theme_minimal()

# Remove outliers:

# Calculate the IQR for Bitcoin and BNN
iqr_bitcoin <- IQR(bitcoin_bnb_performance_wide$Bitcoin)
iqr_bnb <- IQR(bitcoin_bnb_performance_wide$`BNB`)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_bnb_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_bnb_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_bnb <- quantile(bitcoin_bnb_performance_wide$`BNB`, 0.25) - 1.5 * iqr_bnb
upper_bnb <- quantile(bitcoin_bnb_performance_wide$`BNB`, 0.75) + 1.5 * iqr_bnb

# Remove outliers from Bitcoin and BNN columns
bitcoin_bnb_performance_wide_no_outliers <- bitcoin_bnb_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(`BNB` >= lower_bnb & `BNB` <= upper_bnb)

# Verify the number of rows before and after removing outliers
nrow(bitcoin_bnb_performance_wide)  # Before removing outliers
nrow(bitcoin_bnb_performance_wide_no_outliers)  # After removing outliers

# Run a simple linear regression: Bitcoin's performance (independent) vs. BNN's performance (dependent)
lm_model_Bitcoin_BNB <- lm(`BNB` ~ Bitcoin, data = bitcoin_bnb_performance_wide_no_outliers)

# View the summary of the linear regression model
summary(lm_model_Bitcoin_BNB)

#Insights:
#Bitcoin's daily performance has a strong positive and statistically significant 
#relationship with BNB's daily performance.
#For each percentagle increase in Bitcoin’s daily performance, BNB’s daily performance
#increases by 0.69578 percentage points.
#The model explains about 39% of the variation in BNB's performance 
#(R-squared = 0.3849).  
#The p-values for the Bitcoin coefficient (<2e-16) and the F-statistic
#(< 2.2e-16) indicate that the results are highly statistically significant.
#################################################################################


#Bitcoin's daily performance has a moderate positive correlation (0.57107077)
#with Solana's daily performnace

# Filter the combined data to get only Bitcoin and Solana's daily performance
bitcoin_solana_data <- combined_data %>%
  dplyr::filter(crypto %in% c("Bitcoin", "Solana")) %>%  # Select Bitcoin and Solana
  dplyr::select(date, crypto, daily_performance)

# Reshape the data to wide format for scatter plot
bitcoin_solana_performance_wide <- bitcoin_solana_data %>%
  spread(key = crypto, value = daily_performance)

# Scatter plot of Bitcoin vs Solana's daily performance
ggplot(bitcoin_solana_performance_wide, aes(x = Bitcoin, y = Solana)) + 
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear regression line
  labs(title = "Scatter Plot: Bitcoin vs Solana Daily Performance",
       x = "Bitcoin Daily Performance",
       y = "Solana Daily Performance") +
  theme_minimal()  # Clean theme
#Insight: Bitcoin's daily performance and Solana's daily performance appear
#to be linearly correlated, alghough
#there are quite a few outliers.

# Visualizing Bitcoin and Solana with boxplots to detect outliers
ggplot(bitcoin_solana_performance_wide, aes(x = "Bitcoin", y = Bitcoin)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bitcoin's Daily Performance", x = "Bitcoin", y = "Daily Performance") +
  theme_minimal()

ggplot(bitcoin_solana_performance_wide, aes(x = "Solana", y = Solana)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of Solana's Daily Performance", x = "Solana", y = "Daily Performance") +
  theme_minimal()

# Remove outliers:

# Calculate the IQR for Bitcoin and Solana
iqr_bitcoin <- IQR(bitcoin_solana_performance_wide$Bitcoin)
iqr_solana <- IQR(bitcoin_solana_performance_wide$Solana)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_solana_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_solana_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_solana <- quantile(bitcoin_solana_performance_wide$Solana, 0.25) - 1.5 * iqr_solana
upper_solana <- quantile(bitcoin_solana_performance_wide$Solana, 0.75) + 1.5 * iqr_solana

# Remove outliers from Bitcoin and Solana columns
bitcoin_solana_performance_wide_no_outliers <- bitcoin_solana_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(Solana >= lower_solana & Solana <= upper_solana)

# Verify the number of rows before and after removing outliers
nrow(bitcoin_solana_performance_wide)  # Before removing outliers
nrow(bitcoin_solana_performance_wide_no_outliers)  # After removing outliers

# Run a simple linear regression: Bitcoin's performance (independent) vs. Solana's performance (dependent)
lm_model_Bitcoin_Solana <- lm(Solana ~ Bitcoin, data = bitcoin_solana_performance_wide_no_outliers)

# View the summary of the linear regression model
summary(lm_model_Bitcoin_Solana)

#Insights:
#Bitcoin is a significant predictor of Solana’s daily performance, with a
#coefficient of approximately 1.15, indicating a strong positive relationship.
#This means that for every percentage point that Bitcoin's daily performance
#increases, Solana's daily performanc increases by 1.15 percentage points.
#The p-value for Bitcoin’s coefficient is very small, suggesting a very strong
#relationship between Bitcoin and Solana's performance.
#The model explains 32.7% of the variation in Solana’s daily performance.
#The residual standard error of 3.681 suggests that there is significant 
#variability in the residuals. 
#################################################################################


#Bitcoin's daily performance has a moderate positive correlation (0.52208384)
#with XRP's daily performnace

# Create scatter plot of Bitcoin's daily performance vs XRP's daily performance

# Filter the combined data to get only Bitcoin and XRP's daily performance
bitcoin_xrp_data <- combined_data %>%
  dplyr::filter(crypto %in% c("Bitcoin", "XRP")) %>%  # Select Bitcoin and XRP
  dplyr::select(date, crypto, daily_performance)

# Reshape the data to wide format for scatter plot
bitcoin_xrp_performance_wide <- bitcoin_xrp_data %>%
  spread(key = crypto, value = daily_performance)

# Scatter plot of Bitcoin vs XRP's daily performance
ggplot(bitcoin_xrp_performance_wide, aes(x = Bitcoin, y = XRP)) + 
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot points
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Linear regression line
  labs(title = "Scatter Plot: Bitcoin vs XRP Daily Performance",
       x = "Bitcoin Daily Performance",
       y = "XRP Daily Performance") +
  theme_minimal()  # Clean theme
##Insight: Bitcoin's daily performance and Ethereum's daily performance 
#appear to be somewhat linearly correlated.
#There are, however, extreme outliers.

# Visualizing Bitcoin and XRP with boxplots to detect outliers
ggplot(bitcoin_xrp_performance_wide, aes(x = "Bitcoin", y = Bitcoin)) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = "Boxplot of Bitcoin's Daily Performance", x = "Bitcoin", y = "Daily Performance") +
  theme_minimal()

ggplot(bitcoin_xrp_performance_wide, aes(x = "XRP", y = XRP)) +
  geom_boxplot(fill = "lightgreen", color = "black") +
  labs(title = "Boxplot of XRP's Daily Performance", x = "XRP", y = "Daily Performance") +
  theme_minimal()

# Remove outliers:

# Calculate the IQR for Bitcoin and XRP
iqr_bitcoin <- IQR(bitcoin_xrp_performance_wide$Bitcoin)
iqr_xrp <- IQR(bitcoin_xrp_performance_wide$XRP)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_xrp_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_xrp_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_xrp <- quantile(bitcoin_xrp_performance_wide$XRP, 0.25) - 1.5 * iqr_xrp
upper_xrp <- quantile(bitcoin_xrp_performance_wide$XRP, 0.75) + 1.5 * iqr_xrp

# Remove outliers from Bitcoin and XRP columns
bitcoin_xrp_performance_wide_no_outliers <- bitcoin_xrp_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(XRP >= lower_xrp & XRP <= upper_xrp)

# Verify the number of rows before and after removing outliers
nrow(bitcoin_xrp_performance_wide)  # Before removing outliers
nrow(bitcoin_xrp_performance_wide_no_outliers)  # After removing outliers

# Run a simple linear regression: Bitcoin's performance (independent) vs. 
#XRP's performance (dependent)
lm_model_Bitcoin_XRP <- lm(XRP ~ Bitcoin, data = bitcoin_xrp_performance_wide_no_outliers)

# View the summary of the linear regression model
summary(lm_model_Bitcoin_XRP)

#Insisghts:
#Bitcoin and XRP have a statistically significant positive relationship. For every
#1 percentage point increase in Bitcoin's daily performance, XRP's daily performance
#is predicted to increase by approximately 0.72 percentage points.
#The regression model explains only about 30% of the variation in XRP's daily
#performance. 
#The model has a moderate predictive power
#Residual errors are around 2.4 percentage points, so predictions have some 
#uncertainty.
#################################################################################


#Determine model with lowest residual standard error:

# Create a list containing the names of the models
models <- list(lm_model_Bitcoin_Ethereum, lm_model_Bitcoin_Lido, lm_model_Bitcoin_Cardano, 
               lm_model_Bitcoin_BNB, lm_model_Bitcoin_Solana, lm_model_Bitcoin_XRP)

# Create a data frame with model names and residual standard errors
residual_errors <- data.frame(
  Model = c("lm_model_Bitcoin_Ethereum", "lm_model_Bitcoin_Lido", "lm_model_Bitcoin_Cardano", 
            "lm_model_Bitcoin_BNB", "lm_model_Bitcoin_Solana", "lm_model_Bitcoin_XRP"),
  Residual_Standard_Error = sapply(models, function(model) summary(model)$sigma)
)

#View residual_errors
residual_errors

# Find the model with the lowest residual standard error
lowest_residual_model <- residual_errors[which.min(residual_errors$Residual_Standard_Error), ]
#Insight: the lowest residual standard error is 1.909276

# Print the name of the model with the lowest residual standard error
print(paste("The model with the lowest residual error is:", lowest_residual_model$Model))
#The model with the lowest residual standard error is lm_model_Bitcoin_Ethereum

#Therefore, the model with the lowest residual standard error is the
#lm_model_Bitcoin_Ethereum model,
#whose residual standard error is 1.909276.

#Diagnose the model with the lowest lowest residual standard error
#(lm_model_Bitcoin_Ethereum)

# Residuals vs Fitted plot (check for homoscedasticity and linearity)
plot(lm_model_Bitcoin_Ethereum, 1)
#Insight: the Residuals vs Fitted plot shows no clear pattern. 
#So there appear to be no issues like non-linearity or non-constant 
#variance of residuals (heteroscedasticity).

#Histogram of the residuals to check for skewness:
hist(residuals(lm_model_Bitcoin_Ethereum), breaks = 30, col = "lightblue", main = "Histogram of Residuals", xlab = "Residuals")
#Insight: the histogram of residuals does not show any clear skewness;
#this supports the assumption of normality of the residuals

#Check for skewness of residuals:

# Extract residuals from the model (lm_model_Bitcoin_Ethereum)
residuals_bitcoin_ethereum <- residuals(lm_model_Bitcoin_Ethereum)

#View residuals
residuals_bitcoin_ethereum

# Calculate the skewness of the residuals
skewness_residuals <- e1071::skewness(residuals_bitcoin_ethereum)

# Print the skewness value
skewness_residuals
#Insight: the skewness value of 0.635 suggests a moderate positive skew 
#in the residuals, as opposed to a substantial skew (e.g., greater 
#than 1 or less than -1)

## Check for autocorrelation of residuals using Durbin-Watson test

# Install lmtest (if not already installed)
install.packages("lmtest")

# Load the lmtest package
library(lmtest)

dwtest(lm_model_Bitcoin_Ethereum)
#Insight: Since DW ≈ 2 and the p-value is high (0.75), one can conclude 
#that there is no significant autocorrelation in the residuals of this model;
#thus, the assumption of independence of errors holds for this model.

#Determine if log transformation of independent variable and/or independent 
#variable needed:

# Create a histogram for Bitcoin's daily performance (without outliers) 
ggplot(bitcoin_ethereum_performance_wide_no_outliers, aes(x = Bitcoin)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..density..), fill = "blue", alpha = 0.2) +  # Add density plot
  labs(title = "Histogram of Bitcoin's Daily Performance (No Outliers)",
       x = "Bitcoin Daily Performance",
       y = "Frequency") +
  theme_minimal()
#Insight: the distribution of Bitcoin's daily performance looks normally distributed.

# Create a histogram for Ethereum's daily performance (without outliers) 
ggplot(bitcoin_ethereum_performance_wide_no_outliers, aes(x = Ethereum)) +
  geom_histogram(binwidth = 0.5, fill = "lightgreen", color = "black", alpha = 0.7) +
  geom_density(aes(y = ..density..), fill = "green", alpha = 0.2) +  # Add density plot
  labs(title = "Histogram of Ethereum's Daily Performance (No Outliers)",
       x = "Ethereum Daily Performance",
       y = "Frequency") +
  theme_minimal()
#Insight: the distribution of Ethereum's daily performance looks normally distributed.

#Determine skewness of Bitcoin's daily performance
skewness_bitcoin_daily_performance <- e1071::skewness(bitcoin_ethereum_performance_wide_no_outliers$Bitcoin)

#View Bitcoin's daily performance skewness value
skewness_bitcoin_daily_performance
#Insight: the skewness value  of Bitcoin daily performace (0.09086852) indicates
#a very slight positive skew in the distribution of Bitcoin's daily performance.
#So there is no compelling reason to apply log transformation to Bitcoin's daily performance

#Determine skewness of Ethereum's daily performance
skewness_ethereum_daily_performance <- e1071::skewness(bitcoin_ethereum_performance_wide_no_outliers$Ethereum)

#View Ethereum's daily performance skewness value
skewness_ethereum_daily_performance
#Insight: the skewness value of Ethereum's daily performance (0.06791918) 
#indicates a very slight positive skew in the distribution of Ethereum's daily 
#performance So there is no compelling reason to apply log transformation to Ethereums's 
#daily performance

#############################################################################
#Explore adding lagged variables derived from Bitcoin's daily performance
#as predictors to the Bitcoin vs Ethereum model
#############################################################################

# Create lagged variables for Bitcoin's daily performance (1 to 5 days)
bitcoin_ethereum_performance_wide <- bitcoin_ethereum_performance_wide %>%
  arrange(date) %>%
  mutate(
    Bitcoin_Lag1 =  dplyr::lag(Bitcoin, 1),
    Bitcoin_Lag2 =  dplyr::lag(Bitcoin, 2),
    Bitcoin_Lag3 =  dplyr::lag(Bitcoin, 3),
    Bitcoin_Lag4 =  dplyr::lag(Bitcoin, 4),
    Bitcoin_Lag5 =  dplyr::lag(Bitcoin, 5)
  )

# View the data to ensure the lagged variables were created correctly
head(bitcoin_ethereum_performance_wide)

# Remove rows with missing lagged values (e.g., the first 5 rows)
bitcoin_ethereum_performance_wide <- bitcoin_ethereum_performance_wide %>%
  dplyr::filter(!is.na(Bitcoin_Lag1) & !is.na(Bitcoin_Lag2) & !is.na(Bitcoin_Lag3) & !is.na(Bitcoin_Lag4) & !is.na(Bitcoin_Lag5))

# View the cleaned data
head(bitcoin_ethereum_performance_wide)

# Rearrange columns
bitcoin_ethereum_performance_wide <- bitcoin_ethereum_performance_wide %>%
  dplyr::select(date, everything())  # Ethereum will be the first column, and all other columns will follow

# View the updated data
head(bitcoin_ethereum_performance_wide)

# Fit a multiple linear regression model that contains lagged Bitcoin
#performance as predictors
lm_model_Bitcoin_Ethereum_with_lags <- lm(Ethereum ~ Bitcoin + Bitcoin_Lag1 + Bitcoin_Lag2 + Bitcoin_Lag3 + Bitcoin_Lag4 + Bitcoin_Lag5, 
                                          data = bitcoin_ethereum_performance_wide)

# View the summary of the new model
summary(lm_model_Bitcoin_Ethereum_with_lags)

#Insights:
#Bitcoin: The coefficient for Bitcoin is 1.044937 with a very low p-value 
#(< 2e-16), indicating that Bitcoin's daily performance is a highly 
#statistically significant predictor of Ethereum's daily performance. 
#This means that changes in Bitcoin's performance strongly influence 
#Ethereum's performance.
#Bitcoin: For every percentage point increase in Bitcoin's daily 
#performance, Ethereum's daily performance is expected to increase by
#1.044937 percentage points, assuming all other predictors remain constant. 
#This aligns with the strong positive correlation you previously observed.
#Bitcoin_Lag1: The coefficient for the first lag (Bitcoin_Lag1) is 0.005847,
#and its p-value is 0.770, which is not significant (p > 0.05). This 
#suggests that the performance of Bitcoin one day ago does not have a
#statistically significant relationship with the current performance of Ethereum.
#Bitcoin_Lag2 to Bitcoin_Lag5: The lagged Bitcoin variables
#(Bitcoin_Lag2, Bitcoin_Lag3, Bitcoin_Lag4, and Bitcoin_Lag5) all 
#have insignificant p-values , meaning they do not significantly 
#predict Ethereum’s performance after adjusting for Bitcoin's 
#current performance.
#The coefficients for the lagged Bitcoin variables suggest that,
#on average, the past performance of Bitcoin (from one day to five
#days ago) does not significantly affect the current performance of Ethereum.
#The model has an R-squared of 0.6562, meaning that 65.62% of the 
#variance in Ethereum's daily performance is explained by the current
#performance of Bitcoin and the lagged performance by one day of Bitcoin.
#The Adjusted R-squared of 0.6538 accounts for the number of predictors
#and suggests that the model fit is quite good.
#The F-statistic of 453.2 with a p-value of < 2.2e-16 indicates that 
#the model as a whole is highly significant. This #means that at least 
#one of the predictors  is statistically significantly related to
#Ethereum’s daily performance.
#The residuals appear to be reasonably well-distributed, but there may
#still be some large outliers (e.g., residuals with a maximum of 23.7892).
#The residual standard error is 2.46, meaning that the model's 
#predictions are, on average, off by about 2.46 percentage points
#of Ethereum's daily performance.

# Check for multicollinearity of predictors using Variance Inflation
#Factor (VIF)
vif(lm_model_Bitcoin_Ethereum_with_lags)
#Insight: the VIF values of the model's predictors are all lower than 10;
#this indicates absence of multicollinearity in the model.

#################################################

# Create additional lagged variables for Bitcoin's daily performance
#so that there are now lagged variables for 1 to 10 days
bitcoin_ethereum_performance_wide <- bitcoin_ethereum_performance_wide %>%
  arrange(date) %>%
  mutate(
    Bitcoin_Lag1 =  dplyr::lag(Bitcoin, 1),
    Bitcoin_Lag2 =  dplyr::lag(Bitcoin, 2),
    Bitcoin_Lag3 =  dplyr::lag(Bitcoin, 3),
    Bitcoin_Lag4 =  dplyr::lag(Bitcoin, 4),
    Bitcoin_Lag5 =  dplyr::lag(Bitcoin, 5),
    Bitcoin_Lag6 =  dplyr::lag(Bitcoin, 6),
    Bitcoin_Lag7 =  dplyr::lag(Bitcoin, 7),
    Bitcoin_Lag8 =  dplyr::lag(Bitcoin, 8),
    Bitcoin_Lag9 =  dplyr::lag(Bitcoin, 9),
    Bitcoin_Lag10 = dplyr::lag(Bitcoin, 10)
  )

# Remove rows with missing lagged values (e.g., the first 10 rows)
bitcoin_ethereum_performance_wide <- bitcoin_ethereum_performance_wide %>%
  dplyr::filter(!is.na(Bitcoin_Lag1) & !is.na(Bitcoin_Lag2) & !is.na(Bitcoin_Lag3) & 
                  !is.na(Bitcoin_Lag4) & !is.na(Bitcoin_Lag5) & !is.na(Bitcoin_Lag6) &
                  !is.na(Bitcoin_Lag7) & !is.na(Bitcoin_Lag8) & !is.na(Bitcoin_Lag9) & 
                  !is.na(Bitcoin_Lag10))

# View the updated data to check the new lagged variables
head(bitcoin_ethereum_performance_wide)

# Fit a new multiple linear regression model that contains additional lagged Bitcoin performance as predictors (up to 10 days)
lm_model_Bitcoin_Ethereum_with_lags_10 <- lm(Ethereum ~ Bitcoin + Bitcoin_Lag1 + Bitcoin_Lag2 + Bitcoin_Lag3 + 
                                               Bitcoin_Lag4 + Bitcoin_Lag5 + Bitcoin_Lag6 + Bitcoin_Lag7 + 
                                               Bitcoin_Lag8 + Bitcoin_Lag9 + Bitcoin_Lag10, 
                                             data = bitcoin_ethereum_performance_wide)

# View the summary of the new model
summary(lm_model_Bitcoin_Ethereum_with_lags_10)
#Insights:
#The coefficient for Bitcoin is 1.045689, which means that for every
#1 percentage point increase in Bitcoin’s daily performance, Ethereum’s
#daily performance is expected to increase by 1.045689 percentage points.
#This is statistically significant (p-value < 2e-16).
#Except for Bitcoin_Lag10, all the lagged variables have high p-values,
#indicating that their coefficients are not statistically significant. 
#This suggests that the lagged performance of Bitcoin for these periods
#does not have a strong predictive power for Ethereum’s performance.
#Bitcoin_Lag10 has a coefficient of 0.042327 with a p-value of 0.0285, 
#which is statistically significant at the 5% significance level. 
#This suggests that the 10-day lag of Bitcoin’s performance has a 
#statistically significant impact on Ethereum’s daily performance.
#The multiple R-squared is  0.6751;  this means that 67.51% of the 
#variance in Ethereum’s daily performance can be explained by Bitcoin's
#current and past (lagged) performances. This is a moderately strong
#model fit.
#The adjusted R-squared is 0.6717; this value adjusts the R-squared
#to account for the number of predictors in the model; since the 
#difference between the R-squared and the adjusted R-squared is 
#small, it suggests that the inclusion of lagged variables doesn’t 
#introduce much noise or overfitting.
#The Residual Standard Error is 2.362; the residual standard error
#is an estimate of the average distance between the observed and 
#predicted values; on average, the model’s predictions are off by 2.362 percentage points of Ethereum's daily performance. 

#################################################################################

# Run a multiple linear regression with Ethereum as the dependent variable 
#and only two predictors: Bitcoin and Bitcoin_Lag10
lm_model_Bitcoin_Bitcoin_lag10_Ethereum <- lm(Ethereum ~ Bitcoin + Bitcoin_Lag10, 
                                      data = bitcoin_ethereum_performance_wide)

# View the summary of the new model
summary(lm_model_Bitcoin_Bitcoin_lag10_Ethereum)

#Insights:
#The median residual is close to 0 (-0.1621), which suggests that the model
#doesn’t systematically overestimate or underestimate the values.
#Bitcoin coefficient (1.04483): this is the most significant predictor. 
#For each 1 percentage point increase in Bitcoin’s daily performance, 
#Ethereum’s daily performance increases by 1.04483 percentage points. 
#The extremely low p-value (< 2e-16) indicates that this relationship 
#is highly statistically significant.
#The Bitcoin coefficient has a very low p-value (< 2e-16), indicating
#it is statistically significant at all typical significance levels 
#(0.01, 0.05, 0.10).
#Bitcoin_Lag10 coefficient (0.04165): this shows that for each 1 
#percentage point increase in Bitcoin’s performance 10 days ago, 
#Ethereum’s daily performance is expected to increase by 0.04165 
#percentage points. This is statistically significant with a 
#p-value of 0.0299, indicating that the 10-day lagged performance
#of Bitcoin has a moderate effect on Ethereum’s performance.
#The Bitcoin_Lag10 coefficient has a p-value of 0.0299, which is
#significant at the 5% level; this shows that the lagged performance
#of Bitcoin for 10 days has some predictive power for Ethereum's 
#performance.
#Residual Standard Error of 2.348 indicates that the model’s 
#predictions are off by 2.348 percentage points in terms of
#Ethereum’s daily performance.
#Multiple R-squared of 0.6741 means that 67.41% of the variance
#in Ethereum's daily performance is explained by Bitcoin's 
#performance and the Bitcoin_Lag10; this indicates a fairly 
#strong relationship between the predictors and the dependent
#variable.
#Adjusted R-squared of 0.6736 adjusts for the number of predictors
#in the model and confirms that the model is still explaining 
#a large proportion of variance after accounting for the two predictors.
#The F-statistic tests the overall significance of the model. 
#Since the p-value is very small (< 2.2e-16), this indicates that the
#model as a whole is statistically significant and at least one of the
#predictors (Bitcoin or Bitcoin_Lag10) is important in explaining 
#Ethereum's performance.


#Perform a diagnostic check on the lm_model_Bitcoin_Bitcoin_lag10_Ethereum model,

# Residuals vs Fitted plot (check for homoscedasticity and linearity)
plot(lm_model_Bitcoin_Bitcoin_lag10_Ethereum, 1)
#Insight: the Residuals vs Fitted plot shows that the residuals seem to be 
#randomly scattered around 0 without forming patterns;
#thus, homoscedasticity (constant variance of the residuals) seems to be
#present in the model.
#equivalently, heteroscedasticity (non-constance variance of the residuals), 
#seems to be absent form the model.

#Check residuals for normality:
# Plot histogram of residuals
hist(residuals(lm_model_Bitcoin_Bitcoin_lag10_Ethereum), 
     main = "Histogram of Residuals", 
     xlab = "Residuals", 
     col = "lightblue", 
     breaks = 30)

#Insight: residuals seem to be normally distributed.

# Check for Autocorrelation of Residuals:
# Perform the Durbin-Watson test
dwtest(lm_model_Bitcoin_Bitcoin_lag10_Ethereum)
#Insights:
#The DW statistic of 1.9942 indicates no significant autocorrelation 
#in the residuals of your model.
#The p-value ( 0.457) indicates that there is no evidence of positive
#autocorrelation; therefore, the assumption of independence of residuals
#is likely satisfied.

#Check for multicollinearity in the lm_model_Bitcoin_Bitcoin_lag10_
#Ethereum model
vif(lm_model_Bitcoin_Bitcoin_lag10_Ethereum)
#Insight: the lm_model_Bitcoin_Bitcoin_lag10_Ethereum model shows
#no multicollinearity of the predictors

#Fit a robust regression model which is less sensitive to violations
#of the normality assumption.
robust_model <- rlm(Ethereum ~ Bitcoin + Bitcoin_Lag10, data = bitcoin_ethereum_performance_wide)

#View summary
summary(robust_model)

#Insights:
#The median residual is very close to 0 (-0.04077), which suggests that the
#model’s predictions are centered around the true values of Ethereum’s 
#daily performance.
#The 1st and 3rd quartiles of the residuals (-1.05 and 1.05) show that 
#most residuals are within this range, indicating that the model is 
#performing well for a majority of the data points.
#The coefficient for Bitcoin is 1.0352, which indicates that for every
#1 perccentage point increase in Bitcoin's daily performance, Ethereum's
#daily performance is expected to increase by 1.0352 percentage points.
#The t-value for Bitcoin is 69.5568, which is very large, suggesting that
#this coefficient is highly statistically significant; this indicates 
#that Bitcoin has a strong effect on Ethereum’s performance.
#The coefficient for Bitcoin_Lag10 is 0.0451, meaning that for every
#1 percentage point increase in Bitcoin’s performance 10 days ago, Ethereum’s
#performance is expected to increase by 0.0451 units.
#The t-value of 3.0506 suggests that this lagged variable is statistically 
#significant, and its effect on Ethereum’s performance is not negligible.
#The residual standard error (RSE) of 1.562 provides a measure of how much
#the observed values of Ethereum’s performance differ from the predicted 
#values; in other words, it tells typical size of the errors (residuals) in the model.
#A lower residual standard error suggests a better fit of the model to 
#the data, as it indicates that the predicted values are relatively
#close to the actual values.
#In this case, the residual standard error (RSE) of  1.562  is fairly
#low, which indicates that the model's predictions are generally accurate. 
#It is also the lowest RSE of any of the models created so far.
#Therefore, out of all the models developed so far, it is the model that
#provides the best predictions.

#################################################################################

#Improve the lm_model_Bitcoin_Bitcoin_lag10_Ethereum model by removing outliers:

#Remove outliers:

# Calculate the IQR for Bitcoin, Bitcoin_Lag10 and Ethereum
iqr_bitcoin <- IQR(bitcoin_ethereum_performance_wide$Bitcoin)
iqr_bitcoin_lag10 <- IQR(bitcoin_ethereum_performance_wide$Bitcoin_Lag10)
iqr_ethereum <- IQR(bitcoin_ethereum_performance_wide$Ethereum)

# Calculate the lower and upper bounds for outliers
lower_bitcoin <- quantile(bitcoin_ethereum_performance_wide$Bitcoin, 0.25) - 1.5 * iqr_bitcoin
upper_bitcoin <- quantile(bitcoin_ethereum_performance_wide$Bitcoin, 0.75) + 1.5 * iqr_bitcoin

lower_bitcoin_lag10 <- quantile(bitcoin_ethereum_performance_wide$Bitcoin_Lag10, 0.25) - 1.5 * iqr_bitcoin_lag10
upper_bitcoin_lag10 <- quantile(bitcoin_ethereum_performance_wide$Bitcoin_Lag10, 0.75) + 1.5 * iqr_bitcoin_lag10

lower_ethereum <- quantile(bitcoin_ethereum_performance_wide$Ethereum, 0.25) - 1.5 * iqr_ethereum
upper_ethereum <- quantile(bitcoin_ethereum_performance_wide$Ethereum, 0.75) + 1.5 * iqr_ethereum

# Remove outliers from Bitcoin. Bitcoin_Lag10 and Ethereum columns
bitcoin_ethereum_performance_wide_no_outliers <- bitcoin_ethereum_performance_wide %>%
  dplyr::filter(Bitcoin >= lower_bitcoin & Bitcoin <= upper_bitcoin) %>%
  dplyr::filter(Bitcoin_Lag10 >= lower_bitcoin & Bitcoin_Lag10 <= upper_bitcoin_lag10) %>%
  dplyr::filter(Ethereum >= lower_ethereum & Ethereum <= upper_ethereum)

#View the data
bitcoin_ethereum_performance_wide_no_outliers

# Verify the number of rows before and after removing outliers
nrow(bitcoin_ethereum_performance_wide)  # Before removing outliers
nrow(bitcoin_ethereum_performance_wide_no_outliers)  # After removing outliers

# Install MASS (if not already installed)
install.packages("MASS")

# Load the MASS package
library(MASS)

#Fit a robust regression model 
robust_model_no_outliers <- rlm(Ethereum ~ Bitcoin + Bitcoin_Lag10, data = bitcoin_ethereum_performance_wide_no_outliers)

#View summary
summary(robust_model_no_outliers)


#Insights:
#The median residual is -0.03854, very close to zero, which indicates that 
#on average, the model closely estimates Ethereum's daily performance.
# The coefficient of Bitcoin (0.9792) suggests that for every 1 percent increase
#in Bitcoin, Ethereum increases by 0.9792 percentaage points, holding 
#Bitcoin_Lag10 constant.
#The t-value of the Bitcoin coefficient (45.5038 ) is very large, suggesting
#that the relationship between Bitcoin and Ethereum is highly statistically
#significant.
#The coefficient for Bitcoin_Lag10 indicates that for every 1 percent 
#increase in Bitcoin_Lag10, Ethereum increases by 0.0419 percentage points,
#holding Bitcoin constant.
#The t-statistic for Bitcoin_Lag10 (2.0421) is statistically significant 
#at the 0.05 level, suggesting that there is a significant effect of
#Bitcoin_Lag10 on Ethereum, 
#The residual standard error (1.388) is the standard deviation of the
#residuals. It indicates the typical size of the prediction errors; 
#in this case it indicates that
##the typical error of the model's predictions is 1.388 % of 
#Ethereum's daily performance.

#Diagnose model:

# Create a Residuals vs Fitted Plot fo check for Homoscedasticity
#and Linearity
# Extract residuals and fitted values
residuals_robust_no_outliers <- residuals(robust_model_no_outliers)
fitted_robust_no_outliers <- fitted(robust_model_no_outliers)

# Residuals vs Fitted plot
plot(fitted_robust_no_outliers, residuals_robust_no_outliers, 
     main = "Residuals vs Fitted (Robust Model- No Outlilers)", 
     xlab = "Fitted Values", ylab = "Residuals", 
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)
#Insight: the residuals seem to be randomly scattered around zero.
#No systematic patter is detected, which could suggest  non-linearity
#or heteroscedasticity.

#Create a Histogram of Residuals to check for normality of residuals
# Histogram of residuals
hist(residuals_robust_no_outliers, 
     main = "Histogram of Residuals (Robust Model)", 
     xlab = "Residuals", 
     col = "lightblue", 
     breaks = 30)
#Insight: the residuals seem to be normally distributed.

# Install lmtest (if not already installed)
install.packages("lmtest")

# Load the lmtest package
library(lmtest)

#Perform Durbin-Watson Test to test for autocorrelation of residuals  
# Perform Durbin-Watson test
dw_test_robust_no_outlieers <- dwtest(robust_model_no_outliers)

# View result
dw_test_robust_no_outlieers
#Insight:
#DW = 1.9577 indicates that there is no significant autocorrelation
#in the model's residuals, as the value is close to 2.
#The p-value = 0.2327 further supports this conclusion, as it is
#well above the typical threshold (0.05), meaning there's no evidence
#to suggest positive autocorrelation of residuals in the model.

#Determine the Variance Inflation Factors (VIF) to test for Multicollinearity

# Check for multicollinearity using VIF
vif(robust_model_no_outliers)
#Insight: Since both Bitcoin and Bitcoin_Lag10 have VIF values close
#to 1 (i.e., 1.000047), neither predictor is collinear with the other;
#so there is no multicollinearity in this model.

################################################################

#Perform cross-validatioin of the robust_model_no_outliers model:

#Split Data into Training and Test Sets:

# Set a seed for reproducibility
set.seed(123)

# Create a train-test split (e.g., 80% for training and 20% for testing)
trainIndex <- createDataPartition(bitcoin_ethereum_performance_wide_no_outliers$Ethereum, 
                                  p = 0.8, 
                                  list = FALSE, 
                                  times = 1)

# Split the data into training and test sets
train_data <- bitcoin_ethereum_performance_wide_no_outliers[trainIndex, ]
test_data <- bitcoin_ethereum_performance_wide_no_outliers[-trainIndex, ]

dim(train_data)
dim(test_data)

#Perform Cross-Validation on the Training Set:

# Perform 5-fold cross-validation on the training set
cv_model <- train(Ethereum ~ Bitcoin + Bitcoin_Lag10, 
                  data = train_data, 
                  method = "rlm",  # robust regression model
                  trControl = trainControl(method = "cv", number = 100))  # 100-fold CV

# View cross-validation results
cv_model
#Insights:
#The optimal model (based on RMSE) is the one without an intercept 
#and using Hampel’s psi function for robust regression. 
#This combination provides the best balance of error minimization 
#(lowest RMSE), while also maintaining a reasonable Rsquared and MAE.
#R-squared of 0.6256571 ndicates that the model explains about 
#62.5% of the variance in Ethereum’s price using Bitcoin and
#Bitcoin_Lag10 as predictors.
#This suggests a moderate fit of the model to the data.
#The MAE value of 1.330348 indicates that the average absolute
#error between the predicted and actual values of Ethereum's daily 
#performance is around 1.33%.


#Evaluate the Model on the Test Set:
# Predict on the test set using the trained model
predictions <- predict(cv_model, newdata = test_data)

# Compare predictions with actual values
actuals <- test_data$Ethereum

# Calculate RMSE (Root Mean Squared Error)
rmse <- sqrt(mean((predictions - actuals)^2))
print(paste("RMSE: ", rmse))
#Insight: the RMSE is only  1.72443190852286; that is, this model's 
#prediction of Ethereum's daily performance is typically
#off by only 1.72%.

#Compare RMSE to Ethereum's daily performance in the test set:

#Calculate the median value of Ethereum's daily performance in the test set
median_daily_performance_Ethereum_test_set <- median(test_data$Ethereum)
#Compare RMSE to median value of  Ethereum's daily performance in the test set
median_daily_performance_Ethereum_test_set
rmse
#Insight: 
#The median value of Ethereum's daily performance in the test set is 
#0.04453825, that is,  0.04453825%.  
#The model's RMSE of 1.72% seems high compared to the median value of
#Ethereum's daily performance in the test set of 0.04453825%

#Calculate the average value of Ethereum's daily performance in the test set
average_daily_performance_Ethereum_test_set <- mean(test_data$Ethereum)
#Compare RMSE to average value of  Ethereum's daily performance in the test set
average_daily_performance_Ethereum_test_set
rmse
#Insight:
#The average value of Ethereum's daily performance in the test set is 
#0.1216692, that is,  0.1216692 %.
#The model's RMSE of 1.72% seems high compared to the average value
#of Ethereum's daily performance in the test set of 0.1216692 %.

#Calculate the standard deviation value of Ethereum's daily performance
#in the test set
sd_daily_performance_Ethereum_test_set <- sd(test_data$Ethereum)
#Compare RMSE to standard deviation of  Ethereum's daily performance in the test set
sd_daily_performance_Ethereum_test_set
rmse
#Insight:
#The standard deviation of Ethereum's daily performance in the test 
#set is 2.761402, that is, 2.761402%
#The model's RMSE of 1.72% seems low compared to the standard deviation
#value of Ethereum's daily performance in the test set of  2.761402%.
#In fact, the standard deviation of Ethereum's daily performance is 
#1.6 times the value of the RMSE.
#In other words, the value of the RMSE is only 62% the value of the 
#standard deviation of Ethereum's daily performance in the test set.
#Therefore, the typical error of this model, as measured by the RMSE, 
#is well within one standard deviation of Ethereum's daily performance.


# Calculate MAE (Mean Absolute Error)
mae <- mean(abs(predictions - actuals))
print(paste("MAE: ", mae))
#Insight: the MAE is only  1.28271768214534; that is, this model's 
#prediction of Ethereum's daily performance is typically
#off by only 1.28%.

#Compare MAE to the Ethereum's daily performance in the test set:

#Compare MAE to median value of  Ethereum's daily performance in 
#the test set
median_daily_performance_Ethereum_test_set
mae
#Insight: #The model's MAE of 1.282718%  seems high compared to 
#the median value of Ethereum's daily performance in the test set of
#0.04453825%`

#Compare MAE to average value of  Ethereum's daily performance 
#in the test set
average_daily_performance_Ethereum_test_set
mae
#The model's MAE of 1.282718 seems high compared to the average 
#value of Ethereum's daily performance in the test set of 0.1216692 %.

#Compare MAE to standard deviation of  Ethereum's daily performance in the test set
sd_daily_performance_Ethereum_test_set
mae
#Insight:
#The standard deviation of Ethereum's daily performance in the
#test set is 2.761402, that is, 2.761402%
#The model's MAE of 1.28% seems low compared to the standard 
#deviation value of Ethereum's daily performance in the test
#set of 2.761402%.
#In fact, the standard deviation of Ethereum's daily performance
#is more than 2.15 times the value of the MAE.
#In other words, the value of the MAE is only 46% the value of
#the standard deviation of Ethereum's daily performance in the test set.
#Therefore, the typical error of this model, as measured by the
#MAE, is well within one standard deviation of Ethereum's daily performance.

#################################################################################
#RESULTS
#################################################################################
#The RMSE of the robust model that uses the daily performance of Bitcoin 
#and the daily performance of Bitcoin lagged by 10 days as predictors
#is only  1.72; that is, this model's prediction of Ethereum's daily 
#performance is typically off by only 1.72%.
#When this typical error is compared to the standard deviation of  
#Ethereum's daily performance, which is 2.76%, it is evident 
#that this model's performance is good. Also, the MAE of this model is
#only  1.28; that is, this model's prediction of Ethereum's daily performance
#is typically
#off by only 1.28%. When one compares this typical error to the standard 
#deviation of  Ethereum's daily performance (2.76%), it is 
#evident that this model's performance is good.  

#################################################################################
#CONCLUSION
#################################################################################

#When one compares the RMSE (1.72%) of the robust model that uses the 
#daily performance of Bitcoin and the daily performance of Bitcoin lagged 
#by 10 days as predictors
#to the standard deviation of Ethereum's daily performance (2.76%) , 
#one can conclude that the RMSE is relatively small.
#Therefore, one can conclude that this model predicts Ethereum's daily
#performance with relatively high accuracy. Furthermore, When one compares
#the MAE (1.28%)  of 
#this  model to the standard deviation of Ethereum's daily performance (2.76%), 
#one can conclude that the MAE is quite small.
#Therefore, one can conclude that this model predicts Ethereum's daily performance
#with relatively high accuracy.

