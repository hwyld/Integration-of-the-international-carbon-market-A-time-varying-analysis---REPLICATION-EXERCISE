## ECOM90022 Research Methods, Semester 1, 2024
## Transform cleaned data into weekly returns and run summary statistics
## Author: Henry Wyld
## Date of creation: 2024-03-31

#-------------------------------------
# clear memory
rm(list=ls())    
#----------------------------------

# Load required packages
#load_packages()

# Import your data
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(data.table)
library(xts)

# Set the working directory
setwd("C:/Users/henry/OneDrive - The University of Melbourne/Master of Applied Econometrics/2024/Semester 1/Research Methods/Research Paper/ICAP data")

# Read the CSV file
df <- readr::read_csv("Research_Data_EUR_denom_allowance_prices_trimmed.csv", locale = readr::locale(encoding = "UTF-8"))

# Remove the first two columns
Research_Data <- df[, -c(1:1)]

####### Data Transformation ########
#### Weekly Returns ####
#---------------------------------------

## From Paper: Weekly returns are calculated as the change in log price, from Friday-to-Friday. The continuously compounded returns of four sets 

#----------------------


# Log prices of the data but keep date column
Research_Data_log_prices <- Research_Data %>%
  mutate(across(where(is.numeric), log))

# Daily returns
Research_Data_daily_returns <- Research_Data_log_prices %>% 
  mutate(across(where(is.numeric), ~ . - lag(.)))  

# Add a column that contains the day of the week
Research_Data_daily_returns$Day <- weekdays(as.Date(Research_Data_daily_returns$Date))

# Add 3 new columns that sum the daily returns for each week from Friday to Friday
Research_Data_daily_returns <- Research_Data_daily_returns %>%
  # Ensure 'Date' is in Date format
  mutate(Date = as.Date(Date)) %>%
  # Create a 'WeekID' that identifies weeks based on Fridays but starts from 1
  mutate(WeekID = cumsum(wday(Date) == 6)) %>%
  # Group by this 'WeekID' to ensure aggregation from Friday to Friday
  group_by(WeekID) %>%
  # Sum the returns for each group
  mutate(EUR_EUR_Weekly_Return = sum(EUR_EUR, na.rm = TRUE),
         NZ_EUR_Weekly_Return = sum(NZ_EUR, na.rm = TRUE),
         Hubei_EUR_Weekly_Return = sum(Hubei_EUR, na.rm = TRUE)) %>%
  # Ungroup to remove the grouping structure
  ungroup()

# Calculate weekly return from the sum of the continuous daily returns from Friday to Friday
Research_Data_continuously_compounded_weekly_returns <- Research_Data_daily_returns %>%
  filter(Day == "Friday") %>%
  select(Date,WeekID, EUR_EUR_Weekly_Return, NZ_EUR_Weekly_Return, Hubei_EUR_Weekly_Return)

# WeekID to Date dataframe
Research_Data_weekly_dates <- Research_Data_daily_returns %>%
  filter(Day == "Friday") %>%
  select(Date, WeekID)

# REDUNDANT CODE - DO NOT USE
#---------------------------------------

# Calculate weekly return from the sum of the continuous daily returns from Friday to Friday
# Sum the daily returns for each week
# Research_Data_weekly_returns_cont <- Research_Data_daily_returns %>% 
#  mutate(across(where(is.numeric), ~ sum(.)))

# Trim Research_Data_allowance_price_trimmed to only include Friday dates
# Research_Data_Fridays <- Research_Data_log_prices[weekdays(Research_Data_log_prices$Date) == "Friday", ]


# Calculate first difference of the trimmed dataset
# Research_Data_weekly_returns <- Research_Data_Fridays %>% 
#  mutate(across(where(is.numeric), ~ . - lag(.)))

# Convert the dataframes to time series
# Research_Data_weekly_returns <- xts(Research_Data_weekly_returns[, -1], order.by = as.Date(Research_Data_weekly_returns$Date, format = "%Y-%m-%d"))

# Log difference of the data
# Research_Data_weekly_returns <- log(Research_Data_Fridays[, -1]) - log(lag(Research_Data_Fridays[, -1]))

# Convert the dataframes to time series
# Research_Data_weekly_returns <- xts(Research_Data_weekly_returns[, -1], order.by = as.Date(Research_Data_weekly_returns$Date, format = "%Y-%m-%d"))

#---------------------------------------

# Trim the data to only include the dates from 2014-04-30 to 2021-12-01 (inclusive)
# Wednesday 30th April 2014 to Wednesday 1st December 2021
weekly_returns_trimmed <- Research_Data_continuously_compounded_weekly_returns[Research_Data_continuously_compounded_weekly_returns$Date >= "2014-04-30" & Research_Data_continuously_compounded_weekly_returns$Date <= "2021-12-01", ]

last(weekly_returns_trimmed, 5)

# Study has 397 observations
nrow(weekly_returns_trimmed)

#---------------------------------------

#### Volatilty ####
# From Paper "The main measure is the standard deviation of weekly return over the five-day interval during each week"

#---------------------------------------


# Calculate standard deviation of weekly return over the five-day interval during each week
Research_Data_weekly_volatility <- Research_Data_daily_returns %>%
  filter(Day != "Saturday" & Day != "Sunday") %>%
  group_by(WeekID) %>%
  summarise(EUR_EUR_Weekly_Volatility = sd(EUR_EUR, na.rm = TRUE),
            NZ_EUR_Weekly_Volatility = sd(NZ_EUR, na.rm = TRUE),
            Hubei_EUR_Weekly_Volatility = sd(Hubei_EUR, na.rm = TRUE)) %>%
  ungroup()

# Add the date to the weekly volatility dataframe
Research_Data_weekly_volatility <- merge(Research_Data_weekly_volatility, Research_Data_weekly_dates, by = "WeekID")

# Annualise the weekly volatility
Research_Data_annualised_weekly_volatility <- Research_Data_weekly_volatility %>%
  mutate(across(where(is.numeric), ~ . * sqrt(52) * 100))



# Function to calculate weekly volatility for each week
calculate_weekly_volatility <- function(daily_returns) {
  M <- nrow(daily_returns)  # number of trading days, usually 5
  weekly_volatility <- apply(daily_returns, 2, function(returns) {
    mean_return <- mean(returns)
    sqrt(sum((returns - mean_return)^2) / (M - 1))
  })
  return(weekly_volatility)
}

# Function to annualize the weekly volatility
annualize_weekly_volatility <- function(weekly_volatility) {
  annualized_volatility <- weekly_volatility * sqrt(52) * 100
  return(annualized_volatility)
}


weekly_volatility <- calculate_weekly_volatility(daily_returns)
annualized_volatility <- annualize_weekly_volatility(weekly_volatility)


#---------------------------------------


#### Descriptive statistics ####

# Descriptive stats for both dataframes
#install.packages("psych")
library(psych)

# Basic descriptive statistics
summary(Research_Data_EUR_denom_allowance_prices_trimmed)

# Get detailed descriptive statistics
describe(Research_Data_EUR_denom_allowance_prices_trimmed)

# Plot the data

# Plot the weekly returns
ggplot(Research_Data_continuously_compounded_weekly_returns, aes(x = Date)) +
  geom_line(aes(y = EUR_EUR_Weekly_Return, color = "EUR_EUR")) +
  geom_line(aes(y = NZ_EUR_Weekly_Return, color = "NZ_EUR")) +
  geom_line(aes(y = Hubei_EUR_Weekly_Return, color = "Hubei_EUR")) +
  labs(title = "Weekly Returns",
       x = "Date",
       y = "Weekly Return") +
  scale_color_manual(values = c("EUR_EUR" = "blue", "NZ_EUR" = "red", "Hubei_EUR" = "green")) +
  theme_minimal()

# Save plots together in one file
ggsave("Weekly_Returns_Plot.png", bg = "white")  

# Seperate plots for each series
# Plot the weekly returns for EUR_EUR
ggplot(Research_Data_continuously_compounded_weekly_returns, aes(x = Date)) +
  geom_line(aes(y = EUR_EUR_Weekly_Return, color = "EUR_EUR")) +
  labs(title = "EUR_EUR Weekly Returns",
       x = "Date",
       y = "Weekly Return") +
  scale_color_manual(values = c("EUR_EUR" = "blue")) +
  theme_minimal()  

# Plot the weekly returns for NZ_EUR
ggplot(Research_Data_continuously_compounded_weekly_returns, aes(x = Date)) +
  geom_line(aes(y = NZ_EUR_Weekly_Return, color = "NZ_EUR")) +
  labs(title = "NZ_EUR Weekly Returns",
       x = "Date",
       y = "Weekly Return") +
  scale_color_manual(values = c("NZ_EUR" = "red")) +
  theme_minimal()

# Plot the weekly returns for Hubei_EUR
ggplot(Research_Data_continuously_compounded_weekly_returns, aes(x = Date)) +
  geom_line(aes(y = Hubei_EUR_Weekly_Return, color = "Hubei_EUR")) +
  labs(title = "Hubei_EUR Weekly Returns",
       x = "Date",
       y = "Weekly Return") +
  scale_color_manual(values = c("Hubei_EUR" = "green")) +
  theme_minimal()



# Plot the weekly volatility
ggplot(Research_Data_annualised_weekly_volatility, aes(x = Date)) +
  geom_line(aes(y = EUR_EUR_Weekly_Volatility, color = "EUR_EUR")) +
  geom_line(aes(y = NZ_EUR_Weekly_Volatility, color = "NZ_EUR")) +
  geom_line(aes(y = Hubei_EUR_Weekly_Volatility, color = "Hubei_EUR")) +
  labs(title = "Weekly Volatility",
       x = "Date",
       y = "Weekly Volatility") +
  scale_color_manual(values = c("EUR_EUR" = "blue", "NZ_EUR" = "red", "Hubei_EUR" = "green")) +
  theme_minimal()  

# Save plots together in one file
ggsave("Weekly_Volatility_Plot.png", bg = "white")  