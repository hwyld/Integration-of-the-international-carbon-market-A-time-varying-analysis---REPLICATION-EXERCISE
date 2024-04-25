## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
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
install.packages("psych")
library(psych)

# Set the working directory
setwd("C:/Users/henry/OneDrive - The University of Melbourne/Master of Applied Econometrics/2024/Semester 1/Research Methods/Research Paper/Data")

# Read the CSV file
ICAP_df <- readr::read_csv("ICAP_EUR_denom_allowance_prices_trimmed.csv", locale = readr::locale(encoding = "UTF-8"))
Clearblue_df <- readr::read_csv("Clearblue_data.csv", locale = readr::locale(encoding = "UTF-8"))

# Rename the DateTime column to Date
Clearblue_df <- rename(Clearblue_df, Date = DateTime)


# Function to convert dataframe to xts, assuming the first column is the date
convert_to_xts <- function(df, date_col_name, date_format = "%Y-%m-%d") {
  # Check if the date column exists
  if (!date_col_name %in% names(df)) {
    stop("Date column specified does not exist in the dataframe")
  }
  
  # Convert the date column to Date class
  date_col <- as.Date(df[[date_col_name]], format = date_format)
  
  # Convert all other columns to numeric
  data_cols <- df[, !(names(df) %in% date_col_name)]
  data_cols <- data.frame(lapply(data_cols, function(x) as.numeric(as.character(x))))
  
  # Create the xts object
  xts_object <- xts(data_cols, order.by = date_col)
  
  return(xts_object)
}

# Example usage:
# Assuming ICAP_df and Clearblue_df are already loaded and have the correct date columns
ICAP_df_xts <- convert_to_xts(ICAP_df, "Date")
Clearblue_df_xts <- convert_to_xts(Clearblue_df, "Date")

# Remove the first column of ICAP_df
ICAP_df_xts <- ICAP_df_xts[, -1]


head(ICAP_df_xts, 5)
head(Clearblue_df_xts, 5)

# Function to find start and end dates excluding NA
get_valid_dates <- function(series) {
  valid_dates <- index(series[!is.na(series)])  # Get dates for non-NA values
  start_date <- format(min(valid_dates), "%Y-%m-%d")
  end_date <- format(max(valid_dates), "%Y-%m-%d")
  return(c(Start = start_date, End = end_date))
}

# Apply the function to each column
valid_date_info_ICAP <- sapply(ICAP_df_xts, get_valid_dates)
valid_date_info_Clearblue <- sapply(Clearblue_df_xts, get_valid_dates)

# Compute summary statistics for each series, excluding NA values
summary_stats_ICAP <- sapply(ICAP_df_xts, function(x) describe(x[!is.na(x)]))
summary_stats_Clearblue <- sapply(Clearblue_df_xts, function(x) describe(x[!is.na(x)]))

# Function to merge summary statistics with date info
#combine_stats_and_dates <- function(stats, dates) {
#  stats_df <- as.data.frame(t(stats))
#  dates_df <- as.data.frame(t(dates))
#  combined <- cbind(stats_df, dates_df)
#  return(combined)
#}

# Apply the function to each series
#final_results_ICAP <- Map(combine_stats_and_dates, summary_stats_ICAP, valid_date_info_ICAP)
#final_results_Clearblue <- Map(combine_stats_and_dates, summary_stats_Clearblue, valid_date_info_Clearblue)

# Load knitr for table output
#---------------------------------------

if (!require("knitr")) install.packages("knitr", dependencies=TRUE)
library(knitr)

# Display tables
sapply(summary_stats_ICAP, knitr::kable)
sapply(summary_stats_Clearblue, knitr::kable)

# Creating and printing tables for statistics
kable(summary_stats_ICAP, caption = "Summary Statistics for ICAP Dataset")
kable(summary_stats_Clearblue, caption = "Summary Statistics for Clearblue Dataset")

# Creating and printing tables for dates
kable(valid_date_info_ICAP, caption = "Start and End Dates for ICAP Dataset")
kable(valid_date_info_Clearblue, caption = "Start and End Dates for Clearblue Dataset")

# Export the Tables with stargazer
#install.packages("stargazer")
library(stargazer)

# Export and save the tables to HTML
length(summary_stats_ICAP)

### Need to round the values to 3 decimal places
summary_stats_ICAP <- round(summary_stats_ICAP, 3)
summary_stats_Clearblue <- round(summary_stats_Clearblue, 3)

stargazer(summary_stats_ICAP, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for ICAP Dataset",
          out= "table1.html")

stargazer(summary_stats_Clearblue, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Clearblue Dataset",
          out= "table2.html")

stargazer(valid_date_info_ICAP, type = "html", title = "Start and End Dates for ICAP Dataset",out= "table3.html")
stargazer(valid_date_info_Clearblue, type = "html", title = "Start and End Dates for Clearblue Dataset",out= "table4.html")


#---------------------------------------

#### Create the Research Data ####
# EU ETS based on ICAP : EUR_EUR 
# NZ ETS based on ICAP : NZ_EUR 
# CCA ETS based on Clearblue : CCA...Front.December...ICE
# Hubei ETS based on ICAP : Hubei_EUR 
#---------------------------------------
# Create the Dataframe by merging the ICAP and Clearblue dataframes
df <- merge(ICAP_df_xts, Clearblue_df_xts, by = "Date")
colnames(df)

# Keep the following columns

Research_Data <- df[, c("EUR_EUR", "NZ_EUR", "CCA...Front.December...ICE", "Hubei_EUR")]

head(df, 5)
head(Research_Data, 5)

# Upload Research Data to global environment
assign("Research_Data", Research_Data, envir = .GlobalEnv)

#---------------------------------------

####### Data Transformation ########
### Handling NAs 
#---------------------------------------
# Summarize NA presence in Research_Data
summary(is.na(Research_Data))

# Load necessary package
if (!require(zoo)) {
    install.packages("zoo")
}
library(zoo)

# Forward fill NA values (carry forward the last known value)
Research_Data_ffill <- na.locf(Research_Data)

summary(is.na(Research_Data_ffill))

# plot both the original and forward filled data on the same plot
# Convert xts objects to data frames, capturing Date indices
Research_Data_df <- data.frame(Date = index(Research_Data), coredata(Research_Data))
Research_Data_ffill_df <- data.frame(Date = index(Research_Data_ffill), coredata(Research_Data_ffill))

# Add a 'Type' column to distinguish between original and forward-filled data
Research_Data_df$Type <- "Original"
Research_Data_ffill_df$Type <- "Forward-Filled"

# Combine the data frames
combined_df <- rbind(Research_Data_df, Research_Data_ffill_df)

# Melt the data for plotting (if using ggplot2 and the data is wide)
if (!require(reshape2)) install.packages("reshape2")
library(reshape2)
combined_df_long <- melt(combined_df, id.vars = c("Date", "Type"), variable.name = "Variable", value.name = "Price")

# Plot using ggplot2
ggplot(combined_df_long, aes(x = Date, y = Price, color = Type, linetype = Type)) +
    geom_line() +
    facet_wrap(~ Variable, scales = "free_y") +  # Create a separate plot for each variable
    labs(title = "Comparison of Original vs Forward-Filled Data", x = "Date", y = "Price") +
    scale_color_manual(values = c("Original" = "blue", "Forward-Filled" = "red")) +
    theme_minimal()

# Subset into individual vectors

# Define the function
subsetAndCleanNA <- function(data) {
  # Validate input data type
  if (!inherits(data, c("data.frame", "xts"))) {
    stop("The data must be a data frame or an xts object.")
  }
  
  # Initialize a list to store cleaned data for each column
  cleaned_data_list <- list()
  
  # Get the number of columns
  num_columns <- ncol(data)
  
  # Loop over each column in the dataset
  for (i in 1:num_columns) {
    # Extract the column data, ensuring it remains an appropriate object
    column_data <- data[, i, drop = FALSE]  # Avoid dropping dimensions

    # Remove NA values
    clean_column_data <- column_data[complete.cases(column_data), , drop = FALSE]

    # Add the cleaned column to the list
    cleaned_data_list[[names(data)[i]]] <- clean_column_data
  }

  # Return the list of cleaned data
  return(cleaned_data_list)
}


# Assuming 'Research_Data_ffill' is an xts object that has been forward-filled
cleaned_datasets <- subsetAndCleanNA(Research_Data_ffill)

# Check results
lapply(cleaned_datasets, head)  # Display the head of each dataset in the list

#### Weekly Returns ####
#---------------------------------------

## From Paper: Weekly returns are calculated as the change in log price, from Friday-to-Friday. The continuously compounded returns of four sets 

#----------------------

# Install and load quantmod
if (!require(quantmod)) install.packages("quantmod", dependencies=TRUE)
library(quantmod)


# Applying log to all numeric columns directly in an xts object
Research_Data_log_prices <- log(Research_Data)

# Calculate daily returns
daily_returns <- Delt(x = Research_Data, k = 1, type = "arithmetic")  # or use dailyReturn(Research_Data)

# Resample the data to weekly frequency, ensuring it ends on Friday
weekly_data <- to.period(Research_Data, period = "weeks", indexAt = "lastof", k = 1, endof = "week", wday = 6)

# Calculate log returns using the endpoint values of the weekly data
weekly_log_returns <- diff(log(Cl(weekly_data)))  # Using diff(log()) directly for log returns

# Remove NA values from returns
weekly_log_returns <- na.omit(weekly_log_returns)

# Print the first few rows of returns to check
print(head(weekly_log_returns))

# Plot the weekly log returns
plot(weekly_log_returns, main = "Continuously Compounded Weekly Returns", col = "blue", ylab = "Log Returns")


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