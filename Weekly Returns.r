## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Transform data into weekly returns and volatility, run summary statistics
## Author: Henry Wyld
## Date of creation: 2024-03-31

## WEEKLY COLATILITY NEEDS CHECKING  - DIFF FROM PAPER##

#-------------------------------------
# clear memory
rm(list=ls())    
#----------------------------------

## Packages ##
#----------------------------------
# Source the package setup script
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Carbon-Markets"
setwd(Git)
source("Packages.R")

####### Import and Format Data #######
#---------------------------------------
# Read the CSV file
cleaned_datasets <- read_csv("Research_data.csv", locale = locale(encoding = "UTF-8"))

# Verify the structure of the data
print(head(cleaned_datasets))

# Convert dataframe to xts, assuming the first column after Date removal is the date
cleaned_datasets <- cleaned_datasets[, -1]  # Removes the first column
print(head(cleaned_datasets))

# Modify the function to explicitly use dplyr's select
convert_to_xts <- function(df, date_col_name, date_format = "%Y-%m-%d") {
    if (!date_col_name %in% names(df)) {
        stop("Date column specified does not exist in the dataframe: ", date_col_name)
    }
  
    df[[date_col_name]] <- as.Date(df[[date_col_name]], format = date_format)
  
    # Explicitly use dplyr's select function
    data_cols <- dplyr::select(df, -dplyr::all_of(date_col_name)) %>%
        mutate(across(everything(), as.numeric))
  
    xts_object <- xts(data_cols, order.by = df[[date_col_name]])
    return(xts_object)
}

# Convert data to xts using the function
cleaned_datasets_xts <- convert_to_xts(cleaned_datasets, "Date")
print(head(cleaned_datasets_xts))

# Define the function to calculate weekly returns
calculate_weekly_returns <- function(data) {
  aligned_data <- apply.weekly(data, FUN = last)
  returns <- diff(log(aligned_data))
  return(na.omit(returns))
}

# Initialize the weekly returns list with proper names
weekly_returns_list <- setNames(vector("list", ncol(cleaned_datasets_xts)), colnames(cleaned_datasets_xts))

# Calculate weekly returns
for (i in seq_along(weekly_returns_list)) {
  weekly_returns_list[[i]] <- calculate_weekly_returns(cleaned_datasets_xts[, i])
}

# Combine the weekly returns into a single xts object
weekly_returns <- do.call(merge, weekly_returns_list)
print(head(weekly_returns))

#---------------------------------------

#### Annualised Weekly Volatilty ####
## NOT COMPLETE YET ##
# From Paper "The main measure is the standard deviation of weekly return over the five-day interval during each week"

#---------------------------------------

# Assuming cleaned_datasets_xts is already loaded and is an xts object
daily_returns <- diff(log(cleaned_datasets_xts))  # calculate daily log returns

# Create a weekly index to group by trading weeks
weekly_index <- cut(index(daily_returns), breaks = "week", labels = FALSE)

# Add the weekly index to the daily_returns data frame
daily_returns <- cbind(as.data.frame(daily_returns), Week = weekly_index)

# Check the structure of daily_returns
str(daily_returns)

cleaned_datasets_xts <- convert_to_xts(daily_returns, "Date")

# Convert daily returns to an xts object
daily_returns_xts <- xts(daily_returns[, -ncol(daily_returns)], order.by = index(daily_returns))

# Calculate the average of the daily returns for each week and each market using aggregate 
avg_weekly_returns <- aggregate(daily_returns, by = list(Week = weekly_index), FUN = mean)

# Calculate the number of valid observations for each week and then subtract 1 to get the number of trading days
num_trading_days <- aggregate(daily_returns, by = list(Week = weekly_index), FUN = function(x) sum(!is.na(x)))

# Drop the last column from the avg_weekly_returns dataframe
avg_weekly_returns <- avg_weekly_returns[, -ncol(avg_weekly_returns)]
num_trading_days <- num_trading_days[, -ncol(num_trading_days)]

# Add 4 new columns for the average weekly returns in eahc market to the daily_returns dataframe,do not use merge
daily_returns$EUA_avg <- avg_weekly_returns$EUA[match(daily_returns$Week, avg_weekly_returns$Week)]
daily_returns$NZU_avg <- avg_weekly_returns$NZU[match(daily_returns$Week, avg_weekly_returns$Week)]
daily_returns$CCA_avg <- avg_weekly_returns$CCA[match(daily_returns$Week, avg_weekly_returns$Week)]
daily_returns$HBEA_avg <- avg_weekly_returns$HBEA[match(daily_returns$Week, avg_weekly_returns$Week)]

# Take squared difference between daily return and average weekly return
daily_returns$EUA_diff <- (daily_returns$EUA - daily_returns$EUA_avg)^2
daily_returns$NZU_diff <- (daily_returns$NZU - daily_returns$NZU_avg)^2
daily_returns$CCA_diff <- (daily_returns$CCA - daily_returns$CCA_avg)^2
daily_returns$HBEA_diff <- (daily_returns$HBEA - daily_returns$HBEA_avg)^2

# Put the squared differences into a new data frame
squared_diff <- daily_returns[, c("Week","EUA_diff", "NZU_diff", "CCA_diff", "HBEA_diff")]

# Sum the squared differences for each week and each market, keep the weekly index as the first column
sum_squared_diff <- aggregate(squared_diff[, -1], by = list(Week = squared_diff$Week), FUN = sum)

# Divide the sum of squared differences by the number of trading days minus 1 to get the variance, do not apply on the Week column keeping this index within teh dataframe
variance <- sum_squared_diff[, -1] / (num_trading_days[, -1] - 1)

# Take the square root of the variance to get the standard deviation
volatility <- sqrt(variance)

# add the weekly index to the volaility data frame
volatility <- cbind(as.data.frame(volatility), Week = sum_squared_diff$Week)

# Ensure the volatility is still an xts object
volatility_xts <- xts(volatility, order.by = index(weekly_index))

#### --- 
# Assuming cleaned_datasets_xts is already loaded and is an xts object
daily_returns <- diff(log(cleaned_datasets_xts))  # calculate daily log returns

# Create a weekly index to group by trading weeks
weekly_index <- cut(index(daily_returns), breaks = "week", labels = FALSE)

# Add the weekly index to the daily_returns data frame
daily_returns <- cbind(as.data.frame(daily_returns), Week = weekly_index)

# Calculate the average of the daily returns for each week and each market using aggregate
avg_weekly_returns <- aggregate(daily_returns[, -ncol(daily_returns)], by = list(Week = weekly_index), FUN = mean)

# Calculate the number of valid observations for each week and then subtract 1 to get the number of trading days
num_trading_days <- aggregate(daily_returns[, -ncol(daily_returns)], by = list(Week = weekly_index), FUN = function(x) sum(!is.na(x)))

# Ensure to remove extra columns if any exist after aggregation
avg_weekly_returns <- avg_weekly_returns[, -ncol(avg_weekly_returns), drop = FALSE]
num_trading_days <- num_trading_days[, -ncol(num_trading_days), drop = FALSE]

# Assuming daily_returns is an xts object or convert it back to xts with correct indexing
daily_returns_xts <- xts(daily_returns[, -ncol(daily_returns)], order.by = index(daily_returns))

# Match and add average weekly returns to the daily_returns data frame
for (market in colnames(daily_returns_xts)) {
    avg_col_name <- paste(market, "avg", sep = "_")
    daily_returns_xts[[avg_col_name]] <- avg_weekly_returns[[market]][match(daily_returns_xts$Week, avg_weekly_returns$Week)]
    
    # Calculate squared differences
    diff_col_name <- paste(market, "diff", sep = "_")
    daily_returns_xts[[diff_col_name]] <- (daily_returns_xts[[market]] - daily_returns_xts[[avg_col_name]])^2
}

# Aggregate squared differences to sum them up per week per market
sum_squared_diff <- sapply(colnames(daily_returns_xts)[grepl("diff", colnames(daily_returns_xts))], function(col) {
    tapply(daily_returns_xts[[col]], daily_returns_xts$Week, sum, na.rm = TRUE)
}, simplify = "data.frame")

# Calculate the variance
variance <- sum_squared_diff / (num_trading_days$V1 - 1)

# Take the square root of the variance to get the standard deviation
volatility <- sqrt(variance)

# Convert the volatility into an xts object
volatility_xts <- xts(volatility, order.by = as.Date(names(sum_squared_diff$EUA_diff), format = "%Y-%U"))

# Print results to verify
print(head(volatility_xts))

#---------------------------------------




# Function to calculate daily returns and more detailed weekly statistics
calculate_detailed_daily_returns <- function(data) {
  # Calculate daily logarithmic returns
  daily_returns <- diff(log(data))  # log returns

  return(detailed_stats)
}

detailed_returns_stats <- calculate_detailed_daily_returns(cleaned_datasets_xts)

# Calculate standard deviation of weekly return over the five-day interval during each week
calculate_weekly_volatility <- function(data) {
  # Assume data is weekly returns; calculating standard deviation as a measure of volatility
  weekly_vol <- apply.weekly(data, FUN = function(x) sd(x, na.rm = TRUE))
  return(weekly_vol)
}

# Initialize the weekly volatility list directly with proper names
weekly_volatility_list <- vector("list", length = ncol(cleaned_datasets_xts))
names(weekly_volatility_list) <- colnames(cleaned_datasets_xts)

#  Loop through each column in cleaned_datasets_xts to calculate weekly volatility
for (i in seq_along(weekly_volatility_list)) {
  # Access the column as an xts object
  individual_series <- cleaned_datasets_xts[, i]  # Ensure access by column index to keep xts format
  # Calculate weekly volatility for the series
  weekly_volatility_list[[i]] <- calculate_weekly_volatility(individual_series)
}

# Optionally, check the results for the first series
tail(weekly_volatility_list)

# Annualise the weekly volatility
annualised_weekly_volatility <- lapply(weekly_volatility_list, function(x) x * sqrt(52))

# Combine the weekly volatility into a single xts object
weekly_volatility <- do.call(merge, annualised_weekly_volatility)

head(weekly_volatility, 5)
#---------------------------------------

### SUBSET DATA ###
# Trim the data to only include the dates from 2014-04-30 to 2021-12-01 (inclusive)
# Wednesday 30th April 2014 to Wednesday 1st December 2021
Research_Data_weekly_returns <- weekly_returns["2014-04-30/2021-12-01"]
Research_Data_weekly_volatility <- weekly_volatility["2014-04-30/2021-12-01"]

last(Research_Data_weekly_returns, 5)
last(Research_Data_weekly_volatility, 5)

# Study has 397 observations
nrow(Research_Data_weekly_returns)
nrow(Research_Data_weekly_volatility)


#### Descriptive statistics ####
## From Paper: "The descriptive statistics of the weekly returns and weekly volatility are presented in Table 2 in Panel A and B."

####### Summary Statistics #######
#---------------------------------------
# Function to find start and end dates excluding NA
get_valid_dates <- function(series) {
  valid_dates <- index(series[!is.na(series)])  # Get dates for non-NA values
  start_date <- format(min(valid_dates), "%Y-%m-%d")
  end_date <- format(max(valid_dates), "%Y-%m-%d")
  return(c(Start = start_date, End = end_date))
}

# Apply the function to each column
valid_date_returns <- sapply(Research_Data_weekly_returns, get_valid_dates)
valid_date_volatility <- sapply(Research_Data_weekly_volatility, get_valid_dates)

# Print the structure and a summary of weekly returns to check for any anomalies
print(str(Research_Data_weekly_returns))
summary(Research_Data_weekly_returns)

# count the number of NA values in the weekly returns
sapply(Research_Data_weekly_returns, function(x) sum(is.na(x)))

library(psych)

# Safely apply describe to ensure it doesn't fail silently
safe_describe <- function(x) {
  if (length(x) > 0 && is.numeric(x)) {
    describe(x)
  } else {
    return(NULL)  # or alternatively return a list with NA values for expected metrics
  }
}

# Compute summary statistics for each series, excluding NA values
summary_stats_returns <- sapply(Research_Data_weekly_returns, safe_describe)
summary_stats_volatility <- sapply(Research_Data_weekly_volatility, safe_describe)

## Display the Summary Statistics
# Load knitr for table output
if (!require("knitr")) install.packages("knitr", dependencies=TRUE)
library(knitr)

# Display tables
sapply(summary_stats_returns, knitr::kable)
sapply(summary_stats_volatility, knitr::kable)

# Creating and printing tables for statistics
kable(summary_stats_returns, caption = "Summary Statistics for ICAP Dataset")
kable(summary_stats_volatility, caption = "Summary Statistics for Clearblue Dataset")

# Creating and printing tables for dates
kable(valid_date_returns, caption = "Start and End Dates for ICAP Dataset")
kable(valid_date_volatility, caption = "Start and End Dates for Clearblue Dataset")

# Export the Tables with stargazer
#install.packages("stargazer")
library(stargazer)

# Export and save the tables to HTML
length(summary_stats_returns)

# Round to 3 decimal places
# Rounding only numeric columns in a data frame
summary_stats_returns[] <- lapply(summary_stats_returns, function(x) {
  if (is.numeric(x)) round(x, 3) else x
})

summary_stats_volatility[] <- lapply(summary_stats_volatility, function(x) {
  if (is.numeric(x)) round(x, 3) else x
})

stargazer(summary_stats_returns, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Returns",
          out= "Summary Statistics for Returns.html")

stargazer(summary_stats_volatility, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Volatility",
          out= "Summary Statistics for Volatility.html")

stargazer(valid_date_returns, type = "html", title = "Start and End Dates for Returns",out= "Dates Returns.html")
stargazer(valid_date_volatility, type = "html", title = "Start and End Dates for Clearblue Volatility",out= "Dates Returns.html")

# Plot the data

# Plot the weekly returns as 4 separate charts for each series but merge them into one file
# Convert xts objects to data frames, capturing Date indices
Research_Data_continuously_compounded_weekly_returns <- data.frame(Date = index(Research_Data_weekly_returns), coredata(Research_Data_weekly_returns))

# Melt the data for plotting (if using ggplot2 and the data is wide)
Research_Data_continuously_compounded_weekly_returns_long <- melt(Research_Data_continuously_compounded_weekly_returns, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Return")

# Create 4 separate plots for each series and save them in the save file as a 2 by 2 grid
# Plot the weekly returns for each series
ggplot(Research_Data_continuously_compounded_weekly_returns_long, aes(x = Date)) +
  geom_line(aes(y = Weekly_Return, color = Series)) +
  labs(title = "Weekly Returns",
       x = "Date",
       y = "Weekly Return") +
  scale_color_manual(values = c("EUR_EUR" = "blue", "NZ_EUR" = "red", "CCA...Front.December...ICE" = "green", "Hubei_EUR" = "purple")) +
  facet_wrap(~ Series, scales = "free_y") +  # Create a separate plot for each series
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



# Plot the weekly volatility as 4 separate charts for each series but merge them into one file
# Convert xts objects to data frames, capturing Date indices
Research_Data_annualised_weekly_volatility <- data.frame(Date = index(Research_Data_weekly_volatility), coredata(Research_Data_weekly_volatility))

# Melt the data for plotting (if using ggplot2 and the data is wide)
Research_Data_annualised_weekly_volatility_long <- melt(Research_Data_annualised_weekly_volatility, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Volatility")

# Create 4 separate plots for each series and save them in the save file as a 2 by 2 grid
# Plot the weekly volatility for each series
ggplot(Research_Data_annualised_weekly_volatility_long, aes(x = Date)) +
  geom_line(aes(y = Weekly_Volatility, color = Series)) +
  labs(title = "Weekly Volatility",
       x = "Date",
       y = "Weekly Volatility") +
  scale_color_manual(values = c("EUR_EUR" = "blue", "NZ_EUR" = "red", "CCA...Front.December...ICE" = "green", "Hubei_EUR" = "purple")) +
  facet_wrap(~ Series, scales = "free_y") +  # Create a separate plot for each series
  theme_minimal()
  

# Save plots together in one file
ggsave("Weekly_Volatility_Plot.png", bg = "white")