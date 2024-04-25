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

####### Import and Format Data #######
#---------------------------------------
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

# Use ICAP_df and Clearblue_df to create xts objects
ICAP_df_xts <- convert_to_xts(ICAP_df, "Date")
Clearblue_df_xts <- convert_to_xts(Clearblue_df, "Date")

# Remove the first column of ICAP_df
ICAP_df_xts <- ICAP_df_xts[, -1]

head(ICAP_df_xts, 5)
head(Clearblue_df_xts, 5)

#---------------------------------------

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

## Display the Summary Statistics
# Load knitr for table output
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

####### Create the Research Data #######
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

### Handling Missing Data/NAs ###
## Forward Fill NA values ##
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

## Subset into individual vectors ##
# Define the function
subsetAndCleanNA <- function(data) {
  if (!inherits(data, c("data.frame", "xts"))) {
    stop("The data must be a data frame or an xts object.")
  }
  
  cleaned_data_list <- list()
  num_columns <- ncol(data)
  
  for (i in 1:num_columns) {
    column_data <- data[, i, drop = FALSE]
    clean_column_data <- column_data[complete.cases(column_data), , drop = FALSE]
    cleaned_data_list[[names(data)[i]]] <- clean_column_data
  }
  
  return(cleaned_data_list)
}

# Assuming 'Research_Data_ffill' is an xts object that has been forward-filled
cleaned_datasets <- subsetAndCleanNA(Research_Data_ffill)
lapply(cleaned_datasets, head)  # Display the head of each dataset in the list


#### Weekly Returns ####
## From Paper: Weekly returns are calculated as the change in log price, from Friday-to-Friday. The continuously compounded returns of four sets 
#---------------------------------------

if (!require(quantmod)) install.packages("quantmod", dependencies=TRUE)
library(quantmod)

calculate_weekly_returns <- function(data) {
  aligned_data <- apply.weekly(data, FUN = last)
  returns <- diff(log(aligned_data))
  return(na.omit(returns))
}

# Initialize the weekly returns list directly with proper names
weekly_returns_list <- vector("list", length = length(cleaned_datasets))
names(weekly_returns_list) <- names(cleaned_datasets)

# Loop through each column in cleaned_datasets to calculate weekly returns
for (i in seq_along(cleaned_datasets)) {
  # Extract column as an individual xts object
  individual_series <- cleaned_datasets[[i]]  # Access the correct list element
  # Calculate weekly returns for the series
  weekly_returns_list[[i]] <- calculate_weekly_returns(individual_series)
}

# Check if you've loaded the zoo package for plotting
if (!require(zoo)) install.packages("zoo")
library(zoo)

# Example of plotting the first series' returns if not empty
if (length(weekly_returns_list[[1]]) > 0) {
  plot(as.zoo(weekly_returns_list[[1]]), main = "Weekly Logarithmic Returns", xlab = "Date", ylab = "Log Returns", col = "blue")
} else {
  cat("No data to plot for the first series.")
}

# Combine the weekly returns into a single xts object
weekly_returns <- do.call(merge, weekly_returns_list)

head(weekly_returns, 5)


### SUBSET DATA ###
# Trim the data to only include the dates from 2014-04-30 to 2021-12-01 (inclusive)
# Wednesday 30th April 2014 to Wednesday 1st December 2021
Research_Data_weekly_returns <- weekly_returns["2014-04-30/2021-12-01"]

last(Research_Data_weekly_returns, 5)

# Study has 397 observations
nrow(Research_Data_weekly_returns)

#---------------------------------------

#### Volatilty ####
# From Paper "The main measure is the standard deviation of weekly return over the five-day interval during each week"

#---------------------------------------
# Calculate standard deviation of weekly return over the five-day interval during each week
calculate_weekly_volatility <- function(data) {
  weekly_volatility <- apply.weekly(data, FUN = sd)
  return(weekly_volatility)
}

# Initialize the weekly volatility list directly with proper names
weekly_volatility_list <- vector("list", length = length(cleaned_datasets))
names(weekly_volatility_list) <- names(cleaned_datasets)

# Loop through each column in cleaned_datasets to calculate weekly volatility
for (i in seq_along(cleaned_datasets)) {
  # Extract column as an individual xts object
  individual_series <- cleaned_datasets[[i]]  # Access the correct list element
  # Calculate weekly volatility for the series
  weekly_volatility_list[[i]] <- calculate_weekly_volatility(individual_series)
}

# Combine the weekly volatility into a single xts object
weekly_volatility <- do.call(merge, weekly_volatility_list)

head(weekly_volatility, 5)
#---------------------------------------


#### Descriptive statistics ####
## From Paper: "The descriptive statistics of the weekly returns and weekly volatility are presented in Table 2 in Panel A and B."

#---------------------------------------
# Function to find start and end dates excluding NA
Research_Data_dates <- sapply(Research_Data_weekly_returns, get_valid_dates)

# Compute summary statistics for each series, excluding NA values
summary_stats <- sapply(Research_Data_weekly_returns, function(x) describe(x))

# Round to 3 decimal places
# Rounding only numeric columns in a data frame
summary_stats[] <- lapply(summary_stats, function(x) {
  if (is.numeric(x)) round(x, 3) else x
})

## Display the Summary Statistics
# Load knitr for table output
if (!require("knitr")) install.packages("knitr", dependencies=TRUE)
library(knitr)

# Display tables
sapply(summary_stats, knitr::kable)

# Creating and printing tables for statistics
kable(summary_stats, caption = "Summary Statistics for Carbon Price Weekly Return")

# Creating and printing tables for dates
kable(Research_Data_dates, caption = "Start and End Dates for each Carbon Price Weekly Return Dataset")

# Export the Tables with stargazer

stargazer(summary_stats, 
          type = "html", 
          digits = 3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Carbon Price Weekly Return",
          out= "Summary Statistics for Carbon Price Weekly Return.html")

stargazer(Research_Data_dates,
         type = "html", 
         title = "Start and End Dates for each Carbon Price Weekly Return Dataset", # nolint
         out= "Start and End Dates for each Carbon Price Weekly Return Dataset.html")

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