## ECOM90022 Research Methods, Semester 1, 2024
## Read the ICAP Price Explorer data and format in R
## Author: Henry Wyld
## Date of creation: 2024-03-21

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

# Set the working directory
setwd("C:/Users/henry/OneDrive - The University of Melbourne/Master of Applied Econometrics/2024/Semester 1/Research Methods/Research Paper/ICAP data")

# Read the CSV file
#df <- readr::read_csv("Raw ICAP Data trimmed.csv", locale = readr::locale(encoding = "UTF-8"))

# Read the Excel file
df <- readxl::read_excel("Raw ICAP Data.xlsx")

# Remove the column names and the first row
df <- df[-c(1:1), ]

head(df,5)

# Replace the Column names with the first row
colnames(df) <- df[1, ]

#### Extract the Allowance Price data ####
#-----------------------------------------

# Create a dataframe called allowance_price and import the the Date column first
allowance_price <- df[, 1]
nrow(allowance_price)
# Extract only the columns with Allowance price in the header of row 3
col_names <- df[2, ]

# Find the indices of the column names that contain "Allowance price"
indices <- grep("Allowance Price", col_names)

# Append only the columns with "Allowance price" in the name
for (i in indices) {
  allowance_price <- cbind(allowance_price, df[, i])
}

# Remove the first 2 rows
allowance_price <- allowance_price[-c(1:2), ]

# Rename the first column to "Date"
colnames(allowance_price)[1] <- "Date"

head(allowance_price,5)

# Convert the dataframe to a daily time series
allowance_price <- as.data.frame(allowance_price)

# Look at a few values in the 'Date' column
head(allowance_price$Date)

# Convert the Date column to a date format
allowance_price$Date <- as.Date(allowance_price$Date, format = "%d.%m.%Y")

# Convert the rest of the columns to numeric
for (i in 2:ncol(allowance_price)) {
  allowance_price[, i] <- as.numeric(allowance_price[, i])
}

#-----------------------------------------

#### Extract the EUR denominated Allowance Price data ####
#--------------------------------------------------------

# Create a dataframe called EUR_denom_allowance_prices and import the the Date column first
EUR_denom_allowance_prices <- df[, 1]

nrow(EUR_denom_allowance_prices)
nrow(df)
# Extract only the columns with _EUR in the header of row 3
col_names <- df[3, ]

# Find the indices of the column names that contains _EUR
indices <- grep("_EUR", col_names)

# Append only the columns with _EUR in the name
for (i in indices) {
  EUR_denom_allowance_prices <- cbind(EUR_denom_allowance_prices, df[, i])
}



# Remove the first 2 rows
EUR_denom_allowance_prices <- EUR_denom_allowance_prices[-c(1:2), ]


# Rename the column names to that of the first row
colnames(EUR_denom_allowance_prices) <- EUR_denom_allowance_prices[1, ]

head(EUR_denom_allowance_prices,5)

# Remove the first row
EUR_denom_allowance_prices <- EUR_denom_allowance_prices[-c(1:1), ]

# Convert the dataframe to a daily time series
EUR_denom_allowance_prices <- as.data.frame(EUR_denom_allowance_prices)

# Look at a few values in the 'Date' column
head(EUR_denom_allowance_prices$Date)

# Convert the Date column to a date format
EUR_denom_allowance_prices$Date <- as.Date(EUR_denom_allowance_prices$Date, format = "%d.%m.%Y")

# Convert the rest of the columns to numeric
for (i in 2:ncol(EUR_denom_allowance_prices)) {
  EUR_denom_allowance_prices[, i] <- as.numeric(EUR_denom_allowance_prices[, i])
}

head(EUR_denom_allowance_prices,5)

#--------------------------------------------------------


#### PLot the data - Allowance Price ####
#---------------------------------------
# Load required packages
library(tidyverse)

# Reshape the data to long format
allowance_price_long <- allowance_price %>% pivot_longer(-Date, names_to = "Variable", values_to = "Value")

# Plot the time series
ggplot(allowance_price_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_minimal()

#---------------------------------------

#### PLot the data - EUR denominated prices ####
#---------------------------------------

# Reshape the data to long format
EUR_denom_allowance_prices_long <- EUR_denom_allowance_prices %>% pivot_longer(-Date, names_to = "Variable", values_to = "Value")

# Plot the time series
ggplot(EUR_denom_allowance_prices_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_minimal()

#---------------------------------------

# Export as a CSV file
write.csv(allowance_price, "allowance_price.csv")
write.csv(EUR_denom_allowance_prices, "EUR_denom_allowance_prices.csv")

# Descriptive stats for both dataframes

str(allowance_price)
str(EUR_denom_allowance_prices)

# stop the script
#stop()
