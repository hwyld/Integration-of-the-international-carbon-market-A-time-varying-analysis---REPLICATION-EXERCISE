## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Transform data into weekly returns and volatility, run summary statistics
## Author: Henry Wyld
## Date of creation: 2024-03-31

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


# Install dplyr if it's not already installed
if (!require(dplyr)) {
    install.packages("dplyr")
    library(dplyr)
} else {
    library(dplyr)
}

# Also ensure that xts and readr are loaded
library(xts)
library(readr)

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

# From Paper "The main measure is the standard deviation of weekly return over the five-day interval during each week"
## NOT COMPLETE YET ##
#---------------------------------------
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
print(weekly_volatility_list[[1]])

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

# Compute summary statistics for each series, excluding NA values
summary_stats_returns <- sapply(Research_Data_weekly_returns, function(x) describe(x[!is.na(x)]))
summary_stats_volatility <- sapply(Research_Data_weekly_volatility, function(x) describe(x[!is.na(x)]))

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
          title = "Summary Statistics for ICAP Dataset",
          out= "table1.html")

stargazer(summary_stats_volatility, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Clearblue Dataset",
          out= "table2.html")

stargazer(valid_date_returns, type = "html", title = "Start and End Dates for ICAP Dataset",out= "table3.html")
stargazer(valid_date_volatility, type = "html", title = "Start and End Dates for Clearblue Dataset",out= "table4.html")

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