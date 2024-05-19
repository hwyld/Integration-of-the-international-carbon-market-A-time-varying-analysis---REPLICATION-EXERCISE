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

####### Calculate Weekly Returns #######
#---------------------------------------

# Define the function to calculate weekly returns from Friday-to-Friday.
# The function assumes the data is already in xts format.
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

## Double Check Friday to Friday - Can remove ##
#---------------------------------------
# Define a custom function to calculate weekly returns from Friday-to-Friday
calculate_weekly_returns <- function(data) {
  # Ensure that the 'data' is an xts object and has a proper Date index
  if (!inherits(data, "xts")) stop("Data must be an xts object")

 # Find indexes for all Fridays within the data range
  fridays <- which(format(index(data), "%A") == "Friday")

  # Make sure it starts and ends with Fridays (if not, adjust accordingly)
  if (length(fridays) > 1) {
    aligned_data <- data[fridays]

    # Calculate weekly returns using logarithmic differences between consecutive Fridays
    returns <- diff(log(aligned_data))
  } else {
    returns <- xts()  # Return an empty xts object if there are not enough Fridays
  }

  return(na.omit(returns))
}

# Initialize the weekly returns list with proper names
weekly_returns_list <- setNames(vector("list", ncol(cleaned_datasets_xts)), colnames(cleaned_datasets_xts))

# Calculate weekly returns
for (i in seq_along(weekly_returns_list)) {
  weekly_returns_list[[i]] <- calculate_weekly_returns(cleaned_datasets_xts[, i])
}

# Combine the weekly returns into a single xts object
weekly_returns1 <- do.call(merge, weekly_returns_list)
print(head(weekly_returns1))
print(head(weekly_returns))

# Save weekly index for later use
weekly_index_returns <- cut(index(weekly_returns), breaks = "week", labels = FALSE)
#---------------------------------------

#### Annualised Weekly Volatilty #### 
# From Paper "The main measure is the standard deviation of weekly return over the five-day interval during each week"
#---------------------------------------
# Assuming cleaned_datasets_xts is already loaded and is an xts object
daily_returns_xts <- diff(log(cleaned_datasets_xts))  # calculate daily log returns

# Create a weekly index to group by trading weeks Friday to Friday
weekly_index <- cut(index(daily_returns_xts), breaks = "week", labels = FALSE)

# Save daily index for later use
daily_index <- index(daily_returns_xts)

# Add the weekly index to the daily_returns data frame
daily_returns <- cbind(as.data.frame(daily_returns_xts), Week = weekly_index)

# Check the structure of daily_returns
str(daily_returns)

# Convert daily_returns to an xts object
#daily_returns <- as.xts(daily_returns, order.by = daily_index)

# Calculate the average of the daily returns for each week and each market using aggregate 
avg_weekly_returns <- aggregate(daily_returns, by = list(Week = weekly_index), FUN = mean)

# Calculate the number of valid observations for each week
num_trading_days <- aggregate(!is.na(daily_returns), by = list(Week = weekly_index), FUN = sum)

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
sum_squared_diff <- aggregate(squared_diff, by = list(Week = weekly_index), FUN = sum)

# Remove column 2 from the sum_squared_diff dataframe
sum_squared_diff <- sum_squared_diff[, -2]

# Divide the sum of squared differences by the number of trading days minus 1 to get the variance, do not apply on the Week column keeping this index within teh dataframe
variance <- sum_squared_diff[, -1] / (num_trading_days[, -1] - 1)

# Take the square root of the variance to get the standard deviation
volatility <- sqrt(variance)

# Annualise the volatility by multiplying 100 by sqrt(52)
volatility <- volatility * 100 * sqrt(52)

# Retrieve the weekly dates for use in the zoo object
week_dates <- as.Date(names(num_trading_days$Week), format = "%Y-%U")

# Calculate the first day of each week for indexing
first_day_of_week <- tapply(daily_index, weekly_index, min)

# Convert 'first_day_of_week' to Date class if not already
first_day_of_week <- as.Date(first_day_of_week)

tail(first_day_of_week)
tail(weekly_returns)

# Create an xts object for variance
variance <- xts(variance, order.by = first_day_of_week)
volatility <- xts(volatility, order.by = first_day_of_week)

#---------------------------------------

# Simplified weekly volatility calculation
#---------------------------------------
# Calculate the standard deviation of weekly returns over the five-day interval during each week

# Calculate standard deviation for each column individually using the TTR package
#volatility_list <- lapply(weekly_returns, function(column_data) {
#  runSD(x = column_data, n = 5, sample = TRUE, cumulative = FALSE)
#})

# Convert the list back to an xts object if you want to keep all results together
#volatility_2 <- do.call(merge, volatility_list)

# Convert to a data frame
#vol_2 <- data.frame(Date = index(volatility), Volatility = coredata(volatility))

#last(vol_2, 15)
#last(volatility, 15)
#---------------------------------------

### SUBSET DATA ###
#---------------------------------------
# Trim the data to only include the dates from 2014-04-30 to 2021-12-01 (inclusive)
# Wednesday 30th April 2014 to Wednesday 1st December 2021
Research_Data_weekly_returns <- weekly_returns["2014-04-30/2021-12-08"]
Research_Data_weekly_volatility <- volatility["2014-04-30/2021-12-08"]

last(Research_Data_weekly_returns, 5)
last(Research_Data_weekly_volatility, 5)

# Check for any missing values
sum(is.na(Research_Data_weekly_returns))
sum(is.na(Research_Data_weekly_volatility))

# Fill missing values with NA using sample mean (2014-05-02 to 2015-01-09, 37 missing values)
Research_Data_weekly_returns <- apply(Research_Data_weekly_returns, 2, function(x) {
  na_index <- is.na(x)
  x[na_index] <- mean(x, na.rm = TRUE)
  x
})

Research_Data_weekly_volatility <- apply(Research_Data_weekly_volatility, 2, function(x) {
  na_index <- is.na(x)
  x[na_index] <- mean(x, na.rm = TRUE)
  x
})

# Study has 397 observations
nrow(Research_Data_weekly_returns)
nrow(Research_Data_weekly_volatility)

# Change Column Names to match the paper; EU ETS, NZ ETS, CA CaT, HB ETS
colnames(Research_Data_weekly_returns) <- c("EU ETS", "NZ ETS", "CA CaT", "HB ETS")

# Rename the Column Names from Research_Data_weekly_volatility to match Research_Data_weekly_returns
colnames(Research_Data_weekly_volatility) <- colnames(Research_Data_weekly_returns)

# Convert both datasets as xts objects using the index from the row names
Research_Data_weekly_returns <- xts(Research_Data_weekly_returns, order.by = as.Date(rownames(Research_Data_weekly_returns)))
Research_Data_weekly_volatility <- xts(Research_Data_weekly_volatility, order.by = as.Date(rownames(Research_Data_weekly_volatility)))

#---------------------------------------

#### Descriptive statistics ####
## From Paper: "The descriptive statistics of the weekly returns and weekly volatility are presented in Table 2 in Panel A and B."
#---------------------------------------
# Function to get the date range of valid values using the xts object's index
get_valid_dates <- function(series) {
  start_date <- index(series)[1]
  end_date <- index(series)[length(index(series))]
  return(c(Start_Date = start_date, End_Date = end_date))
}
# Apply the function to each column
valid_date_returns <- sapply(Research_Data_weekly_returns, get_valid_dates)
valid_date_volatility <- sapply(Research_Data_weekly_volatility, get_valid_dates)

# Print the structure and a summary of weekly returns to check for any anomalies
print(str(Research_Data_weekly_returns))
summary(Research_Data_weekly_returns)

# count the number of NA values in the weekly returns
#sapply(Research_Data_weekly_returns, function(x) sum(is.na(x)))

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

## ADF test ##
# Function to perform ADF test and extract the test statistic
perform_adf_test <- function(series) {
  test_result <- ur.df(series, type = "drift", selectlags = "BIC")
  return(test_result@teststat[1]) # Extract the test statistic
}

# Apply ADF test to each column in the returns and volatility datasets
adf_results_returns <- sapply(Research_Data_weekly_returns, perform_adf_test)
adf_results_volatility <- sapply(Research_Data_weekly_volatility, perform_adf_test)

# Adding ADF test results to summary statistics
summary_stats_returns <- rbind(summary_stats_returns, ADF = adf_results_returns)
summary_stats_volatility <- rbind(summary_stats_volatility, ADF = adf_results_volatility)

# Transpose to match required format
summary_stats_returns <- t(summary_stats_returns)
summary_stats_volatility <- t(summary_stats_volatility)

# Round to 3 decimal places
# Rounding only numeric columns in a data frame
summary_stats_returns[] <- lapply(summary_stats_returns, function(x) {
  if (is.numeric(x)) round(x, 3) else x
})

summary_stats_volatility[] <- lapply(summary_stats_volatility, function(x) {
  if (is.numeric(x)) round(x, 3) else x
})

# Trim each data frame to only include the relevant columns; Mean, Min, Max, St Dev, Skew, Kurt, ADF Test Statistic
summary_stats_returns <- summary_stats_returns[, c("mean", "min", "max", "sd", "skew", "kurtosis", "ADF")]
summary_stats_volatility <- summary_stats_volatility[, c("mean", "min", "max", "sd", "skew", "kurtosis", "ADF")]

# Rename the columns to match the paper
colnames(summary_stats_returns) <- c("Mean", "Min", "Max", "St.dev.", "Skew.", "Kurt.", "ADF")
colnames(summary_stats_volatility) <- c("Mean", "Min", "Max", "St.dev.", "Skew.", "Kurt.", "ADF")

# Save as data frames
summary_stats_returns <- as.data.frame(summary_stats_returns)
summary_stats_volatility <- as.data.frame(summary_stats_volatility)

# Add a Series column to match with the source article data
summary_stats_returns$Series <- rownames(summary_stats_returns)
summary_stats_volatility$Series <- rownames(summary_stats_volatility)

# Create data frames for returns and volatility series
source_article_returns <- data.frame(
  Series = c("EU ETS", "NZ ETS", "CA CaT", "HB ETS"),
  Mean = c(0.007, 0.008, 0.003, 0.001),
  Min = c(-0.312, -0.112, -0.326, -0.437),
  Max = c(0.243, 0.232, 0.202, 0.342),
  St.dev. = c(0.060, 0.036, 0.028, 0.063),
  Skew. = c(-0.312, 1.944, -3.064, -0.764),
  Kurt. = c(5.996, 12.041, 59.992, 17.080),
  ADF = c(-14.37, -10.27, -12.74, -18.74)
)

source_article_volatility <- data.frame(
  Series = c("EU ETS", "NZ ETS", "CA CaT", "HB ETS"),
  Mean = c(16.734, 7.167, 4.842, 15.604),
  Min = c(1.368, 0.690, 0.819, 0.002),
  Max = c(78.881, 75.503, 57.247, 56.364),
  St.dev. = c(10.081, 7.474, 5.266, 12.531),
  Skew. = c(1.782, 4.350, 5.718, 1.098),
  Kurt. = c(8.622, 29.793, 46.145, 3.557),
  ADF = c(-4.07, -5.74, -4.09, -5.45)
)

# Combine the summary statistics with the source article data
combine_summary_stats <- function(summary_stats, source_article) {
  # Join the data frames by Series column
  combined_data <- merge(summary_stats, source_article, by = "Series", suffixes = c("_Summary", "_Source"))
  return(combined_data)
}

# Combine returns and volatility data
combined_returns <- combine_summary_stats(summary_stats_returns, source_article_returns)
combined_volatility <- combine_summary_stats(summary_stats_volatility, source_article_volatility)

# Create a new data frame that removes all series except
comparison_means_returns <- combined_returns[, c("Series", "Mean_Summary", "Mean_Source")]
comparison_means_volatility <- combined_volatility[, c("Series", "Mean_Summary", "Mean_Source")]

# Start and End Dates for Returns and Volatility using the indexes from the xts objects
#valid_date_returns <- data.frame(Start_Date = index(Research_Data_weekly_returns)[1], End_Date = index(Research_Data_weekly_returns)[length(index(Research_Data_weekly_returns))])
#valid_date_volatility <- data.frame(Start_Date = index(Research_Data_weekly_volatility)[1], End_Date = index(Research_Data_weekly_volatility)[length(index(Research_Data_weekly_volatility))])

# Drop the Series column from the summary statistics dataframes
summary_stats_returns <- summary_stats_returns[, -8]
summary_stats_volatility <- summary_stats_volatility[, -8]

# Add Source description
source_description <- "Source: Authors' calculations based on data from ICAP for carbon prices series from EU ETS, NZ ETS, and HB ETS, and Clearblue Markets and Refinitiv for CA CaT. Data sampled from Friday May 2, 2014, to Friday December 3, 2021 (inclusive of source data accounting for Friday to Friday returns.). The ADF test is the Augmented Dickey-Fuller test for unit roots in the time series data with lag length determined by the BIC criterion. The p-value is the probability of observing the test statistic if the null hypothesis of a unit root is true. The null hypothesis is rejected if the p-value is less than 0.01."
#Source: Own elaboration based on data from Bloomberg, Reuters, and Wind Database. Note: Sample including carbon prices series from EU ETS, NZ ETS, CA CaT, and HB ETS from April 30, 2014, to December 1, 2021. The hypothesis of the Augmented Dicky Fuller (ADF) test is H0: non-stationary against H1: stationary. The lag length is determined by BIC criterion. *** denotes significance at 1 % level

# transpose the data frames
summary_stats_returns <- t(t(summary_stats_returns))
summary_stats_volatility <- t(t(summary_stats_volatility))
comparison_means_returns <- t(t(comparison_means_returns))
comparison_means_volatility <- t(t(comparison_means_volatility))

## Export the tables to HTML
stargazer(summary_stats_returns, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Returns",
          out= "Summary Statistics for Returns.html",
          notes = source_description
        )

stargazer(summary_stats_volatility, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Volatility",
          out= "Summary Statistics for Volatility.html",
          notes = source_description)

# Add the source description to the comparison tables
source_description_Comparison <- "Note: Summary represents data from this replication exercise. Source represents data taken from Lyu and Scholtens (2022)."

stargazer(comparison_means_returns, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Comparison of Means for Returns",
          out= "Comparison of Means for Returns.html",
          notes = source_description_Comparison
        )

stargazer(comparison_means_volatility, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Comparison of Means for Volatility",
          out= "Comparison of Means for Volatility.html",
          notes = source_description_Comparison
        )

#---------------------------------------

### Plot the data ###
#---------------------------------------
# Plot the weekly returns as 4 separate charts for each series but merge them into one file
# Convert xts objects to data frames, capturing Date indices
Research_Data_continuously_compounded_weekly_returns <- data.frame(Date = index(Research_Data_weekly_returns), coredata(Research_Data_weekly_returns))

# Match Column Names to the paper
colnames(Research_Data_continuously_compounded_weekly_returns) <- c('Date',colnames(Research_Data_weekly_returns))

# Melt the data for plotting (if using ggplot2 and the data is wide)
Research_Data_continuously_compounded_weekly_returns_long <- melt(Research_Data_continuously_compounded_weekly_returns, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Return")

# Create 4 separate plots for each series and save them in the save file as a 2 by 2 grid
# Plot the weekly returns for each series
a <- ggplot(Research_Data_continuously_compounded_weekly_returns_long, aes(x = Date)) +
  geom_line(aes(y = Weekly_Return, color = Series)) +
  labs(title = "Weekly Returns",
       x = "Date",
       y = "Volatility") +
  scale_color_manual(values = c("EUA" = "blue", "NZU" = "red", "CCA" = "green", "HBEA" = "purple")) +
  facet_wrap(~ Series, scales = "free_y") +  # Create a separate plot for each series
  theme_minimal()


# Save plots together in one file
#ggsave("Weekly_Returns_Plot.png", bg = "white")  

# Convert the ggplot object to a plotly object
plotly::ggplotly(a)

# Seperate plots for each series
# Create 4 separate plots for each series and save them in the same file as a 2 by 2 grid
plot <- ggplot(Research_Data_continuously_compounded_weekly_returns_long, aes(x = Date, y = Weekly_Return)) +
  geom_line(aes(color = Series), size = 0.7) +
  facet_wrap(~ Series, scales = "free", ncol = 2) +  # Create a 2 by 2 grid
  labs(x = "Date", y = "Volatility") +
  scale_color_manual(values = c("EU ETS" = "blue", "NZ ETS" = "green", "CA CaT" = "brown", "HB ETS" = "black")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set x-axis to show yearly ticks
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),  # Remove grid lines
    axis.title = element_text(size = 16),  # Increase size of axis titles
    axis.text = element_text(size = 14),  # Increase size of axis labels
    strip.text = element_text(size = 16, face = "bold", hjust = 0),  # Left-align series labels
    axis.line.x.bottom = element_line(color = "black", size = 0.5),  # Add x-axis line
    axis.line.y.left = element_line(color = "black", size = 0.5),  # Add y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Length of the ticks
  )

# Save plots together in one file
ggsave("Weekly_Returns_Plot_Combo.png", plot = plot, width = 12, height = 8, bg = "white")
#---------------------------------------

#### Plot Weekly Volatility ####
#---------------------------------------
# Plot the weekly volatility as 4 separate charts for each series but merge them into one file
# Convert xts objects to data frames, capturing Date indices
Research_Data_annualised_weekly_volatility <- data.frame(Date = index(Research_Data_weekly_volatility), coredata(Research_Data_weekly_volatility))

# Match Column Names to the paper
colnames(Research_Data_annualised_weekly_volatility) <- c('Date',colnames(Research_Data_weekly_volatility))

# Melt the data for plotting (if using ggplot2 and the data is wide)
Research_Data_annualised_weekly_volatility_long <- melt(Research_Data_annualised_weekly_volatility, id.vars = "Date", variable.name = "Series", value.name = "Weekly_Volatility")

# Create 4 separate plots for each series and save them in the save file as a 2 by 2 grid
# Plot the weekly volatility for each series
p <- ggplot(Research_Data_annualised_weekly_volatility_long, aes(x = Date)) +
  geom_line(aes(y = Weekly_Volatility, color = Series)) +
  labs(title = "Weekly Volatility",
       x = "Date",
       y = "Weekly Volatility") +
  scale_color_manual(values = c("EUR_EUR" = "blue", "NZ_EUR" = "red", "CCA...Front.December...ICE" = "green", "Hubei_EUR" = "purple")) +
  facet_wrap(~ Series, scales = "free_y") +  # Create a separate plot for each series
  theme_minimal()

# Save plots together in one file
#ggsave("Weekly_Volatility_Plot.png", bg = "white")

# Save plots as plotly interactive plots
# Convert the ggplot object to a plotly object
plotly::ggplotly(p)

# Seperate plots for each series
# Create 4 separate plots for each series and save them in the same file as a 2 by 2 grid
plot <- ggplot(Research_Data_annualised_weekly_volatility_long, aes(x = Date, y = Weekly_Volatility)) +
  geom_line(aes(color = Series), size = 0.7) +
  facet_wrap(~ Series, scales = "free", ncol = 2) +  # Create a 2 by 2 grid
  labs(x = "Date", y = "Volatility") +
  scale_color_manual(values = c("EU ETS" = "blue", "NZ ETS" = "green", "CA CaT" = "brown", "HB ETS" = "black")) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +  # Set x-axis to show yearly ticks
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),  # Remove grid lines
    axis.title = element_text(size = 16),  # Increase size of axis titles
    axis.text = element_text(size = 14),  # Increase size of axis labels
    strip.text = element_text(size = 16, face = "bold", hjust = 0),  # Left-align series labels
    axis.line.x.bottom = element_line(color = "black", size = 0.5),  # Add x-axis line
    axis.line.y.left = element_line(color = "black", size = 0.5),  # Add y-axis line
    axis.ticks = element_line(color = "black", size = 0.5),  # Add axis ticks
    axis.ticks.length = unit(0.2, "cm")  # Length of the ticks
  )

# Save plots together in one file
ggsave("Weekly_Volatility_Plot_Combo.png", plot = plot, width = 12, height = 8, bg = "white")

#---------------------------------------

## Data Export ##
#---------------------------------------

# Convert xts object to a data frame
Research_Data_weekly_returns_df <- as.data.frame(Research_Data_weekly_returns)
Research_Data_weekly_volatility_df <- as.data.frame(Research_Data_weekly_volatility)

# Add the index (date) as a column in the data frame
Research_Data_weekly_returns_df$Date <- index(Research_Data_weekly_returns)
Research_Data_weekly_volatility_df$Date <- index(Research_Data_weekly_volatility)

# Move the Date column to the first position
Research_Data_weekly_returns_df <- Research_Data_weekly_returns_df[, c(ncol(Research_Data_weekly_returns_df), 1:(ncol(Research_Data_weekly_returns_df)-1))]
Research_Data_weekly_volatility_df <- Research_Data_weekly_volatility_df[, c(ncol(Research_Data_weekly_volatility_df), 1:(ncol(Research_Data_weekly_volatility_df)-1))]

# Export the data to CSV files
write.csv(Research_Data_weekly_returns_df, "Research_Data_weekly_returns.csv", row.names = FALSE)
write.csv(Research_Data_weekly_volatility_df, "Research_Data_weekly_volatility.csv", row.names = FALSE)

#---------------------------------------