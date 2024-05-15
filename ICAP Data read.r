## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Read the ICAP Price Explorer data and format in R
## Author: Henry Wyld
## Date of creation: 2024-03-21

#-------------------------------------
# clear memory
rm(list = ls())
#----------------------------------

## Packages ##
#----------------------------------
# Source the package setup script
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Carbon-Markets"
setwd(Git)
source("Packages.R")

#----------------------------------

# Set the working directory
setwd(ICAP_Data)

# Read the Excel file
df <- readxl::read_excel("Raw ICAP Data.xlsx")

# Remove the column names and the first row
df <- df[-c(1:1), ]

head(df,5)

# Replace the Column names with the first row
colnames(df) <- df[1, ]

#### Extract the Allowance Price data in original currency ####
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

# Concatentate the first 3 rows to create the column names and call the column names 'Full Names'
col_names <- paste(allowance_price[1, ], allowance_price[2, ], allowance_price[3, ], sep = "_")

# Rename the column names to that of the first row
colnames(allowance_price) <- col_names

# Remove the first 3 rows
allowance_price <- allowance_price[-c(1:3), ]

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

# Create a dataframe called EUR_denom_allowance_prices and import the  Date column first
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

# Convert the Date column to a date format
EUR_denom_allowance_prices$Date <- as.Date(EUR_denom_allowance_prices$Date, format = "%d.%m.%Y")

# Convert the rest of the columns to numeric
for (i in 2:ncol(EUR_denom_allowance_prices)) {
  EUR_denom_allowance_prices[, i] <- as.numeric(EUR_denom_allowance_prices[, i])
}

head(EUR_denom_allowance_prices,5)

#--------------------------------------------------------

# Export as a CSV file
write.csv(allowance_price, "allowance_price.csv")
write.csv(EUR_denom_allowance_prices, "EUR_denom_allowance_prices.csv")

#### Data Trimming ####
# Allowance price data set
#----------------------
# Trim data sets to only cover EU ETS, Hubei ETS, NZ ETS, and WCI ETS

# Call the new allowance_price to Research_Data_allowance_price
# Only keep columns 2, 3, 4, 6, and 3rd to last column, remove others
Research_Data_allowance_price_trimmed <- allowance_price[, c(1, 2, 3, 6, ncol(allowance_price)-2)]

# Trim the data to cover only April 30, 2014 through December 1, 2021
# Call the new Research_Data_allowance_price to Research_Data_allowance_price_trimmed
#Research_Data_allowance_price_trimmed <- Research_Data_allowance_price[Research_Data_allowance_price$Date >= "2014-04-30" & Research_Data_allowance_price$Date <= "2021-12-01", ]

#----------------------

# EUR_denom data set
#----------------------

# Trim data sets to only cover EU ETS, Hubei ETS, NZ ETS, and WCI ETS

# Call the new EUR_denom_allowance_prices to Research_Data_EUR_denom_allowance_prices
# Only keep columns 2, 3, 4, 6, and 3rd to last column, remove others
Research_Data_EUR_denom_allowance_prices_trimmed <- EUR_denom_allowance_prices[, c(1, 2, 3, 6, ncol(EUR_denom_allowance_prices)-2)]

# Trim the data to cover only April 30, 2014 through December 1, 2021
# Call the new Research_Data_EUR_denom_allowance_prices to Research_Data_EUR_denom_allowance_prices_trimmed
#Research_Data_EUR_denom_allowance_prices_trimmed <- Research_Data_EUR_denom_allowance_prices[Research_Data_EUR_denom_allowance_prices$Date >= "2014-04-30" & Research_Data_EUR_denom_allowance_prices$Date <= "2021-12-01", ]

head(Research_Data_EUR_denom_allowance_prices_trimmed,5)

# Trim California data (Col 3) from each - TEMP SOLUTION
#Research_Data_allowance_price_trimmed <- Research_Data_allowance_price_trimmed[-3]
#Research_Data_EUR_denom_allowance_prices_trimmed <- Research_Data_EUR_denom_allowance_prices_trimmed[-3]

#----------------------

####### Data Cleaning ########

#---------------------------------------

# Allowance price data set
# Replace any invalid values or NAs with last valid observation
#Research_Data_allowance_price_trimmed <- zoo::na.locf(Research_Data_allowance_price_trimmed)

# EUR_denom data set
# Replace any invalid values or NAs with last valid observation
#Research_Data_EUR_denom_allowance_prices_trimmed <- zoo::na.locf(Research_Data_EUR_denom_allowance_prices_trimmed)

# Remove weekends and public holidays
Research_Data_allowance_price_trimmed <- Research_Data_allowance_price_trimmed[!weekdays(Research_Data_allowance_price_trimmed$Date) %in% c("Saturday", "Sunday"), ]
Research_Data_EUR_denom_allowance_prices_trimmed <- Research_Data_EUR_denom_allowance_prices_trimmed[!weekdays(Research_Data_EUR_denom_allowance_prices_trimmed$Date) %in% c("Saturday", "Sunday"), ]

#---------------------------------------

#### Plot the data - Allowance Price ####
#---------------------------------------

## Domestic Currency Allowance prices ##
# Reshape the data to long format
allowance_price_long <- Research_Data_allowance_price_trimmed %>% pivot_longer(-Date, names_to = "Variable", values_to = "Value")

# Plot the time series
ggplot(allowance_price_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_minimal() +
  ggtitle("Local Currency Denominated Allowance Prices") +
  labs(caption = "Source: ICAP")

# Save the plot
ggsave("Allowance_Price_Plot.png",bg = "white")

## EUR denominated Allowance prices ##
EUR_allowance_price_long <- Research_Data_EUR_denom_allowance_prices_trimmed %>% pivot_longer(-Date, names_to = "Variable", values_to = "Value")

# Ensure your Date column is in POSIXct format
EUR_allowance_price_long$Date <- as.POSIXct(EUR_allowance_price_long$Date, format = "%Y-%m-%d")

# Plot the time series with the Title "EUR Denominated Allowance Prices" and Add source as ICAP
a <- ggplot(EUR_allowance_price_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_minimal() +
  ggtitle("EUR Denominated Allowance Prices") +
  labs(caption = "Source: ICAP")

# Define custom JavaScript for dynamic x-axis adjustment
customJS <- "
function(el, x) {
  var gd = document.getElementById(el.id);
  gd.on('plotly_relayout', function(eventdata){
    // Check if the eventdata has an x-axis update
    if(eventdata['xaxis.range[0]'] && eventdata['xaxis.range[1]']) {
      var update = {
        'xaxis.range': [eventdata['xaxis.range[0]'], eventdata['xaxis.range[1]']]
      };
      Plotly.relayout(gd, update);
    }
  });
}
"

# Add range selector buttons and apply onRender with custom JavaScript
final_plot <- p %>% layout(
  xaxis = list(
    type = "date",
    rangeselector = list(
      buttons = list(
        list(count = 6, label = "6m", step = "month", stepmode = "backward"),
        list(count = 1, label = "1y", step = "year", stepmode = "backward"),
        list(count = 5, label = "5y", step = "year", stepmode = "backward"),
        list(step = "all", label = "All")
      )
    ),
    rangeslider = list(visible = TRUE)
  )
) %>% onRender(customJS)

# Display the final plot
final_plot

# Save the plot
ggsave("EUR Allowance_Price_Plot.png",bg = "white")

#---------------------------------------

#### Plot the data - EUR denominated prices ####
#---------------------------------------

# Reshape the data to long format
EUR_denom_allowance_prices_long <- Research_Data_EUR_denom_allowance_prices_trimmed %>% pivot_longer(-Date, names_to = "Variable", values_to = "Value")

# Plot the time series
ggplot(EUR_denom_allowance_prices_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_minimal()

# Save the plot with a white background
ggsave("EUR_denom_Allowance_Price_Plot.png", bg = "white")

#---------------------------------------

#### Export the data ####
# Export cleaned and trimmed data
#---------------------------------------
write.csv(Research_Data_allowance_price_trimmed, "ICAP_allowance_price_trimmed.csv")
write.csv(Research_Data_EUR_denom_allowance_prices_trimmed, "ICAP_EUR_denom_allowance_prices_trimmed.csv")

# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(Research_Data_EUR_denom_allowance_prices_trimmed, "ICAP_EUR_denom_allowance_prices_trimmed.csv")
# Final HTML file
htmlwidgets::saveWidget(final_plot, "EUR_Allowance_Price_Plot.html")
#---------------------------------------

# stop the script
stop()


# Extract the monthly average excahnge rates from df denoted as Exchange Rate (EUR) from the ICAP data

# Create a dataframe called EUR_denom_exchange_rates that extracts 
# column 1 as Date,
# 6 as EUR to Exchange Rate (EUR), 
# 19 as USD to Exchange Rate (EUR), 
# 32 as CAD to Exchange Rate (EUR), 
# 58 as NZD to Exchange Rate (EUR),
# 84 as KRW to Exchange Rate (EUR),
# 97 as CHF to Exchange Rate (EUR),
# 136 as GBP to Exchange Rate (EUR),
# 149 as CNH to Exchange Rate (EUR),
# 251 as CNY to Exchange Rate (EUR),
EUR_denom_exchange_rates <- df[, c(1, 6, 19, 32, 58, 84, 97, 136, 149, 251)]

# Rename as above
colnames(EUR_denom_exchange_rates) <- c("Date", "EUR", "USD", "CAD", "NZD", "KRW", "CHF", "GBP", "CNH", "CNY")

# Remove the first 3 rows
EUR_denom_exchange_rates <- EUR_denom_exchange_rates[-c(1:3), ]

# Convert the dataframe to a daily time series
EUR_denom_exchange_rates <- as.data.frame(EUR_denom_exchange_rates)

# Convert the Date column to a date format
EUR_denom_exchange_rates$Date <- as.Date(EUR_denom_exchange_rates$Date, format = "%d.%m.%Y")

# Convert the rest of the columns to numeric
for (i in 2:ncol(EUR_denom_exchange_rates)) {
  EUR_denom_exchange_rates[, i] <- as.numeric(EUR_denom_exchange_rates[, i])
}

# Export as a CSV file
write.csv(EUR_denom_exchange_rates, "EUR_denom_exchange_rates.csv")