## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Read the Clearblue data and format in R
## Author: Henry Wyld
## Date of creation: 2024-04-21

#-------------------------------------
# clear memory
rm(list=ls())    
#----------------------------------

# Load required packages
#load_packages()


# Load necessary libraries
library(readxl)
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(data.table)
library(xts)
install.packages("htmltools")
library(htmltools)
install.packages("plotly")
library(plotly)
install.packages("radiant")
install.packages("httpgd")
library(httpgd)

update.packages(ask = FALSE)

# Set the working directory
setwd("C:/Users/henry/OneDrive - The University of Melbourne/Master of Applied Econometrics/2024/Semester 1/Research Methods/Research Paper/ClearBlue Data/Data used in Paper")

## Read the datasets from Excel files ##
#-----------------------------------------

# Set Paths to the Excel files
path <- "C:/Users/henry/OneDrive - The University of Melbourne/Master of Applied Econometrics/2024/Semester 1/Research Methods/Research Paper/ClearBlue Data/Data used in Paper"

# Create loop to read all Excel files in the directory
files <- list.files(path, pattern = "*.xlsx", full.names = TRUE)

read_and_format_excel <- function(file) {
  # Initially read only the first two rows to get the column names and number of columns
  temp_df <- read_excel(file, n_max = 2, col_names = FALSE)
  col_names <- as.character(unlist(temp_df[1, ])) # Save the column names from the first row
  num_cols <- ncol(temp_df)
  
  # Create a vector of column types with 'date' for the first column and 'numeric' for the others
  col_types <- c("date", rep("numeric", num_cols - 1))
  
  # Read the entire dataset with the specified column types, skipping the first row
  df <- read_excel(file, skip = 1, col_names = FALSE, col_types = col_types)
  
  # Replace the column names with the saved column names
  colnames(df) <- col_names
  
  # Return the formatted dataframe
  return(df)
}

# Run the function across the list of files creating dataframes for each file
dataframes <- lapply(files, read_and_format_excel)

# Create a list of dataframes with meaningful names
dataframes <- setNames(dataframes, gsub(".xlsx", "", list.files(path, pattern = "*.xlsx")))

# Check the structure of the dataframes
lapply(dataframes, head)

# Convert the list of dataframes to individual dataframes
dataframes <- lapply(dataframes, as.data.frame)

# Create the dataframes in the global environment
list2env(dataframes, envir = .GlobalEnv)


#-------------------------------------

### Data Trimming  ###

# Remove Auction price from the EU ETS dataframe
eu_ets_trimmed <- eu_ets[, -grep("Auction", colnames(eu_ets))]

# Remove all numeric columns except last one from new_zealand_ets dataframe
new_zealand_ets_trimmed <- new_zealand_ets[, c(1, ncol(new_zealand_ets))]

# Remove all numeric columns except those that contain CCA in the column name for the WCI series
wci_trimmed <- WCI[, c(1, grep("CCA", colnames(WCI)))]


# Merging multiple data frames using full_join for a full outer join
merged_data <- eu_ets_trimmed %>%
  full_join(new_zealand_ets_trimmed, by = "DateTime") %>%
  full_join(wci_trimmed, by = "DateTime")

# Rename the columns in the merged data

colnames(merged_data)[2] <- "EUA - Front December - ICE"
colnames(merged_data)[3] <- "EUA - Front Month - ICE"
colnames(merged_data)[4] <- "NZU - Cash Spot"
colnames(merged_data)[5] <- "CCA - Front December - ICE"
colnames(merged_data)[6] <- "CCA - Cash Spot"


#-------------------------------------


#### Plot the data - Allowance Price ####
#---------------------------------------
# Load required packages
library(tidyverse)

# Plot the time series
# Remove the existing gtable package
remove.packages("gtable")

# Reinstall gtable
install.packages("gtable")

# Load the package to check if it works
library(gtable)
#library(ggplot2)

ggplot(merged_data, aes(x = DateTime)) +
  geom_line(aes(y = `EUA - Front December - ICE`, color = "EUA")) +
  geom_line(aes(y = `NZU - Cash Spot`, color = "NZU")) +
  geom_line(aes(y = `CCA - Front December - ICE`, color = "CCA")) +
  labs(title = "Clearblue Data Plot",
       x = "Date Time",
       y = "Price") +
  scale_color_manual(values = c("EUA" = "red", "NZU" = "blue", "CCA" = "#d0ff00")) +
  theme_minimal()

# Save the plot
ggsave("Clearblue_Plot.png",bg = "white")


## EUR denominated Allowance prices ##
EUR_allowance_price_long <- merged_data %>% pivot_longer(-DateTime, names_to = "Variable", values_to = "Value")

# Plot the time series
ggplot(EUR_allowance_price_long, aes(x = Date, y = Value, color = Variable)) +
  geom_line() +
  labs(x = "Date", y = "Value", color = "Variable") +
  theme_minimal()

# Save the plot
ggsave("Allowance_Price_Plot.png",bg = "white")


#p <- plot_ly(merged_data, x = ~x, y = price, type = 'scatter', mode = 'lines')

#---------------------------------------

# Order the data by DateTime
merged_data <- merged_data[order(merged_data$DateTime), ]

# Save the merged data to a CSV file
write.csv(merged_data, "Clearblue_data.csv", row.names = FALSE)
