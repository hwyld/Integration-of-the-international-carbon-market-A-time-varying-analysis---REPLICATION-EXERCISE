## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Read the Clearblue data and format in R
## Author: Henry Wyld
## Date of creation: 2024-04-21

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


# Load necessary libraries
#library(readxl)
#library(dplyr)
#library(readr)
#library(tidyr)
#library(ggplot2)
#library(lubridate)
#library(tibble)
#library(data.table)
#library(xts)
#install.packages("htmltools")
#library(htmltools)
#install.packages("plotly")
#library(plotly)
#install.packages("radiant")
#install.packages("httpgd")
#library(httpgd)


# Set the working directory
setwd(Clearblue_Data)

## Read the datasets from Excel files ##
#-----------------------------------------

# Set Paths to the Excel files
path <- Clearblue_Data

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

# Plot the time series
a <- ggplot(merged_data, aes(x = DateTime)) +
  geom_line(aes(y = `EUA - Front December - ICE`, color = "EUA")) +
  geom_line(aes(y = `NZU - Cash Spot`, color = "NZU")) +
  geom_line(aes(y = `CCA - Front December - ICE`, color = "CCA")) +
  labs(title = "Clearblue Data Plot",
       x = "Date Time",
       y = "Price") +
  scale_color_manual(values = c("EUA" = "red", "NZU" = "blue", "CCA" = "#d0ff00")) +
  theme_minimal()

# Convert to Plotly object
p <- ggplotly(a)

# Define custom JavaScript for dynamic x-axis adjustment
customJS <- "
function(el, x) {
  var gd = document.getElementById(el.id);
  gd.on('plotly_relayout', function(eventdata){
    if(eventdata['xaxis.range[0]'] && eventdata['xaxis.range[1]']) {
      var update = {
        'xaxis.range': [eventdata['xaxis.range[0]'], eventdata['xaxis.range[1]']]
      };
      Plotly.relayout(gd, update);
    }
  });
}
"

# Add range selector buttons, source annotation, and apply onRender with custom JavaScript
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
  ),
  annotations = list(
    list(
      text = "Source: Clearblue",
      x = 0.01, # Position on the x-axis
      xref = "paper", # Relative to the entire width of the plot
      y = -0.2, # Position on the y-axis
      yref = "paper", # Relative to the entire height of the plot
      showarrow = FALSE, # No arrow pointing
      xanchor = "left",
      yanchor = "top",
      font = list(
        family = "Arial",
        size = 12,
        color = "grey"
      )
    )
  )
) %>% onRender(customJS)

# Display the final plot
final_plot

# Save the plot to an HTML file
htmlwidgets::saveWidget(final_plot, "Clearblue_Price_Plot.html")

# Save the plot
ggsave("Clearblue_Plot.png",bg = "white")

#---------------------------------------

# Order the data by DateTime
merged_data <- merged_data[order(merged_data$DateTime), ]

# Save the merged data to a CSV file
write.csv(merged_data, "Clearblue_data.csv", row.names = FALSE)

# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(merged_data, "Clearblue_data.csv")
# Final HTML file
htmlwidgets::saveWidget(final_plot, "Clearblue_Price_Plot.html")
