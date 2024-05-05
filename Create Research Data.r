## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Compare datasets from both sources and create the research data
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

# Check for NA-only columns and apply describe safely
safe_describe <- function(x) {
  non_na_values <- x[!is.na(x)]
  if (length(non_na_values) == 0) {
    return(list(Count = NA, Mean = NA, SD = NA))  # Return NA for stats if no data
  } else {
    return(describe(non_na_values))
  }
}

# Apply the function to each column
summary_stats_ICAP <- sapply(ICAP_df_xts, safe_describe)
summary_stats_Clearblue <- sapply(Clearblue_df_xts, safe_describe)

## Display the Summary Statistics
# Convert summary statistics to a data frame
summary_stats_to_df <- function(stats) {
  stats_df <- do.call(rbind, stats)
  stats_df <- as.data.frame(stats_df)
  return(stats_df)
}

# Convert valid date information to a data frame
dates_to_df <- function(dates) {
  dates_df <- t(dates)
  dates_df <- as.data.frame(dates_df)
  names(dates_df) <- c("Start", "End")
  return(dates_df)
}

# Convert your summary statistics arrays to data frames
summary_stats_ICAP_df <- summary_stats_to_df(summary_stats_ICAP)
summary_stats_Clearblue_df <- summary_stats_to_df(summary_stats_Clearblue)

# Convert date info arrays to data frames
valid_date_info_ICAP_df <- dates_to_df(valid_date_info_ICAP)
valid_date_info_Clearblue_df <- dates_to_df(valid_date_info_Clearblue)

# Export the Tables with stargazer to HTML
stargazer(summary_stats_ICAP_df, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for ICAP Dataset",
          out= "Summary Statistics for ICAP Dataset.html")

stargazer(summary_stats_Clearblue_df, 
          type = "html", 
          digits=3, align=TRUE,
          intercept.bottom=FALSE,
          title = "Summary Statistics for Clearblue Dataset",
          out= "Summary Statistics for Clearblue Dataset.html")

stargazer(valid_date_info_ICAP_df, 
          type = "html", 
          title = "Start and End Dates for ICAP Dataset",
          out= "Dates for ICAP Dataset.html")

stargazer(valid_date_info_Clearblue_df, 
          type = "html", 
          title = "Start and End Dates for Clearblue Dataset",
          out= "Dates for Clearblue Dataset.html")

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

# Remove non-trading days
combined_df <- combined_df[!duplicated(combined_df$Date), ]

# Melt the data for plotting (if using ggplot2 and the data is wide)

combined_df_long <- melt(combined_df, id.vars = c("Date", "Type"), variable.name = "Variable", value.name = "Price")

# Plot using ggplot2
a <- ggplot(combined_df_long, aes(x = Date, y = Price, color = Type, linetype = Type)) +
    geom_line() +
    facet_wrap(~ Variable, scales = "free_y") +  # Ensures each variable has its own y scale
    labs(title = "Comparison of Original vs Forward-Filled Data", x = "Date", y = "Price") +
    scale_color_manual(values = c("Original" = "blue", "Forward-Filled" = "red")) +
    theme_minimal()

# Convert to Plotly object
p <- ggplotly(a)

# Optionally, display the plot in an interactive viewer if running locally
#print(p)

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
)

# Save plot as a PNG file
ggsave("Forward_Filled_Data_Plot.png", bg = "white")

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

# Turn the list into a data frame and ensure the dates are kept
cleaned_datasets <- do.call(cbind, cleaned_datasets)
cleaned_datasets <- data.frame(Date = index(cleaned_datasets), coredata(cleaned_datasets))

# Remove col1
#cleaned_datasets <- cleaned_datasets[, -1]

# Rename to EUA, NZU, CCA, HBEA
colnames(cleaned_datasets) <- c("Date","EUA", "NZU", "CCA", "HBEA") 

#---------------------------------------

# Export to Git
#---------------------------------------
# Publish both data sets to Git
setwd(Git)
# Final Data Set
write.csv(cleaned_datasets, "Research_data.csv")

#---------------------------------------