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

# Load the data
#----------------------------------

# Read the CSV file
Refinitiv_df <- readr::read_csv("Refinitiv_HUBEI.csv", locale = readr::locale(encoding = "UTF-8"))

head(Refinitiv_df)

#----------------------------------

# Convert the date to a date format
Refinitiv_df$`Date` <- as.Date(Refinitiv_df$`Date`, format = "%d/%m/%Y")

# Convert all other columns to numeric
Refinitiv_df <- as.data.frame(Refinitiv_df) %>% mutate_if(is.character, as.numeric)

# Convert HBEHBEA (TRDPRC_1) to EUR denominated using EURCNY= (BID)
Refinitiv_df$HBEA_EUR <- Refinitiv_df$`HBEHBEA (TRDPRC_1)` * 1 / Refinitiv_df$`EURCNY= (BID)`
tail(Refinitiv_df)

# Save to global environment
assign("Refinitiv_df", Refinitiv_df, envir = .GlobalEnv)

#----------------------------------

# Plot the data
#----------------------------------

ggplot2 <- ggplot(Refinitiv_df, aes(x = Date)) +
  geom_line(aes(y = `HBEHBEA (TRDPRC_1)`), color = "blue") +
  geom_line(aes(y = HBEA_EUR), color = "red") +
  labs(title = "Hubei Emission Allowance Price",
       x = "Date",
       y = "Price (CNY)") +
  theme_minimal()

ggplot2
#----------------------------------

# Import ICAP data for comparison
#----------------------------------
ICAP_df <- readr::read_csv("ICAP_EUR_denom_allowance_prices_trimmed.csv", locale = readr::locale(encoding = "UTF-8"))

# Merge the two datasets

ICAP_Refinitiv_df <- merge(ICAP_df, Refinitiv_df, by = "Date", all = TRUE)

# Remove all columns that are not needed
ICAP_Refinitiv_df <- ICAP_Refinitiv_df[, c("Date", "Hubei_EUR", "HBEA_EUR")]

# Trim the data set for when Hubei_EUR starts
ICAP_Refinitiv_df <- ICAP_Refinitiv_df[!is.na(ICAP_Refinitiv_df$Hubei_EUR), ]

# Create a new column for the difference between the two
ICAP_Refinitiv_df$Diff <- ICAP_Refinitiv_df$Hubei_EUR - ICAP_Refinitiv_df$HBEA_EUR

# Compare Hubei_EUR and HBEA_EUR include legend
ggplot2 <- ggplot(ICAP_Refinitiv_df, aes(x = Date)) +
  geom_line(aes(y = Hubei_EUR), color = "blue") +
  geom_line(aes(y = HBEA_EUR), color = "red") +
  geom_line(aes(y = Diff), color = "green") +
  labs(title = "Hubei Emission Allowance Price",
       x = "Date",
       y = "Price (EUR)") +
  theme_minimal()

ggplot2

# Summary stats for both the Hubei_EUR and HBEA_EUR
summary(ICAP_Refinitiv_df$Hubei_EUR)
summary(ICAP_Refinitiv_df$HBEA_EUR)
summary(ICAP_Refinitiv_df$Diff)

# Start date for Hubei_EUR
# Find the first non-missing date for Hubei_EUR
start_index_Hubei_EUR <- which(!is.na(ICAP_Refinitiv_df$Hubei_EUR))[1]
start_date_Hubei_EUR <- ICAP_Refinitiv_df$Date[start_index_Hubei_EUR]

# Find the first non-missing date for HBEA_EUR
start_index_HBEA_EUR <- which(!is.na(ICAP_Refinitiv_df$HBEA_EUR))[1]
start_date_HBEA_EUR <- ICAP_Refinitiv_df$Date[start_index_HBEA_EUR]

# Output the start dates
start_index_Hubei_EUR
start_date_HBEA_EUR

# Create the new column with conditional values based on the start date of HBEA_EUR
ICAP_Refinitiv_df$Hubei_EUR_adj <- ifelse(ICAP_Refinitiv_df$Date >= start_date_HBEA_EUR, 
                                          ICAP_Refinitiv_df$HBEA_EUR, 
                                          ICAP_Refinitiv_df$Hubei_EUR)


# Create the ggplot
g <- ggplot(ICAP_Refinitiv_df, aes(x = Date)) +
  geom_line(aes(y = Hubei_EUR_adj, color = "Adjusted Hubei EUR"), size = 1) +
  geom_line(aes(y = Hubei_EUR, color = "Original Hubei EUR"), size = 1, linetype = "dashed") +
  labs(title = "Adjusted Hubei Emission Allowance Price",
       x = "Date",
       y = "Price (EUR)",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Adjusted Hubei EUR" = "green", "Original Hubei EUR" = "red"))

# Convert ggplot to plotly
plotly_g <- ggplotly(g)

# Export the plotly plot to HTML
htmlwidgets::saveWidget(plotly_g, "HBEA_Adjusted.html")
#----------------------------------

tail(ICAP_Refinitiv_df)
tail(ICAP_df)

# Replace Hubei_EUR in ICAP_df with the adjusted Hubei_EUR from ICAP_Refinitiv_Data, matching dates
ICAP_df$Hubei_EUR <- ICAP_Refinitiv_df$Hubei_EUR_adj[match(ICAP_df$Date, ICAP_Refinitiv_df$Date)]

# Check the data
tail(ICAP_df)
tail(ICAP_Refinitiv_df)

# Export the data
write.csv(ICAP_df, "ICAP_Refinitiv_EUR_denom_allowance_prices_trimmed.csv", row.names = FALSE)