## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## TVP-VAR model estimation procedures in R using the ConnectednessApproach package
## Author: Henry Wyld
## Date of creation: 2024-03-20

## References
## https://sites.google.com/view/davidgabauer/econometric-code?authuser=0
## https://sites.google.com/site/fk83research/code?authuser=0

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
#----------------------------------

## Import data ##
#----------------------------------
return_df <- read.csv("Research_Data_weekly_returns.csv")
vol_df <- read.csv("Research_Data_weekly_volatility.csv")

# Convert the data to zoo objects
return_zoo <- zoo(return_df[, -1], order.by = as.Date(return_df$Date))
vol_zoo <- zoo(vol_df[, -1], order.by = as.Date(vol_df$Date))
#----------------------------------

## Data Cleaning ##
#----------------------------------

# If there are any NAs or infinite values,  removing or imputing them
#return_zoo <- na.omit(return_zoo)  # Removes entire rows where any NA values are present

# Alternatively, impute NAs - example using simple mean imputation (customize as needed)
na_fill_values <- sapply(return_zoo, function(column) mean(column, na.rm = TRUE))
return_zoo <- na.approx(return_zoo, rule = 2)  # Linear interpolation
return_zoo <- na.fill(return_zoo, na_fill_values)  # Filling remaining NAs with column means

# Alternatively, impute NAs - example using simple mean imputation (customize as needed)
na_fill_values <- sapply(vol_zoo, function(column) mean(column, na.rm = TRUE))
vol_zoo <- na.approx(vol_zoo, rule = 2)  # Linear interpolation
vol_zoo <- na.fill(vol_zoo, na_fill_values)  # Filling remaining NAs with column means

## Ensure there are no NAs or infinite values ##
summary(return_zoo)
any(is.na(return_zoo))
any(is.infinite(return_zoo))

## Ensure there are no NAs or infinite values ##
summary(vol_zoo)
any(is.na(vol_zoo))
any(is.infinite(vol_zoo))

#----------------------------------

## Define Event Study Window ##
#----------------------------------

# Import data
events_study_df <- read.csv("events_study_data.csv")

# Convert the StartDate and EndDate columns to Date objects where format is dd/mm/yyyy
events_study_df$StartDate <- as.Date(events_study_df$StartDate, format = "%d/%m/%Y")
events_study_df$EndDate <- as.Date(events_study_df$EndDate, format = "%d/%m/%Y")

# Optionally, write to a CSV file
#write.csv(events_df, "events_study_data.csv", row.names = FALSE)
#----------------------------------

## TVP-VAR model - Returns ##
#----------------------------------

# Specify the lag order
lag_order <- 1  # his analysis uses first-order VARs (p = 1) (selected by Schwarz information criterion), 
H <- 10 # with 10-step-ahead forecasts (H = 10).

# This study considers forgetting factor, kappa1=0.99 and decay factor kappa2=0.96 
# follows Antonakakis et al. to keep the decay factors constant at fixed values.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
forgetting_factor <- 0.99
decay_factor <- 0.96

# David Gabauer approach
# https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
# https://cran.r-project.org/web/packages/ConnectednessApproach/ConnectednessApproach.pdf

dca = ConnectednessApproach(return_zoo, 
                            nlag=lag_order, 
                            nfore=H,
                            window.size=200,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified

## The TVP-VAR connectedness approach is implemented according to:
##  Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer

## Computing connectedness measures
#DCA = list()
#WINDOW.SIZE = c(50, 100, 200)
#for (i in 1:length(WINDOW.SIZE)) {
#  return[[i]] = suppressMessages(ConnectednessApproach(return_zoo, 
#                              nlag=lag_order, 
#                              nfore=H,
#                              window.size=WINDOW.SIZE[i]))
#}
#----------------------------------

## EXTRACT DATA ##

#----------------------------------
# Extract the connectedness measures
str(dca)
TCI_return <- as.data.frame(dca$TCI)
TCI_return$time <- as.Date(row.names(TCI_return))
to_return <- as.data.frame(dca$TO)
from_return  <- as.data.frame(dca$FROM)
NET_return <- as.data.frame(dca$NET)

print(names(TCI_return))
head(TCI_return)
# Assuming TCI_return is a data frame with a 'time' column for the x-axis and a 'TCI' column for the y-axis
ggplot(TCI_return, aes(x = time, y = TCI)) + 
  geom_line(color = "blue", size = 2) +  # 'size' is used instead of 'lwd'
  labs(x = "Time", y = "TCI", title = "Total Connectedness Index (TCI) - Returns") +
  theme_minimal()  # Optional: Adds a minimal theme
#----------------------------------

## Total Connectedness Index - TCI ##

#----------------------------------
# The total connectedness index (TCI) illustrates the average impact a shock in one series has on all others.

# Start PDF device before creating the plot
pdf("TCI_returns.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
# Increase the left margin further for the y-axis title
par(mar=c(5, 5.5, 4, 2) + 0.1)  # Increased left margin

# Plot TCI data with adjusted limits and margins
PlotTCI(dca, 
        ca = NULL,
        ylim = c(0, 50))

# Add titles and axis labels with adjusted positions
#title("Total Connectedness Index (TCI) - Returns", line = 2.5, cex.main = 1.5)

# Overlay the event study window

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers TO markets ##

#----------------------------------
# The total directional connectedness TO others represents the impact series i has on all other series

# Start PDF device before creating the plot
pdf("TO_returns.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotTO(dca, ylim = c(0, 60))

# Add titles and axis labels with adjusted positions
#title("'TO' Others - Returns", line = 2.5, cex.main = 1.5, col.main = "black", font.main = 2)

# Overlay the event study window

# Close the device and save the plot
dev.off()
#----------------------------------

## Dynamic directional spillovers FROM markets ##
#----------------------------------
# The total directional connectedness TO others represents the impact series i has on all other series

# Start PDF device before creating the plot
pdf("FROM_returns.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotFROM(dca, ylim = c(0, 60))  # Adjust these numbers as needed

# Add titles and axis labels with adjusted positions
#title("'FROM' Others - Returns", line = 2.5, cex.main = 1.5, col.main = "black", font.main = 2)

# Overlay the event study window

# Close the device and save the plot
dev.off()
#----------------------------------

## Net Total Directional Connectedness - NET ##
#----------------------------------

# The Net Total Directional Connectedness (NET) measures the difference between the 
# total directional connectedness TO and FROM others results in the net total directional connectedness.

# Start PDF device with adjusted height for more header space
pdf("NET_returns.pdf", width = 8, height = 6)

# Plot the connectedness measures - Net Pairwise Total Connectedness
# Adjusting margins and possibly outer margins within the function call
PlotNET(dca, ylim = c(-50, 50), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

# Add titles and axis labels with adjusted positions
#title("Net Total Directional Connectedness (NET) - Returns", line = 1, cex.main = 1.5, col.main = "black", font.main = 2)

# Close the PDF device
dev.off()

#----------------------------------

## Forecast Error Variance Decomposition (FEVD) ##
#----------------------------------
# The average connectedness matrix of the system is calculated as the average of the connectedness matrices over the entire sample period.

# Forecast Error Variance Decomposition (FEVD)
FEVD_returns <- dca$TABLE

# Remove the rows named "Inc.Own" and "NPT"
FEVD_returns <- FEVD_returns[!(rownames(FEVD_returns) %in% c("Inc.Own", "NPT")), ]

# Put the table into a stargazer table
# Create the stargazer table and export to HTML
stargazer::stargazer(FEVD_returns, type = "html", summary = FALSE, title = "Table 3. Average connectedness matrix of the Return system.", 
                     out = "connectedness_returns.html")

#----------------------------------

## TVP-VAR model - Volatility ##
#----------------------------------

# David Gabauer approach
# https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer
# https://cran.r-project.org/web/packages/ConnectednessApproach/ConnectednessApproach.pdf

dca = ConnectednessApproach(vol_zoo, 
                            nlag=lag_order, 
                            nfore=H,
                            window.size=200,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=forgetting_factor, kappa2=decay_factor, prior="BayesPrior"))) # TVP-VAR model with forgetting factor and decay factor as specified

## The TVP-VAR connectedness approach is implemented according to:
##  Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.
# See https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer


## EXTRACT DATA ##

#----------------------------------
# Extract the connectedness measures
str(dca)
TCI_vol <- as.data.frame(dca$TCI)
to_vol <- as.data.frame(dca$TO)
from_vol  <- as.data.frame(dca$FROM)
NET_vol <- as.data.frame(dca$NET)

## Total Connectedness Index - TCI ##

#----------------------------------
# The total connectedness index (TCI) illustrates the average impact a shock in one series has on all others.

# Start PDF device before creating the plot
pdf("TCI_vol.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
# Increase the left margin further for the y-axis title
par(mar=c(5, 5.5, 4, 2) + 0.1)  # Increased left margin

# Plot TCI data with adjusted limits and margins
PlotTCI(dca, 
        ca = NULL,
        ylim = c(0, 50))

# Add titles and axis labels with adjusted positions
#title("Total Connectedness Index (TCI) - vol", line = 2.5, cex.main = 1.5)

# Overlay the event study window

# Close the device and save the plot
dev.off()

#----------------------------------

## Dynamic directional spillovers TO markets ##

#----------------------------------
# The total directional connectedness TO others represents the impact series i has on all other series

# Start PDF device before creating the plot
pdf("TO_vol.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotTO(dca, ylim = c(0, 60))

# Add titles and axis labels with adjusted positions
#title("'TO' Others - vol", line = 2.5, cex.main = 1.5, col.main = "black", font.main = 2)

# Overlay the event study window

# Close the device and save the plot
dev.off()
#----------------------------------

## Dynamic directional spillovers FROM markets ##
#----------------------------------
# The total directional connectedness TO others represents the impact series i has on all other series

# Start PDF device before creating the plot
pdf("FROM_vol.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
par(mar=c(10, 4.5, 5, 2) + 0.1)  # Adjust these numbers as needed

PlotFROM(dca, ylim = c(0, 60))  # Adjust these numbers as needed

# Add titles and axis labels with adjusted positions
#title("'FROM' Others - vol", line = 2.5, cex.main = 1.5, col.main = "black", font.main = 2)

# Overlay the event study window

# Close the device and save the plot
dev.off()
#----------------------------------

## Net Total Directional Connectedness - NET ##
#----------------------------------

# The Net Total Directional Connectedness (NET) measures the difference between the 
# total directional connectedness TO and FROM others results in the net total directional connectedness.

# Start PDF device with adjusted height for more header space
pdf("NET_vol.pdf", width = 8, height = 6)

# Plot the connectedness measures - Net Pairwise Total Connectedness
# Adjusting margins and possibly outer margins within the function call
PlotNET(dca, ylim = c(-50, 50), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

# Add titles and axis labels with adjusted positions
#title("Net Total Directional Connectedness (NET) - vol", line = 1, cex.main = 1.5, col.main = "black", font.main = 2)

# Close the PDF device
dev.off()

#----------------------------------

## Forecast Error Variance Decomposition (FEVD) ##
#----------------------------------
# The average connectedness matrix of the system is calculated as the average of the connectedness matrices over the entire sample period.

# Forecast Error Variance Decomposition (FEVD)
FEVD_vol <- dca$TABLE

# Put the table into a stargazer table
# Create the stargazer table
stargazer::stargazer(FEVD_vol, type = "text", summary = FALSE, title = "Table 3. Average connectedness matrix of the Volatility system.", 
                     out = "table_vol.txt")
#----------------------------------


# Plot TCI data with event study window
#----------------------------------

# Start PDF device before creating the plot
pdf("TCI_returns_event_study.pdf", width = 8, height = 6)  # Size in inches (default)

# Set larger margins (bottom, left, top, right) to avoid clipping of titles/labels
# Increase the left margin further for the y-axis title
par(mar=c(5, 5.5, 4, 2) + 0.1)  # Increased left margin

# Plot TCI data with adjusted limits and margins
PlotTCI(dca, 
        ca = events_study_df,
        ylim = c(0, 50))

# Add titles and axis labels with adjusted positions
#title("Total Connectedness Index (TCI) - Returns", line = 2.5, cex.main = 1.5)

# Close the device and save the plot
dev.off()
