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

colnames(return_zoo) <- c("EU ETS", "NZ ETS", "CA CaT", "HB ETS")
colnames(vol_zoo) <- c("EU ETS", "NZ ETS", "CA CaT", "HB ETS")

#----------------------------------

## Define Event Study Window ##
#----------------------------------

# Import data
events_study_df <- read.csv("events_study_data.csv")

# Convert the StartDate and EndDate columns to Date objects where format is dd/mm/yyyy
events_study_df$StartDate <- as.Date(events_study_df$StartDate, format = "%d/%m/%Y")
events_study_df$EndDate <- as.Date(events_study_df$EndDate, format = "%d/%m/%Y")
events_study_df$Midpoint <- as.Date(events_study_df$Midpoint, format = "%d/%m/%Y")

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
TCI_return$Date <- as.Date(row.names(TCI_return))
to_return <- as.data.frame(dca$TO)
from_return  <- as.data.frame(dca$FROM)
NET_return <- as.data.frame(dca$NET)

print(names(TCI_return))
head(TCI_return)

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

PlotFROM(dca, ylim = c(0, 50))  # Adjust these numbers as needed

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
PlotNET(dca, ylim = c(-30, 30), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

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

# Ensure FEVD_returns is a data frame
FEVD_returns <- as.data.frame(FEVD_returns)

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
TCI_vol$Date <- as.Date(row.names(TCI_vol))
to_vol <- as.data.frame(dca$TO)
from_vol  <- as.data.frame(dca$FROM)
NET_vol <- as.data.frame(dca$NET)

print(names(TCI_vol))


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

PlotTO(dca, ylim = c(0, 70))

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

PlotFROM(dca, ylim = c(0, 50))  # Adjust these numbers as needed

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
PlotNET(dca, ylim = c(-50, 40), mar = c(5, 4, 6, 2), oma = c(0, 0, 4, 0))

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

# Remove the rows named "Inc.Own" and "NPT"
FEVD_vol <- FEVD_vol[!(rownames(FEVD_vol) %in% c("Inc.Own", "NPT")), ]

# Ensure FEVD_returns is a data frame
FEVD_vol <- as.data.frame(FEVD_vol)

# Put the both FEVD dataframes into a a signle stargazer table
# Generate the HTML table for FEVD_returns
stargazer::stargazer(FEVD_returns, type = "html", summary = FALSE, 
                     title = "Table 3. Average connectedness matrix of the system.", 
                     out = "FEVD_returns.html", out.header = FALSE)

# Generate the HTML table for FEVD_volatility without title
stargazer::stargazer(FEVD_vol, type = "html", summary = FALSE, 
                     title = "", # No title for the second table
                     out = "FEVD_volatility.html", out.header = FALSE)

# Read the HTML files
html_lines_returns <- readLines("FEVD_returns.html")
html_lines_volatility <- readLines("FEVD_volatility.html")

# Insert the caption for returns
caption_returns <- '<caption style="text-align:left;">Panel A: Return connectedness (%)</caption>'
table_start_index_returns <- which(grepl("<table", html_lines_returns, fixed = TRUE))[1]
html_lines_returns <- append(html_lines_returns, caption_returns, after = table_start_index_returns)

# Insert the caption for volatility
caption_volatility <- '<caption style="text-align:left;">Panel B: Volatility connectedness (%)</caption>'
table_start_index_volatility <- which(grepl("<table", html_lines_volatility, fixed = TRUE))[1]
html_lines_volatility <- append(html_lines_volatility, caption_volatility, after = table_start_index_volatility)

# Combine both HTML tables into a single HTML file
combined_html_lines <- c(html_lines_returns, "<br><br>", html_lines_volatility)

# Add final caption at the bottom
caption_connectedness <- '<tr><td colspan="7" style="text-align:left;">Aligning with Lyu and Scholtens (2022), this analysis uses first-order VARs (p = 1) as selected by Schwarz information criterion, with 10-step-ahead forecasts (H = 10).</td></tr>'
# Locate the end of the second table to append the caption
table_end_index_volatility <- which(grepl("</table>", html_lines_volatility, fixed = TRUE))[1]
combined_html_lines <- append(combined_html_lines, caption_connectedness, after = length(combined_html_lines) - (length(html_lines_volatility) - table_end_index_volatility))

# Write the combined HTML content back to a new file
writeLines(combined_html_lines, "combined_connectedness.html")
#----------------------------------


# Plot TCI data with event study window
#----------------------------------

# Define custom colors for each category (adjust these to match the chart)
category_colors <- c(
        "global politics" = "orange", 
        "carbon market" = "red", 
        "weather" = "gold",
        "energy" = "blue",
        "finance" = "blue",
        "covid-19" = "purple")

# Ensure the x-axis scale matches the TCI chart range
max(TCI_return$Date)
max(events_study_df$EndDate)
min(TCI_return$Date)
min(events_study_df$StartDate)

# Add an EventNumber column to the TCI dataframe mapping the event number to repeat between the StartDate and EndDate

# Replace any events_study_df$EndDate with the maximum date if it exceeds the TCI data range
events_study_df$EndDate <- pmin(events_study_df$EndDate, max(TCI_return$Date))

# Replace any events_study_df$StartDate with the minimum date if it precedes the TCI data range
events_study_df$StartDate <- pmax(events_study_df$StartDate, min(TCI_return$Date))

# Calculate the midpoint for event labels
events_study_df <- events_study_df %>%
  mutate(
    Midpoint = as.Date((as.numeric(StartDate) + as.numeric(EndDate)) / 2, origin = "1970-01-01"),
    LabelY = rep(seq(35, 40, length.out = n()), length.out = n())  # Alternate y positions
  )

# Define the x-axis range based on the TCI data and event study window
x_range <- range(TCI_return$Date, events_study_df$StartDate, events_study_df$EndDate)

# Create the plot
ggplot() + 
  # Add shaded areas for events
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_return$Date)), xmax = pmin(EndDate, max(TCI_return$Date)), ymin = 0, ymax = 40, fill = Category), alpha = 0.2) +  
  # Add StartDate and EndDate vertical lines restricted to y = 0
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  # Add EventNumber labels between StartDate and EndDate
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  
  # Shade the area under the curve as grey
  geom_ribbon(data = TCI_return, aes(x = Date, ymin = 0, ymax = TCI), fill = "darkgrey", alpha = 1) +  
  # Plot the TCI line
  geom_line(data = TCI_return, aes(x = Date, y = TCI), color = "black", size = 0.5) +  
  # Add labels
  labs(x = "year", y = "Total spillover index", title = "Total Return Connectedess Event Study") +
  # Use a minimal theme
  theme_minimal() +  
  # Adjust colors for categories
  scale_fill_manual(values = category_colors, name = "Category") +  
  # Customize text sizes and positions for axis titles and labels
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",  # Position the legend on the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.line.x.bottom = element_line(color = "black", size = 1),  # Add x-axis line
    axis.line.y.left = element_line(color = "black", size = 1)  # Add y-axis line
  ) +
  # Ensure the same x-axis scale and set y-axis limits
  scale_x_date(limits = c(min(TCI_return$Date), max(TCI_return$Date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 40))

# Save the plot as a PNG with white background
ggsave("TCI_returns_with_events.png", width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------

# Plot TCI data with event study window for volatility
#----------------------------------

# Create the plot
ggplot() + 
  # Add shaded areas for events
  geom_rect(data = events_study_df, aes(xmin = pmax(StartDate, min(TCI_vol$Date)), xmax = pmin(EndDate, max(TCI_vol$Date)), ymin = 0, ymax = 40, fill = Category), alpha = 0.2) +  
  # Add StartDate and EndDate vertical lines restricted to y = 0
  geom_segment(data = events_study_df, aes(x = StartDate, xend = StartDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  geom_segment(data = events_study_df, aes(x = EndDate, xend = EndDate, y = 0, yend = 40), linetype = "solid", color = "grey", size = 0.25, alpha = 0.25) +  
  # Add EventNumber labels between StartDate and EndDate
  geom_text(data = events_study_df, aes(x = Midpoint, y = LabelY, label = EventNumber), angle = 90, vjust = 0.5, hjust = 0) +  
  # Shade the area under the curve as grey
  geom_ribbon(data = TCI_vol, aes(x = Date, ymin = 0, ymax = TCI), fill = "black", alpha = 1) +  
  # Plot the TCI line
  geom_line(data = TCI_vol, aes(x = Date, y = TCI), color = "black", size = 1) +  
  # Add labels
  labs(x = "year", y = "Total spillover index", title = "Total Volatility Connectedess Event Study") +
  # Use a minimal theme
  theme_minimal() +  
  # Adjust colors for categories
  scale_fill_manual(values = category_colors, name = "Category") +  
  # Customize text sizes and positions for axis titles and labels
  theme(
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    axis.text = element_text(size = 10),
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom",  # Position the legend on the right
    panel.grid = element_blank(),  # Remove grid lines
    axis.line.x.bottom = element_line(color = "black", size = 1),  # Add x-axis line
    axis.line.y.left = element_line(color = "black", size = 1)  # Add y-axis line
  ) +
  # Ensure the same x-axis scale and set y-axis limits
  scale_x_date(limits = c(min(TCI_vol$Date), max(TCI_vol$Date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(0, 40))

# Save the plot as a PNG with white background
ggsave("TCI_volatility_with_events.png", width = 8, height = 6, dpi = 300, bg = "white")

#----------------------------------