## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## TVP-VAR model estimation procedures in R
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

## Import data ##
#----------------------------------
return_df <- read.csv("Research_Data_weekly_returns.csv")
vol_df <- read.csv("Research_Data_weekly_volatility.csv")

# Convert the data to zoo objects
return_zoo <- zoo(return_df[, -1], order.by = as.Date(return_df$Date))
vol_zoo <- zoo(vol_df[, -1], order.by = as.Date(vol_df$Date))

## Ensure there are no NAs or infinite values ##
# If there are any NAs or infinite values, consider removing or imputing them
return_zoo <- na.omit(return_zoo)  # Removes entire rows where any NA values are present

# Alternatively, impute NAs - example using simple mean imputation (customize as needed)
na_fill_values <- sapply(return_zoo, function(column) mean(column, na.rm = TRUE))
return_zoo <- na.approx(return_zoo, rule = 2)  # Linear interpolation
return_zoo <- na.fill(return_zoo, na_fill_values)  # Filling remaining NAs with column means

summary(return_zoo)
any(is.na(return_zoo))
any(is.infinite(return_zoo))

# Assuming 'return_df' is your dataframe with time series data
data_matrix <- as.matrix(return_df)

# Convert 'return_df' to a matrix excluding the Date column
data_matrix <- as.matrix(return_df[,-1])  # Excludes the first column, which is 'Date'

# Replace NAs with 0
data_matrix[is.na(data_matrix)] <- 0

head(data_matrix,5)
#----------------------------------

## TVP-VAR model ##
#----------------------------------

# Specify the lag order
lag_order <- 1  # Change this as needed

# Fit the Bayesian VAR model with stochastic volatilities
# Setting hyperparameters and specifying the number of lags
result <- bvar.sv.tvp(y = data_matrix, p = 1, draws = 5000, burnin = 1000)

# The type argument specifies the estimation method. "mcmc" uses Markov Chain Monte Carlo (MCMC) methods.
# The draws argument specifies the number of draws for the MCMC estimation.
# Adjust draws as needed. More draws typically lead to better results but require more computation time.

# David Gabauer approach
# https://gabauerdavid.github.io/ConnectednessApproach/2020AntonakakisChatziantoniouGabauer

dca = ConnectednessApproach(return_zoo, 
                            nlag=1, 
                            nfore=10,
                            window.size=200,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))

# Estimate the TVP-VAR model
tvp_var_fit <- fit(tvp_var_model)

## The TVP-VAR connectedness approach is implemented according to:
##  Antonakakis, N., Chatziantoniou, I., & Gabauer, D. (2020). Refined measures of dynamic connectedness based on time-varying parameter vector autoregressions. Journal of Risk and Financial Management, 13(4), 84.
## Computing connectedness measures
DCA = list()
WINDOW.SIZE = c(50, 100, 200)
for (i in 1:length(WINDOW.SIZE)) {
  DCA[[i]] = suppressMessages(ConnectednessApproach(return_zoo, 
                              nlag=1, 
                              nfore=12,
                              window.size=WINDOW.SIZE[i]))
}

# Summary of the estimation results
summary(tvp_var_fit)

# Plot the time-varying parameters
plot(tvp_var_fit)

# Plot the connectedness measures - Dynamic Total Connectedness
PlotTCI(dca, ca=DCA)

# Plot the connectedness measures - Dynamic Net Connectedness
PlotNET(dca, ca=DCA)

# Plot the connectedness measures - Net Pairwise Directional Connectedness
PlotNPDC(dca, ca=DCA)

# Install and load the bsts package
install.packages("bsts")
library(bsts)

# Import your data
VAR_df <- read.csv("EUR_denom_allowance_prices.csv")

# Convert your data to a time series object
VAR_ts <- ts(VAR_df)

# Specify the model
state.spec <- AddLocalLinearTrend(list(), VAR_ts)
state.spec <- AddSeasonal(state.spec, VAR_ts, nseasons = 4)

# Estimate the model
bsts.model <- bsts(VAR_ts, state.spec, niter = 1000, ping = 0, seed = 123)

# Summary of the estimation results
summary(bsts.model)