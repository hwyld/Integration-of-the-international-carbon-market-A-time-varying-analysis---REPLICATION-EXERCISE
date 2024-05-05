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

# Assuming 'return_df' is your dataframe with time series data
data_matrix <- as.matrix(return_df)

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

dca = ConnectednessApproach(return_zoo, 
                            nlag=1, 
                            nfore=10,
                            window.size=200,
                            model="TVP-VAR",
                            connectedness="Time",
                            VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.96, prior="BayesPrior")))

# Estimate the TVP-VAR model
tvp_var_fit <- fit(tvp_var_model)

# Summary of the estimation results
summary(tvp_var_fit)

# Plot the time-varying parameters
plot(tvp_var_fit)

# Additional diagnostic tests or analysis can be performed as needed


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