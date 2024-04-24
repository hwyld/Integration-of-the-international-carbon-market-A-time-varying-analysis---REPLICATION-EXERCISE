## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## TVP-VAR model estimation procedures in R
## Author: Henry Wyld
## Date of creation: 2024-03-20

## References
## https://sites.google.com/view/davidgabauer/econometric-code?authuser=0
## https://sites.google.com/site/fk83research/code?authuser=0


# Install and load required packages
install.packages("vars")
library(vars)

# Import your data
VAR_df <- read.csv("EUR_denom_allowance_prices.csv")

head(VAR_df,5)

# Specify the lag order
lag_order <- 1  # Change this as needed

# Set up the TVP-VAR model
tvp_var_model <- TVP_VAR(data = VAR_df, p = lag_order, type = "mcmc", draws = 1000)
# The type argument specifies the estimation method. "mcmc" uses Markov Chain Monte Carlo (MCMC) methods.
# The draws argument specifies the number of draws for the MCMC estimation.
# Adjust draws as needed. More draws typically lead to better results but require more computation time.

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