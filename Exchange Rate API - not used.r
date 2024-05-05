## ECOM90022 Research Methods, Semester 1, 2024
## Read data from priceR package for necessary currency conversion rates
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

# Assuming that each dataframe has a 'Date' column formatted as Date class
last_date <- "2007-01-02 UTC"

# Most recent date in the datasets
most_recent_date <- today()

if (!require(priceR)) {
    install.packages("priceR")
}
install.packages("priceR")
library(priceR)

Sys.setenv("EXCHANGERATEHOST_ACCESS_KEY"="a406efb8497ce30bec62e3bb5ad17b82")

# Retrieve NZD to EUR exchange rates
nz <- historical_exchange_rates("NZD", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve USD to EUR exchange rates
usd <- historical_exchange_rates("USD", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve CNY to EUR exchange rates
cny <- historical_exchange_rates("CNY", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve AUD to EUR exchange rates
aud <- historical_exchange_rates("AUD", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve GBP to EUR exchange rates
gbp <- historical_exchange_rates("GBP", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve JPY to EUR exchange rates
jpy <- historical_exchange_rates("JPY", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve CAD to EUR exchange rates
cad <- historical_exchange_rates("CAD", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Retrieve KRW to EUR exchange rates
skw <- historical_exchange_rates("KRW", to = "EUR",
                          start_date = last_date, end_date = most_recent_date)

# Merge into a single dataframe
exchange_rates <- merge(usd, nz, cny, aud, gbp, jpy, cad, skw)

tail(exchange_rates)
tail(nz)

# Export as CSV
write.csv(exchange_rates, "historical_exchange_rates.csv")