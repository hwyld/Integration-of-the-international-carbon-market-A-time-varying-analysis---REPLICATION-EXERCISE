## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Full Run Script for TVP-VAR
## Author: Henry Wyld
## Date of creation: 2024-03-21

#-------------------------------------
# clear memory
rm(list = ls())
#----------------------------------

## Packages ##
#----------------------------------
# Source the package setup script
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Carbon-Markets"
setwd(Git)
source("Packages.R")

#----------------------------------

# Set the working directory to where your R scripts are located
setwd("/path/to/your/scripts")

# ICAP Data Read - Loads and preprocesses data from ICAP
source("ICAP Data Read.r")

# Clearblue Data Read - Loads and preprocesses data from Clearblue
source("Clearblue Data Read.r")

# Merge Refinitiv HBEA prices - Merges Refinitiv HBEA price data with existing datasets
source("Merge Refinitiv HBEA prices.r")

# Create Research Data - Aggregates and prepares research data for analysis
source("Create Research Data.r")

# Weekly Returns - Calculates weekly returns from the processed data
source("Weekly Returns.r")

# TVP-VAR model - Fits a Time-Varying Parameter Vector Autoregression model on the dataset
source("TVP-VAR model.r")
