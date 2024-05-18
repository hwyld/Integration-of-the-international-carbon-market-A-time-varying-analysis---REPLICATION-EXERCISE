## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Run File for Replication of the TVP-VAR Model
## Henry Wyld

# Set the working directory to where your R scripts are located
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Carbon-Markets"
setwd(Git)

# Load the necessary packages
source("Packages.r")

# ICAP Data Read - Loads and preprocesses data from ICAP
source("ICAP Data Read.r")

# Clearblue Data Read - Loads and preprocesses data from Clearblue
setwd(Git)
source("Clearblue Data Read.r")

# Merge Refinitiv HBEA prices - Merges Refinitiv HBEA price data with existing datasets
setwd(Git)
source("Merge Refinitiv HBEA prices.r")

# Create Research Data - Aggregates and prepares research data for analysis
setwd(Git)
source("Create Research Data.r")

# Weekly Returns - Calculates weekly returns from the processed data
setwd(Git)
source("Weekly Returns.r")

# TVP-VAR model - Fits a Time-Varying Parameter Vector Autoregression model on the dataset
setwd(Git)
source("TVP-VAR model.r")
