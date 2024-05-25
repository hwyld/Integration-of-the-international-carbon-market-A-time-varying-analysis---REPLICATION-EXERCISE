## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Run File for Replication of the TVP-VAR Model
## Henry Wyld

###### READ THIS TO RUN ######
# Run each script individually in the order below

# Warnings may appear after running source("ICAP Data Read.r"), this is expected. 
# Continue running the scripts in order.

# If want to reproduce from raw data, then:
#-------------------------------------
# Need the following source files 
# ICAP : ("Raw ICAP Data.xlsx") - source from the ICAP_Data wd
# Clearblue : (eu_ets.xlsx"),(new_zealand_ets.xlsx"),(WCI.xlsx") - source from the Clearblue_Data wd
# Exchange rate data : ("Refinitiv_Exchange_Rates.csv") - source from Git wd
# Reuters data for HBEA : ("Refinitiv_HUBEI.csv") - source from Git wd
# Event Study data: ("events_study_data.csv") - source from Git wd
# Run all R files in run list below
#-------------------------------------

# If not wanting to reproduce from raw data, then:
#-------------------------------------
# Need the following file:
# Cleaned Research Data: ("Research_data.csv") - source from Git wd
# Event Study data: ("events_study_data.csv") - source from Git wd
# Only Run those files marked 1) in run list below
#-------------------------------------

# If running from the single wd, update all 3 working directories in the Packages.r file

# 1)
# Set the working directory to where your R scripts are located
Git <- "C:/Users/henry/OneDrive - The University of Melbourne/GitHub/TVP-VAR-for-Carbon-Markets"
setwd(Git)

# 1)
# Load the necessary packages
source("Packages.r")

    # ICAP Data Read - Loads and preprocesses data from ICAP
    source("ICAP Data Read.r")

    # Warnings may appear after running script, this is expected. 
    # Continue running the scripts in order.

    # Clearblue Data Read - Loads and preprocesses data from Clearblue
    setwd(Git)
    source("Clearblue Data Read.r")

    # Warnings may appear after running script, this is expected. 
    # Continue running the scripts in order.

    # Merge Refinitiv HBEA prices - Merges Refinitiv HBEA price data with existing datasets
    setwd(Git)
    source("Merge Refinitiv HBEA prices.r")

    # Warnings may appear after running script, this is expected. 
    # Continue running the scripts in order.

    # Create Research Data - Aggregates and prepares research data for analysis
    setwd(Git)
    source("Create Research Data.r")

# 1)
# Weekly Returns - Calculates weekly returns from the processed data
setwd(Git)
source("Weekly Returns.r")

# 1)
# TVP-VAR model - Fits a Time-Varying Parameter Vector Autoregression model on the dataset
setwd(Git)
source("TVP-VAR model.r")

# Finished
message("All scripts have been run successfully.")