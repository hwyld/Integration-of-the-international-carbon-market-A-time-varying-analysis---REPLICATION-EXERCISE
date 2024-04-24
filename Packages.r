## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Helpful functions for installing and loading packages
# Henry Wyld

# Function to install and load packages
load_packages <- function() {
  # List of packages with descriptions
  packages <- list(
    "devtools" = "install and load packages from GitHub",
    "ggplot2" = "flexible package for plotting in R", 
    "rio" = "easily read data into R", 
    "car" = "joint linear restrictions in R", 
    "lmtest" = "hypothesis tests in R", 
    "sandwich" = "robust variance estimator in R", 
    "quantmod" = "download financial data", 
    "dynlm" = "time-series models in R", 
    "estimatr" = "robust standard errors in R", 
    "zoo" = "handling time-series dates in R", 
    "broom" = "used for ggplot2 tidy", 
    "tidyr" = "used for ggplot2 spread", 
    "scales" = "used for formatting scales in ggplot2",
    "dplyr" = "data manipulation and transformation", 
    "stringr" = "string operations", 
    "lubridate" = "date-time manipulations"
  )

  # Function to check and install packages
  check_and_install <- function(pkg, desc){
    if(!pkg %in% installed.packages()){
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
    message(paste(pkg, "-", desc))
  }

  # Apply the function to each package
  mapply(check_and_install, names(packages), packages)
}