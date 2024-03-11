## ECOM90022 Research Methods, Semester 1, 2024
## TVP-VAR model estimation procedures in R
## Author: Henry Wyld
## Date of creation: 2024-03-20


# Install and load required packages
install.packages("vars")
library(vars)

# Import your data
# Assuming your data is already imported and named appropriately

# Specify the lag order
lag_order <- 1  # Change this as needed

# Set up the TVP-VAR model
tvp_var_model <- TVP_VAR(data = your_data_matrix, p = lag_order, type = "mcmc", draws = 1000)  
# Adjust draws as needed. More draws typically lead to better results but require more computation time.

# Estimate the TVP-VAR model
tvp_var_fit <- fit(tvp_var_model)

# Summary of the estimation results
summary(tvp_var_fit)

# Plot the time-varying parameters
plot(tvp_var_fit)

# Additional diagnostic tests or analysis can be performed as needed
