## ECOM90022 Research Methods, Semester 1, 2024
## Replication for Chenyan Lyu, Bert Scholtens, Integration of the international carbon market: A time-varying analysis, Renewable and Sustainable Energy Reviews, Volume 191,
## Visualisation of Time-Varying Parameters Vector Autoregression (TVP-VAR) Model
## Author: Henry Wyld
## Date of creation: 2024-03-20


# Plot Impulse Response Functions (IRF)
plot(IRF(tvp_var_fit))

# Plot Variance Decomposition
plot(varDecomp(tvp_var_fit))

# Plot Connectedness Index
plot(connectedness(tvp_var_fit))

# Plot Spillover Index
plot(spillover(tvp_var_fit))

# Construct Network Plot
library(igraph)
network_matrix <- fitted(tvp_var_fit)  # Get the fitted values to construct the network
network_graph <- graph_from_adjacency_matrix(network_matrix, mode = "directed")
plot(network_graph, edge.arrow.size = 0.5, layout = layout.circle)

# Plot Heatmaps
# Connectedness Matrix
connectedness_matrix <- connectedness(tvp_var_fit)
heatmap(connectedness_matrix, main = "Connectedness Matrix")

# Spillover Matrix
spillover_matrix <- spillover(tvp_var_fit)
heatmap(spillover_matrix, main = "Spillover Matrix")

# Dynamic Correlation Plots
# Extract Dynamic Correlation Matrices
dynamic_correlation_matrices <- dcc(tvp_var_fit)
# Plot Dynamic Correlation Matrix
plot(dynamic_correlation_matrices, plot.type = "l")


## Using plotly for interactive plots

# Plot Connectedness Index with plotly
connectedness_plot <- plot_ly(x = ~time(tvp_var_fit), y = ~connectedness(tvp_var_fit), type = 'scatter', mode = 'lines', name = 'Connectedness Index')
connectedness_plot <- connectedness_plot %>% layout(title = 'Connectedness Index Over Time')

# Plot Spillover Index with plotly
spillover_plot <- plot_ly(x = ~time(tvp_var_fit), y = ~spillover(tvp_var_fit), type = 'scatter', mode = 'lines', name = 'Spillover Index')
spillover_plot <- spillover_plot %>% layout(title = 'Spillover Index Over Time')

# Display the plots
connectedness_plot
spillover_plot