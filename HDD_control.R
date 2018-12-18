# This is the control file
# László Kiss 10.08/2018
# HDD_CONTROL - this file
# HDD_common_order.R Step 1 of the algorithm 
# HDD_backshift.R Step 2 of the algorithm
# HDD_geo_distance.R Step 3/a of the algorithm
# HDD_difference_vector.R Step 3/b of the algorithm
 

library(forecast)
library(raster)


datadir <- getwd()

#Reading the Heating Degree Days data
data_file <-  paste(datadir,"/Euro_regio_Heating.rda")
data_file <- gsub(" ", "", data_file, fixed = TRUE)

#Reading the geographic data of NUTS2 regions
regio_centers_file <-  paste(datadir,"/regio_centers.rda")
regio_centers_file <- gsub(" ", "", regio_centers_file, fixed = TRUE)

#Load datafiles to memory
data <- load(file = data_file)
regio_centers <- load(file = regio_centers_file)

#Convert timeseries from data
y <- ts(data_all_HDD_wide_spread_ts[,3:26],
        start =c(1974,1), end = c(2017,12), frequency = 12)

source("HDD_common_order.R")
source("HDD_geo_distance.R")
source("HDD_difference_vector.R")
source("HDD_backshift.R")
source("HDD_Fourier_Transform.R")

# Step 1 of the algorithm
# Determine the orders, function can be found in this file: HDD_common_order.R 
parameters <- det_common_order(y)  # it is takes a few minutes, 

# Find the common order
# function can be found in this file: HDD_common_order.R 
common_order <- det_max_order(parameters)
# View(common_order)


# Step 2 of the algorithm
# Apply the backshift operator to timeseries data
# function can be found in this file: HDD_backshift.R
y <- apply_backshift(y,12)


# Step 3/a of the algorithm
# Calculate the distances between the geographical centers of NUTS2 regions  
# function can be found in this file: HDD_geo_distance.R
geo_distance <- det_geo_distance(df_centers)

# View(geo_distance)
# dif_vector <- difference_vector(geo_distance)
# View(dif_vector)

# Step 3/b of the algorithm
# Determine the matrix of differencies
# function can be found in this file: HDD_difference_vector.R
ts_matrix <- data_all_HDD_wide_spread_ts[3:ncol(data_all_HDD_wide_spread_ts)]
diff_matrix <- apply(ts_matrix,1,difference_vector)



# Step 3/c of the algorithm
# Calculate the smoothed periodogram 
# function can be found in this file: HDD_Fourier_Transform.R

y_list <- list()

for(i in 1:ncol(y)){
  y_list[[i]] <- (y[,i])
}

nfft <- length(y[,1])
overlap <- 40
spM <- pe_matrix (y_list,nfft,overlap)

source("HDD_plot_map.R")
