# Determine the |c*omega|^2
# László Kiss 2018.11.27
# HDD_backshift.R 
# Step 3/d of the algorithm

library("astsa")
library("pracma")


my_arima = arima(ts_matrix[,1],order = c(1,0,1), seasonal = c(3,1,0))


phi = c(my_arima$coef[[1]])

omega = spM$omega[[1]]


e_power <- function(omega){
  
  x = exp(omega*(0+1i))
  return(x)
}

e_power_to_s <- function(omega){

  x = exp(omega*(0+1i)*12)
  return(x)
}


z <- lapply(omega,e_power)
u <- lapply(omega,e_power_to_s )

