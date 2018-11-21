# Apply the backshift operator to timeseries data
# László Kiss 2018.11.05
# HDD_backshift.R 
# Step 2 of the algorithm


apply_backshift <- function(ts_vector,m){
  
  
  ts_vector_backshift <- ts_vector
  
  ts_vector_backshift <- apply(ts_vector,1,function(x) c(diff(ts_vector,lag =  m)))
  # test2 <- apply(test,2,function(x) c(NA, diff(x)))
    
  return(ts_vector_backshift)
  
}
