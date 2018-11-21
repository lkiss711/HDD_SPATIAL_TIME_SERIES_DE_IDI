# Determine the matrix of differencies
# László Kiss 2018.11.05
# HDD_difference_vector.R 
# Step 3/b of the algorithm


difference_vector <- function(v){
  lv <- length(v)
  v.nagy <- rep(v[1:(lv-1)],seq(from=lv-1,to=1)) 
  v.uj <- c()
  for (k in c(2:(lv))) {
    v.uj <- c(v.uj,v[k:lv])
  }
  return(v.uj-v.nagy)    
}


