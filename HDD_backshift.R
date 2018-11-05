# Ez a fájl alkalmazza a BACKSHIFT operátort az idősorokra vonatkozóan.
# Kiss László 2018.11.05
# HDD_backshift.R
# Algoritmus 2. lépés


apply_backshift <- function(ts_vector,m){
  
  first_pos <-  m + 2
  ts_vector_backshift <- ts_vector
  
  for(i in 1:length(ts_vector)){
    
    if(i > m+2){

      ts_vector_backshift[[i]] = ts_vector[[i]] - ts_vector[[i-1]] - ts_vector[[(i-m)]] + ts_vector[[(i-m-1)]]
      
    }
    else{
      
      ts_vector_backshift[[i]] = NA
    
    }
    
  }
  
  ts_vector <- ts_vector_backshift
  return(ts_vector)
  
}
