# Ez a fájl határozza meg a differenciákat.
# Kiss László 2018.10.08
# HDD_difference_vector.R
# Algoritmus 3/b lépés


difference_vector <- function(v){
  lv <- length(v)
  v.nagy <- rep(v[1:(lv-1)],seq(from=lv-1,to=1)) 
  v.uj <- c()
  for (k in c(2:(lv))) {
    v.uj <- c(v.uj,v[k:lv])
  }
  return(v.uj-v.nagy)    
}


