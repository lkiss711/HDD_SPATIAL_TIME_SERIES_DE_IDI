# László Kiss
# 10-04-2018 
# HDD_SPATIAL_TIME_SERIES_DE_IDI ver2.0

library(forecast)
# library(dplyr)
# library(plyr)
library(raster)
# library(TSA)
# library(astsa)



pe_vector <-  function(x,nfft,overlap) {
  lx <- length(x)
  nadvance=trunc(nfft*(1-overlap/100))
  nrecs=trunc((lx-nfft*overlap/100)/nadvance)
  Pe <- rep( 0, len=nfft)
  locseg = 1:nfft
  for(i in 1:nrecs){
    xseg   <-  x[locseg]  
    Xf <- fft(xseg)
    Pe <-  Pe+abs(Xf)^2 #sapply(Pe, function(x) {
    locseg  <-  locseg + nadvance;  
  }
  Pe <- Pe/nrecs/nfft
  Pe[1] <-  mean(Pe[2:3])   # [,2:3]
  se=exp(mean(log(Pe[1:nfft])))
  sPe <- Pe/se
  omega <- c(0,2*pi/nfft*(1:nfft))
  result <- list(sPe=sPe,se=se,omega=omega)
  return(result)
}


pe_matrix <- function(y,nfft,overlap){
  ts_periodogram_list <- list()
  ts_periodogram_list <-  sapply(y, pe_vector, nfft,overlap)
  sp <- ts_periodogram_list[seq(1, 3*length(y), by=3 )]
  se <- ts_periodogram_list[seq(2, 3*length(y), by=3 )]
  omega <- ts_periodogram_list[3]
  return(list(sp=sp,se=se,omega=omega))
}


difference_vector <- function(v){
  lv <- length(v)
  v.nagy <- rep(v[1:(lv-1)],seq(from=lv-1,to=1)) 
  v.uj <- c()
  for (k in c(2:(lv))) {
    v.uj <- c(v.uj,v[k:lv])
  }
  return(v.uj-v.nagy)    
}


datadir <- getwd()

data_file <-  paste(datadir,"/Euro_regio_Heating.rda")
data_file <- gsub(" ", "", data_file, fixed = TRUE)
regio_centers_file <-  paste(datadir,"/regio_centers.rda")
regio_centers_file <- gsub(" ", "", regio_centers_file, fixed = TRUE)

data <- load(file = data_file)
regio_centers <- load(file = regio_centers_file)


# Algoritmus 1. lépés

y <- ts(data_all_HDD_wide_spread_ts[,3:26],
        start =c(1974,1), end = c(2017,12), frequency = 12)

parameters <- list()
coef_list <- list()


for(i in 1:ncol(y)){
  y1 <- y[,i]

  AuModel <- auto.arima(y1,  max.p=5, max.q=5,
                        max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1,
                        start.p=1, start.q=1, start.P=1, start.Q=1)



  parameters[[i]] <- AuModel$arma
  coef_list[[i]] <- AuModel$coef
  new_col <- length(parameters[[i]])+1
  parameters[[i]][[new_col]] <- coef_list[[i]][[length(coef_list[[i]])]]
  print(parameters[[i]])
}


# Algoritmus 2. lépés


???
  
  
# Algoritmus 3/a lépés
  
  gdis <- pointDistance(df_centers[,2:3], lonlat=TRUE)

  # View(gdis)

  gdisV <- gdis[lower.tri(gdis, diag = FALSE)]
  
  # View(gdisV)
  
  colnames(gdis) <- df_centers[,1]
  rownames(gdis) <- df_centers[,1]

# Algoritmus 3/b lépés
  
  
  ts_matrix <- data_all_HDD_wide_spread_ts[3:ncol(data_all_HDD_wide_spread_ts)]
  
  # diff_matrix <- differences_matrix(ts_matrix)
  diff_matrix <- apply(ts_matrix,2,difference_vector)
  # write.csv2(diff_matrix,"diffmatrix.csv")
  # View(diff_matrix)
  
# Algoritmus 3/b lépés
  
    
  y_list <- list()
  
  nfft <- length(y[,1])
  overlap <- 40
  
  for(i in 1:ncol(y)){
    y_list[[i]] <- (y[,i])
  }
  
  spM <- pe_matrix (y_list,nfft,overlap)
  # View(spM)  
