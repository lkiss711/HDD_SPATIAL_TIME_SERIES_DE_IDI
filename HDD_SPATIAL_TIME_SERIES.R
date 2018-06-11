# László Kiss
# 06-02-2018 
# HDD_SPATIAL_TIME_SERIES_DE_IDI ver2.0

library(forecast)
library(dplyr)
library(plyr)
library(raster)
library(TSA)



pe <-  function(x,nfft,overlap) {
  
  lx <- length(x)
  # result <- list(center=center,spread=spread)
  nadvance=trunc(nfft*(1-overlap/100))
  nrecs=trunc((lx-nfft*overlap/100)/nadvance)
  Pe <- matrix( rep( 0, len=nfft), nrow = 1)
  locseg = 1:nfft
  for(i in 1:nrecs){
       # MATLAB code: xseg   = x(locseg);  ???  
       Xf <- fft(x[1:nfft])
       Pe <-  sapply(Pe, function(x) {Pe+abs(Xf^2/nfft)})
       locseg = locseg + nadvance;  
      }
  Pe=Pe/nrecs;
  Pe[1]= mean(Pe[,2:3])
  se=exp(mean(log(Pe(2:nfft))))
  Pe=Pe/se
  result <- c(Pe,se)
  return(result)
}


datadir <- getwd()

data_file <-  paste(datadir,"/Euro_regio_Heating.rda")
data_file <- gsub(" ", "", data_file, fixed = TRUE)
regio_centers_file <-  paste(datadir,"/regio_centers.rda")
regio_centers_file <- gsub(" ", "", regio_centers_file, fixed = TRUE)

data <- load(file = data_file)
regio_centers <- load(file = regio_centers_file)

gdis <- pointDistance(df_centers[,2:3], lonlat=TRUE)

colnames(gdis) <- df_centers[,1]
rownames(gdis) <- df_centers[,1]


y <- ts(data_all_HDD_wide_spread_ts[,3:26],
        start =c(1974,1), end = c(2017,12), frequency = 12)

parameters <- list()
coef_list <- list()

print(ncol(y))

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


df_parameters <- as.data.frame(matrix(unlist(parameters),nrow=ncol(y),byrow = T))


df_parameters <- setNames(df_parameters, c("p", "q", "P", "Q", "m", "d", "D","drift"))

write.csv2(df_parameters,"parameters.csv")
write.csv2(gdis,"distancematrix.csv")


for(i in 1:(length(colnames(df_parameters))-1)){
  print(colnames(df_parameters)[[i]])
  print(table(df_parameters[,colnames(df_parameters)[[i]]]))
}

ts_periodogram_list <- list()

for(i in 1:ncol(y)){
  
  ts_periodogram_list[[i]] <- periodogram(y[,i])
  names(ts_periodogram_list)[[i]] <- colnames(y)[[i]]  

}



