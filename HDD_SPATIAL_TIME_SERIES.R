# László Kiss
# 06-02-2018 
# HDD_SPATIAL_TIME_SERIES_DE_IDI ver2.0

library(forecast)
library(dplyr)
library(plyr)
library(raster)
library(TSA)



pe_vector <-  function(x,nfft,overlap) {
  lx <- length(x)
  nadvance=trunc(nfft*(1-overlap/100))
  nrecs=trunc((lx-nfft*overlap/100)/nadvance)
  Pe <- rep( 0, len=nfft)
  locseg = 1:nfft
  for(i in 1:nrecs){
       xseg   = x[locseg]  
       Xf <- fft(xseg)
       Pe <-  sapply(Pe, function(x) {Pe+abs(Xf)^2/nfft})
       locseg = locseg + nadvance;  
      }
  Pe=Pe/nrecs;
  Pe[1]= mean(Pe[,2:3])
  se=exp(mean(log(Pe[2:nfft])))
  Pe=Pe/se
  result <- c(Pe,se)
  return(result)
}


pe_matrix <- function(x,nfft,overlay){
  
  ts_periodogram_list <- list()
  col_number <- ncol(x)
  for(i in 1:col_number){
    y <- x[,i]
    ts_periodogram_list[[i]] <- pe_vector(y,nfft,overlay)
  }
  return(ts_periodogram_list)
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

sX <- c(ncol(y),nrow(y))
lx <- sX[[2]]

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



