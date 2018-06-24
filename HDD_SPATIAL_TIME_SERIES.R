# László Kiss
# 06-02-2018 
# HDD_SPATIAL_TIME_SERIES_DE_IDI ver2.0

library(forecast)
library(dplyr)
library(plyr)
library(raster)
library(TSA)
library(astsa)


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

#
#    2018.06.25
#

x <- y[,1]
arima(x, order = c(2, 0, 0))
par(mfrow=c(3,1))
k = kernel("daniell", 4)
Spx.ave = spec.pgram(x, k, taper=0, log="no")
# abline(v=c(.25,1,2,3), lty=2)
spObj <- spec.arma(ar=c(1,-.9), log="no", main="Autoregression")
(sze <- exp(mean(log(spObj$spec))))
nfft <- length(y[,1])
# overlap <- 0
# overlap <- 20
overlap <- 40
spu <- pe_vector(x,nfft,overlap)
plot(spu$omega[1:(nfft/2)] ,spu$sPe[1:(nfft/2)]*spu$se,type='l') 

sqrt(spu$se)


par(mfrow=c(1,1))

y_list <- list()

for(i in 1:ncol(y)){
  y_list[[i]] <- (y[,i])
}

spM <- pe_matrix (y_list,nfft,overlap)
# plot(spM$omega[[1]][1:(nfft/2)], spM$sp[[1]][1:(nfft/2)],col="red",type='l')
# lines(spM$omega[[1]][1:(nfft/2)],spM$sp[[3]][1:(nfft/2)],col="green")
# sqrt(spM$se[[ 1]])
