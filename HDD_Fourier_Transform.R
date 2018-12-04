# Calculate the smoothed periodogram
# László Kiss 2018.11.05
# HDD_Fourier_Transform.R 
# Step 3/c of the algorithm



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
  sPe <- Pe/se1
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