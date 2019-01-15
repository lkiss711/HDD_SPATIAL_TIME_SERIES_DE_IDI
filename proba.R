library("LSTS")
library("astsa")

periodogram <- smooth.periodogram(y[,1], plot = TRUE, spar = 0)

model_000 <- sarima(y[,1],1,0,1,3,1,0,12, details = FALSE)

sarima.for(y[,1],10,1,0,1,3,1,0,12)

