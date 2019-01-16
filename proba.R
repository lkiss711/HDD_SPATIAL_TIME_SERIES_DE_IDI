library("LSTS")
library("astsa")
library("stats")

periodogram <- smooth.periodogram(y[,1], plot = TRUE, spar = 0)

model_000 <- sarima(y[,1],1,0,1,3,1,0,12, details = FALSE)

sarima.for(y[,1],10,1,0,1,3,1,0,12)

# 
# xx <- decompose(x)
# xx_stl <- stl(x, s.window = "periodic")
# 
# plot(xx_stl$time.series)
# 
# plot(xx$trend)
# 
# plot.stl(x)
# 
# x <- ts(y[,1], start = c(1974,1), frequency = 12)
