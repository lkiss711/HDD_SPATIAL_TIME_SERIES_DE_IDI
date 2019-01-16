library("LSTS")
library("astsa")
library("stats")

periodogram <- smooth.periodogram(y[,1], plot = TRUE, spar = 0)

model_000 <- sarima(y[,1],1,0,1,3,1,0,12, details = FALSE)

fcast <- sarima.for(y[,1],10,1,0,1,3,1,0,12)

all_df <- rbind(data_all_HDD_wide_spread_ts[,3],fcast$pred)

pred_ts <- ts(all_df, start = c(1974,1), frequency = 12)
plot.ts(pred_ts)

View(y[,1])
View(fcast$pred)
View(data_all_HDD_wide_spread_ts[,3])


# p00 <- plot_ly(x = ~x, y = ~y, mode = 'lines', text = paste(tm, "days from today"))
# p00
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
