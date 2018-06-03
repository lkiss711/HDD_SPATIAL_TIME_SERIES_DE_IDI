# László Kiss
# 06-02-2018 
# HDD_SPATIAL_TIME_SERIES_DE_IDI ver2.0

library(forecast)
library(dplyr)
library(plyr)

datadir <- getwd()

data_file <-  paste(datadir,"/Euro_regio_Heating.rda")
data_file <- gsub(" ", "", data_file, fixed = TRUE)

data <- load(file = data_file)


y <- ts(data_all_HDD_wide_spread_ts[,3:26],
        start =c(1974,1), end = c(2017,12), frequency = 12)

parameters <- list()
parameters_diff <- list()

for(i in 1:ncol(y)){
  y1 <- y[,i]
  ydiff1 <- diff(y[,i],12)
  
  AuModel <- auto.arima(y1,  max.p=5, max.q=5,
                        max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1, 
                        start.p=1, start.q=1, start.P=1, start.Q=1)
  
  AuModel_diff <- auto.arima(ydiff1,  max.p=5, max.q=5,
                             max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1, 
                             start.p=1, start.q=1, start.P=1, start.Q=1)
  
  
  parameters[[i]] <- AuModel$arma
  parameters_diff[[i]] <- AuModel_diff$arma
  
}

df_parameters <- as.data.frame(matrix(unlist(parameters),nrow=24,byrow = T))
df_parameters_diff <- as.data.frame(matrix(unlist(parameters_diff),nrow=24,byrow = T))


df_parameters <- setNames(df_parameters, c("p", "q", "P", "Q", "m", "d", "D"))
df_parameters_diff <- setNames(df_parameters_diff, c("p", "q", "P", "Q", "m", "d", "D"))
# write.csv2(df_parameters,"df_parameters.csv")
# write.csv2(df_parameters_diff,"df_parameters_diff.csv")

df_parameters_with_count <- ddply(df_parameters,.(p,q,P,Q,m,d,D),nrow)
df_parameters_with_count <- setNames(df_parameters_with_count, c("p", "q", "P", "Q", "m", "d", "D","count"))

df_parameters_diff_with_count <- ddply(df_parameters_diff,.(p,q,P,Q,m,d,D),nrow)
df_parameters_diff_with_count <- setNames(df_parameters_diff_with_count, c("p", "q", "P", "Q", "m", "d", "D","count"))

df_parameters_with_count <- df_parameters_with_count[order(-df_parameters_with_count$count),]
df_parameters_diff_with_count <- df_parameters_diff_with_count[order(-df_parameters_diff_with_count$count),]

write.csv2(df_parameters_with_count,"df_parameters_with_count.csv")
write.csv2(df_parameters_diff_with_count,"df_parameters_diff_with_count.csv")



p <- df_parameters_with_count[1,"p"]
P <- df_parameters_with_count[1,"P"]
q <- df_parameters_with_count[1,"q"]
Q <- df_parameters_with_count[1,"Q"]
d <- df_parameters_with_count[1,"d"]
D <- df_parameters_with_count[1,"D"]
m <- df_parameters_with_count[1,"m"]




View(df_parameters_with_count[1,])

