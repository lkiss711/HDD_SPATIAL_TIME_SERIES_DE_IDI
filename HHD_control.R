# Ez a fajl hajtja végre az alkalmazást, itt van a kontroll.
# Kiss László 2018.10.08
# HDD_CONTROL


library(forecast)
library(raster)


datadir <- getwd()

# HDD adatok beolvasása fileba
data_file <-  paste(datadir,"/Euro_regio_Heating.rda")
data_file <- gsub(" ", "", data_file, fixed = TRUE)

# NUTS2 regiók földrajzi középpontjának beolvasásása fileba
regio_centers_file <-  paste(datadir,"/regio_centers.rda")
regio_centers_file <- gsub(" ", "", regio_centers_file, fixed = TRUE)

# adatfileok memóriába történő beolvasásása
data <- load(file = data_file)
regio_centers <- load(file = regio_centers_file)

# adatfajlbol idosor konvertalas
y <- ts(data_all_HDD_wide_spread_ts[,3:26],
        start =c(1974,1), end = c(2017,12), frequency = 12)

source("HDD_common_order.R")
source("HDD_geo_distance.R")

parameters <- det_common_order(y)
common_order <- det_max_order(parameters)