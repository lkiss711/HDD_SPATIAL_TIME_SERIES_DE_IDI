# Ez a fajl hajtja végre az alkalmazást, itt van a kontroll.
# Kiss László 2018.10.08
# HDD_CONTROL - jelenlegi fájl
# HDD_common_order.R Algoritmus 1. lépés
# HDD_backshift.R Algoritmus 2. lépés
# HDD_geo_distance.R Algoritmus 3/a lépés
# HDD_difference_vector.R Algoritmus 3/b lépés
# 

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
source("HDD_difference_vector.R")
source("HDD_backshift.R")
source("HDD_Fourier_Transform.R")

# Algoritmus 1. lépés
# A parameterek meghatarozasa - a fuggveny a "HDD_common_order.R" fajlban van 
parameters <- det_common_order(y)  # ez a lépés eltart pár percig, de működik

# A leggyakrabban elofordulo parameterek meghatarozasa
# - a fuggveny a "HDD_common_order.R" fajlban van
common_order <- det_max_order(parameters)
# View(common_order)


# Algoritmus 2. lépés
# A backshift operátor alkalmazása
# - a fuggveny a "HDD_backshift.R" fajlban van
y <- apply_backshift(y,12)


# Algoritmus 3/a lépés
# A regiok foldrajzi kozeppontjanak meghatarozasa - 
# a fuggveny a "HDD_geo_distance.R" fajlban van
geo_distance <- det_geo_distance(df_centers)

# View(geo_distance)
# dif_vector <- difference_vector(geo_distance)
# View(dif_vector)

# Algoritmus 3/b lépés
# A differencia matrix meghatarozasa - 
# a fuggveny a "HDD_difference_vector.R" fajlban van
ts_matrix <- data_all_HDD_wide_spread_ts[3:ncol(data_all_HDD_wide_spread_ts)]
diff_matrix <- apply(ts_matrix,1,difference_vector)



# Algoritmus 3/c lépés
# A differencia matrix meghatarozasa - 
# a fuggveny a "HDD_Fourier_Transform.R" fajlban van

y_list <- list()

for(i in 1:ncol(y)){
  y_list[[i]] <- (y[,i])
}

nfft <- length(y[,1])
overlap <- 40
spM <- pe_matrix (y_list,nfft,overlap)



