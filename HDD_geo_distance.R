# Ez a fájl határozza meg a földrajzi távolságot a régió középpontjai között.
# Kiss László 2018.10.08
# HDD_geo_distance.R
# Algoritmus 3/a lépés

library(raster)

det_geo_distance <- function(df_centers){

      gdis <- pointDistance(df_centers[,2:3], lonlat=TRUE)
      
      # View(gdis)
      
      # gdisV <- gdis[lower.tri(gdis, diag = FALSE)]
      
      # View(gdisV)
      
      colnames(gdis) <- df_centers[,1]
      rownames(gdis) <- df_centers[,1]
      return(gdis)
}
