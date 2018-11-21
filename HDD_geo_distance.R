# Calculate the distances between the geographical centers of NUTS2 regions  
# László Kiss 2018.11.05
# HDD_geo_distance.R 
# Step 3/a of the algorithm

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
