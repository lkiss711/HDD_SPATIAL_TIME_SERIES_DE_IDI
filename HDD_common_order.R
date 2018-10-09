# Ez a fájl határozza meg a közös rendet az idősorokra vonatkozóan.
# Kiss László 2018.10.08
# HDD_common_order.R
# Algoritmus 1. lépés

library(plyr)

det_common_order <- function(y_local){
      
      
      parameters <- list()
      coef_list <- list()
      
      
      for(i in 1:ncol(y_local)){
        y1 <- y_local[,i]
        
        AuModel <- auto.arima(y1,  max.p=5, max.q=5,
                              max.P=3, max.Q=3, max.order=8, max.d=2, max.D=1,
                              start.p=1, start.q=1, start.P=1, start.Q=1)
        
        
        
        parameters[[i]] <- AuModel$arma
        coef_list[[i]] <- AuModel$coef
        new_col <- length(parameters[[i]])+1
        parameters[[i]][[new_col]] <- coef_list[[i]][[length(coef_list[[i]])]]
        print(parameters[[i]])
      }
      
      df_parameters <- as.data.frame(matrix(unlist(parameters),nrow=ncol(y_local),byrow = T))
      
      df_parameters <- setNames(df_parameters, c("p", "q", "P", "Q", "m", "d", "D","drift"))

      return(df_parameters)
}

# Ez a függvény meghatározza a leggyakrabban előforduló paraméter-vektort. 

det_max_order <- function(df_orders){
  
  orders <- count(df_orders, vars = c("p", "q", "P", "Q", "m", "d", "D"))
  
  orders <- arrange(orders,desc(orders[,"freq"]))
  
  common_order <- orders[1,1:ncol(orders)-1]
  
  return(common_order)
}
