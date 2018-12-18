# Plot data
# László Kiss 2018.12.18
# HDD_plot_data.R 

library("countrycode")
library("eurostat")
library("plotly")


# Sys.setenv("plotly_username"="lkiss711")
# Sys.setenv("plotly_api_key"="")

NUTS2_codes <- data.frame(colnames(data_all_HDD_wide_spread_ts[,3:26]))
colnames(NUTS2_codes) = c("geo")
# get_eurostat_dic("geo")
# NUTS2_codes$eustat_codes <- label_eurostat(NUTS2_codes[,"geo"], fix_duplicated = TRUE)

data2plot <- data_all_HDD_wide_spread_ts[,2:26]

p_ts <- plot_ly(data = data2plot, x = ~time)
p_ts <-   add_lines(p_ts,y = ~BE10 ,name = 'BE10', visible = T, line = list(width = 4, color = "#00587b"))
p_ts <- p_ts %>%   layout(title = 'Heating Degree Days in Benelux States by NUTS2 regions')

for(ds in NUTS2_codes$geo){
  
  if(ds != 'BE10'){
    p_ts <-   add_lines(p_ts,y =  eval(parse(text = paste("y = data2plot$",ds)),)
                        ,name = ds, visible = F)

  }
  
}


visible_list <- list()

for(i in 1:length(NUTS2_codes$geo)){
  visible_sublist <- list()
  for(j in 1:length(NUTS2_codes$geo)){
    if(i == j){
      visible_sublist[[j]] = T
    }else{
      visible_sublist[[j]] = F
    }          
  }
  visible_list[[i]] = visible_sublist
}



p_ts <- 
  (p_ts %>% 
     layout(
       updatemenus = list(
         list(yanchor = 'auto',buttons =list(
           list(method = "restyle",args = list("visible", visible_list[[1]]),label = NUTS2_codes$geo[[1]]),
           list(method = "restyle",args = list("visible", visible_list[[2]]),label = NUTS2_codes$geo[[2]]),
           list(method = "restyle",args = list("visible", visible_list[[3]]),label = NUTS2_codes$geo[[3]]),
           list(method = "restyle",args = list("visible", visible_list[[4]]),label = NUTS2_codes$geo[[4]]),
           list(method = "restyle",args = list("visible", visible_list[[5]]),label = NUTS2_codes$geo[[5]]),
           list(method = "restyle",args = list("visible", visible_list[[6]]),label = NUTS2_codes$geo[[6]]),
           list(method = "restyle",args = list("visible", visible_list[[7]]),label = NUTS2_codes$geo[[7]]),
           list(method = "restyle",args = list("visible", visible_list[[8]]),label = NUTS2_codes$geo[[8]]),
           list(method = "restyle",args = list("visible", visible_list[[9]]),label = NUTS2_codes$geo[[9]]),
           list(method = "restyle",args = list("visible", visible_list[[10]]),label = NUTS2_codes$geo[[10]]),
           list(method = "restyle",args = list("visible", visible_list[[11]]),label = NUTS2_codes$geo[[11]]),
           list(method = "restyle",args = list("visible", visible_list[[12]]),label = NUTS2_codes$geo[[12]]),
           list(method = "restyle",args = list("visible", visible_list[[13]]),label = NUTS2_codes$geo[[13]]),
           list(method = "restyle",args = list("visible", visible_list[[14]]),label = NUTS2_codes$geo[[14]]),
           list(method = "restyle",args = list("visible", visible_list[[15]]),label = NUTS2_codes$geo[[15]]),
           list(method = "restyle",args = list("visible", visible_list[[16]]),label = NUTS2_codes$geo[[16]]),
           list(method = "restyle",args = list("visible", visible_list[[17]]),label = NUTS2_codes$geo[[17]]),
           list(method = "restyle",args = list("visible", visible_list[[18]]),label = NUTS2_codes$geo[[18]]),
           list(method = "restyle",args = list("visible", visible_list[[19]]),label = NUTS2_codes$geo[[19]]),
           list(method = "restyle",args = list("visible", visible_list[[20]]),label = NUTS2_codes$geo[[20]]),
           list(method = "restyle",args = list("visible", visible_list[[21]]),label = NUTS2_codes$geo[[21]]),
           list(method = "restyle",args = list("visible", visible_list[[22]]),label = NUTS2_codes$geo[[22]]),
           list(method = "restyle",args = list("visible", visible_list[[23]]),label = NUTS2_codes$geo[[23]]),
           list(method = "restyle",args = list("visible", visible_list[[24]]),label = NUTS2_codes$geo[[24]])
         )))))
p_ts

options(browser = 'false')
api_create(p_ts, filename = "hdd_ts_by_nuts2")


