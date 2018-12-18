# Put the NUTS2 regions center to a map
# László Kiss 2018.12.18
# HDD_plot_map.R 

library(leaflet)
library(maps)
library(htmltools)
library(htmlwidgets)

icons <- awesomeIcons(
  icon = 'disc',
  iconColor = 'black',
  library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
  markerColor = 'blue',
  squareMarker = TRUE
)

bounds <- map('world', fill=TRUE, plot=FALSE)


map <- leaflet(data = df_centers) %>%
 setView(5.18, 51.56, zoom = 6) %>% 
  addProviderTiles("CartoDB.Positron", group = "Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
  addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>%

  addMarkers(~lng, ~lat,popup = 
               paste("Region name: ", df_centers$name, "<br>",
                     "NUTS2 code: ", df_centers$geo, "<br>",
                     "Longitude: ", df_centers$lng, "<br>",
                     "Latitude: ", df_centers$lat, "<br>"
               ),  group = "Sites") %>% 
  
  addPolygons(data=bounds, group="States", weight=0, fillOpacity = 0) %>%
  addScaleBar(position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Map", "Satellite", "Relief"),
    options = layersControlOptions(collapsed = FALSE)
  )
invisible(print(map))


