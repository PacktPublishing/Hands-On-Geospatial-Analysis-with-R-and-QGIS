library(leaflet)
library(rgdal)
bd = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","BGD_adm3_data_re")
class(bd)
head(bd@data)

# Creating a basic leaflet map
library(dplyr)
bd %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons()

# create color palette with colorNumeric()
bd@data$value1 = as.numeric(bd@data$value1)
color_pal = colorNumeric("Blues", domain = bd@data$value1)

# Highlighting with values
bd %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(weight = 1, color = ~color_pal(value1),
              # Labels for displaying value
              label = ~paste0("Value of the indicator ", value1),
              # Add highlight options
              highlight = highlightOptions(weight = 5, color = "white",
                                           bringToFront = TRUE))

# Using base map from 'CartoDB'
bd %>%
  leaflet() %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(weight = 1, color = ~color_pal(value1),
              # Labels for displaying value
              label = ~paste0("Value of the indicator ", value1),
              # Add highlight options
              highlight = highlightOptions(weight = 5, color = "white",
                                           bringToFront = TRUE))


  