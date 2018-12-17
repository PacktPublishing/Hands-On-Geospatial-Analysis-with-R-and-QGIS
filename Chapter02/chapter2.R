# SpatialPoints
library(sp)
library(rgdal)
library(maptools)
map = readOGR("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data","indicator")
plot(map)

class(map)

# Importing an excel file
bd_val = read.csv("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data/r_val.csv", stringsAsFactors = FALSE)
str(bd_val)

# Convert it into SpatialPointsDataframe
coordinates(bd_val) = c("lon", "lat")
str(bd_val)

plot(bd_val, col = "blue", pch = 19)

# SpatialLines
highway = readOGR("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data","dhaka_gazipur")
plot(highway)

map_dhaka = readOGR("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data","dhaka")
plot(map_dhaka)


# Use max.level = 2 to show a reduced or succinct structure
str(map_dhaka, max.level = 2)

# load another map
map_bd = readOGR("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data","BGD_adm3_data_re")
head(map_bd@data)

str(map_bd@polygons, max.level = 2)

# 6th element in the Polygons slot of map_bd
sixth_element = map_bd@polygons[[6]]
# Have a succinct display of the 7th element of the bd@Polygons
str(sixth_element, max.level = 2)

# Structure of the 2nd polygon inside seventh_element
str(sixth_element@Polygons[[2]], max.level = 2)

# plot() the coords slot of the 2nd element of the Polygons slot.
plot(sixth_element@Polygons[[2]]@coords)

# To acccess a column
map_bd$NAME_3
# or
map_bd[["NAME_3"]]

# Adding point data on polygon data
plot(map_bd)
points(bd_val, pch=19, col="blue")

# Using different projection
map_bd = spTransform(map_bd, CRS("+proj=longlat +datum=WGS84"))
class(map_bd)

# plot quantitative data
library(GISTools)
choropleth(map_bd, as.numeric(map_bd$value2))

# Plot qualitative data
#install.packages("RColorBrewer")
library(RColorBrewer)
dhaka_div = readOGR("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data","dhaka_div")
# check how many unique elements map_bd$NAME_3 has by writing unique(dhaka_div$NAME_3)
unique(dhaka_div$NAME_3)
# There are 7 unique districts and so pick 7 colors
colors = colorRampPalette(brewer.pal(12, "Set3"))(7)
dhaka_div$NAME_3 = as.factor(as.character(dhaka_div$NAME_3))
spplot(dhaka_div, "NAME_3", main = "Coloring different districts of Dhaka division", col.regions = colors, col = "white")


# Using tmap
install.packages("tmap")
library(tmap)

# load a map
map_bd = readOGR("F:/Hands-on-Geospatial-Analysis-Using-R-and-QGIS/Chapter02/Data","BGD_adm3_data_re")
#head(map_bd@data)
str(map_bd@data)
map_bd$value1 = as.numeric(map_bd$value1)


#str(map_bd@data)
qtm(shp = map_bd, fill = "value1")


tm_shape(map_bd) +
  tm_borders() +
  tm_fill(col="value1") +
  tm_compass() + # This puts a compass on the bottom right of the map 
  tmap_style("cobalt")

# Using bubbles
tm_shape(map_bd) +
  tm_bubbles(size = "value1", style = "quantile") +
  tm_borders(col="orange3") # Add a colorful border

# Labeling
tm_shape(map_bd) +
  tm_fill(col = "value1", style = "quantile") +
  tm_borders() +
  tm_text(text = "NAME_3", size = 0.5)

# Another
tm_shape(map_bd) +
  tm_fill(col = "value1", style = "quantile", title = "Value of  quantitative indicator", palette = "Blues") + 
  tm_borders(col = "grey30", lwd = 0.6) +
  tm_text(text = "NAME_3", size = 0.5) +
  tm_credits("Source: Author", position = c("right", "top"))

