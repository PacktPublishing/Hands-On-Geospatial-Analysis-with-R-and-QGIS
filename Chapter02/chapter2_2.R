# Installing ggmap
install.packages("devtools")
devtools::install_github("dkahle/ggmap") 
register_google(key = "google_key_here")
library(ggmap)


#Coordinates of Dhaka
code = geocode("Dhaka")
str(code)
code

# we will uninstall and install old ggmap
#remove.packages("ggplot2")
library(devtools)
#install_github("hadley/ggplot2@v2.2.0")
library(ggmap)
register_google(key = "our_google_key")


dhaka_location = c(lon = code[1][,], lat = code[2][,])
#Download the map of Dhaka
dhaka_map = get_map(location = dhaka_location, zoom = 7)
#Displaying the map of Dhaka
ggmap(dhaka_map)


# zoomed in map
dhaka_map2 = get_map(location = dhaka_location, zoom = 13)
#Displaying the map of Dhaka
ggmap(dhaka_map2)



# zoomed out map
dhaka_map3 = get_map(location = dhaka_location, zoom = 5)
#Displaying the map of Dhaka
ggmap(dhaka_map3)

# Importing an excel file
bd_val = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data/r_val.csv", stringsAsFactors = FALSE)
str(bd_val)
bd_val$ind = factor(bd_val$ind)

# Plotting point data
ggmap(dhaka_map) +
  geom_point(aes(lon, lat), data = bd_val)

# Coloring point data
ggmap(dhaka_map) +
  geom_point(aes(lon, lat, color = value), data = bd_val)

# change ind column to factor type
bd_val$ind = factor(bd_val$ind)
# Coloring point data according to factor data
ggmap(dhaka_map) +
  geom_point(aes(lon, lat, color = ind), data = bd_val)


# Map size to value
ggmap(dhaka_map) +
  geom_point(aes(lon, lat, size = value), data = bd_val)

# Different options for get_map()
?get_map()

# toner-background
dhaka_map_toner = get_map(location = dhaka_location, zoom = 7, maptype="toner-background")
ggmap(dhaka_map_toner)

# satellite-background
dhaka_map_satellite = get_map(location = dhaka_location, zoom = 7, maptype="satellite")
ggmap(dhaka_map_satellite)

# Facetting
ggmap(dhaka_map, base_layer=
        ggplot(bd_val, aes(lon, lat, color = value))) +
  geom_point() + facet_wrap(~ind)

# Adding color argument
ggmap(dhaka_map, base_layer=
        ggplot(bd_val, aes(lon, lat, color = ind))) +
  geom_point() + facet_wrap(~ind)

# qmplot
qmplot(lon, lat, data = bd_val, geom = "point", color = ind) + facet_wrap(~ind)

# SpatialPoints
library(sp)
library(rgdal)
library(maptools)
map = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","indicator_point")
plot(map)

class(map)

# SpatialLines
highway = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","dhaka_highway_polylines")
plot(highway)

# Let's add background to this
install.packages("OpenStreetMap", depend = T)
#install.packages("rJava")
install.packages("PBSmapping")
#library(rJava)
library(RgoogleMaps)
library(PBSmapping)


map_dhaka = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","dhaka")
plot(map_dhaka)
# convert to polyset
polyset = SpatialPolygons2PolySet(map_dhaka)
head(polyset)
# Compuute the bounding box for longitude, latitude points
bounding_box = qbbox(lat = polyset[, "Y"], lon = polyset[, "X"])
# download background map
background_map = GetMap.bbox(bounding_box$lonR, bounding_box$latR)
# Overlaying polygons on a map
PlotPolysOnStaticMap(background_map, polyset, lwd = 3, col = rgb(0.3, 0.6, 0.3, 0.05), add = F)


# we use max.level = 2 to reduce displaying nestect structure
str(map_dhaka, max.level = 2)

# load another map
map_bd = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","BGD_adm3_data_re")
head(map_bd@data)

str(map_bd@polygons, max.level = 2)

#7th element in the Polygons slot of map_bd
seventh_element = map_bd@polygons[[6]]
# make it succinct with max.level = 2 in str() for the 7th element of the bd@Polygons
str(seventh_element, max.level = 2)

# Structure of the 2nd polygon inside seventh_element
str(seventh_element@Polygons[[2]], max.level = 2)

# plot() the coords slot of the 2nd element of the Polygons slot.
plot(seventh_element@Polygons[[2]]@coords)

# To acccess a column
map_bd$NAME_3
# or
map_bd[["NAME_3"]]

map_bd = spTransform(map_bd, CRS("+proj=longlat +datum=WGS84"))
class(bd)

# plot quantitative data
library(GISTools)
choropleth(map_bd, map_bd$value4)

# Plot qualitative data
#install.packages("RColorBrewer")
library(RColorBrewer)
dhaka_div = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","dhaka_div")
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
map_bd = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","BGD_adm3_data_re")
#head(map_bd@data)
str(map_bd@data)
map_bd$value1 = as.numeric(map_bd$value1)
#str(map_bd@data)
qtm(shp = map_bd, fill = "value1")

# Using fill to have a choropleth map
tm_shape(map_bd) +
  tm_borders() + # This adds a border
  tm_fill(col="value1") +
  tm_compass() + # This puts a compass on the bottom left of the map 
  tmap_style("cobalt")

# Using bubbles
tm_shape(map_bd) +
  tm_bubbles(size = "value1", style = "quantile") +
  tm_borders(col="orange3") # Add a colorful border

# labeling
tm_shape(map_bd) +
  tm_fill(col = "value1", style = "quantile") +
  tm_borders() +
  tm_text(text = "NAME_3", size = 0.5)

# More stylized map
tm_shape(map_bd) +
  tm_fill(col = "value1", style = "quantile", title = "Value of  quantitative indicator", palette = "Blues") + 
  tm_borders(col = "grey30", lwd = 0.6) +
  tm_text(text = "NAME_3", size = 0.5) +
  tm_credits("Source: Author", position = c("right", "top"))