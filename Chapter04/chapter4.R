# Select Gazipur

library(sp)
library(rgdal)
library(maptools)

map_bd = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","BGD_adm3_data_re")
#head(map_bd@data)

isGazipur = map_bd$NAME_3 == "Gazipur"
# This will select only those feature(s) whose value equals to "Gazipur"
gazipur = map_bd[isGazipur, ]
# Now, save it using writeOGR()
writeOGR(obj=gazipur, dsn="F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data", layer="gazipur", driver="ESRI Shapefile")

# Now import gazipur.shp and plot it
gazipur = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","gazipur")
plot(gazipur, col = "blue")

# Now, plot the map of Dhaka
dhaka = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","dhaka")
plot(dhaka)

# Let's merge these two shapefiles
dhaka_gazipur = raster::union(dhaka, gazipur)
str(dhaka_gazipur, max.level = 2)
plot(dhaka_gazipur, col = "blue")




# Clip
points = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 4/Data/arbitrary_indicator.csv")
coordinates(points) =  ~ lon + lat
summary(points)

plot(map_bd, col = "gray", border = "blue", main = "Map of Bangladesh with arbitrary points plotted")
plot(points, ad = TRUE, pch=19, cex=.4, col = "red")

# Make the two layers to have the same projection system
# Set the projection of points which is saved as SpatialPoints type in R
#x = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(points) <- CRS(proj4string(dhaka))
points_dhaka <- points[dhaka, ]

# Now, plot this data on Dhaka map, we can see that points now have been clipped to Dhaka
plot(dhaka, col = "gray", border = "blue", main = "Points clipped to the map of Dhaka")
plot(points_dhaka, add = TRUE, pch=20, cex=1, col = "red")

# Difference
not_dhaka = gDifference(points, dhaka)
plot(map_bd, col = "gray", border = "blue", main = "All the points except Dhaka's are plotted")
plot(not_dhaka, add = TRUE, pch=20, cex=0.4, col = "red")

# Plotted on Dhaka only
plot(dhaka, col = "gray", border = "blue", main = "gDifference() succesfully removed points inside Dhaka")
plot(not_dhaka, add = TRUE, pch=20, cex=0.4, col = "red")

# Creating buffer
railway_dhaka = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","railway_dhaka")
plot(railway_dhaka, col = "blue", main = "Railway in Dhaka")

# Create a buffer of 5 km ((1/112.32)*5 = 0.04451566951 radian
#x = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
library(raster)
# railway_dhaka = spTransform(railway_dhaka, CRS("+proj=longlat +ellps=WGS84"))
# buffer_rail = gBuffer(railway_dhaka, width = 0.04451566951)
dhaka = spTransform(dhaka, CRS("+proj=longlat +ellps=WGS84"))
dhaka_buffer = gBuffer(dhaka, width = 0.04451566951)
plot(dhaka_buffer, col = "gray", main = "Buffer of 5 km around railway in Dhaka")
plot(dhaka, col = "blue")

# Calcuating area
# Convert longitude latitude to UTM
x = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
bd_utm = spTransform(map_bd, CRS(x))

# Calculate the area
bd_utm$area = gArea(bd_utm, byid = TRUE) / 1000^2
# We can now check that a new column data with area for each feature has bee created
head(bd_utm@data)

