library(raster)
library(rgeos)
library(rgdal)
library(maptools)
library(sp)

library(RStoolbox)
# Set the working directory
setwd("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 4/Data")
# Read metadata
meta_data = readMeta("Mosaic/LC08_L1TP_137043_20180410_20180417_01_T1/LC08_L1TP_137043_20180410_20180417_01_T1_MTL.txt")
summary(meta_data)

# Reading raster data
library(raster)
band4 = raster("Mosaic/LC08_L1TP_137043_20180410_20180417_01_T1/LC08_L1TP_137043_20180410_20180417_01_T1_B4.TIF")
band4
plot(band4)


# Stacking rasters
band1 = raster("Mosaic/LC08_L1TP_137043_20180410_20180417_01_T1/LC08_L1TP_137043_20180410_20180417_01_T1_B1.TIF")
band2 = raster("Mosaic/LC08_L1TP_137043_20180410_20180417_01_T1/LC08_L1TP_137043_20180410_20180417_01_T1_B2.TIF")
band3 = raster("Mosaic/LC08_L1TP_137043_20180410_20180417_01_T1/LC08_L1TP_137043_20180410_20180417_01_T1_B3.TIF")
band5 = raster("Mosaic/LC08_L1TP_137043_20180410_20180417_01_T1/LC08_L1TP_137043_20180410_20180417_01_T1_B5.TIF")
stacked = stack(band1, band2, band3, band4, band5)

# plot stacked
plot(stacked)

# Names of rasters
names(stacked)

# Change projection of band4 image from UTM to long lat
band4_ll = projectRaster(band4, crs = '+proj=longlat')
band4_ll
print(band4_ll)

# Back to UTM
x = " +proj=utm +zone=48 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
band4_utm = projectRaster(band4_ll, crs=x)

# False color composite
plotRGB(stacked, r = 3, g = 2, b = 1, stretch = "hist")

# Slope, Aspect, hillshade
setwd("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 5/Data")
dem = raster("DEM/dem_chittagong.tif")
plot(dem)

slope = terrain(dem, opt = "slope", unit = "degrees")
plot(slope)

aspect = terrain(dem, opt = "aspect")
plot(aspect)

hill_dem = hillShade(slope, aspect, 40, 270)
plot(hill_dem)

# NDVI
NDVI = (band4 - band5) / (band4 + band5)
plot(NDVI)

# Rules 
rules = c(-1, 0, 1, 0, 0.5, 2, 0.5, 1, 3)
class = matrix(rules, ncol = 3, byrow = TRUE)

####Now classify and plot
classified_ndvi = reclassify(NDVI, class)
plot(classified_ndvi)