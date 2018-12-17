migration = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 7/Data/migration.csv")
head(migration)

library(rgdal)
syl = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 7/Data", "syl_div")

library(sp)
syl = spTransform(syl, CRS("+proj=longlat +datum=WGS84"))

migration_spdf = merge(syl, migration, by.x="ID_4", by.y="ID")
class(migration_spdf)


spplot(migration_spdf, zcol="val")


#Spatial autocorrelation test
library(spdep)
neighbor_syl = poly2nb(syl)

# Let's have a look at the nb object
plot(syl, col="gray") 
plot(neighbor_syl, coordinates(syl), add = TRUE, col="blue")

# Run Moran's I test
moran.test(migration_spdf$val, nb2listw(neighbor_syl))

moran.mc(migration_spdf$val, nb2listw(neighbor_syl), nsim = 499)

# Spatial autoregression
syl_sar = spautolm(migration_spdf$val~1, listw=nb2listw(neighbor_syl))
summary(syl_sar)


# SAR with predictors
syl_sar_predictor = spautolm(val~agri+density, weights=pop, data=migration_spdf, listw=nb2listw(neighbor_syl))
summary(syl_sar_predictor)

# Poisson
migration_glm = glm(migration ~ agri+density, offset = pop, data=migration_spdf, family = poisson)
summary(migration_glm)

# Check residuals
migration_spdf$residual = residuals(migration_glm)
spplot(migration_spdf, "residual")

# Test residuals for spatial correlation

moran.mc(migration_spdf$residual, nb2listw(neighbor_syl), 499)


###Spatial Interpolation

### Suppose we have temperetature measurements from different stations and we want to spatially interpolate these values to other areas. We will look at the dem file we used before and will plot values in its extent.

#load dem
library(raster)
dem = raster("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter07/Data/dem.tif")  
plot(dem, main="Elevation")
#view coordinate reference system
dem@crs

# Aggregation
dem = aggregate(dem, 4)

# Load the cssv file with temperature data
values = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter07/Data/temp.csv")
# Convert to a SpatialPointsDataFrame
coordinates(values) = ~ longitude + latitude
# Set the coordinate reference system using CRS
proj4string(values) = CRS("+proj=longlat +datum=WGS84")

# Transform to the CRS of the raster file
temp = spTransform(values, CRS(proj4string(dem)))
head(temp@data)

# Convert raster to points
raster = rasterToPoints(dem, spatial = TRUE)
library(rgeos)
# Calculate distance matrix
distance = gDistance(temp, raster, byid = TRUE)
# Calculate nearest point index
nearest_index = apply(dist, 1, which.min)


raster$value = values$value[nearest_index]
raster = rasterize(raster, dem, "value")
plot(raster)

# An intercept-only model

library(gstat)
g = gstat(formula = value ~ 1, data = temp)
print(g)


# Temperature prediction using interpolate()

prediction = interpolate(dem, g)
plot(prediction)
plot(temp, add = TRUE, pch = 18, cex = 0.5)

# Geostatistics

library(sp)
data(meuse)
coordinates(meuse) = c("x", "y")
head(meuse@data)

meuse_coord = meuse@coords
zinc = meuse@data[, 4]
# cbind this two
meuse_zinc = cbind(meuse_coord, zinc)
head(meuse_zinc)

spplot(meuse, "zinc")

spplot(meuse, "zinc", do.log=TRUE)

meuse_zinc = as.geodata(meuse_zinc)
class(meuse_zinc)

points.geodata(meuse_zinc, xlab="X", ylab="Y", pt.divide="quintile")

# Set the chart option to one row, two columns 
par(mfrow=c(1,2))
# Variogram cloud
plot(variog(meuse_zinc,option="cloud"),main="Variogram Cloud")
#Variogram 
plot(variog(meuse_zinc),main="Binned Variogram")
# draw a line
lines(variog(meuse_zinc))
# set the default setting of chart option of 1 row, 1 column
par(mfrow=c(1,1))

library(gstat)
plot(variogram(zinc ~ 1, meuse))

summary(meuse_zinc)

# Variogram
model = variog(meuse_zinc,uvec=seq(40,1600,l=15),bin.cloud=T)
plot(model,main="Variogram Cloud", bin.cloud=T)


model = variog(meuse_zinc)
model_fit = variofit(model)
model_fit

vgm()

model2 = variogram(zinc~1, meuse)
fit.variogram(model2, vgm("Sph"))

model2.fit = fit.variogram(model2, vgm(c("Mat", "Exp", "Gau", "Sph")))
model2.fit

data(meuse.grid)
coordinates(meuse.grid) = c("x", "y")
meuse.grid = as(meuse.grid, "SpatialPixelsDataFrame")

krig_simple = krige(zinc ~ 1, meuse, meuse.grid, model2.fit, beta = 10)
names(krig_simple)
spplot(krig_simple, "var1.pred")

krig_simple$exceedanceProb = 1 - pnorm(1200, mean = krig_simple$var1.pred, sd = sqrt(krig_simple$var1.var))

spplot(krig_simple, zcol = "exceedanceProb")

no_rows = dim(meuse@data)[1]
sample_size = floor(0.75 * no_rows)
train_no = sample(seq_len(no_rows), size = sample_size)
train = meuse[train_no, ]
validation = meuse[-train_no, ]

# In vgm, first argument is partial sill, second is model, third is range
train_fit = fit.variogram(variogram(zinc~1, train), vgm(134746.5, "Mat", 1200, 1))

validation_pred = krige(zinc ~ 1, train, validation, train_fit)

kriging_residual = validation$zinc - validation_pred$var1.pred
mean_residual = validation$zinc - mean(validation$zinc)
1 - sum(kriging_residual^2)/sum(mean_residual^2)


