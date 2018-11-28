#install.packages("spatstat")

library(spatstat)

bd_ppp = ppp(x = runif(500, 87.71, 92.79), y = runif(500, 20.6, 26.7), c(87.7, 92.8), c(20.59, 26.71))
bd_ppp
plot(bd_ppp, asp = 1)

summary(bd_ppp)


install.packages("GISTools")
require(GISTools)
data(newhaven)
class(newhaven)

location = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 6/Data/ppp1.csv")
ppp_object = ppp(location$lon, location$lat, c(min(location$lon), max(location$lon)), c(min(location$lat), max(location$lat)))
plot(ppp_object)
dim(location)

#ppp_object = ppp(location$lon, location$lat, c(89.6, 91.01), c(23.35, 24.364))
# Remove duplicates
lonlat = paste(location$lon, location$lat, sep="")

location = location[!duplicated(lonlat), ]
im(location)
write.csv(location, "F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 6/Data/ppp1.csv")
library(sp)
library(rgdal)
library(maptools)
dhaka_div = readOGR("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 2/Data","dhaka_div")
plot(dhaka_div)

summary(marked_ppp)




### Marked point pattern
marked = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 6/Data/ppp2.csv")
marked_ppp = ppp(marked$lon, marked$lat, c(min(marked$lon), max(marked$lon)), c(min(marked$lat), max(marked$lat)), marks = factor(marked$value))
plot(marked_ppp, cols = c("green", "red"), pch = c(19, 20), main = "Disease point pattern")

plot(split(marked_ppp))
summary(marked_ppp)

plot(density(split(marked_ppp)), main = "Densities for yes and no", ribbon = FALSE)

plot(relrisk(marked_ppp), main = "Relative proportions of intensity")

location3 = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 6/Data/ppp3.csv")
ppp_object3 = ppp(location3$lon, location3$lat, c(min(location3$lon), max(location3$lon)), c(min(location3$lat), max(location3$lat)))
quadrat.test(ppp_object3)

quadrat.test(ppp_object3, alternative = "clustered")


#install.packages("osmar")

ppp_gf = envelope(ppp_object3, Gest, correction = "border")
plot(ppp_gf, main = "G-function")

ppp_gf = envelope(ppp_object3, Kest, correction = "border")
plot(ppp_gf, main = "K-function")


mad.test(ppp_object3, Kest)

dclf.test(ppp_object3, Kest)


### L function
ppp_lf = envelope(ppp_object3, Lest, correction = "border")
plot(ppp_lf, main = "L function")

### Spatial segregation

install.packages("spatialkernel")
library(spatialkernel)

# bandwidth
bandwidth = spseg(marked_ppp, h = seq(0, 100000, by = 50), opt = 1)
# Selected bandwidth
print(bandwidth$hcv)

# 1000 simulations
simulations = spseg(pts = marked_ppp, h = bandwidth$hcv, opt = 3, ntest = 100, proc = FALSE)
# Plot the segregation map for "yes"
plotmc(simulations, "yes")


