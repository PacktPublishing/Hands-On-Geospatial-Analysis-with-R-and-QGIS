landslide = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/slope_hazard.csv")
str(landslide)
range(landslide$Slope_PC)

dem_safe = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/dem_safe.csv")
str(dem_safe)
# Remove id column
dem_safe$id = NULL
str(dem_safe)

slope_safe = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/slope_safe.csv")
str(slope_safe)
# Remove id column
slope_safe$id = NULL

# Combine
safe = cbind(dem_safe, slope_safe)

# Create a hazard indicator variable
safe$hazard = 0

dem_hazard = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/dem_hazard.csv")
str(dem_hazard)
# Remove Longitude and Latitude column
dem_hazard$Longitude = NULL
dem_hazard$Latitude = NULL
str(dem_hazard)

slope_hazard = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/slope_hazard.csv")
str(slope_hazard)
# Remove Longitude and Latitude column
slope_hazard$Longitude = NULL
slope_hazard$Latitude = NULL
str(slope_hazard)

hazard = cbind(dem_hazard, slope_hazard)
# Indicator variable indicating landslide
hazard$hazard = 1

# Composite dataset of hazard and safe zones
landslide = rbind(safe, hazard)
head(landslide)

write.csv(landslide, "F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/model_data.csv")

# Logistic regression
logistic_fit = glm(as.factor(hazard)~ DEM_PC_UTM + Slope_PC, data=landslide, family=binomial)
summary(logistic_fit)

library(raster)
dem = raster("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/DEM_PC_UTM.tif")
slope = raster("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/slope_PC.tif")

# Note that, you can get different vales for these coefficients
val = 3.111299 + dem * (-0.010680) + slope * (-0.123692)

# Predicted probability
logistic_prob = predict(logistic_fit, type="response")

# vector of hazard where we assume that a hazard occurred if the probability is greater than 0.65
pred_class = rep(0, nrow(landslide))
pred_class[logistic_prob > 0.65] = 1
prob = 1/(1+exp(val *(-1)))

# Confusion matrix
confusion = table(pred_class, landslide$hazard)
confusion

# Accuracy
sum(diag(confusion))/sum(confusion)

# Classified probability and plot
class = c(0, 0.3, 1, 0.3, 0.7, 2, 0.7, 1, 3)
class_matrix = matrix(class, ncol=3, byrow = TRUE)
risk_class = reclassify(prob, class_matrix)
plot(risk_class)

prob[prob <= 0.65] = 0
prob[prob > 0.65] = 1
plot(prob)

# CART
landslide_model = read.csv("F:/Hands-on Geospatial Analysis Using R and QGIS/Chapter 10/Data/model_data.csv")
landslide_model$X = NULL
landslide_model$hazard = as.factor(landslide_model$hazard)
str(landslide_model)

install.packages("rpart")
library(rpart)
install.packages("rattle")
library(rattle)
install.packages("rpart.plot")
library(rpart.plot)

# Randomly arrange the dataset
set.seed(0)
n = nrow(landslide_model)
random_data = landslide_model[sample(n),]
train = random_data[1:round(0.7 * n),]
test = random_data[(round(0.7 * n) + 1):n,]

tree_train = rpart(hazard ~ ., train, method = "class", control = rpart.control(cp=0.010))

# Predict the outcome
pred = predict(tree_train, test, type = "class")

# Calculate the confusion matrix
(confusion = table(test$hazard, pred))

# Accuracy
sum(diag(confusion))/sum(confusion)


# Randoim forest

library(randomForest)
rf = randomForest(hazard ~ ., train)

pred = predict(rf, test, type = "class")
mean(pred == test$hazard)
