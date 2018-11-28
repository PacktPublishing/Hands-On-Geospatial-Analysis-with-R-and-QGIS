# Vector
val = c(1, 2, 3, 4, 5,  6)
x = c(1, 2.0, 3.0, 4, 5, "Hello", "OK")
# Check class
class(x)
# label vectors
temperature = c(morning = 20, before_noon = 23, after_noon = 25, evening = 22, night =  18)

# Basic operations with vector
jan_price = c(10, 20, 30)
increase = c(1, 2, 3)
mar_price = jan_price + increase
june_price = c(20, 25, 33)

# Matrix
matrix(jan_price, mar_price, june_price)
all_prices = matrix(c(jan_price, mar_price, june_price), nrow= 3)
all_prices

all_prices2 = matrix(c(jan_price, mar_price, june_price), nrow= 3, byrow = TRUE)
all_prices2

items = c("potato", "rice", "oil")
class(items)

# Array
# Create six vectors
jan_2018 = c(10, 11, 20)
mar_2018 = c(20, 22, 25)
june_2018 = c(30, 33, 33)
jan_2017 = c(10, 10, 17)
mar_2017 = c(18, 23, 21)
june_2017 = c(25, 31, 35)

# Now combine these vectors into array
combined = array(c(jan_2018, mar_2018, june_2018, jan_2017, mar_2017, june_2017),dim = c(3,3,2))
combined

# Dataframe
all_prices3 = data.frame(items, jan_price, mar_price, june_price)
all_prices3

all_prices3$mar_price

# Deleting first item from all_prices3 and save it as all_prices4 
all_prices4 = all_prices3[-1]
all_prices4

# Add a row using rbind()
pen = c(3, 4, 3.5)
all_prices4 = rbind(all_prices4, pen)
all_prices4

# Add a column using cbind()
aug_price = c(22, 24, 31, 5)
all_prices4 = cbind(all_prices4, aug_price)
all_prices4
# or

all_prices3[["mar_price"]]
all_prices3[2, 3]

# list
all_prices_list = list(items, jan_price, mar_price, june_price)
all_prices_list

# Now suppose, we add price of egg to items and we also add prices for jan_price and mar_price and not for june_price. 

items = c("potato", "rice", "oil", "egg")
jan_price = c(10, 20, 30, 15)
mar_price = c(11, 22, 33, 18)

# Now items, jan_price and mar_price have 4 elements whereas june_price have 3 elements. So, we can't use dataframe in 
# this case to store all of these values in a single variable. Not to worry, lists is to rescue here. Using list, we can
# get almost all the advantages of dataframe in addition to its capacity of storing different set of elements (columns in
# case of dataframe) with different lengths.

all_prices_list2 = list(items, jan_price, mar_price, june_price)
all_prices_list2


all_prices_list2[2]
class(all_prices_list2[2])

all_prices_list2[[2]]
class(all_prices_list2[[2]])


# Looping in R
jan = all_prices4$jan_price
for(price in jan){
  print(price^2)
}

# Functions
square = function(data){
  for(price in jan){
    print(price^2)
  }
}

square(all_prices4$jan_price)

power_function = function(data, power){
  for(price in data){
    print(price^power)
  }
}

power_function(all_prices4$june_price, 4)

power_function2 = function(data, power){
  data^power
}

# apply
all_prices
apply(all_prices, 1, sd)
apply(all_prices, 2, sum)

# lapply]
# applying power_function2 on every element of june_price column of all_prices4 dataframe
lapply(all_prices4$june_price, power_function2, 4)

# Returning a vector after using lapply
unlist(lapply(all_prices4$june_price, power_function2, 4))


combined2 = list(matrix(c(jan_2018, mar_2018, june_2018), nrow = 3), 
                 matrix(c(jan_2017, mar_2017, june_2017), nrow = 3))
combined2

lapply(combined2, "[", 2, )

# sapply
# use sapply to apply a function over all elements of june_price and to return the result as a vector
sapply(all_prices4$june_price, power_function2, 4)


# tapply
# Define a dataframe for prices of different items in different months in different years
all_prices = data.frame(items = rep(c("potato", "rice", "oil"), 4), 
                        jan_price = c(10, 20, 30, 10, 18, 25, 9, 17, 24, 9, 19, 27), 
                        mar_price = c(11, 22, 33, 13, 25, 32, 12, 21, 33, 15, 27, 39), 
                        june_price = c(20, 25, 33, 21, 24, 40, 17, 22, 27, 13, 18, 23)
                        )
all_prices


# First convert items column to factor so that wer can use tapply()
all_prices$items = factor(all_prices$items)
str(all_prices)
tapply(all_prices$mar_price, factor(all_prices$items), mean)

# Make a scatter plot
# Simulate 50 normal random numbers and store as x
x = rnorm(50)
y = rnorm(50)
# pch = 19 stands for filled dot
plot(x, y, pch = 19, col = 'blue')

str(all_prices)
library(ggplot2)
ggplot(all_prices, aes(x = items, y = jan_price)) +
  geom_point()

ggplot(all_prices, aes(x = items, y = jan_price)) +
  geom_point() +
  geom_point(stat = "summary", fun.y = "mean", colour = "red", size = 3)

# Faceted plot
ggplot(all_prices, aes(x = jan_price, y = june_price)) +
  geom_point() +
  facet_grid(. ~ items) 

# Faceted scatter plots with linear model fit and confidence interval
ggplot(all_prices, aes(x = jan_price, y = june_price)) +
  geom_point() +
  facet_grid(. ~ items) +
  # se = TRUE inside stat_smooth() shows
  stat_smooth(method = "lm", se = TRUE, col = "red")