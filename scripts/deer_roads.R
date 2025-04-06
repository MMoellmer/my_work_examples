#A linear regression analysis of whether photo capture of deer is correlated to distance from roads

#rlang::last_trace() - run this code to identify placement of errors when they occur

library (ggplot2)
library(pander)
library(mosaic)
library(ggpubr)
library(car)

#organize data, discover linear equation
head(deer_roads)
model <- lm(Photos ~ Road, data = deer_roads)
model
#linear equation: Photos = 135.676 + (-0.234)(Road)

#a simple linear graph of the data
ggplot(deer_roads, aes(x = Road, y = Photos)) + geom_point() + stat_smooth()

#data with regression line
ggplot(deer_roads, aes(Road, Photos)) + geom_point() + stat_smooth(method = lm)
summary(model)
#Adjusted R^2 = 0.8037, P-value = <0.05, T-value = <0, N = 18
# to be statistically significant, p-value must be greater than 0.05 and t-value must be larger than 0
#F-stat and adjusted R^2 must be greater than 0 - the closer to 1 the better

#conclusion:There is a negative correlation between the number of deer pictures taken and their distances from the road
#Conclusion:This model is statistically significant and can be used to predict the number of photos of deer that are taken based on distance from the road
#note: deviation can be reduced by larger sample size N >= 30

