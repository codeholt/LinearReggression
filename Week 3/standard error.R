library(tidyverse)
library(car)
library(pander)
#page56-57 bujo

speed.lm <- lm(dist~speed,data=cars)
summary(speed.lm)


sqrt((15.38^2)/sum((cars$speed-mean(cars$speed))^2))

#if predicted is law, here is 2 stdev away in each direction
confint(speed.lm)

