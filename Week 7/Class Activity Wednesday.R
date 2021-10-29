library(tidyverse)
library(mosaic)
library(car)

#creating the dataset
X <- c(1, 4,  7,   8,  10, 20)
Y <- c(3, 5, 18, 13, 12,   1)
w <- c(.2, .2, 1, 1, 1, 1)

mylm <- lm(Y ~ X, weights=w)
plot(Y ~ X, pch=21, bg=rgb(1-w,1-w,1-w), col="orange")
abline(mylm)

View(Utilities)

util.lm <- lm(gasbill~month+I(month^2),data=Utilities)
b <- util.lm$coefficients

#ggplot 
ggplot(Utilities, aes(x=month,y=gasbill))+
  geom_point(color = "orange")+
  stat_function(fun=function(x) b[1]+b[2]*x + b[3]*x^2, color="skyblue")+
  geom_smooth(method = "loess",se=T, color = "red")+
  theme_bw()

#base r
plot(gasbill~month,data=Utilities,
     pch = 16, col = "orange")
#Lowess Curve
lines(lowess(Utilities$month,
             Utilities$gasbill), col = "red")
curve(b[1]+b[2]*x + b[3]*x^2, add=TRUE, col = "skyblue")






