library(tidyverse)
library(car)
library(mosaic)

p1Data <- read.csv("p1Data-1.csv", header=TRUE)
p2Data <- read.csv("p2Data-1.csv", header=TRUE)
p3Data <- read.csv("p3Data-1.csv", header=TRUE)

#shows patterns between variables
#Ignore bottom and left
pairs(p1Data)


#shows potential pattern lines
pairs(p1Data,panel = panel.smooth)
#colors by X2 variable
pairs(p1Data,panel = panel.smooth, col=as.factor(p1Data$X2))

#obvious 2 lines model, but looking as if we didnt know
mylm1 <- lm(Y~X4,data=p1Data)
summary(mylm1)

#the dot iterates along each column in dataset
plot(mylm1$residuals~.,p1Data)

#we saw that X2 had a pattern that we hadn't already used
mylm2 <- lm(Y~X4+X2+X3,data=p1Data)#x3 isnt significant in this case
summary(mylm2)

mylm2 <- lm(Y~X4+X2+X2:X4,data=p1Data)#x2 isnt significant in this case
summary(mylm2)

mylm2 <- lm(Y~X4+X2:X4,data=p1Data)#the interaction adds significant change
summary(mylm2)
pairs(cbind(Res=mylm2$residuals,
            fit = mylm2$fitted.values, p1Data),
      panel=panel.smooth, col=as.factor(p1Data$X2))
