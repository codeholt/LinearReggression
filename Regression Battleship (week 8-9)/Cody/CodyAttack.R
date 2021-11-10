library(tidyverse)
library(car)
library(mosaic)

rbdata2 <- read.csv("rbdata.csv")


#model
mylm2 <- lm(y~x7+
              x3+x3:x7+
              x6+x6:x7,data=rbdata2)
summary(mylm2)
pairs(cbind(R=mylm2$residuals,fit=mylm2$fitted.values,rbdata2),pch=16, panel=panel.smooth)
#no x or y transformations
#or x8
#is x7

#not x1
#no more than 2 on off switches
#neither x9 or x10




#x10 looks promising
#x3 holds some significance