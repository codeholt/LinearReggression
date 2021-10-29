library(tidyverse)
library(car)
library(mosaic)
library(Ecdat)

?Wages1

wage.lm <- lm(wage~school+I(school^2), data=Wages1)
summary(wage.lm)



lob.lm <- lm(height~age+I(age^2)+I(age^3)+I(age^4)+I(age^5), data=Loblolly)

plot(lob.lm, which=1)

