library(tidyverse)
library(car)
library(mosaic)

rbdata2 <- read.csv("rbdata.csv")
View(rbdata2)

#model
mylm2 <- lm(y~x7+I(x7^2)+I(x7^3)+
              x3+x3:x7+I(x7^2):x3+I(x7^3):x3
              ,data=rbdata2)
summary(mylm2)
b <- coef(mylm2)
pairs(cbind(R=mylm2$residuals,fit=mylm2$fitted.values,rbdata2),
      pch=16, panel=panel.smooth,
      col = interaction(rbdata2$x3))
#no x or y transformations
#or x8
#is x7

#not x1
#no more than 2 on off switches
#neither x9 or x10

table(rbdata2$x10)


#x10 looks promising
#x3 holds some significance
palette(c("skyblue","skyblue4","orange","orangered", "firebrick", "firebrick4", "green", "limegreen"))
rbdata2 %>% 
  ggplot(aes(x=x7,y=y, color = as.factor(x3))) +
  scale_fill_manual(values = palette())+
  scale_color_manual(values = palette())+
  geom_point()+
  stat_function(fun=function(x) b[1]+b[2]*x+b[3]*x^2+b[4]*x^3, color = "skyblue")+
  stat_function(fun=function(x) (b[1]+b[5])+(b[2]+b[6])*x+(b[3]+b[7])*x^2+(b[4]+b[8])*x^3)
