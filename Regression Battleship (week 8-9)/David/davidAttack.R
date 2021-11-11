library(tidyverse)
library(car)
library(mosaic)
rbdata1 <- read.csv("rbdata.csv")


#model
mylm1 <- lm(y~x7+I(x7^2)+I(x7^3)+
              x3+x3:x7+x3:I(x7^2)+I(x7^3):x3+
              
              x10:x3:x7
              ,data=rbdata1)

#+x9:x7+x9:I(x7^2)
summary(mylm1)
b <- coef(mylm1)
pairs(cbind(R=mylm1$residuals,fit=mylm1$fitted.values,rbdata1),pch=16, panel=panel.smooth,
      col = interaction(rbdata1$x3,rbdata1$x10))
table(rbdata1$x9)
#x7:x6 showed some significance
#uses polynomial x variable transformations
#no y transformations
#r^2 over point 8
#2 or more on off switches (at least 4 lines)
#x9 is an on off switch

rbdata1 %>% 
  ggplot(aes(x=x7,y=y, color = interaction(x3,x10)))+
  scale_fill_manual(values = palette())+
  scale_color_manual(values = palette())+
  geom_point()+
  #geom_smooth(method='lm', se=F, formula = y~poly(x,3))
  #x3=0,x10=0
  stat_function(fun=function(x) b[1]+b[2]*x+b[3]*x^2+b[4]*x^3, color = palette()[1])+
  #x3=1,x10=0
  stat_function(fun=function(x) (b[1]+b[5])+(b[2]+b[6])*x+(b[3]+b[7])*x^2+(b[4]+b[8])*x^3,color = palette()[2])+
  #x3=0,x10=1
  stat_function(fun=function(x) b[1]+(b[2]+b[9])*x+b[3]*x^2+b[4]*x^3, color = palette()[3])+
  #x3=1,x10=1
  stat_function(fun=function(x) (b[1]+b[5])+(b[2]+b[6]+b[9])*x+(b[3]+b[7])*x^2+(b[4]+b[8])*x^3,color = palette()[4])


palette(c("green", "limegreen","skyblue4","orange","orangered", "firebrick", "firebrick4","skyblue"))
